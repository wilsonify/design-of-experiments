#!/usr/bin/env python3
"""
Extract text from all PDFs in docs/ and generate organized raw text + summaries.

For each PDF:
  - Creates a sanitized subdirectory under docs/
  - Saves raw.txt (full text)
  - Saves summary.md (AI-style summary) if raw text > 10KB
"""

import os
import re
import sys
import io
import traceback

# Resolve the reference tree relative to the repository root, not this script,
# so moving the tooling does not silently change what it scans or writes.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from doe_paths import BOOKS_DIR, REFERENCE_DIR  # noqa: E402

# ---- Helper: sanitize PDF filename to directory name ----
def pdf_to_dirname(filename):
    """Convert a PDF filename to a suitable directory name."""
    name = filename
    # Remove .pdf extension
    if name.lower().endswith(".pdf"):
        name = name[:-4]
    # Replace spaces and special chars with dashes, keep alphanumeric
    name = re.sub(r'[\s_]+', '-', name.strip())
    name = re.sub(r'[^a-zA-Z0-9\-()]', '', name)
    return name

def chunk_text(text, max_chars=4000):
    """Split text into chunks for processing."""
    chunks = []
    paragraphs = text.split('\n')
    current = []
    current_len = 0
    for para in paragraphs:
        if current_len + len(para) > max_chars and current:
            chunks.append('\n'.join(current))
            current = [para]
            current_len = len(para)
        else:
            current.append(para)
            current_len += len(para)
    if current:
        chunks.append('\n'.join(current))
    return chunks

def simple_summarize(text, max_chars=3000):
    """
    Generate a summary using a simple extract-and-condense approach:
    - Extract section headings, figure captions, first/last paragraphs
    - Identify key tables and numbers
    - Return a structured markdown summary
    """
    lines = text.split('\n')

    # Collect structural info
    sections = []
    for line in lines:
        stripped = line.strip()
        # Match section-like headers (e.g., "1.", "A.1", "Chapter", "Section")
        if re.match(r'^(\d+\.[\d\.]*\s+[A-Z]|Chapter\s+\w+|[A-Z][a-z]+:|Section\s+)', stripped) and len(stripped) < 120:
            sections.append(stripped)

    # Extract first 20 meaningful lines (intro/exposition)
    intro = []
    for line in lines:
        stripped = line.strip()
        if stripped and len(stripped) > 20 and not re.match(r'^\d+$', stripped):
            intro.append(stripped)
        if len(intro) >= 15:
            break

    # Extract figure/table references and key terms
    figures = re.findall(r'(Figure\s*[\d\.]+[\u2014\-\s].*?\n)', text[:50000])
    tables = re.findall(r'(Table\s*[\d\.]+[\u2014\-\s].*?\n)', text[:50000])

    # Collect unique keywords (capitalized terms that appear frequently)
    words = re.findall(r'\b([A-Z][a-z]+(?:\s+[A-Z][a-z]+)*)\b', text)
    word_freq = {}
    for w in words:
        w = w.strip()
        if 3 < len(w) < 40:
            word_freq[w] = word_freq.get(w, 0) + 1
    top_terms = sorted(word_freq.items(), key=lambda x: -x[1])[:50]

    # Build summary
    summary_parts = []

    # Title from first content
    summary_parts.append(f"## Overview\n")
    summary_parts.append(f"Document length: {len(text)} characters across {len(lines)} lines.\n")

    # Introduction
    if intro:
        summary_parts.append("## Introduction\n")
        summary_parts.append('\n'.join(intro[:10]) + "\n")

    # Section outline
    if sections:
        summary_parts.append("## Section Outline\n")
        for s in sections[:40]:
            summary_parts.append(f"- {s}\n")
        if len(sections) > 40:
            summary_parts.append(f"- ... and {len(sections)-40} more sections\n")

    # Figures
    if figures:
        summary_parts.append("## Figures\n")
        for f in figures[:15]:
            summary_parts.append(f"- {f.strip()}\n")

    # Tables
    if tables:
        summary_parts.append("## Tables\n")
        for t in tables[:10]:
            summary_parts.append(f"- {t.strip()}\n")

    # Key terms
    if top_terms:
        summary_parts.append("## Key Terms\n")
        for term, freq in top_terms[:30]:
            summary_parts.append(f"- **{term}** (appears {freq} times)\n")

    return ''.join(summary_parts)

def extract_pdf_text(filepath):
    """Extract text from a PDF using pymupdf (fitz) or pypdf."""
    try:
        import fitz  # pymupdf
        doc = fitz.open(filepath)
        text = ""
        for i, page in enumerate(doc):
            text += f"\n\n--- PAGE {i+1} ---\n\n"
            text += page.get_text()
        doc.close()
        return text
    except ImportError:
        pass

    try:
        import pypdf
        reader = pypdf.PdfReader(filepath)
        text = ""
        for i, page in enumerate(reader.pages):
            text += f"\n\n--- PAGE {i+1} ---\n\n"
            text += page.extract_text() or ""
        return text
    except ImportError:
        pass

    raise RuntimeError("No PDF library available (need pymupdf or pypdf)")

def main():
    # Find all PDFs under reference/books/ (the reference tree's source files).
    pdf_files = []
    for root, dirs, files in os.walk(BOOKS_DIR):
        for f in files:
            if f.lower().endswith(".pdf"):
                pdf_files.append(os.path.join(root, f))

    print(f"Found {len(pdf_files)} PDF files in {BOOKS_DIR}\n")

    # Sort for consistent output
    pdf_files.sort()

    extracted_count = 0
    summarized_count = 0
    skipped_count = 0

    for pdf_path in pdf_files:
        filename = os.path.basename(pdf_path)
        dirname = pdf_to_dirname(filename)

        # Determine output directory: extracted text lands beside the
        # reference material (reference/<dirname>/), never inside books/.
        rel_dir = os.path.dirname(pdf_path)
        if os.path.abspath(rel_dir) in (
            os.path.abspath(REFERENCE_DIR),
            os.path.abspath(BOOKS_DIR),
        ):
            out_dir = os.path.join(REFERENCE_DIR, dirname)
        else:
            # Keep relative subdirectory structure
            out_dir = os.path.join(rel_dir, dirname)

        os.makedirs(out_dir, exist_ok=True)
        raw_path = os.path.join(out_dir, "raw.txt")

        # Skip if already extracted (check for existing raw.txt with content)
        if os.path.exists(raw_path) and os.path.getsize(raw_path) > 0:
            file_size = os.path.getsize(raw_path)
            print(f"  SKIP (existing): {filename} -> {raw_path} ({file_size} bytes)")
            skipped_count += 1
            continue

        # Check if it's the main John Lawson book (chapters already extracted)
        if "Design and Analysis of Experiments With R - John Lawson.pdf" == filename:
            # Check if chapter subdirs already exist
            if os.path.exists(os.path.join(REFERENCE_DIR, "John Lawson - Design and Analysis of Experiments With R", "C01-Introduction", "raw.txt")):
                print(f"  SKIP (chapters already extracted): {filename}")
                skipped_count += 1
                continue

        print(f"  EXTRACTING: {filename} -> {raw_path}")

        try:
            text = extract_pdf_text(pdf_path)
            with open(raw_path, "w", encoding="utf-8", errors="replace") as f:
                f.write(text)

            file_size = os.path.getsize(raw_path)
            extracted_count += 1
            print(f"  -> {file_size} bytes, {text.count(chr(10))} lines")

            # Generate summary for long texts (> 10KB)
            if file_size > 10000:
                summary_path = os.path.join(out_dir, "summary.md")
                print(f"  -> Generating summary.md")

                # Use a chunked approach for very large texts
                summary = simple_summarize(text)

                with open(summary_path, "w", encoding="utf-8", errors="replace") as f:
                    f.write(f"# Summary: {filename}\n\n")
                    f.write(summary)

                summarized_count += 1
                print(f"  -> Summary: {os.path.getsize(summary_path)} bytes")

        except Exception as e:
            print(f"  ERROR: {e}")
            traceback.print_exc()

    print(f"\n{'='*60}")
    print(f"Done: {extracted_count} extracted, {summarized_count} summarized, {skipped_count} skipped")

if __name__ == "__main__":
    main()
