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

SUMMARY_MIN_BYTES = 10_000

# The main course text is extracted chapter-by-chapter elsewhere; when those
# chapter directories exist the whole-book PDF must not be extracted again.
LAWSON_BOOK = "Design and Analysis of Experiments With R - John Lawson.pdf"
LAWSON_CHAPTER_PARTS = (
    "John Lawson - Design and Analysis of Experiments With R",
    "C01-Introduction",
    "raw.txt",
)

MAX_SECTIONS = 40
MAX_FIGURES = 15
MAX_TABLES = 10
MAX_KEY_TERMS = 30
MAX_INTRO_LINES = 15
MAX_SECTION_LEN = 120
MIN_TERM_LEN = 3
MAX_TERM_LEN = 40

_SECTION_RE = re.compile(
    r'^(\d+\.[\d\.]*\s+[A-Z]|Chapter\s+\w+|[A-Z][a-z]+:|Section\s+)'
)
_PAGE_NUMBER_RE = re.compile(r'^\d+$')
_FIGURE_RE = re.compile(r'(Figure\s*[\d\.]+[\u2014\-\s].*?\n)')
_TABLE_RE = re.compile(r'(Table\s*[\d\.]+[\u2014\-\s].*?\n)')
_CAPITAL_TERM_RE = re.compile(r'\b([A-Z][a-z]+(?:\s+[A-Z][a-z]+)*)\b')


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

def _collect_sections(lines):
    """Return stripped section-like header lines (e.g. '2.3 Model', 'Chapter 4')."""
    return [
        stripped
        for stripped in (line.strip() for line in lines)
        if len(stripped) < MAX_SECTION_LEN and _SECTION_RE.match(stripped)
    ]

def _collect_intro(lines, limit=MAX_INTRO_LINES):
    """Return the first *limit* meaningful (non page-number) lines."""
    intro = []
    for line in lines:
        stripped = line.strip()
        if len(stripped) > 20 and not _PAGE_NUMBER_RE.match(stripped):
            intro.append(stripped)
            if len(intro) >= limit:
                break
    return intro

def _collect_top_terms(text, limit=50):
    """Return the most frequent capitalised multi-word terms in *text*."""
    word_freq = {}
    for word in _CAPITAL_TERM_RE.findall(text):
        word = word.strip()
        if MIN_TERM_LEN < len(word) < MAX_TERM_LEN:
            word_freq[word] = word_freq.get(word, 0) + 1
    return sorted(word_freq.items(), key=lambda item: -item[1])[:limit]

def _render_list(parts, title, items, limit):
    """Append a '## title' bullet list (first *limit* items) when non-empty."""
    if not items:
        return
    parts.append(f"## {title}\n")
    for item in items[:limit]:
        parts.append(f"- {item.strip()}\n")

def _render_section_outline(parts, sections):
    """Append the section outline, noting any sections beyond the cap."""
    _render_list(parts, "Section Outline", sections, MAX_SECTIONS)
    if len(sections) > MAX_SECTIONS:
        parts.append(f"- ... and {len(sections) - MAX_SECTIONS} more sections\n")

def _render_key_terms(parts, top_terms):
    """Append the key-terms list, each with the number of times it appears."""
    if not top_terms:
        return
    parts.append("## Key Terms\n")
    for term, freq in top_terms[:MAX_KEY_TERMS]:
        parts.append(f"- **{term}** (appears {freq} times)\n")

def simple_summarize(text, max_chars=3000):
    """
    Generate a summary using a simple extract-and-condense approach:
    - Extract section headings, figure captions, first/last paragraphs
    - Identify key tables and numbers
    - Return a structured markdown summary
    """
    lines = text.split('\n')
    sections = _collect_sections(lines)
    intro = _collect_intro(lines)
    figures = _FIGURE_RE.findall(text[:50000])
    tables = _TABLE_RE.findall(text[:50000])
    top_terms = _collect_top_terms(text)

    # Build summary
    summary_parts = [
        "## Overview\n",
        f"Document length: {len(text)} characters across {len(lines)} lines.\n",
    ]

    # Introduction
    if intro:
        summary_parts.append("## Introduction\n")
        summary_parts.append('\n'.join(intro[:10]) + "\n")

    _render_section_outline(summary_parts, sections)
    _render_list(summary_parts, "Figures", figures, MAX_FIGURES)
    _render_list(summary_parts, "Tables", tables, MAX_TABLES)
    _render_key_terms(summary_parts, top_terms)

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

def _find_pdf_files(directory):
    """Return every PDF under *directory* (any depth)."""
    pdf_files = []
    for root, _dirs, files in os.walk(directory):
        pdf_files.extend(
            os.path.join(root, name)
            for name in files
            if name.lower().endswith(".pdf")
        )
    return pdf_files

def _output_dir_for(pdf_path):
    """Return the directory extracted text for *pdf_path* should land in."""
    dirname = pdf_to_dirname(os.path.basename(pdf_path))
    rel_dir = os.path.dirname(pdf_path)

    # Extracted text lands beside the reference material
    # (reference/<dirname>/), never inside books/.
    flat_locations = (os.path.abspath(REFERENCE_DIR), os.path.abspath(BOOKS_DIR))
    if os.path.abspath(rel_dir) in flat_locations:
        return os.path.join(REFERENCE_DIR, dirname)
    # Keep relative subdirectory structure
    return os.path.join(rel_dir, dirname)

def _book_chapters_already_extracted(filename):
    """True when the main Lawson book's chapters were extracted separately."""
    if filename != LAWSON_BOOK:
        return False
    chapter_raw = os.path.join(REFERENCE_DIR, *LAWSON_CHAPTER_PARTS)
    return os.path.exists(chapter_raw)

def _extract_to_raw(pdf_path, raw_path):
    """Extract *pdf_path* into *raw_path* and return the extracted text."""
    text = extract_pdf_text(pdf_path)
    with open(raw_path, "w", encoding="utf-8", errors="replace") as f:
        f.write(text)
    return text

def _write_summary(out_dir, filename, raw_path, text):
    """Write summary.md for long extractions; return 1 when written, else 0."""
    if os.path.getsize(raw_path) <= SUMMARY_MIN_BYTES:
        return 0

    summary_path = os.path.join(out_dir, "summary.md")
    print(f"  -> Generating summary.md")

    # Use a chunked approach for very large texts
    summary = simple_summarize(text)

    with open(summary_path, "w", encoding="utf-8", errors="replace") as f:
        f.write(f"# Summary: {filename}\n\n")
        f.write(summary)

    print(f"  -> Summary: {os.path.getsize(summary_path)} bytes")
    return 1

def main():
    # Find all PDFs under reference/books/ (the reference tree's source files).
    pdf_files = sorted(_find_pdf_files(BOOKS_DIR))

    print(f"Found {len(pdf_files)} PDF files in {BOOKS_DIR}\n")

    extracted_count = 0
    summarized_count = 0
    skipped_count = 0

    for pdf_path in pdf_files:
        filename = os.path.basename(pdf_path)
        out_dir = _output_dir_for(pdf_path)

        os.makedirs(out_dir, exist_ok=True)
        raw_path = os.path.join(out_dir, "raw.txt")

        # Skip if already extracted (check for existing raw.txt with content)
        if os.path.exists(raw_path) and os.path.getsize(raw_path) > 0:
            file_size = os.path.getsize(raw_path)
            print(f"  SKIP (existing): {filename} -> {raw_path} ({file_size} bytes)")
            skipped_count += 1
            continue

        # Check if it's the main John Lawson book (chapters already extracted)
        if _book_chapters_already_extracted(filename):
            print(f"  SKIP (chapters already extracted): {filename}")
            skipped_count += 1
            continue

        print(f"  EXTRACTING: {filename} -> {raw_path}")

        try:
            text = _extract_to_raw(pdf_path, raw_path)
            file_size = os.path.getsize(raw_path)
            extracted_count += 1
            print(f"  -> {file_size} bytes, {text.count(chr(10))} lines")
            summarized_count += _write_summary(out_dir, filename, raw_path, text)
        except Exception as e:
            print(f"  ERROR: {e}")
            traceback.print_exc()

    print(f"\n{'='*60}")
    print(f"Done: {extracted_count} extracted, {summarized_count} summarized, {skipped_count} skipped")

if __name__ == "__main__":
    main()
