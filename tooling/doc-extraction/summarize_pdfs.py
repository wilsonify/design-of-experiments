#!/usr/bin/env python3
"""
Concise summary generator for PDF raw text files.

Instead of concatenating entire paragraphs (which produces bloated output),
this script:
  - Groups raw PDF lines into proper paragraphs (stripping page-number and
    page-header artifacts that PDF extraction injects mid-text)
  - Samples the first sentence of each major paragraph for the Introduction
  - Limits "Key Concepts" to ~200 words by sampling condensed excerpts from
    the most information-dense paragraphs
  - Extracts formulas as grouped blocks (consecutive math lines joined)
  - Extracts R functions, R packages, figures, and tables as tight lists
  - Keeps total summary size reasonable (~2–7 KB per document)

Usage:
    python summarize_pdfs.py            # scan reference/ and archive/course/extracted/
    python summarize_pdfs.py <dir>      # summarize only <dir>/raw.txt
"""

import os
import re
import sys

# Resolve the reference/archive trees from the repository root so the tooling
# is portable and does not depend on the caller's working directory.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from doe_paths import default_targets  # noqa: E402

MIN_SIZE_FOR_SUMMARY = 10_000        # only summarize raw.txt > 10 KB
MAX_KEY_CONCEPTS_WORDS = 200         # ~200 words for the Key Concepts section
MAX_SECTION_OUTLINE = 15             # cap number of section-level one-liners
MAX_INTRO_SENTENCES = 6              # cap number of intro sentences shown

# Matches PDF page-header footer artefacts like "2 INTRODUCTION" or "12 INTRODUCTION"
_PAGE_HEADER_RE = re.compile(r'^\d+\s+[A-Z][A-Z\s&-]+$')
_ISBN_RE = re.compile(r'^ISBN\s', re.IGNORECASE)
_LICENSE_RE = re.compile(
    r'(creative\s+commons|©|\(c\)|copyright|attribut|'
    r'conditions are met|all rights reserved|'
    r'complete description of the license|'
    r'you (must|may not|can) |build upon|share.?alike)',
    re.IGNORECASE,
)

# Targeted filter for CC license/copyright block lines (paragraph extraction).
# Intentionally narrower than _LICENSE_RE — excludes "attribut" to avoid false
# positives on academic words like "attributable".
_CC_BLOCK_RE = re.compile(
    r'(creative\s+commons|©|\(c\)|copyright|'
    r'all rights reserved|complete description of the license|'
    r'conditions are met|you (must|may not|can) |'
    r'build upon|share.?alike|'
    r'this work is licensed|http://creativecommons\.org|'
    r'provided the following)',
    re.IGNORECASE,
)

# Sentence splitter: splits on ". " / "? " / "! " boundaries
_SENT_RE = re.compile(r'(?<=[.!?])\s+')

# Math-symbol characters that indicate a formula line
_MATH_CHARS = r'αβσμχ²τργδεθλνπω'
_MATH_RE = re.compile(
    r'[=~≤≥±√∑∫∏' + _MATH_CHARS + r'\\]'
)


# --------------------------------------------------------------------------- #
#  Paragraph / sentence helpers
# --------------------------------------------------------------------------- #
def split_into_sentences(text: str) -> list[str]:
    """Split a chunk of text into individual sentences."""
    text = re.sub(r'\s+', ' ', text).strip()
    sents = _SENT_RE.split(text)
    return [s for s in sents if s and len(s) > 5]


def _is_skippable_line(stripped: str) -> bool:
    """Return True for lines that are PDF artefacts (page numbers, headers)."""
    if not stripped:
        return True
    # Bare page number
    if re.match(r'^\d+$', stripped):
        return True
    # Page header: "12 INTRODUCTION" or "4 CHAPTER TITLE"
    if _PAGE_HEADER_RE.match(stripped):
        return True
    # Page marker injected by extract_all_pdfs.py
    if stripped.startswith('--- PAGE'):
        return True
    # "This page intentionally left blank"
    if 'intentionally left blank' in stripped.lower():
        return True
    # Skip Creative Commons license / copyright block lines
    if _CC_BLOCK_RE.search(stripped):
        return True
    return False


def extract_paragraphs(text: str) -> list[str]:
    """
    Group raw PDF text lines into clean paragraphs.

    PDF extraction inserts line-breaks mid-word/mid-sentence and page-number /
    page-header lines.  Page-header and page-number lines are silently dropped
    (without flushing the current paragraph) so that text flowing across pages
    is re-joined correctly.  Genuine blank lines terminate paragraphs.
    """
    raw_lines = text.split('\n')
    paragraphs: list[str] = []
    current: list[str] = []

    for line in raw_lines:
        stripped = line.strip()

        if _is_skippable_line(stripped):
            # Artefact line: skip WITHOUT flushing — the paragraph likely
            # continues on the next page.
            continue

        if not stripped:
            # blank line → end of paragraph
            if current and len(' '.join(current)) > 30:
                paragraphs.append(' '.join(current))
            current = []
        else:
            current.append(stripped)

    if current and len(' '.join(current)) > 30:
        paragraphs.append(' '.join(current))

    # Merge very short paragraphs (likely PDF artefacts) into neighbours
    merged: list[str] = []
    for p in paragraphs:
        if len(p) < 40 and merged:
            merged[-1] += ' ' + p
        else:
            merged.append(p)

    # Filter out paragraphs with too few words
    clean = [p for p in merged if len(p.split()) >= 6]
    return clean


def extract_headings(text: str) -> list[str]:
    """
    Extract section/subsection headings from the text.

    Filters out page-header artefacts, ISBN lines, and license text.
    """
    headings = []
    lines = text.split('\n')
    for line in lines:
        stripped = line.strip()

        # Must look like a heading (starts with a number + capital, or is all-caps)
        looks_like_heading = (
            re.match(r'^\d+[\.\d]*\s+[A-Z]', stripped)
            or re.match(r'^[A-Z][A-Z\s&-]{4,}$', stripped)
            or re.match(r'^Section\s+\d+', stripped, re.IGNORECASE)
            or re.match(r'^Chapter\s+\d+', stripped, re.IGNORECASE)
        )

        if not looks_like_heading:
            continue
        if len(stripped) > 140:
            continue

        # Filter out page-header artefacts like "2 INTRODUCTION", "4 INTRODUCTION"
        if _PAGE_HEADER_RE.match(stripped):
            continue

        # Filter out ISBN and licence lines
        if _ISBN_RE.match(stripped) or _LICENSE_RE.search(stripped):
            continue

        # Strip trailing page numbers from headings: "10.1 Discussion 145" → "10.1 Discussion"
        # Done BEFORE the all-caps check so "CHAPTER 1" → "CHAPTER" is caught.
        stripped = re.sub(r'\s+\d+$', '', stripped)

        # Skip single-word all-caps headings (e.g., "CHAPTER", "CONTENTS")
        # Check AFTER stripping trailing page numbers.
        if re.match(r'^[A-Z]+$', stripped):
            continue

        headings.append(stripped)

    # De-duplicate while preserving order
    seen: set[str] = set()
    unique: list[str] = []
    for h in headings:
        if h not in seen:
            seen.add(h)
            unique.append(h)
    return unique


def extract_first_sentences(paragraphs: list[str], n: int = MAX_INTRO_SENTENCES) -> list[str]:
    """Return the first sentence of each of the first *n* clean paragraphs."""
    result = []
    for p in paragraphs[:n]:
        sents = split_into_sentences(p)
        if sents:
            first = sents[0].strip()
            # Skip sentences that are too short or look like table-of-content entries
            if len(first.split()) < 4:
                continue
            result.append(first)
    return result


def extract_key_concepts(paragraphs: list[str], max_words: int = MAX_KEY_CONCEPTS_WORDS) -> str:
    """
    Build a ~200-word key-concepts blurb.

    Takes the first sentence of the longest paragraphs (longer paragraphs are
    more likely to contain rich, complete ideas) until *max_words* is reached.
    """
    sorted_paras = sorted(paragraphs, key=len, reverse=True)

    collected: list[str] = []
    word_total = 0

    for p in sorted_paras:
        sents = split_into_sentences(p)
        for s in sents:
            words = s.split()
            if word_total + len(words) > max_words:
                remaining = max_words - word_total
                if remaining > 20:
                    collected.append(' '.join(words[:remaining]))
                return ' '.join(collected).strip()
            collected.append(s)
            word_total += len(words)
        if word_total >= max_words:
            break

    return ' '.join(collected).strip()


def extract_formulas(text: str) -> list[str]:
    """
    Extract mathematical / statistical formulas.

    Groups consecutive lines containing math symbols into single formula
    blocks (PDF formulas often span multiple lines).
    """
    lines = text.split('\n')
    formulas: list[str] = []
    current_block: list[str] = []

    for line in lines:
        stripped = line.strip()
        if _MATH_RE.search(stripped) and 10 < len(stripped) < 400:
            current_block.append(stripped)
        else:
            if current_block:
                combined = ' '.join(current_block)
                if len(combined) > 15:
                    formulas.append(combined)
                current_block = []

    if current_block:
        combined = ' '.join(current_block)
        if len(combined) > 15:
            formulas.append(combined)

    # De-duplicate
    seen: set[str] = set()
    unique = []
    for f in formulas:
        if f not in seen:
            seen.add(f)
            unique.append(f)
    return unique[:20]


def extract_r_functions(text: str) -> list[str]:
    """Extract R function calls (e.g., lm(, anova(, etc.)."""
    r_funcs = re.findall(r'\b[a-z][a-zA-Z_]+\(', text)
    unique: list[str] = []
    for f in r_funcs:
        name = f.strip()
        if name not in unique and len(unique) < 30:
            unique.append(name)
    return unique


def extract_r_packages(text: str) -> list[str]:
    """Extract R package names from library()/require() calls."""
    packages = re.findall(r'(?:library|require)\s*\(\s*[\'"](\w+)[\'"]\s*\)', text)
    unique: list[str] = []
    for p in packages:
        if p not in unique:
            unique.append(p)
    return unique


def extract_figures(text: str) -> list[str]:
    """Extract Figure references."""
    figs = re.findall(r'Figure\s+\d+[\.\d]*[^\n]{0,120}', text)
    # Keep only those that look like captions, not page-header noise
    clean = [f.strip() for f in figs if len(f.strip()) > 10]
    # De-duplicate
    seen: set[str] = set()
    unique = []
    for f in clean:
        if f not in seen:
            seen.add(f)
            unique.append(f)
    return unique[:12]


def extract_tables(text: str) -> list[str]:
    """Extract Table references."""
    tabs = re.findall(r'Table\s+\d+[\.\d]*[^\n]{0,120}', text)
    clean = [t.strip() for t in tabs if len(t.strip()) > 10]
    seen: set[str] = set()
    unique = []
    for t in clean:
        if t not in seen:
            seen.add(t)
            unique.append(t)
    return unique[:10]


# --------------------------------------------------------------------------- #
#  Summary builder
# --------------------------------------------------------------------------- #
def generate_summary(text: str, filename: str) -> str:
    """Produce a concise markdown summary for a single raw.txt file."""
    lines = text.split('\n')
    paragraphs = extract_paragraphs(text)
    headings = extract_headings(text)[:MAX_SECTION_OUTLINE]

    parts: list[str] = []

    # ---- Overview ----
    parts.append("## Overview\n\n")
    parts.append(f"- **Source**: {filename}\n")
    parts.append(f"- **Length**: {len(text):,} characters, {len(lines):,} lines, "
                 f"{len(paragraphs)} paragraphs\n")
    parts.append(f"- **Sections**: {len(headings)} headings detected\n\n")

    # ---- Introduction (first sentences of first few paragraphs) ----
    intro_sents = extract_first_sentences(paragraphs)
    if intro_sents:
        parts.append("## Introduction\n\n")
        for s in intro_sents:
            parts.append(f"- {s}\n")
        parts.append("\n")

    # ---- Section outline ----
    if headings:
        parts.append("## Section Outline\n\n")
        for h in headings:
            parts.append(f"- {h}\n")
        if len(headings) >= MAX_SECTION_OUTLINE:
            parts.append(f"- ... and more sections\n")
        parts.append("\n")

    # ---- Key Concepts (~200 words) ----
    key_concepts = extract_key_concepts(paragraphs)
    if key_concepts:
        parts.append("## Key Concepts\n\n")
        parts.append(f"{key_concepts}\n\n")

    # ---- Formulas ----
    formulas = extract_formulas(text)
    if formulas:
        parts.append("## Key Formulas\n\n")
        for f in formulas:
            parts.append(f"- `{f}`\n")
        parts.append("\n")

    # ---- R Functions ----
    r_funcs = extract_r_functions(text)
    if r_funcs:
        parts.append("## R Functions\n\n")
        parts.append(", ".join(r_funcs[:25]) + "\n\n")

    # ---- R Packages ----
    r_pkgs = extract_r_packages(text)
    if r_pkgs:
        parts.append("## R Packages\n\n")
        parts.append(", ".join(r_pkgs) + "\n\n")

    # ---- Figures ----
    figures = extract_figures(text)
    if figures:
        parts.append("## Figures\n\n")
        for fig in figures:
            parts.append(f"- {fig}\n")
        parts.append("\n")

    # ---- Tables ----
    tables = extract_tables(text)
    if tables:
        parts.append("## Tables\n\n")
        for tbl in tables:
            parts.append(f"- {tbl}\n")

    return ''.join(parts)


# --------------------------------------------------------------------------- #
#  Directory processing
# --------------------------------------------------------------------------- #
def process_directory(dirpath: str) -> tuple[int, str]:
    """Process a single directory that contains raw.txt."""
    raw_path = os.path.join(dirpath, "raw.txt")
    summary_path = os.path.join(dirpath, "summary.md")

    if not os.path.exists(raw_path):
        return 0, "no raw.txt"

    file_size = os.path.getsize(raw_path)

    # Only summarize if > 10 KB
    if file_size <= MIN_SIZE_FOR_SUMMARY:
        return file_size, "skipped (small)"

    with open(raw_path, 'r', encoding='utf-8', errors='replace') as f:
        text = f.read()

    filename = os.path.basename(dirpath)
    summary = generate_summary(text, filename)

    with open(summary_path, 'w', encoding='utf-8', errors='replace') as f:
        f.write(f"# Summary: {filename}\n\n")
        f.write(summary)

    return file_size, f"summary written ({os.path.getsize(summary_path):,} bytes)"


def main():
    """Walk the reference and archive trees and summarize every raw.txt."""
    # Allow targeting specific directories; default to the whole corpus.
    targets = sys.argv[1:] if len(sys.argv) > 1 else default_targets()

    results: list[tuple[str, int, str]] = []

    for target in targets:
        if os.path.isdir(target):
            for root, dirs, files in os.walk(target):
                if "raw.txt" in files:
                    size, status = process_directory(root)
                    if size > 0:
                        results.append((root, size, status))
        else:
            # Treat as a directory containing raw.txt
            dirpath = target
            size, status = process_directory(dirpath)
            if size > 0:
                results.append((dirpath, size, status))

    # Print results sorted by file size (largest first)
    print(f"Processed {len(results)} files with raw.txt\n")
    for dirpath, size, status in sorted(results, key=lambda x: -x[1]):
        dirname = os.path.basename(dirpath)
        print(f"  {dirname:50s} {size:>10,} bytes  {status}")


if __name__ == "__main__":
    main()
