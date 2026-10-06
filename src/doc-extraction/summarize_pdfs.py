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

RAW_FILENAME = "raw.txt"
SUMMARY_FILENAME = "summary.md"

MIN_SIZE_FOR_SUMMARY = 10_000        # only summarize raw.txt > 10 KB
MAX_KEY_CONCEPTS_WORDS = 200         # ~200 words for the Key Concepts section
MAX_SECTION_OUTLINE = 15             # cap number of section-level one-liners
MAX_INTRO_SENTENCES = 6              # cap number of intro sentences shown

MAX_HEADING_LEN = 140                # headings longer than this are prose
MIN_PARAGRAPH_LEN = 30               # merged paragraph must be longer than this
MAX_FORMULA_LINES = 400              # longest line still considered a formula
MAX_FORMULAS = 20                    # cap number of formulas listed
MAX_FIGURES = 12                     # cap number of figures listed
MAX_TABLES = 10                      # cap number of tables listed
MAX_R_FUNCTIONS = 30                 # cap number of R functions listed

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

# Section headings: numbered ("3.2 Model"), all-caps ("CHAPTER TWO"), or
# labelled ("Section 4", "Chapter 4").
_HEADING_RES = (
    re.compile(r'^\d[\d.]*\s+[A-Z]'),
    re.compile(r'^[A-Z][A-Z\s&-]{4,}$'),
    re.compile(r'^Section\s+\d+', re.IGNORECASE),
    re.compile(r'^Chapter\s+\d+', re.IGNORECASE),
)
# Trailing page-number artefact: "10.1 Discussion 145" -> "10.1 Discussion".
# Only ever applied to lines already capped at MAX_HEADING_LEN characters, so the
# bounded whitespace run matches exactly what "\s+" would; the open-ended form
# makes a partial ".sub()" match backtrack polynomially (Sonar S8786).
_TRAILING_PAGE_NUMBER_RE = re.compile(r'\s{1,200}\d+$')
_SHOUTED_WORD_RE = re.compile(r'^[A-Z]+$')

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


def _flush_paragraph(paragraphs: list[str], current: list[str]) -> None:
    """Append the buffered lines to *paragraphs* when they form a real block."""
    if current and len(' '.join(current)) > MIN_PARAGRAPH_LEN:
        paragraphs.append(' '.join(current))


def extract_paragraphs(text: str) -> list[str]:
    """
    Group raw PDF text lines into clean paragraphs.

    PDF extraction inserts line-breaks mid-word/mid-sentence and page-number /
    page-header lines.  ``_is_skippable_line`` treats those — and blank lines —
    as artefacts, so every artefact line is dropped without flushing (the text
    flowing across a page break is re-joined) and the surviving lines are
    grouped into paragraph blocks.
    """
    paragraphs: list[str] = []
    current: list[str] = []

    for line in text.split('\n'):
        stripped = line.strip()
        if _is_skippable_line(stripped):
            continue
        current.append(stripped)

    _flush_paragraph(paragraphs, current)

    # Merge very short paragraphs (likely PDF artefacts) into neighbours
    merged: list[str] = []
    for p in paragraphs:
        if len(p) < 40 and merged:
            merged[-1] += ' ' + p
        else:
            merged.append(p)

    # Filter out paragraphs with too few words
    return [p for p in merged if len(p.split()) >= 6]


def _looks_like_heading(stripped: str) -> bool:
    """True when a line has the shape of a section/subsection heading."""
    return any(pattern.match(stripped) for pattern in _HEADING_RES)


def _is_heading_noise(stripped: str) -> bool:
    """True for page-header, ISBN, or licence lines masquerading as headings."""
    if _PAGE_HEADER_RE.match(stripped):
        return True
    return bool(_ISBN_RE.match(stripped) or _LICENSE_RE.search(stripped))


def extract_headings(text: str) -> list[str]:
    """
    Extract section/subsection headings from the text.

    Filters out page-header artefacts, ISBN lines, and license text.
    """
    headings = []

    for line in text.split('\n'):
        stripped = line.strip()

        if not _looks_like_heading(stripped):
            continue
        if len(stripped) > MAX_HEADING_LEN:
            continue
        if _is_heading_noise(stripped):
            continue

        # Strip trailing page numbers from headings: "10.1 Discussion 145" → "10.1 Discussion"
        # Done BEFORE the all-caps check so "CHAPTER 1" → "CHAPTER" is caught.
        stripped = _TRAILING_PAGE_NUMBER_RE.sub('', stripped)

        # Skip single-word all-caps headings (e.g., "CHAPTER", "CONTENTS")
        # Check AFTER stripping trailing page numbers.
        if _SHOUTED_WORD_RE.match(stripped):
            continue

        headings.append(stripped)

    return _dedupe_preserving_order(headings)


def _dedupe_preserving_order(items: list[str]) -> list[str]:
    """Return *items* without duplicates, keeping first-seen order."""
    seen: set[str] = set()
    unique: list[str] = []
    for item in items:
        if item not in seen:
            seen.add(item)
            unique.append(item)
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


def _is_formula_line(stripped: str) -> bool:
    """True when a line looks like (part of) a formula."""
    return bool(_MATH_RE.search(stripped)) and 10 < len(stripped) < MAX_FORMULA_LINES


def _flush_formula_block(formulas: list[str], block: list[str]) -> None:
    """Append a completed multi-line formula block when it is substantial."""
    if not block:
        return
    combined = ' '.join(block)
    if len(combined) > 15:
        formulas.append(combined)


def extract_formulas(text: str) -> list[str]:
    """
    Extract mathematical / statistical formulas.

    Groups consecutive lines containing math symbols into single formula
    blocks (PDF formulas often span multiple lines).
    """
    formulas: list[str] = []
    block: list[str] = []

    for line in text.split('\n'):
        stripped = line.strip()
        if _is_formula_line(stripped):
            block.append(stripped)
            continue
        _flush_formula_block(formulas, block)
        block = []

    _flush_formula_block(formulas, block)
    return _dedupe_preserving_order(formulas)[:MAX_FORMULAS]


def extract_r_functions(text: str) -> list[str]:
    """Extract R function calls (e.g., lm(, anova(, etc.)."""
    r_funcs = re.findall(r'\b[a-z][a-zA-Z_]+\(', text)
    unique: list[str] = []
    for f in r_funcs:
        name = f.strip()
        if name not in unique and len(unique) < MAX_R_FUNCTIONS:
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
    return _dedupe_preserving_order(clean)[:MAX_FIGURES]


def extract_tables(text: str) -> list[str]:
    """Extract Table references."""
    tabs = re.findall(r'Table\s+\d+[\.\d]*[^\n]{0,120}', text)
    clean = [t.strip() for t in tabs if len(t.strip()) > 10]
    return _dedupe_preserving_order(clean)[:MAX_TABLES]


# --------------------------------------------------------------------------- #
#  Summary builder
# --------------------------------------------------------------------------- #
def _append_section(parts: list[str], title: str, rendered_items: list[str],
                    trailing_blank: bool = True) -> None:
    """Append a markdown section unless it has no content."""
    if not rendered_items:
        return
    parts.append(f"## {title}\n\n")
    parts.extend(rendered_items)
    if trailing_blank:
        parts.append("\n")


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
    _append_section(parts, "Introduction", [f"- {s}\n" for s in intro_sents])

    # ---- Section outline ----
    outline = [f"- {h}\n" for h in headings]
    if len(headings) >= MAX_SECTION_OUTLINE:
        outline.append("- ... and more sections\n")
    _append_section(parts, "Section Outline", outline)

    # ---- Key Concepts (~200 words) ----
    key_concepts = extract_key_concepts(paragraphs)
    if key_concepts:
        parts.append("## Key Concepts\n\n")
        parts.append(f"{key_concepts}\n\n")

    # ---- Formulas ----
    formulas = extract_formulas(text)
    _append_section(parts, "Key Formulas", [f"- `{f}`\n" for f in formulas])

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
    _append_section(parts, "Figures", [f"- {fig}\n" for fig in figures])

    # ---- Tables (no trailing blank line, unlike the other sections) ----
    tables = extract_tables(text)
    _append_section(parts, "Tables", [f"- {tbl}\n" for tbl in tables],
                    trailing_blank=False)

    return ''.join(parts)


# --------------------------------------------------------------------------- #
#  Directory processing
# --------------------------------------------------------------------------- #
def process_directory(dirpath: str) -> tuple[int, str]:
    """Process a single directory that contains raw.txt."""
    raw_path = os.path.join(dirpath, RAW_FILENAME)
    summary_path = os.path.join(dirpath, SUMMARY_FILENAME)

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


def _record_result(results: list[tuple[str, int, str]], dirpath: str) -> None:
    """Summarize *dirpath* and record it when it holds a sizable raw.txt."""
    size, status = process_directory(dirpath)
    if size > 0:
        results.append((dirpath, size, status))


def _collect_results(results: list[tuple[str, int, str]], target: str) -> None:
    """Record every raw.txt under *target* (or *target* itself)."""
    if not os.path.isdir(target):
        # Treat as a directory containing raw.txt
        _record_result(results, target)
        return
    for root, dirs, files in os.walk(target):
        if RAW_FILENAME in files:
            _record_result(results, root)


def main():
    """Walk the reference and archive trees and summarize every raw.txt."""
    # Allow targeting specific directories; default to the whole corpus.
    targets = sys.argv[1:] if len(sys.argv) > 1 else default_targets()

    results: list[tuple[str, int, str]] = []
    for target in targets:
        _collect_results(results, target)

    # Print results sorted by file size (largest first)
    print(f"Processed {len(results)} files with raw.txt\n")
    for dirpath, size, status in sorted(results, key=lambda x: -x[1]):
        dirname = os.path.basename(dirpath)
        print(f"  {dirname:50s} {size:>10,} bytes  {status}")


if __name__ == "__main__":
    main()
