#!/usr/bin/env python3
"""
Extract text from all PPTX files in reference/slides/ and generate organized raw text + summaries.

For each PPTX:
  - Creates reference/slides/<name>/ subdirectory
  - Saves raw.txt (all slide text, slide by slide)
  - Saves summary.md if raw text > 10KB

Slides are order-sensitive, so raw.txt is prefixed with slide numbers and
separators. Text is pulled from text frames, tables, grouped shapes, and
speaker notes.
"""

import os
import re
import sys
import traceback

try:
    from pptx import Presentation
    from pptx.util import Emu
except ImportError:
    print("python-pptx is required: pip install python-pptx")
    sys.exit(1)

# Resolved from the repository root, not the current working directory.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from doe_paths import SLIDES_DIR  # noqa: E402

SUMMARY_MIN_BYTES = 10_000
GROUP_SHAPE_TYPE = 6
NOTES_MARKER = "[notes]"
FOOTER_MARKER = "Design & Analysis of Experiments"
SOFT_LINE_BREAK = "\x0b"

# ---- Helper: sanitize pptx filename to directory name ----
def pptx_to_dirname(filename):
    """Convert a PPTX filename to a suitable directory name."""
    name = filename
    # Remove .pptx extension
    if name.lower().endswith(".pptx"):
        name = name[:-5]
    # Replace spaces and special chars with dashes, keep alphanumeric
    name = re.sub(r'[\s_]+', '-', name.strip())
    name = re.sub(r'[^a-zA-Z0-9\-()]', '', name)
    return name

def shape_sort_key(shape):
    """Order shapes top-to-bottom, then left-to-right."""
    try:
        top = shape.top if shape.top is not None else 0
        left = shape.left if shape.left is not None else 0
        return (top, left)
    except Exception:
        return (0, 0)

def _group_members(shape):
    """Return the member shapes of a group shape (empty list when not a group)."""
    is_group = shape.shape_type == GROUP_SHAPE_TYPE or (
        getattr(shape, "shapes", None) is not None and hasattr(shape, "shapes")
    )
    if not is_group:
        return []
    try:
        return list(shape.shapes)
    except (AttributeError, NotImplementedError):
        return []

def _append_table_lines(shape, lines):
    """Append one '[table] a | b | c' line per non-empty table row."""
    for row in shape.table.rows:
        cells = [c.text.replace("\n", " ").strip() for c in row.cells]
        if any(cells):
            lines.append("[table] " + " | ".join(cells))

def _append_soft_lines(lines, text, indent):
    """Append each soft-wrapped line of *text*, prefixed with *indent*."""
    for soft_line in text.split(SOFT_LINE_BREAK):
        soft_line = soft_line.strip()
        if soft_line:
            lines.append(f"{indent}{soft_line}")

def _append_text_frame_lines(shape, lines, depth):
    """Append a shape's text-frame paragraphs, preserving soft line breaks."""
    if not (getattr(shape, "has_text_frame", False) and shape.has_text_frame):
        return
    indent = "  " * depth
    for para in shape.text_frame.paragraphs:
        # para.text preserves soft line breaks as \x0b (vertical tab).
        # Joining run.text would drop them and glue lines together.
        text = para.text.strip()
        if text:
            _append_soft_lines(lines, text, indent)

def extract_shape_text(shape, lines, depth=0):
    """Recursively pull text out of a shape (handles groups)."""
    # Grouped shapes: recurse into members
    members = _group_members(shape)
    if members:
        for sub in sorted(members, key=shape_sort_key):
            extract_shape_text(sub, lines, depth + 1)
        return

    # Tables: render cell text row by row
    if getattr(shape, "has_table", False) and shape.has_table:
        _append_table_lines(shape, lines)
        return

    # Text frames
    _append_text_frame_lines(shape, lines, depth)

def extract_slide_text(slide):
    """Return list of text lines for one slide, in visual order."""
    lines = []
    shapes = list(slide.shapes)
    for shape in sorted(shapes, key=shape_sort_key):
        extract_shape_text(shape, lines)
    return lines

def slide_title(lines):
    """
    Pick a slide's title: the first content line that is not a repeated
    footer or bare slide number.
    """
    for line in lines:
        s = line.strip()
        if not s or s.startswith(NOTES_MARKER) or s.startswith("[table]"):
            continue
        if FOOTER_MARKER in s or re.fullmatch(r"\d+", s):
            continue
        return s
    return "(no title text)"

def simple_summarize(text, max_chars=8000):
    """
    Generate a summary from extracted slide text:
    - Numbered slide list with each slide's title
    - Count of slides carrying speaker notes
    """
    blocks = re.split(r"^## Slide (\d+)$", text, flags=re.M)
    entries = []
    # blocks[0] is the file header; then alternating (number, body)
    for i in range(1, len(blocks) - 1, 2):
        num, body = blocks[i], blocks[i + 1]
        body_lines = body.strip().split("\n")
        entries.append((num, slide_title(body_lines), body.count(NOTES_MARKER)))

    notes_slides = [n for n, _, has in entries if has]

    parts = ["## Slides\n"]
    for num, title, has in entries:
        suffix = " _(has notes)_" if has else ""
        parts.append(f"- Slide {num}: {title}{suffix}")

    parts.append("")
    parts.append("## Stats\n")
    parts.append(f"- Total slides: {len(entries)}")
    parts.append(f"- Slides with speaker notes: {len(notes_slides)}"
                 + (f" ({', '.join(notes_slides)})" if notes_slides else ""))
    parts.append(f"- Total characters of extracted text: {len(text)}")

    return "\n".join(parts)[:max_chars] + "\n"

def _list_pptx_files():
    """Return the PPTX files directly under the slides directory, sorted."""
    return sorted(
        os.path.join(SLIDES_DIR, f)
        for f in os.listdir(SLIDES_DIR)
        if f.lower().endswith(".pptx")
    )

def _notes_lines(slide):
    """Return a slide's speaker-note lines (marker first), or an empty list."""
    try:
        if not slide.has_notes_slide:
            return []
        notes_text = slide.notes_slide.notes_text_frame.text.strip()
    except (AttributeError, NotImplementedError):
        return []

    if not notes_text:
        return []
    return [NOTES_MARKER] + [
        f"  {line.strip()}" for line in notes_text.split("\n") if line.strip()
    ]

def _render_presentation(prs, filename):
    """Return the full raw.txt text for a presentation, slide by slide."""
    out = [f"# Source: {filename}", f"# Slides: {len(prs.slides)}", ""]

    for i, slide in enumerate(prs.slides, start=1):
        out.append(f"## Slide {i}")
        out.extend(extract_slide_text(slide))
        out.extend(_notes_lines(slide))
        out.append("")

    return "\n".join(out)

def _write_summary(out_dir, filename, text, file_size):
    """Write summary.md for long extractions; return 1 when written, else 0."""
    if file_size <= SUMMARY_MIN_BYTES:
        return 0

    summary_path = os.path.join(out_dir, "summary.md")
    with open(summary_path, "w", encoding="utf-8", errors="replace") as f:
        f.write(f"# Summary: {filename}\n\n")
        f.write(simple_summarize(text))

    print(f"  -> Summary: {os.path.getsize(summary_path)} bytes")
    return 1

def main():
    if not os.path.isdir(SLIDES_DIR):
        print(f"Slides directory not found: {SLIDES_DIR}")
        sys.exit(1)

    pptx_files = _list_pptx_files()
    print(f"Found {len(pptx_files)} PPTX files in {SLIDES_DIR}\n")

    extracted_count = 0
    summarized_count = 0
    skipped_count = 0
    error_count = 0

    for pptx_path in pptx_files:
        filename = os.path.basename(pptx_path)
        dirname = pptx_to_dirname(filename)
        out_dir = os.path.join(SLIDES_DIR, dirname)
        os.makedirs(out_dir, exist_ok=True)
        raw_path = os.path.join(out_dir, "raw.txt")

        # Skip if already extracted
        if os.path.exists(raw_path) and os.path.getsize(raw_path) > 0:
            print(f"  SKIP (existing): {filename} -> {raw_path}")
            skipped_count += 1
            continue

        print(f"  EXTRACTING: {filename} -> {raw_path}")

        try:
            prs = Presentation(pptx_path)
            text = _render_presentation(prs, filename)
            with open(raw_path, "w", encoding="utf-8", errors="replace") as f:
                f.write(text)

            file_size = os.path.getsize(raw_path)
            extracted_count += 1
            print(f"  -> {file_size} bytes, {len(prs.slides)} slides")
            summarized_count += _write_summary(out_dir, filename, text, file_size)
        except Exception as e:
            print(f"  ERROR: {e}")
            traceback.print_exc()
            error_count += 1

    print(f"\n{'='*60}")
    print(f"Done: {extracted_count} extracted, {summarized_count} summarized, "
          f"{skipped_count} skipped, {error_count} errors")

if __name__ == "__main__":
    main()
