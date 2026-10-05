#!/usr/bin/env python3
"""Debug: why does CHAPTER still appear in summaries?"""
import os
import re
import sys

sys.stdout.reconfigure(encoding='utf-8', errors='replace')

# Import the actual functions from summarize_pdfs (same directory as this script)
DOCS_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, DOCS_DIR)
from summarize_pdfs import extract_headings, _is_skippable_line

# Test with C01 raw.txt
path = os.path.join(DOCS_DIR, "John Lawson - Design and Analysis of Experiments With R",
                    "C01-Introduction", "raw.txt")
with open(path, "r", encoding="utf-8", errors="replace") as f:
    text = f.read()

headings = extract_headings(text)
print("=== Headings extracted from C01 ===")
for h in headings[:20]:
    print(f"  {repr(h)}")

# Check: does 'CHAPTER' appear in the headings?
print(f"\n'CHAPTER' in headings: {'CHAPTER' in headings}")

# Let's find all lines in the raw text that are exactly 'CHAPTER' or 'CHAPTER ' etc.
print("\n=== Raw lines containing only CHAPTER ===")
for line in text.split("\n"):
    stripped = line.strip()
    if stripped == "CHAPTER" or stripped.upper() == "CHAPTER":
        print(f"  repr: {repr(stripped)}")
    if re.match(r'^[A-Z]+$', stripped) and len(stripped) >= 4:
        print(f"  all-caps single-word heading candidate: {repr(stripped)}")
