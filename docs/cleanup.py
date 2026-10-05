#!/usr/bin/env python3
"""Clean up: remove empty Lawson dir and temp files."""
import os
import shutil

DOCS = os.path.dirname(os.path.abspath(__file__))

# 1. Remove empty duplicate Lawson directory
empty_dir = os.path.join(DOCS, "Design-and-Analysis-of-Experiments-With-R---John-Lawson")
if os.path.exists(empty_dir):
    contents = os.listdir(empty_dir)
    if not contents:
        shutil.rmtree(empty_dir)
        print(f"Removed empty directory: {empty_dir}")
    else:
        print(f"SKIP (not empty): {empty_dir} contains {len(contents)} items")
else:
    print("Empty Lawson dir not found (already removed)")

# 2. Remove temp/scan files
temp_files = [
    "test_regex.py", "scan_license.py", "scan_license2.py",
    "scan_output.txt", "read_first_lines.py",
]
for tf in temp_files:
    path = os.path.join(DOCS, tf)
    if os.path.exists(path):
        os.remove(path)
        print(f"Removed temp file: {path}")

# 3. Verify
remaining = [f for f in os.listdir(DOCS) if f.endswith('.py')]
print(f"\nRemaining .py files in docs/: {remaining}")
