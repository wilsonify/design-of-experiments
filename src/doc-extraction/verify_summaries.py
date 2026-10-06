#!/usr/bin/env python3
"""Verify summaries are clean of license text and single-word all-caps headings."""
import os
import re
import sys

sys.stdout.reconfigure(encoding='utf-8', errors='replace')

# Resolve the reference/archive trees from the repository root so the tooling
# is portable and does not depend on the caller's working directory.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from doe_paths import default_targets  # noqa: E402

TARGETS = default_targets()

issues = []

for target in TARGETS:
  for root, dirs, files in os.walk(target):
    if "summary.md" not in files:
        continue
    summary_path = os.path.join(root, "summary.md")
    with open(summary_path, "r", encoding="utf-8", errors="replace") as f:
        content = f.read()

    dirname = os.path.basename(root)
    raw_name = dirname.replace("-", " ").replace("SP", "SP").strip()

    # Check 1: No CC license condition lines
    license_leaks = re.findall(
        r'You must properly|You may not|commercial purposes|build upon',
        content, re.IGNORECASE
    )
    if license_leaks:
        issues.append(f"{dirname}: License text leaked: {license_leaks}")

    # Check 2: No CONTAINS/CONTENTS/CHAPTER as bare all-caps in Section Outline
    # Look for lines like "- CONTENTS" or "- CHAPTER" in section outline
    bare_caps = re.findall(r'^- [A-Z]{4,}$', content, re.MULTILINE)
    if bare_caps:
        issues.append(f"{dirname}: Bare all-caps headings in outline: {bare_caps}")

    # Check 3: No ISBN lines in section outline
    isbn = re.findall(r'^- ISBN', content, re.MULTILINE | re.IGNORECASE)
    if isbn:
        issues.append(f"{dirname}: ISBN line in outline: {isbn}")

if issues:
    print("ISSUES FOUND:")
    for issue in issues:
        print(f"  - {issue}")
else:
    print("ALL CLEAN: No license leaks, no bare all-caps headings, no ISBN lines in summaries.")

# Also show summary of all summary.md sizes
print("\n=== Summary sizes ===")
for target in TARGETS:
  for root, dirs, files in os.walk(target):
    if "summary.md" in files:
        path = os.path.join(root, "summary.md")
        size = os.path.getsize(path)
        raw_path = os.path.join(root, "raw.txt")
        raw_size = os.path.getsize(raw_path) if os.path.exists(raw_path) else 0
        ratio = size / raw_size * 100 if raw_size > 0 else 0
        print(f"  {os.path.basename(root):55s} raw={raw_size:>10,}  sum={size:>6,}  ratio={ratio:.1f}%")
