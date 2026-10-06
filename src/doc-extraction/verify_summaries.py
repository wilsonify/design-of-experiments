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

SUMMARY_FILENAME = "summary.md"
RAW_FILENAME = "raw.txt"

TARGETS = default_targets()

_LICENSE_LEAK_RE = re.compile(
    r'You must properly|You may not|commercial purposes|build upon',
    re.IGNORECASE,
)
# Lines like "- CONTENTS" or "- CHAPTER" in the Section Outline
_BARE_CAPS_RE = re.compile(r'^- [A-Z]{4,}$', re.MULTILINE)
_ISBN_LINE_RE = re.compile(r'^- ISBN', re.MULTILINE | re.IGNORECASE)


def _find_summaries():
    """Yield (directory name, summary path) for every summary under TARGETS."""
    for target in TARGETS:
        for root, dirs, files in os.walk(target):
            if SUMMARY_FILENAME in files:
                yield os.path.basename(root), os.path.join(root, SUMMARY_FILENAME)


def collect_issues() -> list[str]:
    """Return one message per summary that leaks text it should have filtered."""
    issues = []
    for dirname, summary_path in _find_summaries():
        with open(summary_path, "r", encoding="utf-8", errors="replace") as f:
            content = f.read()

        # Check 1: No CC license condition lines
        license_leaks = _LICENSE_LEAK_RE.findall(content)
        if license_leaks:
            issues.append(f"{dirname}: License text leaked: {license_leaks}")

        # Check 2: No CONTAINS/CONTENTS/CHAPTER as bare all-caps in Section Outline
        bare_caps = _BARE_CAPS_RE.findall(content)
        if bare_caps:
            issues.append(f"{dirname}: Bare all-caps headings in outline: {bare_caps}")

        # Check 3: No ISBN lines in section outline
        isbn = _ISBN_LINE_RE.findall(content)
        if isbn:
            issues.append(f"{dirname}: ISBN line in outline: {isbn}")

    return issues


def print_summary_sizes() -> None:
    """Print each summary's size and its ratio to the raw text it condenses."""
    print("\n=== Summary sizes ===")
    for target in TARGETS:
        for root, dirs, files in os.walk(target):
            if SUMMARY_FILENAME not in files:
                continue
            path = os.path.join(root, SUMMARY_FILENAME)
            size = os.path.getsize(path)
            raw_path = os.path.join(root, RAW_FILENAME)
            raw_size = os.path.getsize(raw_path) if os.path.exists(raw_path) else 0
            ratio = size / raw_size * 100 if raw_size > 0 else 0
            print(f"  {os.path.basename(root):55s} raw={raw_size:>10,}  "
                  f"sum={size:>6,}  ratio={ratio:.1f}%")


def main() -> None:
    issues = collect_issues()
    if issues:
        print("ISSUES FOUND:")
        for issue in issues:
            print(f"  - {issue}")
    else:
        print("ALL CLEAN: No license leaks, no bare all-caps headings, no ISBN lines in summaries.")

    print_summary_sizes()


if __name__ == "__main__":
    main()
