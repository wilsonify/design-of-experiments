#!/usr/bin/env python3
"""Verify the updated _LICENSE_RE catches license lines but not academic headings."""
import re
import sys

sys.stdout.reconfigure(encoding='utf-8', errors='replace')

_LICENSE_RE = re.compile(
    r'(creative\s+commons|©|\(c\)|copyright|attribut|'
    r'conditions are met|all rights reserved|'
    r'complete description of the license|'
    r'you (must|may not|can) |build upon|share.?alike)',
    re.IGNORECASE,
)

# Lines that SHOULD match (license text)
should_match = [
    '1. You must properly attribute the work.',
    '2. You may not use this work for commercial purposes.',
    '3. You may not alter, transform, or build upon this work.',
    'Copyright (c) 2010 Gary W. Oehlert. All rights reserved.',
    'This work is licensed under a Creative Commons license.',
    'A complete description of the license may be found at',
    'you are free to copy, distribute, and transmit this work',
    'provided the following conditions are met:',
]

# Lines that should NOT match (academic headings)
should_not_match = [
    '1.1 Statistics and Data Collection',
    '12.18 Factors and Levels for Commercial Product Test of Erasers',
    '10 Response Surface Designs',
    '4.1 Analysis of Variance',
    'Chapter 1 Introduction',
    '3.3 Replication and Randomization',
    'Commercial Design Considerations',
    '15 Factorials in Incomplete Blocks',
]

print("=== SHOULD MATCH (license lines) ===")
for s in should_match:
    m = _LICENSE_RE.search(s)
    status = "✓ MATCH" if m else "✗ MISS"
    print(f"  {status:10s} | {s}")

print("\n=== SHOULD NOT MATCH (academic headings) ===")
for s in should_not_match:
    m = _LICENSE_RE.search(s)
    status = "✓ OK" if not m else "✗ FALSE POSITIVE"
    print(f"  {status:15s} | {s}")
