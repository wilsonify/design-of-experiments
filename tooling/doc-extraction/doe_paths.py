"""Repository-root resolution shared by the doc-extraction tooling.

The tooling used to resolve its own directory (``docs/``) and write there, so
moving the scripts silently changed what they scanned and where they wrote.
Everything now resolves against the repository root, the same way
``src/utils/paths.R`` does for the R code.

Layout the tooling reads and writes:

    reference/                     study material (textbooks, slides)
      books/                       source PDFs (git-ignored)
      <Extracted-Name>/raw.txt     extracted text (git-ignored)
      <Extracted-Name>/summary.md  regex-generated summary (tracked)
      slides/                      source PPTX (git-ignored) + extracted text
    archive/course/extracted/      extracted text of the archived deliverables
"""

import os

__all__ = [
    "repo_root",
    "REFERENCE_DIR",
    "BOOKS_DIR",
    "SLIDES_DIR",
    "ARCHIVE_EXTRACTED_DIR",
    "default_targets",
]


def repo_root(start=None):
    """Walk up from this file until the checkout root (a directory with .git)."""
    path = os.path.abspath(start or os.path.dirname(os.path.abspath(__file__)))
    while True:
        if os.path.isdir(os.path.join(path, ".git")):
            return path
        parent = os.path.dirname(path)
        if parent == path:
            raise RuntimeError(
                "Could not locate the repository root from: " + str(start)
            )
        path = parent


ROOT = repo_root()

REFERENCE_DIR = os.path.join(ROOT, "reference")
BOOKS_DIR = os.path.join(REFERENCE_DIR, "books")
SLIDES_DIR = os.path.join(REFERENCE_DIR, "slides")
ARCHIVE_EXTRACTED_DIR = os.path.join(ROOT, "archive", "course", "extracted")


def default_targets():
    """Directories to scan for raw.txt when no explicit target is given."""
    return [d for d in (REFERENCE_DIR, ARCHIVE_EXTRACTED_DIR) if os.path.isdir(d)]
