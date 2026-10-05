# tooling/doc-extraction — reference-text extraction

Python tooling that turns the reference and archive PDFs into searchable text
and heuristic summaries. It is not part of the R build and has no tests; treat
its output as derived and disposable except where committed.

## Scripts

| Script | What it does | Writes |
|--------|--------------|--------|
| `doe_paths.py` | Resolves the repository root (walk-up to `.git`) and the reference/archive directories | — |
| `extract_all_pdfs.py` | Extracts text from every PDF in `reference/books/` | `reference/<Extracted-Name>/raw.txt` |
| `extract_slides.py` | Extracts text from every PPTX in `reference/slides/` | `reference/slides/<name>/raw.txt` |
| `summarize_pdfs.py` | Regex-heuristic summaries of every `raw.txt` > 10 KB in `reference/` and `archive/course/extracted/` | `<dir>/summary.md` |
| `verify_summaries.py` | Greps the committed summaries for licence-text leaks, bare all-caps headings, ISBN lines | stdout report only |
| `tesseractocrstuff.ipynb` | Scratch notebook, kept for provenance | — |

```sh
python tooling/doc-extraction/extract_all_pdfs.py
python tooling/doc-extraction/summarize_pdfs.py
python tooling/doc-extraction/verify_summaries.py
```

## Notes

* All paths resolve from the repository root via `doe_paths.py`, so the tooling
  can be moved or invoked from any working directory. This replaced scripts that
  resolved `docs/` from their own location — moving them used to silently change
  what they scanned.
* `summary.md` is **generated**, not authored; see
  [`../../reference/README.md`](../../reference/README.md).
* Requires `pymupdf` (or `pypdf`) and, for slides, `python-pptx`.
* Retired: `cleanup.py` (one-shot deletion of files that no longer exist),
  `debug_headings.py` (hardcoded path into a directory that has moved),
  `test_regex2.py` (ad-hoc regex check, superseded by `verify_summaries.py`).
