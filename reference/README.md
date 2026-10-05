# reference/ — study material

Read-only study material: textbooks, lecture slides, and their extracted text
and summaries. It is not code and nothing here is built.

```
books/                          source PDFs — GIT-IGNORED (licensing, size)
slides/                         source PPTX (ignored) + extracted slide text
<Extracted-Name>/raw.txt        extracted full text — GIT-IGNORED
<Extracted-Name>/summary.md     generated summary — TRACKED
```

## On the `summary.md` files

The summaries are **not written prose**. They are produced by a regex
heuristic in [`tooling/doc-extraction/`](../tooling/doc-extraction/):
paragraph regrouping, first-sentence sampling for the introduction, a ~200-word
"key concepts" blurb from the longest paragraphs, and regex extraction of
formulas, R functions, packages, figures and tables. Expect them to be
incomplete and occasionally wrong; the extracted `raw.txt` they derive from is
the reliable artifact.

Regenerate:

```sh
python tooling/doc-extraction/summarize_pdfs.py            # whole corpus
python tooling/doc-extraction/summarize_pdfs.py reference  # one tree
python tooling/doc-extraction/verify_summaries.py          # sanity check
```

## Why the sources are not committed

`*.pdf` and `raw.txt` are ignored repository-wide (see `.gitignore`) because the
content is third-party published material and large (the reference tree is
~35 MB). The consequence is deliberate: a fresh clone has the summaries and the
READMEs but **not** the books or extracted text they describe. Re-obtain the
books, then re-run the extraction tooling to rebuild `raw.txt`.

Extracted text for the *course* deliverables lives in
[`archive/course/extracted/`](../archive/course/extracted/) instead, so
reference material and course material stay separable.
