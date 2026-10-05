# archive/ — retired course artifacts

Everything here is a graded 2019 STAT 5309 / 5039 artifact, kept for
provenance. Nothing in `archive/` is executed, tested, or part of the living
project's build.

The code is deliberately **not** refactored. `archive/course/code/` still
reflects the interactive session style it was written in: 124 bare `library()`
calls, 23 `install.packages()` side effects, and 15 `file.choose()` prompts.
That is why it is archived rather than migrated onto `src/utils/`. The `*.Rmd`
documents are the render sources for the deliverables in
`course/deliverables/`.

| Path | Contents | Where it came from |
|------|----------|--------------------|
| `course/code/` | 13 R Markdown documents + 13 R scripts | `src/course/`, originally `labs/*.Rmd` and `R code/*.R` (the date-named scripts are now `session_YYYY-MM-DD.R`) |
| `course/deliverables/` | 30 rendered PDF/DOCX deliverables | originally `docs/course/` and `labs/*.pdf`, `labs/*.docx` |
| `course/extracted/` | 16 extracted-text directories (`raw.txt`, some `summary.md`) | originally `docs/STAT-*`, `docs/UHD-*`, `docs/lab*-twilson` |

Rendering these documents is out of scope for the repository's checks; the
course `.Rmd` files are source-of-truth artifacts, not reproducible builds.
