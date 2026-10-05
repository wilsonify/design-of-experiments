# archive/ — retired course artifacts

Everything here is a graded 2019 STAT 5309 / 5039 artifact, kept for
provenance. Nothing in `archive/` is executed, tested, or part of the living
project's build.

The code is deliberately **not** refactored. It still reflects the interactive
session style it was written in — bare `library()` calls (365 of them),
`install.packages()` side effects (31), and `file.choose()` prompts (15). Its
skipped `*.Rmd` counterparts are the render sources for the deliverables.

| Path | Contents | Where it came from |
|------|----------|--------------------|
| `course/code/` | 13 R Markdown documents + 13 R scripts | `src/course/`, originally `labs/*.Rmd` and `R code/*.R` (the date-named scripts are now `session_YYYY-MM-DD.R`) |
| `course/deliverables/` | 30 rendered PDF/DOCX deliverables | originally `docs/course/` and `labs/*.pdf`, `labs/*.docx` |
| `course/extracted/` | 16 extracted-text directories (`raw.txt`, some `summary.md`) | originally `docs/STAT-*`, `docs/UHD-*`, `docs/lab*-twilson` |

Rendering these documents is out of scope for the repository's checks; the
course `.Rmd` files are source-of-truth artifacts, not reproducible builds.
