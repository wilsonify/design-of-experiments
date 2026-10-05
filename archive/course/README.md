# archive/course — STAT 5309 / 5039 coursework (2019)

Retired course artifacts, kept for provenance. Not built, not tested, not
refactored.

| Path | Files | What it is |
|------|-------|------------|
| `code/` | 13 `.Rmd` | lab, homework, midterm and final documents — the render sources for the deliverables |
| `code/` | 13 `.R` | dated class-session scripts (`session_YYYY-MM-DD.R`), `FTCCode.R`, `R codes.R` |
| `deliverables/` | 30 | rendered PDF/DOCX lab, homework, exam and report deliverables |
| `extracted/` | 16 | extracted text (`raw.txt`) of the deliverables, some with a generated `summary.md` |

The `.R` session scripts were written interactively and still contain bare
`install.packages()` calls (31), bare `library()` calls (365) and
`file.choose()` prompts (15). That is why they are archived rather than
refactored into `src/utils/`: their value is as a record of the sessions, not as
a library.
