# Semester project — desirability optimisation of a multiple-response process

One experiment, fully traceable: question → design → data → analysis → result.

## Chain of custody

| Stage | Where | Notes |
|-------|-------|-------|
| Question | STAT 5309 SP19 semester project brief | `docs/course/STAT 5309 SP19 SEMESTER PROJECT_1.docx` |
| Analysis script | [`scripts/semester_project_twilson.Rmd`](scripts/semester_project_twilson.Rmd) | the only source file for this experiment |
| Input figures | `reports/figures/desireability.png`, `reports/figures/surface.png` | committed inputs (byte-identical to the old `labs/` copies); the Rmd *displays* them via `include_graphics('../../../reports/figures/…')` — it does not regenerate them |
| Rendered report | [`results/semester_project_twilson.pdf`](results/semester_project_twilson.pdf) | rendered artifact, committed |
| Related code | `src/examples/Chapter9.R`, `Chapter10.R` (mixture / response-surface methods used by the project) | shared methodology, not inputs |

## Reproduce

```sh
Rscript -e "rmarkdown::render('experiments/semester-project/scripts/semester_project_twilson.Rmd')"
```

Run from the repository root; the Rmd resolves its figures relative to its own
directory (`scripts/` → `../../../reports/figures/`), so the render is
working-directory independent. Packages: see `doe_packages` in
[`src/utils/dependencies.R`](../../src/utils/dependencies.R)
(`desirability`, `rsm`, `ggplot2`, `knitr`, …).

The PDF in `results/` is a rendered snapshot: R Markdown rendering is
deterministic given the same package versions, but the PDF byte stream may
differ across pandoc/phantom versions. The Rmd is the source of truth.

## Layout contract

* `scripts/` — analysis source only.
* `results/` — rendered outputs of those scripts (PDF committed via the
  `.gitignore` negation `!experiments/**/results/*.pdf`).
* Figures the experiment produces or consumes live in `reports/figures/`, not
  beside the script.
