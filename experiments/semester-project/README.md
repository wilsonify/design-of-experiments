# Semester project — desirability optimisation of a multi-response process

One experiment, traceable end to end: question → design → data → analysis →
result → report.

## Chain of custody

| Stage | Where | Notes |
|-------|-------|-------|
| Question | `archive/course/deliverables/STAT 5309 SP19 SEMESTER PROJECT_1.docx` | four problems: two-level factorials, a rotatable CCD, and a filtration response surface |
| Data | `data/raw/01epitaxial_data.csv`, `02cracking_data.csv`, `03conversion_data.csv`, `04fitration_data.csv` | read by the analysis scripts; no data is inlined in the report |
| Optimisation | [`scripts/desirability.R`](scripts/desirability.R) | seeded; fits the filtration model and regenerates the two figures |
| Figures | `reports/figures/desirability.png`, `reports/figures/surface.png` | **generated** by `desirability.R`; the Rmd displays them with `include_graphics()` |
| Report source | [`scripts/semester_project_twilson.Rmd`](scripts/semester_project_twilson.Rmd) | the narrative and analysis for all four problems |
| Rendered report | [`results/semester_project_twilson.pdf`](results/semester_project_twilson.pdf) | render artifact, committed |
| Related methods | `src/examples/Chapter3.R`, `Chapter10.R` | the book's methods; not inputs to this experiment |

## Reproduce

```sh
Rscript experiments/semester-project/scripts/desirability.R
Rscript -e "rmarkdown::render('experiments/semester-project/scripts/semester_project_twilson.Rmd')"
```

Run from the repository root. The Rmd resolves its figures relative to its own
directory (`scripts/` → `../../../reports/figures/`), so the render is
working-directory independent, and it resolves its data through
`src/utils/paths.R`. Packages: `renv::restore()` from the repository root.

## Known limitation

The PDF in `results/` is a rendered snapshot. R Markdown rendering is
deterministic given the same package versions, but the PDF byte stream can
differ across pandoc/LaTeX versions; the Rmd and `desirability.R` are the
sources of truth, and `make check` verifies the figures regenerate.

## Layout contract

* `scripts/` — analysis source only.
* `results/` — rendered outputs of those scripts (PDF committed via the
  `.gitignore` negation `!experiments/**/results/*.pdf`).
* Figures the experiment produces live in `reports/figures/`, not beside the
  script.
