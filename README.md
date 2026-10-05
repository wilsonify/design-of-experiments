# Design of Experiments

Examples of statistical design and analysis of experiments, primarily in R,
adapted from Lawson's *Design and Analysis of Experiments With R* and a
STAT 5309/5039 course. Knowledge of basic statistics is required.

Structured after the Microsoft Team Data Science Process, adapted for design
of experiments: code, data, experiments, and reporting are separated, every
generated artifact is reproducible from a seeded script, and raw inputs are
never edited.

## Repository layout

```
src/            code — see src/README.md
  design/       deployable unit: regenerates all randomisation plans
  examples/     Chapter2.R … Chapter13.R book examples
  course/       STAT 5309 labs, homework, exams, dated class scripts (archival)
  utils/        shared helpers: paths, dependencies, plotting, design helpers
data/
  raw/          source datasets as received (never edited)
  external/     JMP-format inputs
  interim/      generated plans — disposable, regenerable (Plan.csv, …)
experiments/    one directory per experiment (scripts/ + results/)
  semester-project/   multi-response desirability optimisation
reports/
  figures/      figures shared across experiments and documents
notebooks/      exploratory notebooks (not authoritative)
docs/           reference books + extracted text/summaries, course materials,
                PDF/docx deliverables (docs/course/), tooling, project charter
```

Rule of thumb: **raw is sacred, interim is disposable, results are committed.**

## Quick start

Requirements: R (≥ 4.0) with the packages the scripts use.

```sh
# 1. install/attach every package the repo needs (29 declared)
Rscript -e "source('src/utils/dependencies.R'); load_doe_packages(install = TRUE)"

# 2. regenerate all three design plans deterministically
Rscript src/design/generate_plans.R

# 3. run a chapter example
Rscript src/examples/Chapter2.R

# 4. render the semester-project report
Rscript -e "rmarkdown::render('experiments/semester-project/scripts/semester_project_twilson.Rmd')"
```

All commands run from the repository root. Path helpers in
`src/utils/paths.R` resolve data and figure locations from the repo root, so
outputs land in `data/interim/` and `reports/figures/` regardless of the
caller's working directory.

## Reproducing the generated plans

| Artifact | Seed | Generator |
|----------|------|-----------|
| `data/interim/Plan.csv` | 7638 | `src/design/generate_plans.R` |
| `data/interim/CopterDes.csv` | 2591 | `src/design/generate_plans.R` |
| `data/interim/RCBPlan.csv` | 101 (book example unseeded; seed chosen for reproducibility) | `src/design/generate_plans.R` |

Chapter examples `Chapter2.R`–`Chapter4.R` read and write the same paths, so
script and unit produce identical artifacts. Full charter:
[`docs/project/experiment-plan.md`](docs/project/experiment-plan.md).

## Data and results

* `data/raw/` — 7 CSV datasets + 2 Excel book datasets (byte-identical to the
  originals formerly in `labs/`; git rename detection preserves history).
* `data/external/` — 7 JMP files (formerly `labs/*.jmp`).
* `reports/figures/` — 8 figures, including the two desirability/surface PNGs
  consumed by the semester-project report (committed inputs, not regenerable
  from the Rmd).
* `experiments/semester-project/results/` — rendered PDF report.
* `docs/course/` — curated lab/exam deliverables (PDF/DOCX), explicitly
  un-ignored in `.gitignore`; everything else under `*.pdf` (reference books,
  extraction byproducts like `raw.txt`) stays ignored.

## Provenance map (before → after)

| Before | After |
|--------|-------|
| `R code/Chapter2.R` … `Chapter13.R` | `src/examples/` |
| `R code/FTCCode.R`, `R codes.R`, date-named scripts | `src/course/` (date scripts renamed `session_YYYY-MM-DD.R`) |
| `R code/DOE.ipynb` | `notebooks/exploratory/DOE.ipynb` |
| `R code/Plan.csv` | `data/interim/Plan.csv` (now regenerable) |
| `labs/*.csv` | `data/raw/` |
| `labs/*.jmp` | `data/external/` |
| `labs/*.Rmd` | `src/course/` |
| `labs/*.pdf`, `labs/*.docx` (deliverables) | `docs/course/` |
| `labs/desireability.png`, `labs/surface.png` | `reports/figures/` |
| semester-project Rmd (in `labs/`) | `experiments/semester-project/scripts/` |
| `tesseractocrstuff.ipynb` | `docs/tesseractocrstuff.ipynb` |

Reference documents used for study (Lawson, Everitt, OEHLERT, formula sheets)
remain under `docs/`.
