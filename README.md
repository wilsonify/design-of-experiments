# Design of Experiments

Examples of statistical design and analysis of experiments, primarily in R,
adapted from Lawson's *Design and Analysis of Experiments With R* and a
STAT 5309/5039 course. Knowledge of basic statistics is required.

The repository is organised as a living project plus an archive: code, data,
experiments and reporting are separated; generated artifacts are reproducible
from seeded scripts; raw inputs are never edited; and the 2019 coursework is
kept for provenance without being mixed into the working tree.

## Repository layout

```
src/            living code — see src/README.md
  design/       deployable unit: regenerates all randomisation plans
  examples/     Chapter2.R … Chapter13.R book examples (faithful transcriptions)
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
docs/           the living project's charter — docs/project/experiment-plan.md
reference/      study material: textbooks, slides, extracted text, summaries
archive/        retired 2019 course code and deliverables (not built, not tested)
tooling/        doc-extraction scripts that maintain reference/
```

Rule of thumb: **raw is sacred, interim is disposable, results are committed.**

`reference/` and `archive/` are deliberately separate from `src/`: one is
read-only study material, the other is retired coursework. Neither is a code
unit, and neither participates in the build.

## Quick start

Requirements: R (≥ 4.0), plus `make` and pandoc/LaTeX for `make report`.
Dependencies are declared once in `DESCRIPTION`. If a pinned `renv.lock` is
present, restore it:

```sh
Rscript -e "renv::restore()"                # if renv.lock exists
Rscript -e "renv::init(bare = TRUE); renv::snapshot(prompt = FALSE)"   # make lock
```

```sh
# 1. regenerate all three design plans deterministically
Rscript src/design/generate_plans.R

# 2. run a chapter example
Rscript src/examples/Chapter2.R

# 3. reproduce the semester-project analysis and render its report
Rscript experiments/semester-project/scripts/desirability.R
Rscript -e "rmarkdown::render('experiments/semester-project/scripts/semester_project_twilson.Rmd')"

# 4. run every check (plan stability, script parsing, figure regeneration)
Rscript tests/checks.R        # or: make check
```

All commands run from the repository root. `src/utils/paths.R` resolves data and
figure locations from the root, so `data/interim/` and `reports/figures/` are
written regardless of the caller's working directory.

## Dependencies and checks

* `DESCRIPTION` is the single dependency manifest; `src/utils/dependencies.R`,
  `renv` and CI all read it, so there is no hand-maintained package list to
  drift. `load_doe_packages()` does **not** attach packages by default —
  attaching `MASS`, `dplyr`, `car` and `lattice` together masks `select()`,
  `filter()` and `lag()`.
* `make lock` writes a pinned `renv.lock`. That file is not committed yet (it
  can only be produced by a machine with R installed); CI emits a warning
  until it is, and resolves packages from `DESCRIPTION` in the meantime.
* [`tests/checks.R`](tests/README.md) verifies the plans are byte-stable across
  runs, that the chapter examples agree with the generator, and that the
  experiment regenerates its figures and optimum. `.github/workflows/check.yml`
  runs it, plus a separate job that renders the report.

## Reproducing the generated plans

| Artifact | Seed | Generator |
|----------|------|-----------|
| `data/interim/Plan.csv` | 7638 | `src/design/generate_plans.R` |
| `data/interim/CopterDes.csv` | 2591 | `src/design/generate_plans.R` |
| `data/interim/RCBPlan.csv` | 101 (book example unseeded; seed chosen for reproducibility) | `src/design/generate_plans.R` |

`src/examples/Chapter2.R`–`Chapter4.R` call the same generators, so the
examples and the design unit cannot drift apart. Full charter:
[`docs/project/experiment-plan.md`](docs/project/experiment-plan.md).

## Data and results

* `data/raw/` — 7 CSV datasets + 2 Excel book datasets (byte-identical to the
  originals formerly in `labs/`; git rename detection preserves history).
* `data/external/` — 7 JMP files (formerly `labs/*.jmp`), byte-identical twins
  of the CSVs in `data/raw/`.
* `reports/figures/` — the desirability/surface figures are **generated** by
  `experiments/semester-project/scripts/desirability.R`; the remaining figures
  are committed snapshots.
* `experiments/semester-project/results/` — rendered PDF report.
* `archive/course/deliverables/` — curated lab/exam deliverables (PDF/DOCX),
  explicitly un-ignored in `.gitignore`.
* `reference/` — the books and extracted `raw.txt` are ignored (third-party,
  ~35 MB); the regex-generated `summary.md` files are tracked. See
  [`reference/README.md`](reference/README.md).

## Provenance map (before → after)

| Before | After |
|--------|-------|
| `R code/Chapter2.R` … `Chapter13.R` | `src/examples/` (two syntax typos fixed: `library(mixexp}`, `library{daewr}`) |
| `R code/FTCCode.R`, `R codes.R`, date-named scripts | `archive/course/code/` (date scripts renamed `session_YYYY-MM-DD.R`) |
| `R code/DOE.ipynb` | `notebooks/exploratory/DOE.ipynb` |
| `R code/Plan.csv` | `data/interim/Plan.csv` (now regenerable) |
| `labs/*.csv` | `data/raw/` (deleted duplicate copies) |
| `labs/*.jmp` | `data/external/` |
| `labs/*.Rmd` | `archive/course/code/` |
| `labs/*.pdf`, `labs/*.docx` (deliverables) | `archive/course/deliverables/` |
| `labs/desireability.png`, `labs/surface.png` | `reports/figures/` (now regenerated by a seeded script) |
| semester-project Rmd (in `labs/`) | `experiments/semester-project/scripts/` |
| `tesseractocrstuff.ipynb` | `tooling/doc-extraction/` |
| `docs/*.pdf`, `docs/*/` (books, extracted text) | `reference/` |
| `docs/course/*` | `archive/course/deliverables/` |
| `docs/*.py` | `tooling/doc-extraction/` |
