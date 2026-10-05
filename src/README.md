# src/ — code units

All runnable code lives here, grouped by *what kind of unit it is* rather than
by the course week it was written in. Nothing is executed on import; each unit
is either a script you run, a document you knit, or a library you `source()`.

| Unit | Files | What it is | How to run |
|------|-------|-----------|------------|
| [`design/`](design/) | 1 | **Deployable unit.** Regenerates every randomisation plan in the repo, deterministically. | `Rscript src/design/generate_plans.R` |
| [`examples/`](examples/) | 12 | Book/chapter worked examples (`Chapter2.R` … `Chapter13.R`) — one script per chapter. | `Rscript src/examples/Chapter2.R` (from repo root) |
| [`course/`](course/) | 26 | STAT 5309 course artifacts: 13 R Markdown documents (labs, homework, exams) and 13 R scripts (dated class sessions, `FTCCode.R`, `R codes.R`). Archival — kept for provenance, not refactored. | Knit the `.Rmd`, or `Rscript` the session scripts interactively |
| [`utils/`](utils/) | 4 | Shared libraries, `source()`d by the units above. No side effects on load. | `source("src/utils/paths.R")` |

## utils/ — the cross-cutting concerns extracted out of the scripts

The original scripts each re-solved the same four problems. They are now
declared once:

* **[`utils/paths.R`](utils/paths.R)** — *where do files go?* Every script used
  bare relative paths (`write.csv(plan, "Plan.csv")`) or 13 interactive
  `file.choose()` calls, so output landed in whatever the working directory
  happened to be. `repo_root()` walks up to the checkout root, and
  `repo_path()` / `raw_path()` / `interim_path()` / `figures_path()` resolve
  every read/write against it. Also provides `read_raw_csv()` and
  `write_interim_csv()`.
* **[`utils/dependencies.R`](utils/dependencies.R)** — *what packages?* The repo
  contained 23 scattered `install.packages()` calls and per-file `library()`
  blocks. `doe_packages` is the single manifest (29 packages, a superset of
  every `library()` call in `src/`); `load_doe_packages()` checks, optionally
  installs, and attaches them.
* **[`utils/plotting.R`](utils/plotting.R)** — *the house style of figures.*
  `residual_panel()` replaces the copy-pasted `par(mfrow=c(2,2))` residual
  block, `doe_interaction_plot()` wraps `interaction.plot()` (36 call sites),
  and `model_effects()` / `halfnorm_effects()` / `fullnorm_effects()` centralise
  the `coef(mod)[-1]` + normal-effects-plot idiom used by half/full normal
  effect plots.
* **[`utils/design_helpers.R`](utils/design_helpers.R)** — *making a design
  reproducible.* `set_design_seed()` (an explicit, recorded seed — the original
  RCB plan was unseeded), `randomise_runs()`, and `write_plan()` (always into
  `data/interim/`). Self-sources `paths.R` if the caller has not.

## design/ — the deployable unit

```sh
Rscript src/design/generate_plans.R      # run from anywhere in the repo
```

writes three deterministic plans into `data/interim/`:

| Artifact | Design | Seed | Source |
|----------|--------|------|--------|
| `Plan.csv` | 12-loaf completely randomised, 3 bake times | 7638 | Lawson Ex.1 p.18 (`Chapter2.R`) |
| `CopterDes.csv` | 18-run randomised, BW × WL | 2591 | Lawson Ex.3 p.61 (`Chapter3.R`) |
| `RCBPlan.csv` | 4 × 4 randomised complete block, flowers | 101 | Lawson Ex.1 p.115 (`Chapter4.R`; book example was unseeded — seed chosen here and documented so reruns match) |

`Chapter2.R`–`Chapter4.R` read from the same paths, so script and unit produce
identical artifacts.

## Conventions

* Scripts assume the **repository root** as the working directory when run
  directly (`Rscript src/...`), because `source("src/utils/paths.R")` is
  resolved from there. `paths.R` itself then resolves all data/figure paths
  from the repo root regardless of the caller's CWD.
* Generated artifacts belong in `data/interim/` and `reports/figures/` — never
  next to the script, never in the CWD.
* Randomised designs must call `set_design_seed()` and record the seed in the
  script header.
* Requirements: R (≥ 4.0) with the packages in `doe_packages`; install them all
  with `source("src/utils/dependencies.R"); load_doe_packages(install = TRUE)`.
