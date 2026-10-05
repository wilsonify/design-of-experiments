# src/ — living code units

All current code lives here, grouped by *what kind of unit it is* rather than by
the course week it was written in. Retired 2019 coursework lives in
[`../archive/course/`](../archive/course/README.md) and is not part of the build.

| Unit | Files | What it is | How to run |
|------|-------|-----------|------------|
| [`design/`](design/) | 1 | **Deployable unit.** Regenerates every randomisation plan in the repo, deterministically. | `Rscript src/design/generate_plans.R` |
| [`examples/`](examples/) | 12 | Book/chapter worked examples (`Chapter2.R` … `Chapter13.R`) — one script per chapter. Faithful transcriptions of the book's code, kept unrefactored so the printed output can be compared side by side; they call the generators in `design/` rather than re-deriving the plans. | `Rscript src/examples/Chapter2.R` (from repo root) |
| [`utils/`](utils/) | 4 | Shared libraries, `source()`d by the units above and by `experiments/`. | `source("src/utils/paths.R")` |

## utils/ — the cross-cutting concerns

| File | Question it answers | Used by |
|------|--------------------|---------|
| [`utils/paths.R`](utils/paths.R) | *where do files go?* `repo_root()` walks up to the checkout root; `raw_path()` / `interim_path()` / `figures_path()` resolve every read and write against it. Also `read_raw_csv()` / `write_interim_csv()`. | `design/`, `experiments/` |
| [`utils/dependencies.R`](utils/dependencies.R) | *what packages?* `core_packages` is the manifest for living code; `load_doe_packages()` checks, optionally installs, and attaches them (`attach = FALSE` by default, so `MASS`/`dplyr`/`car` cannot mask each other). | `experiments/`, `tests/` |
| [`utils/plotting.R`](utils/plotting.R) | *the house style of figures.* `residual_panel()` replaces the copy-pasted `par(mfrow=c(2,2))` residual block, `doe_interaction_plot()` wraps `interaction.plot()`, and `model_effects()` / `halfnorm_effects()` / `fullnorm_effects()` centralise the normal-effects-plot idiom. | `experiments/` |
| [`utils/design_helpers.R`](utils/design_helpers.R) | *making a design reproducible.* `set_design_seed()` (explicit, recorded seed) and `write_plan()` (always into `data/interim/`). | `design/` |

The chapter examples in `examples/` are the one place these helpers are
deliberately *not* applied: they reproduce the book's code and figure layout
verbatim, so refactoring them would break the comparison the files exist for.

## design/ — the deployable unit

```sh
Rscript src/design/generate_plans.R      # run from anywhere in the repo
```

writes three deterministic plans into `data/interim/`:

| Artifact | Design | Seed | Source |
|----------|--------|------|--------|
| `Plan.csv` | 12-loaf completely randomised, 3 bake times | 7638 | Lawson Ex.1 p.18 (`Chapter2.R`) |
| `CopterDes.csv` | 18-run randomised, BW × WL | 2591 | Lawson Ex.3 p.61 (`Chapter3.R`) |
| `RCBPlan.csv` | 4 × 4 randomised complete block, flowers | 101 | Lawson Ex.1 p.115 (`Chapter4.R`; the book example is unseeded — this seed is recorded so reruns match) |

`Chapter2.R`–`Chapter4.R` source this unit, so the examples and the deployable
unit cannot drift apart.

## Conventions

* Scripts assume the **repository root** as the working directory when run
  directly (`Rscript src/...`), because `source("src/utils/paths.R")` is
  resolved from there. `paths.R` then resolves data and figure paths from the
  root regardless of the caller's CWD.
* Generated artifacts belong in `data/interim/` and `reports/figures/` — never
  next to the script, never in the CWD.
* Randomised designs must call `set_design_seed()` and record the seed in the
  script header. A design is not reproducible unless its seed is written down.
* Packages come from `renv.lock`; add dependencies with `renv::snapshot()`
  rather than a bare `install.packages()`.
