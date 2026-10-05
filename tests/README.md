# tests/ — repository checks

```sh
Rscript tests/checks.R      # or: make check
```

Plain R, no test framework: the checks are about artifact reproducibility, not
about a package API. Failures print a labelled reason and the script exits
non-zero.

| Check | Why it exists |
|-------|---------------|
| Every living `.R`/`.Rmd` in `src/` and `experiments/` parses | Catches parse errors without executing anything. The examples had two (`library(mixexp}`, `library{daewr}`) that shipped unnoticed. |
| `generate_plans.R` twice → identical bytes | A design that changes on re-run is not reproducible. Catches a missing or dropped `set_design_seed()`. |
| `Chapter2.R`–`Chapter4.R` → identical plan bytes | The README claimed the examples and the design unit produce identical artifacts. This makes it true and keeps it true. |
| `desirability.R` regenerates both figures, and the optimum matches the value quoted in the report | The report's headline result used to be a committed PNG with no code in the repo behind it. |
| Missing `results/desirability_optimum.csv` | The optimisation must publish its result as data, not only as an image. |

## What is deliberately not checked

* **Rendering the report.** `make report` needs pandoc and a LaTeX toolchain; CI
  runs it in a separate job so the fast checks stay fast.
* **`archive/`.** Retired coursework is not built or tested — it is a record.
* **The reference-tree summaries.** `tooling/doc-extraction/verify_summaries.py`
  is a Python sanity check, not part of `make check`; the summaries are derived,
  not authoritative.
* **Numerical agreement with the book.** The examples are transcriptions of
  printed code; asserting the book's own ANOVA tables would be a larger
  undertaking and is not attempted here.
