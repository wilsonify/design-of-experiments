# dependencies.R — the repository's R package manifest and loader.
#
# Cross-cutting concern addressed: every script repeated its own
# library() calls and install.packages() side effects (23 install calls
# across the repo). Declare once here; load explicitly there.
#
# Usage:
#   source("src/utils/dependencies.R")
#   load_doe_packages()                  # errors with an install hint if missing
#   load_doe_packages(install = TRUE)    # installs anything missing first

doe_packages <- c(
  # design generation
  "FrF2", "DoE.base", "AlgDesign", "agricolae", "mixexp", "BsMD", "Vdgraph",
  "crossdes", "GAD",
  # analysis
  "daewr", "rsm", "lme4", "nlme", "car", "lsmeans", "gmodels", "multcomp",
  "MASS", "lattice", "leaps", "lmtest", "pbkrtest", "faraway", "chemCal",
  "desirability",
  # reporting / course materials
  "dplyr", "knitr", "ggplot2", "scatterplot3d"
)

missing_doe_packages <- function(packages = doe_packages) {
  packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
}

load_doe_packages <- function(packages = doe_packages, install = FALSE) {
  missing <- missing_doe_packages(packages)
  if (length(missing) > 0) {
    if (install) {
      install.packages(missing, repos = "https://cloud.r-project.org")
      missing <- missing_doe_packages(missing)
    }
    if (length(missing) > 0) {
      stop(
        "Missing R packages: ", paste(missing, collapse = ", "),
        "\nInstall them with:  load_doe_packages(install = TRUE)",
        call. = FALSE
      )
    }
  }
  invisible(lapply(packages, library, character.only = TRUE))
}
