# design_helpers.R — helpers shared by design-generation scripts.
#
# Cross-cutting concern addressed: every design script re-implemented the
# same steps — set a seed, randomise a run order, write the plan somewhere
# sensible. Centralising them makes generated designs reproducible and
# keeps artifacts inside data/interim/ instead of the caller's working
# directory.
#
# Usage:  source("src/utils/design_helpers.R")

# Load the path helpers if the caller has not already.
if (!exists("write_interim_csv", mode = "function")) {
  candidates <- c("src/utils/paths.R", "../utils/paths.R", "../../utils/paths.R")
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("Source src/utils/paths.R before design_helpers.R (or run from the repo).",
         call. = FALSE)
  }
  source(hit[[1]])
}

# Reproducible randomisation: explicit seed, documented in the artifact.
set_design_seed <- function(seed) {
  if (!is.numeric(seed) || length(seed) != 1L) {
    stop("seed must be a single number — record it so runs are reproducible.",
         call. = FALSE)
  }
  set.seed(seed)
  invisible(seed)
}

# Randomise the run order of a design table, deterministically given seed.
randomise_runs <- function(design, seed) {
  set_design_seed(seed)
  design[sample.int(nrow(design)), , drop = FALSE]
}

# Write a plan to data/interim/ and return its path (invisibly).
write_plan <- function(plan, name) {
  write_interim_csv(plan, name)
}
