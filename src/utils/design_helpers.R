# design_helpers.R — helpers shared by design-generation scripts.
#
# Cross-cutting concern addressed: every design script re-implemented the
# same steps — set a seed, write the plan somewhere sensible. Centralising
# them makes generated designs reproducible and keeps artifacts inside
# data/interim/ instead of the caller's working directory.
#
# Requires the path helpers; sourcing paths.R is the caller's job, so this file
# has no side effects on load:
#
#   source("src/utils/paths.R")
#   source("src/utils/design_helpers.R")

if (!exists("write_interim_csv", mode = "function")) {
  stop("Source src/utils/paths.R before src/utils/design_helpers.R",
       call. = FALSE)
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

# Write a plan to data/interim/ and return its path (invisibly).
write_plan <- function(plan, name) {
  write_interim_csv(plan, name)
}
