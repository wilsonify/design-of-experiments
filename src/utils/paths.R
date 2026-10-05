# paths.R — working-directory-independent paths for this repository.
#
# Cross-cutting concern addressed: scripts used bare relative paths
# (e.g. write.csv(plan, "Plan.csv")) or interactive file.choose(), so
# results landed in whatever directory happened to be the working
# directory. These helpers always resolve from the repository root.
#
# Usage:  source("src/utils/paths.R")   (works from anywhere inside the repo)

repo_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "design_of_experiments.Rproj")) ||
        dir.exists(file.path(path, ".git"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate the repository root from: ", start,
           "\nRun from inside the design-of-experiments checkout.", call. = FALSE)
    }
    path <- parent
  }
}

repo_path   <- function(...) file.path(repo_root(), ...)
data_path   <- function(...) repo_path("data", ...)
raw_path    <- function(...) data_path("raw", ...)
interim_path<- function(...) data_path("interim", ...)
figures_path<- function(...) repo_path("reports", "figures", ...)

# Read a raw dataset by bare filename:  bread <- read_raw_csv("Plan.csv")
read_raw_csv <- function(name, ...) {
  utils::read.csv(raw_path(name), ...)
}

# Write a design/plan artifact to data/interim/ by bare filename.
write_interim_csv <- function(x, name, row.names = FALSE, ...) {
  dir.create(dirname(interim_path(name)), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, interim_path(name), row.names = row.names, ...)
  invisible(interim_path(name))
}
