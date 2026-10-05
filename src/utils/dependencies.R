# dependencies.R — the repository's R package loader.
#
# The manifest is DESCRIPTION. renv, CI and this loader all read that file, so
# there is exactly one place to declare a dependency. (Before this, the repo had
# 31 scattered install.packages() calls, 365 library() calls and a 29-entry
# vector that had to be kept in sync by hand.)
#
# Usage:
#   source("src/utils/dependencies.R")
#   load_doe_packages()                        # errors with an install hint if missing
#   load_doe_packages(attach = TRUE)           # attaches (namespace collisions possible)
#   load_doe_packages(install = TRUE)          # installs anything missing first
#
# Packages are NOT attached by default: attaching MASS, dplyr, car and lattice
# together masks select(), filter(), lag() and recode(). Use ::() unless a
# script is a faithful transcription of book code that calls bare functions.

# Locate DESCRIPTION from the repository root if paths.R has been sourced,
# otherwise fall back to the working directory.
doe_description_path <- function() {
  if (exists("repo_path", mode = "function")) {
    return(repo_path("DESCRIPTION"))
  }
  "DESCRIPTION"
}

# Parse one DCF field into a character vector of package names.
parse_dcf_field <- function(field) {
  if (is.null(field) || is.na(field) || !nzchar(field)) {
    return(character(0))
  }
  parts <- trimws(unlist(strsplit(field, "[,\n]")))
  parts[nzchar(parts)]
}

# The package manifest, read from DESCRIPTION.
doe_packages <- function(which = c("imports", "suggests", "all")) {
  which <- match.arg(which)
  path <- doe_description_path()
  if (!file.exists(path)) {
    stop("Cannot find DESCRIPTION (looked in: ", path, ")", call. = FALSE)
  }
  dcf <- read.dcf(path, fields = c("Imports", "Suggests"))
  imports <- parse_dcf_field(dcf[1, "Imports"])
  suggests <- parse_dcf_field(dcf[1, "Suggests"])
  switch(which,
    imports = imports,
    suggests = suggests,
    all = unique(c(imports, suggests))
  )
}

missing_doe_packages <- function(packages = doe_packages("imports")) {
  packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
}

load_doe_packages <- function(packages = doe_packages("imports"),
                              install = FALSE,
                              attach = FALSE) {
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
        "\nOr restore the pinned set:  renv::restore()",
        call. = FALSE
      )
    }
  }
  if (attach) {
    invisible(lapply(packages, library, character.only = TRUE))
  } else {
    invisible(packages)
  }
}
