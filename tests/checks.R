#!/usr/bin/env Rscript
# checks.R — the repository's checks.
#
#   Rscript tests/checks.R        (or: make check)
#
# Four things are checked, in order of how often they have actually broken:
#
#   1. Every living script parses. This catches the class of error already found
#      in the examples (library(mixexp}, library{daewr}) without running them.
#   2. The generated plans are byte-stable across two runs. A design that
#      changes when you re-run the generator is not a design.
#   3. Chapter2.R–Chapter4.R produce the same plans as the generator. This was
#      claimed in the README and was false: Chapter4.R randomised unseeded.
#   4. The desirability optimisation regenerates its figures and reports the
#      same optimum the report quotes.
#
# Not checked here: rendering the report (needs pandoc + LaTeX, run `make
# report`), and the archive (not built by design). See tests/README.md.

root <- normalizePath(".")
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  stop("Run the checks from the repository root.", call. = FALSE)
}

rscript <- file.path(R.home("bin"), "Rscript")
failures <- character(0)
notes <- character(0)

check <- function(label, ok, detail = NULL) {
  if (isTRUE(ok)) {
    cat("  PASS  ", label, "\n", sep = "")
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    if (!is.null(detail)) cat("        ", detail, "\n", sep = "")
    failures <<- c(failures, label)
  }
  invisible(ok)
}

note <- function(...) notes <<- c(notes, paste0(...))

`%||%` <- function(a, b) if (is.null(a)) b else a

run_r <- function(script) {
  status <- system2(rscript, script, stdout = TRUE, stderr = TRUE)
  list(status = attr(status, "status") %||% 0L, output = status)
}

md5 <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) return(character(0))
  unname(tools::md5sum(paths))
}

cat("\n[1/4] living scripts parse\n")

living <- c(
  list.files("src", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("src", pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE),
  list.files("experiments", pattern = "\\.[Rr]md?$", recursive = TRUE, full.names = TRUE)
)

parse_one <- function(path) {
  if (grepl("\\.Rmd$", path, ignore.case = TRUE)) {
    tmp <- tempfile(fileext = ".R")
    on.exit(unlink(tmp), add = TRUE)
    ok <- tryCatch({
      knitr::purl(path, output = tmp, quiet = TRUE)
      TRUE
    }, error = function(e) conditionMessage(e))
    if (!isTRUE(ok)) return(ok)
    path <- tmp
  }
  tryCatch({
    parse(path)
    TRUE
  }, error = function(e) conditionMessage(e))
}

for (path in living) {
  result <- parse_one(path)
  check(path, isTRUE(result), if (!isTRUE(result)) result)
}

cat("\n[2/4] generated plans are byte-stable across two runs\n")

plans <- c("data/interim/Plan.csv", "data/interim/CopterDes.csv", "data/interim/RCBPlan.csv")

first <- run_r("src/design/generate_plans.R")
check("generate_plans.R runs", first$status == 0L,
      paste(utils::tail(first$output, 3), collapse = " | "))
before <- md5(plans)
check("all three plans are written", length(before) == length(plans),
      paste("wrote:", paste(basename(plans)[file.exists(plans)], collapse = ", ")))

second <- run_r("src/design/generate_plans.R")
after <- md5(plans)
check("re-running the generator changes nothing",
      length(before) == length(after) && identical(before, after),
      "plan bytes differ between runs — a seed is missing")

cat("\n[3/4] the chapter examples agree with the generator\n")

for (script in c("src/examples/Chapter2.R", "src/examples/Chapter3.R", "src/examples/Chapter4.R")) {
  res <- run_r(script)
  check(paste(basename(script), "runs"), res$status == 0L,
        paste(utils::tail(res$output, 3), collapse = " | "))
}

check("examples write the same plan bytes as the generator",
      identical(before, md5(plans)),
      "an example re-derived a plan instead of calling src/design/")

cat("\n[4/4] the experiment regenerates its results\n")

figures <- c("reports/figures/desirability.png", "reports/figures/surface.png")
res <- run_r("experiments/semester-project/scripts/desirability.R")
check("desirability.R runs", res$status == 0L,
      paste(utils::tail(res$output, 3), collapse = " | "))

for (fig in figures) {
  exists <- file.exists(fig)
  check(paste("regenerated", basename(fig)),
        exists && file.info(fig)$size > 0,
        if (!exists) "figure not written" else "figure is empty")
}

# The published optima are checked against the brief's own requirements, not
# against hardcoded numbers: the report reads these files, so a check that simply
# repeated its constants would prove nothing.
optimum_path <- "experiments/semester-project/results/desirability_optimum.csv"
region <- 2^0.5
if (file.exists(optimum_path)) {
  optimum <- utils::read.csv(optimum_path)
  check("problem 4: recommended mean is on the target of 46",
        abs(optimum$yhat[1] - 46) < 0.05,
        paste("yhat =", optimum$yhat[1]))
  check("problem 4: operating point is inside the design region",
        all(abs(c(optimum$x1[1], optimum$x2[1])) <= region + 1e-6),
        paste0("x1=", optimum$x1[1], " x2=", optimum$x2[1], " region=", round(region, 4)))
  # dy/dx1 = b1 + b12 * x2 is 6.0 in coded units at x2 = 0 for the fitted FO+TWI
  # surface, so this asserts a real reduction in sensitivity, not a self-check.
  check("problem 4: sensitivity to x1 is reduced relative to x2 = 0",
        abs(optimum$slope_x1[1]) < 6.0,
        paste("|dy/dx1| =", optimum$slope_x1[1]))
} else {
  check("desirability.R writes results/desirability_optimum.csv", FALSE,
        paste("missing", optimum_path))
}

conversion_path <- "experiments/semester-project/results/conversion_optimum.csv"
if (file.exists(conversion_path)) {
  optimum3 <- utils::read.csv(conversion_path)
  check("problem 3: activity respects the 55-60 constraint",
        optimum3$activity >= 55 && optimum3$activity <= 60,
        paste("activity =", optimum3$activity))
  check("problem 3: operating point is inside the design region",
        all(abs(c(optimum3$time, optimum3$temp, optimum3$catalyst)) <= 1.682 + 1e-6),
        paste("point =", optimum3$time, optimum3$temp, optimum3$catalyst))
  check("problem 3: conversion is not worse than the best observed run",
        optimum3$conversion >= 90,
        paste("conversion =", optimum3$conversion))
} else {
  check("desirability.R writes results/conversion_optimum.csv", FALSE,
        paste("missing", conversion_path))
}

cat("\n----------------------------------------\n")
if (length(notes) > 0) {
  cat("Notes:\n")
  for (n in notes) cat("  - ", n, "\n", sep = "")
}
if (length(failures) == 0) {
  cat("All checks passed.\n")
  quit(status = 0)
} else {
  cat(length(failures), " check(s) failed:\n", sep = "")
  for (f in failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}
