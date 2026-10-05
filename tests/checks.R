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

optimum_path <- "experiments/semester-project/results/desirability_optimum.csv"
if (file.exists(optimum_path)) {
  optimum <- utils::read.csv(optimum_path)
  # Values quoted in the report; the check fails loudly if the model drifts.
  expected <- data.frame(x1 = -0.415, x2 = -0.300)
  close_enough <- all(abs(c(optimum$x1[1], optimum$x2[1]) - c(expected$x1, expected$x2)) < 0.05)
  check("optimum matches the value quoted in the report", close_enough,
        paste0("got x1=", optimum$x1[1], " x2=", optimum$x2[1]))
} else {
  check("desirability.R writes results/desirability_optimum.csv", FALSE,
        paste("missing", optimum_path))
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
