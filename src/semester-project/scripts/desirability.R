# desirability.R — the experiment's computed results.
#
#   Rscript experiments/semester-project/scripts/desirability.R   (or: make figures)
#
# Before this script existed, the two figures the report displays were committed
# PNGs with no code in the repository behind them, and the operating point quoted
# in the report (-0.415, -0.3) could not be traced to any model. This script is
# now the source of both.
#
# Two problems from the brief are solved here:
#
#   Problem 4(b) — filtration: operate near a mean filtration time of 46 while
#                  minimising sensitivity to changes in x1.
#   Problem 3(e) — conversion: maximise conversion subject to activity in [55, 60].
#
# Both models are response surfaces, both searches are seeded grid searches over
# the design region, and both publish their answer as a CSV under results/ so the
# report and tests/checks.R read numbers instead of a picture.

source("src/utils/paths.R")
source("src/utils/design_helpers.R")

library(rsm)
library(desirability)

SEED <- 5309
set_design_seed(SEED)

results_dir <- repo_path("experiments", "semester-project", "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# rsm names coefficients after the expanded terms. Look each one up by name, with
# a positional fallback, so a naming change cannot silently read the wrong
# estimate into the optimisation.
coefficients_for <- function(model, terms) {
  cf <- stats::coef(model)
  out <- numeric(length(terms))
  for (i in seq_along(terms)) {
    out[i] <- if (terms[i] %in% names(cf)) unname(cf[[terms[i]]]) else unname(cf[[i]])
  }
  out
}

# --------------------------------------------------------------------------- #
# Problem 4(b): filtration time — mean near 46, insensitive to x1
# --------------------------------------------------------------------------- #
# The source file names the second factor x3; the brief calls it x2 (pressure).
# Renamed here once, with the raw column kept for traceability.
filtration <- read_raw_csv("04fitration_data.csv")
filtration$x2 <- filtration$x3

# First-order model with the two-way interaction: the model the report describes
# (FO + TWI), which has no curvature terms.
filtration_fit <- rsm(y ~ FO(x1, x2) + TWI(x1, x2), data = filtration)
fb <- coefficients_for(filtration_fit, c("(Intercept)", "x1", "x2", "x1:x2"))
b0 <- fb[1]; b1 <- fb[2]; b2 <- fb[3]; b12 <- fb[4]

predict_y <- function(x1, x2) b0 + b1 * x1 + b2 * x2 + b12 * x1 * x2
# On a bilinear surface the sensitivity to x1 depends on x2 alone.
slope_x1 <- function(x2) b1 + b12 * x2

# Rotatable CCD with k = 2 -> axial distance 2^(2/4).
region <- 2^0.5

# The two objectives, stated explicitly because they are the whole point of (b):
# the mean must be close to 46, and the process must be as insensitive as
# possible to x1.
target <- 46
tolerance <- 1
max_sensitivity <- abs(b1) + abs(b12) * region

d_target <- dTarget(target - tolerance, target, target + tolerance)
d_sensitivity <- dMin(0, max_sensitivity)

# Grid search over the design region, then a composite desirability: the
# geometric mean of the two marginals.
grid <- seq(-region, region, length.out = 401)
grid_2d <- expand.grid(x1 = grid, x2 = grid)
grid_2d$yhat <- predict_y(grid_2d$x1, grid_2d$x2)
grid_2d$d_target <- as.numeric(d_target(grid_2d$yhat))
grid_2d$d_sensitivity <- as.numeric(d_sensitivity(abs(slope_x1(grid_2d$x2))))
grid_2d$D <- sqrt(grid_2d$d_target * grid_2d$d_sensitivity)

best <- grid_2d[which.max(grid_2d$D), , drop = FALSE]
optimum_x1 <- best$x1
optimum_x2 <- best$x2
optimum_y <- best$yhat
optimum_slope <- slope_x1(optimum_x2)
optimum_D <- best$D

# The zero-sensitivity line is the interesting second answer: the surface is
# exactly flat in x1 at x2 = -b1 / b12, but its level there is not 46.
flat_x2 <- -b1 / b12
flat_y <- predict_y(0, flat_x2)

# --------------------------------------------------------------------------- #
# Problem 3(e): conversion maximised subject to activity in [55, 60]
# --------------------------------------------------------------------------- #
conversion <- read_raw_csv("03conversion_data.csv")
# The header misspells std.order; renamed for readability.
names(conversion)[names(conversion) == "std.roder"] <- "std.order"

# Full second-order surface for both responses. The design's two blocks (cube and
# star) are not modelled: the split is a nuisance structure here, not a factor.
# The report states that as a limitation.
so_terms <- c("(Intercept)", "time", "temp", "catalyst",
              "time^2", "temp^2", "catalyst^2",
              "time:temp", "time:catalyst", "temp:catalyst")
conversion_fit <- rsm(conversion ~ SO(time, temp, catalyst), data = conversion)
activity_fit <- rsm(activity ~ SO(time, temp, catalyst), data = conversion)
c_coef <- coefficients_for(conversion_fit, so_terms)
a_coef <- coefficients_for(activity_fit, so_terms)

response <- function(coefs, a, b, c) {
  coefs[1] + coefs[2] * a + coefs[3] * b + coefs[4] * c +
    coefs[5] * a^2 + coefs[6] * b^2 + coefs[7] * c^2 +
    coefs[8] * a * b + coefs[9] * a * c + coefs[10] * b * c
}

axial <- max(abs(c(conversion$time, conversion$temp, conversion$catalyst)))
grid3 <- seq(-axial, axial, length.out = 81)
grid_3d <- expand.grid(time = grid3, temp = grid3, catalyst = grid3)
grid_3d$conversion <- response(c_coef, grid_3d$time, grid_3d$temp, grid_3d$catalyst)
grid_3d$activity <- response(a_coef, grid_3d$time, grid_3d$temp, grid_3d$catalyst)

feasible <- grid_3d[grid_3d$activity >= 55 & grid_3d$activity <= 60, , drop = FALSE]
if (nrow(feasible) == 0) {
  stop("No point in the design region meets the activity constraint.", call. = FALSE)
}
best3 <- feasible[which.max(feasible$conversion), , drop = FALSE]

# --------------------------------------------------------------------------- #
# Figures
# --------------------------------------------------------------------------- #
# Figure 1: the two marginal desirabilities and their composite, as a function of
# x2. For each x2 the mean target is met as closely as x1 allows, which is what
# the operating point will do.
x2_axis <- seq(-region, region, length.out = 241)
attained <- function(x2v) {
  slope <- b1 + b12 * x2v
  if (slope == 0) {
    return(b0 + b2 * x2v)                 # flat in x1: one attainable level
  }
  a <- (target - b0 - b2 * x2v) / slope
  if (abs(a) <= region) {
    return(target)
  }
  ends <- predict_y(c(-region, region), x2v)
  if (all(ends > target)) min(ends) else max(ends)
}
y_curve <- vapply(x2_axis, attained, numeric(1))
d_y_curve <- as.numeric(d_target(y_curve))
d_s_curve <- as.numeric(d_sensitivity(abs(slope_x1(x2_axis))))
d_curve <- sqrt(d_y_curve * d_s_curve)

png(figures_path("desirability.png"), width = 900, height = 450)
graphics::par(mfrow = c(1, 2))
graphics::plot(x2_axis, d_curve, type = "l", lwd = 2,
               xlab = "x2 (pressure, coded)", ylab = "desirability",
               main = "Composite desirability vs x2", font.main = 1)
graphics::lines(x2_axis, d_y_curve, lty = 2)
graphics::lines(x2_axis, d_s_curve, lty = 3)
graphics::legend("bottomright", bty = "n",
                 legend = c("composite", "mean = 46", "insensitive to x1"),
                 lty = c(1, 2, 3), lwd = c(2, 1, 1))
graphics::abline(v = optimum_x2, col = "grey60")
graphics::plot(x2_axis, abs(slope_x1(x2_axis)), type = "l", lwd = 2,
               xlab = "x2 (pressure, coded)", ylab = "|dy/dx1|",
               main = "Sensitivity to x1 vs x2", font.main = 1)
graphics::abline(h = 0, lty = 2)
graphics::abline(v = flat_x2, col = "grey60", lty = 2)
graphics::par(mfrow = c(1, 1))
grDevices::dev.off()

# Figure 2: the fitted surface with the design runs and the recommended point.
surface_x1 <- seq(-region, region, length.out = 121)
surface_x2 <- seq(-region, region, length.out = 121)
surface_grid <- expand.grid(x1 = surface_x1, x2 = surface_x2)
surface_z <- matrix(predict_y(surface_grid$x1, surface_grid$x2),
                    nrow = length(surface_x1), ncol = length(surface_x2))

png(figures_path("surface.png"), width = 800, height = 700)
graphics::contour(x = surface_x1, y = surface_x2, z = surface_z, nlevels = 15,
                  xlab = "x1 (temperature, coded)", ylab = "x2 (pressure, coded)",
                  main = "Fitted filtration time (FO + TWI)", font.main = 1)
graphics::points(filtration$x1, filtration$x2, pch = 19)
graphics::points(optimum_x1, optimum_x2, pch = 4, cex = 2, lwd = 2)
graphics::legend("topleft", bty = "n",
                 legend = c("design runs", "recommended operating point"),
                 pch = c(19, 4), lwd = c(1, 2))
grDevices::dev.off()

# --------------------------------------------------------------------------- #
# Published results
# --------------------------------------------------------------------------- #
optimum_table <- data.frame(
  scenario = c("target 46, least sensitive to x1", "flat in x1 (mean off target)"),
  x1 = c(optimum_x1, 0),
  x2 = c(optimum_x2, flat_x2),
  yhat = c(optimum_y, flat_y),
  slope_x1 = c(optimum_slope, slope_x1(flat_x2)),
  desirability = c(optimum_D, NA_real_),
  seed = SEED
)
utils::write.csv(optimum_table,
                 file.path(results_dir, "desirability_optimum.csv"),
                 row.names = FALSE)

conversion_table <- data.frame(
  time = best3$time, temp = best3$temp, catalyst = best3$catalyst,
  conversion = best3$conversion, activity = best3$activity, seed = SEED
)
utils::write.csv(conversion_table,
                 file.path(results_dir, "conversion_optimum.csv"),
                 row.names = FALSE)

cat("filtration model: intercept", round(b0, 4), "x1", round(b1, 4),
    "x2", round(b2, 4), "x1:x2", round(b12, 4), "\n")
cat("operating point: x1 =", round(optimum_x1, 4), " x2 =", round(optimum_x2, 4),
    " yhat =", round(optimum_y, 3),
    " |dy/dx1| =", round(abs(optimum_slope), 4), "\n")
cat("flat-in-x1 line: x2 =", round(flat_x2, 4), " y =", round(flat_y, 4), "\n")
cat("conversion optimum: time", round(best3$time, 4), "temp", round(best3$temp, 4),
    "catalyst", round(best3$catalyst, 4),
    " conversion", round(best3$conversion, 2),
    " activity", round(best3$activity, 2), "\n")
cat("wrote results to", results_dir, "and figures to", figures_path(), "\n")
