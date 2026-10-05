# plotting.R — shared plotting helpers for DOE diagnostics and effects.
#
# Cross-cutting concern addressed: the same three plotting idioms were
# re-typed in nearly every chapter script:
#   1. a 2x2 residual diagnostics panel
#   2. interaction.plot() with consistent labels/style
#   3. half-normal / normal effect plots from a fitted model's coefficients
#
# Usage:  source("src/utils/plotting.R")

# 1. Residual diagnostics panel: fitted-vs-residual, scale-location,
#    normal Q-Q, and residuals-vs-experimental-unit (if supplied).
#    Reproduces the par(mfrow = c(2, 2)); plot(mod, which = ...) idiom.
residual_panel <- function(model, data = NULL, unit = NULL) {
  op <- graphics::par(mfrow = c(2, 2))
  on.exit(graphics::par(op))
  stats::plot(model, which = 1)
  stats::plot(model, which = 2)
  stats::plot(model, which = 5)
  if (!is.null(data) && !is.null(unit)) {
    r <- stats::residuals(model)
    graphics::plot(r ~ data[[unit]],
             main = "Residuals vs Exp. Unit", font.main = 1,
             xlab = unit, ylab = "Residuals")
    graphics::abline(h = 0, lty = 2)
  }
  invisible(model)
}

# 2. Interaction plot with the house style used throughout the examples.
doe_interaction_plot <- function(x, trace, response, main = "",
                                 xlab = NULL, ylab = NULL, type = "b",
                                 pch = c(18, 24, 22), ...) {
  if (is.null(xlab)) xlab <- deparse(substitute(x))
  if (is.null(ylab)) ylab <- deparse(substitute(response))
  stats::interaction.plot(x, trace, response, type = type, pch = pch,
                   leg.bty = "o", main = main, xlab = xlab, ylab = ylab, ...)
}

# 3. Effect plots from a fitted model.
#    Extracts the non-intercept, non-NA coefficients (the "effects") and
#    draws the half-normal (daewr::halfnorm) or normal (daewr::fullnormal)
#    plot the chapter scripts build by hand.
model_effects <- function(model) {
  cf <- stats::coef(model)
  cf[!is.na(cf) & names(cf) != "(Intercept)"]
}

halfnorm_effects <- function(model, alpha = 0.05, refline = FALSE) {
  if (!requireNamespace("daewr", quietly = TRUE)) {
    stop("Package 'daewr' is required for halfnorm_effects().", call. = FALSE)
  }
  eff <- model_effects(model)
  daewr::halfnorm(eff, names(eff), alpha = alpha, refline = refline)
  invisible(eff)
}

fullnorm_effects <- function(model, alpha = 0.05) {
  if (!requireNamespace("daewr", quietly = TRUE)) {
    stop("Package 'daewr' is required for fullnorm_effects().", call. = FALSE)
  }
  eff <- model_effects(model)
  daewr::fullnormal(eff, names(eff), alpha = alpha)
  invisible(eff)
}
