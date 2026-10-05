# generate_plans.R — regenerate every experiment design plan in this repo.
#
# Deployable unit: run this script and you get all three randomisation
# plans written to data/interim/, deterministically:
#
#   Rscript src/design/generate_plans.R
#
# Plans produced (each mirrors the corresponding book example, with the
# seed made explicit so reruns are reproducible):
#
#   Plan.csv      — 12-loaf completely randomised design, 3 bake times
#                   (Lawson Ex.1 p.18, seed 7638)
#   CopterDes.csv — 18-run randomised design, BW x WL factors
#                   (Lawson Ex.3 p.61, seed 2591)
#   RCBPlan.csv   — 4x4 randomised complete block design, 4 flower types
#                   (Lawson Ex.1 p.115; the book example is unseeded,
#                   seed 101 chosen here so reruns are reproducible)
#
# Traceability: question -> design -> data lives in
# experiments/semester-project/README.md and docs/project/experiment-plan.md.

source("src/utils/paths.R")
source("src/utils/design_helpers.R")

# --- Plan.csv: completely randomised design (bread bake time) -----------

make_bread_plan <- function(seed = 7638) {
  set_design_seed(seed)
  bake_time <- factor(rep(c(35, 40, 45), each = 4))
  plan <- data.frame(
    loaf = 1:12,
    time = sample(bake_time, 12)
  )
  plan
}

# --- CopterDes.csv: randomised 3x3 factorial (weight x length) ----------

make_copter_design <- function(seed = 2591) {
  d <- expand.grid(BW = c(3.25, 3.75, 4.25), WL = c(4, 5, 6))
  d <- rbind(d, d)                      # 18 runs = 9 combos x 2 reps
  set_design_seed(seed)
  d <- d[order(sample(seq_len(nrow(d)))), ]
  d[, c("BW", "WL")]
}

# --- RCBPlan.csv: randomised complete block design (flowers) ------------

make_rcb_plan <- function(seed = 101) {
  treat <- factor(1:4)
  set_design_seed(seed)
  plan <- data.frame(
    TypeFlower   = factor(rep(c("carnation", "daisy", "rose", "tulip"), each = 4)),
    FlowerNumber = rep(treat, 4),
    treatment    = c(sample(treat, 4), sample(treat, 4),
                     sample(treat, 4), sample(treat, 4))
  )
  plan
}

# --- Write everything ---------------------------------------------------

main <- function() {
  plans <- list(
    Plan.csv      = make_bread_plan(),
    CopterDes.csv = make_copter_design(),
    RCBPlan.csv   = make_rcb_plan()
  )
  for (name in names(plans)) {
    path <- write_plan(plans[[name]], name)
    message("wrote ", path, " (", nrow(plans[[name]]), " runs)")
  }
  invisible(plans)
}

if (identical(environment(), globalenv())) {
  main()
}
