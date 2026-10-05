# Makefile — repository entry points. Run from the repository root.
#
# `make` is used by CI (Linux). On a machine without make, every target is a
# one-line Rscript command; see README.md.

R ?= Rscript

.PHONY: all check restore lock plans figures report docs clean

all: check

## restore — install the pinned package set (needs renv.lock)
restore:
	$(R) -e "if (file.exists('renv.lock')) renv::restore(prompt = FALSE) else message('No renv.lock yet — run: make lock')"

## lock — write renv.lock from DESCRIPTION (commit the result)
lock:
	$(R) -e "renv::init(bare = TRUE); renv::snapshot(prompt = FALSE)"

## plans — regenerate the three randomisation plans
plans:
	$(R) src/design/generate_plans.R

## figures — regenerate the experiment's figures and optimum
figures:
	$(R) experiments/semester-project/scripts/desirability.R

## report — render the semester-project PDF (needs pandoc + LaTeX)
report: figures
	$(R) -e "rmarkdown::render('experiments/semester-project/scripts/semester_project_twilson.Rmd')"

## check — parse, plan stability, example agreement, figure regeneration
check:
	$(R) tests/checks.R

## docs — refresh the reference-tree summaries
docs:
	python tooling/doc-extraction/summarize_pdfs.py
	python tooling/doc-extraction/verify_summaries.py

## clean — drop disposable artifacts (never raw data)
clean:
	rm -f data/interim/CopterDes.csv data/interim/RCBPlan.csv Rplots.pdf
