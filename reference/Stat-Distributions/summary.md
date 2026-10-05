# Summary: Stat-Distributions

## Overview

- **Source**: Stat-Distributions
- **Length**: 196,369 characters, 13,372 lines, 1 paragraphs
- **Sections**: 15 headings detected

## Introduction

- Statistics 502 Lecture Notes Peter D.

## Section Outline

- LIST OF FIGURES
- 5.10 Crab residuals . . . . . . . . . . . . . . . . . . . . . . . . . . .
- 5.11 Fitted values versus residuals
- 5.12 Data and log data . . . . . . . . . . . . . . . . . . . . . . . . .
- 5.13 Diagnostics after the log transformation
- 5.14 Mean-variance relationship of the transformed data . . . . . .
- 5.15 Yield-density data
- 6.10 Three datasets exhibiting non-additive effects. . . . . . . . . .
- 6.11 Experimental material in need of blocking. . . . . . . . . . . .
- 6.12 Results of the experiment
- 6.13 Marginal plots, and residuals without controlling for row.
- 6.14 Marginal plots for pain data . . . . . . . . . . . . . . . . . . .
- 6.15 Interaction plots for pain data . . . . . . . . . . . . . . . . . .
- 6.16 Oxygen uptake data
- 6.17 ANOVA and ANCOVA ﬁts to the oxygen uptake data
- ... and more sections

## Key Concepts

Statistics 502 Lecture Notes Peter D. Hoff c⃝December 9, 2009 Contents Principles of experimental design 1.1 Induction . 1.2 Model of a process or system . 1.3 Experiments and observational studies . 1.4 Steps in designing an experiment . Test statistics and randomization distributions 2.1 Summaries of sample populations . 2.2 Hypothesis testing via randomization . 2.3 Essential nature of a hypothesis test . 2.4 Sensitivity to the alternative hypothesis . 2.5 Basic decision theory . Tests based on population models 3.1 Relating samples to populations . 3.2 The normal distribution . 3.3 Introduction to the t-test . 3.4 Two sample tests . 3.5 Checking assumptions . 3.5.1 Checking normality . 3.5.2 Unequal variances . Conﬁdence intervals and power 4.1 Conﬁdence intervals via hypothesis tests . 4.2 Power and Sample Size Determination . 4.2.1 The non-central t-distribution . 4.2.2 Computing the Power of a test . i CONTENTS ii Introduction to ANOVA 5.1 A model for treatment variation . 5.1.1 Model Fitting . 5.1.2 Testing hypothesis with MSE and MST . 5.2 Partitioning sums of squares . 5.2.1 The ANOVA table . 5.2.2 Understanding Degrees of Freedom: . 5.2.3 More sums of squares geometry . 5.3 Unbalanced Designs .

## Key Formulas

- `χ2 distributions . . . . . . . . . . . . . . . . . . . . . . . . . .`
- `γ and power versus sample size, and the normal approximation`
- `Power as a function of n for m = 4, α = 0.05 and ¯τ 2/σ2 = 1`
- `Power as a function of n for m = 4, α = 0.05 and ¯τ 2/σ2 = 2`
- `tracked over eight years on average. Data consists of x= input variables, y=health outcomes, gathered concurrently on existing populations.`
- `block were randomly assigned to treatment (x = 1) and the remaining assigned to control (x = 0).`
- `x = estrogen treatment ϵ = “health consciousness” (not directly measured) y = health outcomes`
- `yA,i = response of the ith unit assigned to treatment A yB,i = response of the ith unit assigned to treatment B i = 1, . . . , n. Then ¯yA̸ = ¯yB provides evidence that treatment affects response, i.e.`
- `• Empirical distribution: ˆPr(a, b] = #(a < yi ≤b)/n`
- `ˆF(y) = #(yi ≤y)/n = ˆPr(−∞, y]`
- `• sample mean or average : ¯y = 1`
- `#(yi ≤y.5)/n ≥1/2 #(yi ≥y.5)/n ≥1/2`
- `> quantile (yA, prob=c ( . 2 5 , . 7 5 ) )`
- `> quantile (yB, prob=c ( . 2 5 , . 7 5 ) )`
- `{|¯yB −¯yA| = 5.93}, the observed difference in the experiment`
- `g(YA, YB) = g({Y1,A, . . . , Y6,A}, {Y1,B, . . . , Y6,B}) = |¯YB −¯YA|.`
- `Observed test statistic: g(11.4, 23.7, . . . , 14.2, 24.3) = 5.93 = gobs`
- `F(x|H0) = Pr(g(YA, YB) ≤x|H0) = #{gk ≤x}`
- `Pr(g(YA, YB) ≥5.93|H0) = 0.056`
- `≈Pr(g(YA, YB) ≥gobs|H0)`

## R Functions

mean(, gt(, gKS(, ypA(, ypB(, pA(, normal(, pt(, test(, tw(, pnorm(, qt(, sum(, lm(, dof(, ni(, qf(, pf(, log(, sd(, abs(, anova(, factor(, nij(, plot(

## Figures

- Figure 1.1: Model of a variable process
- Figure 2.1: Wheat yield distributions
- Figure 2.2: Approximate randomization distribution for the wheat example
- Figure 2.3. For these data,
- Figure 2.3: Histograms and empirical CDFs of the ﬁrst two hypothetical
- Figure 2.5, for which
- Figure 2.4: Randomization distributions for the t and KS statistics for the
- Figure 2.5: Histograms and empirical CDFs of the second two hypothetical
- Figure 2.6: Randomization distributions for the t and KS statistics for the
- Figure 3.1: The population model
- Figure 3.2: χ2 distributions
- Figure 3.3: t-distributions

## Tables

- Table 5.1. How do we
- Table 5.1: ANOVA decomposition
- Table 5.2: ANOVA decomposition, unbalanced case
