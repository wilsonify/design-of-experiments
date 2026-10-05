# Summary: C02-Completely-Randomized-Designs-with-One-Factor

CHAPTER 2 

Completely Randomized Designs with One Factor 

2.1 Introduction In a completely randomized design, abbreviated as CRD, with one treatment factor, n experimental units are divided randomly into t groups. 

Each group is then subject to one of the unique levels or values of the treatment factor. If n = tr is a multiple of t, then each level of the factor will be applied to r unique experimental units, and there will be r replicates of each run with the same level of the treatment factor. 

If n is not a multiple of t, then there will be an unequal number of replicates of each factor level. 

All other known independent variables are held constant so that they will not bias the effects. 

This design should be used when there is only one factor under study and the experimental units are homogeneous. 

For example, in an experiment to determine the effect of time to rise on the height of bread dough, one homogeneous batch of bread dough would be divided into n loaf pans with an equal amount of dough in each. 

The pans of dough would then be divided randomly into t groups.

## Key Formulas

- `If n = tr is a multiple of t, then each level of the factor will be applied to`
- `> f <- factor( rep( c(35, 40, 45 ), each = 4))`
- `> plan <- data.frame( loaf=eu, time=fac ) > write.csv( plan, file = "Plan.csv", row.names = FALSE)`
- `Yij =µi+ϵij (2.1)`
- `level of the treatment factor, i= 1,...,t, j = 1,...,r i, and ri is the number of`
- `Yij =µ+τi+ϵij. (2.2) This is called the effects model and the τis are called the effects. τi represents`
- `assumption Yij ∼N(µ+τi,σ 2) or ϵij ∼N(0,σ 2). For equal number of repli-`
- `wheren= ∑ri. Using the method of maximum likelihood, which is equivalent`
- `Consider a CRD with t = 3 factor levels and ri = 4 replicates for i = 1,...,t.`
- `andϵ∼MVN (0,σ 2I). The least squares estimators for β are the solution to the normal equations X′Xβ =X′y. The problem with the normal equations is thatX′X is singular`
- `all other levels of the factor are compared to it. For the example with t = 3`
- `(X′X)−1X′y = ˆβ =`
- `ˆβ = (X′X)−1X′y =`
- `> mod0 <- lm( height ~ time, data = bread )`
- `2.4.3 Estimation of σ2 and Distribution of Quadratic Forms The estimate of the variance of the experimental error,σ2, isssE/slash.left(n−t). It is`
- `ssE =y′y−ˆβ′X′y =y′(I −X(X′X)−1X′)y,`
- `to the variance of the experimental error, σ2, follows a chi-square distribution with n−t degrees of freedom, that is, ssE/slash.leftσ2 ∼χ2`
- `From this deﬁnition it can be seen that effects, τi, are not estimable, but a cell mean, µ+τi, or a contrast of effects, ∑ciτi, where ∑ci = 0, is estimable. In matrix notationLβ is a set of estimable functions if each row ofL is a lin- ear combination of the rows ofX, andLˆβ is its unbiased estimator.Lˆβ follows the multivariate normal distribution with covariance matrix σ2L′(X′X)−L, and the estimator of the covariance matrix is ˆσ2L′(X′X)−1L. For example,`
- `L= /parenleft.alt40 1 −1 0`
- `Lβ = /parenleft.alt4τ1−τ2`

## R Functions

seed(, factor(, rep(, sample(, frame(, csv(, getwd(, setwd(, library(, lm(, summary(, left(, bi(, contrast(, aov(, par(, plot(, residuals(, abline(, names(, boxcox(, max(, transform(, np(, leftn(

