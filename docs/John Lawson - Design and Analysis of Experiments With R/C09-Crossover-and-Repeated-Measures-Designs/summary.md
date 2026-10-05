# Summary: C09-Crossover-and-Repeated-Measures-Designs


CHAPTER 9 Crossover and Repeated Measures Designs 9.1 Introduction Crossover and repeated measures designs are usually used in situations where runs are blocked by human subjects or large animals. 

The purpose of crossover designs (COD) is to increase the precision of treatment comparisons by comparing them within each subject or animal. 

In a crossover design, each subject or animal will receive all treatments in a different sequence, but the primary aim is to compare the effects of the treatments and not the sequences. 

Crossover designs are used frequently in pharmaceutical research, sensory evaluation of food products, animal feeding trials, and psychological research. 

The primary purpose of repeated measures designs, on the other hand, is to compare trends in the response over time rather than to look at a snapshot at a particular point in time. 

Each subject or animal receives the same treatment throughout the experiment, and repeated measures are taken on each subject over time. 

Repeated measures experiments are similar to split-plot experiments in that there are two sources of error, treatments are compared to the less precise subject to subject error, and the comparison of trends over time between treatments will be compared

## Key Formulas

- `In this representation two treatmentsA andB are being compared.n=n1+n2`
- `Deﬁning πi as the period effect, τj as the treatment effect, and µ to be the`
- `Group 1 (AB) µ+π1+τ1 µ+π2+τ2 Group 2 (BA) µ+π1+τ2 µ+π2+τ1`
- `yijk =µ+si+πj+τk+ϵijk (9.1)`
- `> modl <- lm( pl ~ Subject + Period + Treat, data = antifungal, + contrasts = list(Subject = contr.sum, Period = contr.sum, + Treat = contr.sum)) > Anova(modl, type = "III" )`
- `> lsmeans(modl, pairwise ~ Treat)`
- `the power of the test for treatments as a function ofn=n1+n2, the signiﬁcance`
- `level (α), the expected difference in treatment means ∆ = ¯µ⋅⋅1−¯µ⋅⋅2, and the within patient variance. 

In this example, α = 0.05, ∆ = 10, and σ2 = 326.`
- `of 10 can be achieved with a sample size of n= 54, and a power of 0.9 can be achieved with a sample size of n= 72.`
- `> n <- seq( 40, 80, by = 2)`
- `> data.frame( alpha = alpha, n = n, delta = delta, power = power)`
- `Group 1 (AB) µ11 =µ+π1+τ1 µ22 =µ+π2+τ2+λ1 Group 2 (BA) µ12 =µ+π1+τ2 µ21 =µ+π2+τ1+λ2`
- `ment,τ1 andτ2 are the direct effects of the treatments on the response in the current period, π1 andπ2 are the period effects, and λ1 andλ2 are deﬁned as`
- `carryover effects, λ2 −λ1, assuming the expected response for the subjects`
- `(µ12+µ22)/slash.left2−(µ11+µ21)/slash.left2=τ2−τ1+(λ1−λ2)/slash.left2.`
- `carryover effects are equal (i.e., λ1 =λ2). 

Assuming the expected response for`
- `yijkl =µ+ψi+sij+πk+τl+ϵijkl, (9.2)`
- `random effect of the jth subject in the ith group; and the πk and τl are the`
- `> mod4 <- lmer( pl ~ 1 + Group + (1|Subject:Group) + Period + + Treat, contrasts = list(Group = c1, Period = c1, Treat = c1), + data = antifungal)`
- `Formula: pl ~ 1 + Group + (1 | Subject:Group) + Period + Treat`

## R Functions

library(, lm(, list(, lsmeans(, seq(, sqrt(, qt(, pt(, frame(, lmer(, summary(, data(, head(, square(, williams(, rownames(, paste(, colnames(, with(, tapply(, fixed(, random(, gad(, anova(, factor(

