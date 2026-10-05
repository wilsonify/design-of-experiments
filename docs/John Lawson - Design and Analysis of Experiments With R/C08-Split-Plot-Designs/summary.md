# Summary: C08-Split-Plot-Designs



CHAPTER 8 Split-Plot Designs 8.1 Introduction A factorial design is a powerful tool for aiding a researcher. 

Use of a factorial design in experimentation can dramatically increase the power of detecting factor main effects through hidden replication, while additionally affording a researcher the ability to detect interactions or joint effects of factors. 

When there is an interaction between two factors, the effect of one factor will depend on the level of the other factor, and determination of optimal factor combinations must take that into account. 

Interactions occur frequently in the real world and they can only be quantiﬁed through factorial designs. 

In the factorial and fractional factorial designs discussed in Chapters 3, 4, 6, and 7, it was assumed that combinations of factor levels in the design could be randomly assigned to the experimental units within a block for a blocked factorial design, or to the entire group of experimental units for a completely randomized factorial design. 

Randomization of treatment combinations to experimental units guarantees the validity of the conclusions reached from the analysis of data. 

However, sometimes the levels of one or more factors in the design are more difficult to change or require more experimental material

## Key Formulas

- `> sp <- expand.grid(trayT = factor( c("RoomT", "Hot")), + bakeT = factor( c("low", "mid", "high") )) > wp <- data.frame(short = factor( c("100%", "80%") ))`
- `> splitP <- optBlock( ~ short * (trayT + bakeT + + trayT:bakeT), withinData = sp, blocksizes = rep(6, 4), + wholeBlockData = wp)`
- `arguments 16 and 3 indicate 16 runs with 3 factors. 

WPs=4 indicates four whole-plots, and nfac.WP=1 indicates one whole-plot factor.`
- `> Sp <- FrF2(16, 3, WPs = 4, nfac.WP = 1,factor.names = + list(short = c("80%", "100%"), bakeT = c("low", "high"), + trayT = c("low", "high")))`
- `In FrF2(16, 3, WPs = 4, nfac.WP = 1, factor.names = list(short =`
- `class=design, type= FrF2.splitplot`
- `yij =µ+αi+w(i)j, (8.1)`
- `factor and the jth whole-plot, αi represents the ﬁxed effect of the whole-plot`
- `yijk =µ+αi+w(i)j +βk+αβik+ϵijk, (8.2)`
- `and the kth level of the split-plot factor within the jth whole-plot. 

βk is the effect of the ﬁxed split-plot factor, αβik is the ﬁxed interaction effect, and`
- `yijkl =µ+αi+w(i)j +βk+γl+βγkl+αβik+αγil+αβγikl+ϵijkl (8.3)`
- `yijkl =µ+αi+βj+αβij+w(ij)k+γl+αγil+βγjl+αβγijl+ϵijkl (8.4)`
- `(8.3) is random. 

If the number of levels of the whole-plot factor α is a, the number of levels of the sub-plot factors β and γ are b and c, respectively,`
- `αβ (a−1)(b−1) σ2`
- `αγ (a−1)(c−1) σ2`
- `βγ (b−1)(c−1) σ2`
- `αβγ (a−1)(b−1)(c−1) σ2`
- `Error (a−1)(b−1)(c−1)(n−1) σ2`
- `whole-plot factor α is the mean square for whole plots, and the error term for`
- `> model <- aov(y ~ Short + Short%in%Batch + BakeT +`

## R Functions

library(, grid(, factor(, frame(, rbind(, optBlock(, rep(, list(, data(, fixed(, random(, aov(, gad(, lmer(, anova(, pf(, summary(, cbind(, options(, require(, lsmeans(, predictor(, lm(, coef(, default(


