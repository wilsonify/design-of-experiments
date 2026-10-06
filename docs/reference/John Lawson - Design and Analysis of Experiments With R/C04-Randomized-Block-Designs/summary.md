# Summary: C04-Randomized-Block-Designs


CHAPTER 4 Randomized Block Designs 4.1 Introduction In order to eliminate as much of the natural variation as possible and increase the sensitivity of experiments, it would be advisable to choose the experimental units for a study to be as homogeneous as possible. 

In mathematical terms this would reduce the variance, σ2, of the experimental error and increase the power for detecting treatment factor effects. 

On the other hand, most experimenters would like the conclusions of their work to have wide applicability. 

Consider the following example. 

An experimenter would like to compare several methods of aerobic exercise to see how they affect the stress and anxiety level of experimental subjects. 

Since there is wide variability in stress and anxiety levels in the general population, as measured by standardized test scores, it would be difficult to see any difference among various methods of exercise unless the subjects recruited to the study were a homogeneous group each similar in their level of stress. 

However, the experimenter would like to make general conclusions from his study to people of all stress levels in the general population. 

Blocking can be used in this situation to achieve both objectives.

## Key Formulas

- `this would reduce the variance, σ2, of the experimental error and increase the`
- `> plan<-data.frame(TypeFlower = block, FlowerNumber = flnum, + treatment = t) > write.table(plan, file = "RCBPlan.csv", sep = ",", row.names`
- `> outdesign <- design.rcbd(treat, 4, seed = 11)`
- `yij =µ+bi+τj+ϵij, (4.1) where bi represent the block effects, τj represent the treatment effects. 

The`
- `sums of squares is ssE =y′y−ˆβ′X′y =y′(I −X(X′X)−X′)y, where ˆβ =`
- `R(τ/divides.alt0b,µ)`
- `crd = ssBlk +ssE`
- `mean square. 

If the msBlk is zero, it can be seen that ˆσ2`
- `The error degrees of freedom for the RCB isνrcb = (b−1)(t−1), and the error`
- `number of experimental units would be νcrd =t(b−1). 

The relative efficiency`
- `RE = (νrcb+ 1)(νcrd+ 3) (νrcb+ 3)(νcrd+ 1)`
- `In this case, the rat is represented by the term bi in the model yij =µ+bi+ τj+ϵij. 

The experimental error, represented by ϵij, is the effect of the state of`
- `could be easily misspeciﬁed as yij =µ+τi+ϵij resulting in the wrong analysis`
- `> mod1 <- aov( rate ~ rat + dose, data = drug )`
- `> mod2 <- aov( rate ~ rat + dose, data = drug) > summary.aov(mod2,split = list(dose = list("Linear" = 1, + "Quadratic" = 2,"Cubic" = 3, "Quartic" = 4) ) )`
- `> plot( x, y, xlab = "dose", ylab = "average lever press rate" )`
- `> rate.quad <- lm( y ~ poly( x, 2) ) > lines(xx, predict( rate.quad, data.frame( x = xx) ))`
- `(or rat) is the mean square error ˆ σ2 rcb = 0.00834867. 

The variance of the`
- `= 1.6685+ 0.3006 (5)(10 −1) = 0.043758.`
- `0.0083487 = 5.2413. 

(4.6)`

## R Functions

factor(, sample(, rep(, frame(, table(, library(, rcbd(, levels(, aov(, summary(, contrasts(, poly(, list(, call(, split(, apply(, double(, plot(, seq(, lm(, lines(, predict(, tapply(, dim(, tables(

