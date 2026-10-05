# Summary: C06-Fractional-Factorial-Designs


CHAPTER 6 Fractional Factorial Designs 6.1 Introduction There are two beneﬁts to studying several treatment factors simultaneously in a factorial design. 

First, the interaction or joint effects of the factors can be detected. 

Second, the experiments are more efficient. 

In other words, the same precision of effects can be achieved with fewer experiments than would be required if each of the factors was studied one-at-a-time in separate experiments. 

The more factors included in a factorial design, the greater the efficiency and the greater the number of interactions that may be detected. 

However, the more factors included in a factorial experiment, the greater the number of runs that must be performed. 

When many factors are included in a factorial experiment, one way to reduce the number of runs is to use only two levels of each factor and run only one experiment per cell or treatment combination. 

These ideas were discussed in Sections 3.7 and 3.7.5. 

In the preliminary stage of experimentation, where the objective may be to determine which factors are important from a long list of candidates, a factorial design may require too many experiments to perform even when there are only two levels of each factor and

## Key Formulas

- `function of the number of factors, k. 

With k = 7 or more factors, the large`
- `choice of half the n = 2k runs may not retain the desirable orthogonality`
- `in the ﬁrst three columns, that is, XD = XA ×XB ×XC. 

This means that`
- `three factors in the design, we write symbolically D =ABC. 

This is called the`
- `for elementwise products of columns of coded factor levels. 

The equation, I =`
- `the full factorial. 

Therefore, there is no estimate of σ2, the variance of ex-`
- `with generator E =ABCD . 

If the generator is left off, FrF2 ﬁnds one that is`
- `> design <- FrF2( 16, 5, generators = "ABCD", randomize = FALSE)`
- `class=design, type= FrF2.generators`
- `order (not randomized), remove the option randomize=FALSE to get a ran-`
- `> aliases( lm( y~ (.)^4, data = design)) A = B:C:D:E B = A:C:D:E C = A:B:D:E D = A:B:C:E E = A:B:C:D A:B = C:D:E A:C = B:D:E A:D = B:C:E A:E = B:C:D B:C = A:D:E B:D = A:C:E B:E = A:C:D C:D = A:B:E C:E = A:B:D D:E = A:B:C`
- `Order Ports Temperature (sec) (lb) (days) ˆ σp`
- `put in the mixer. 

The response ˆσp was an estimate of the standard deviation`
- `> soup <- FrF2(16, 5, generators = "ABCD", factor.names = + list(Ports=c(1,3), Temp=c("Cool","Ambient"), MixTime=c(60,80), + BatchWt=c(1500,2000), delay=c(7,1)), randomize = FALSE)`
- `> mod1 <- lm( y ~ (.)^2, data = soup)`
- `The formula formula = y (.)^2 in the call to the lm function causes it`
- `> soupc<-FrF2(16,5,generators="ABCD",randomize=FALSE)`
- `> modc<-lm(y~(.)^2, data=soupc)`
- `> LGB(coef(modc)[-1], rpt = FALSE)`
- `E=Delay Time has a positive effect; this would normally mean that increas-`

## R Functions

library(, info(, runif(, aliases(, lm(, list(, response(, summary(, coef(, areABD(, isABD(, generators(, default(, abs(, design(, tr(, numeric(, frame(, factorial(, rep(, rbind(, optFederov(, pb(, data(, colnames(

