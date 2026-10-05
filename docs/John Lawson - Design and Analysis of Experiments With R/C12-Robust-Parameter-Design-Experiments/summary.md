# Summary: C12-Robust-Parameter-Design-Experiments


CHAPTER 12 Robust Parameter Design Experiments 12.1 Introduction In this chapter no new designs are presented, but rather a slightly different application of previous designs is demonstrated. 

In Chapter 5, sampling experiments were presented for the purpose of characterizing the variability in the response. 

In the other chapters, the main purpose has been to establish cause and effect relationships between the factors and the average response, so that in the future the average response at selected levels of the factors can be predicted, or the levels of the factors can be chosen to produce a desired value of the response. 

In this chapter the purpose is to determine what levels of the factors should be chosen to simultaneously produce a desired value of the response and at the same time minimize the variability of the response. 

After World War II, as Japan was attempting to reestablish telephone communications throughout the country, many problems were encountered because of the poor quality switching systems that had been manufactured in Japan. 

American advisors such as W.E. 

Deming visited Japan and taught the principles of quality control that were most useful to Americans during the war production effort.

## Key Formulas

- `y =Xβ +Zγ +ϵ, (12.1)`
- `tion.X represents the design matrix for the control factors and β represents`
- `array and the noise factor array that resulted in 27−4×21 = 16 runs. 

The design`
- `Generators D =AB,E =AC,F =BC,G =ABC`
- `the 34−2×23 = 72 runs could have been run in a completely random order.`
- `> des.control <- oa.design(nfactors = 4, nlevels = 3, + factor.names = c("A","B","C","D")) > des.noise <- oa.design(nfactors = 3,nlevels = 2, nruns = 8, + factor.names = c("E","F","G"))`
- `RT = R3R2(E2R4+E0R1)`
- `resistors and diodes A = R1, B = R2/slash.leftR1, C=R 4/slash.leftR1, D = E0, and F = E2.`
- `oa.design(nfactors = 8,nlevels=c(2, 3, 3, 3, 3, 3, 3, 3), + factor.names = c("X1", "X2", "X3", "X4", "X5", "X6", "X7" + ,"X8"), randomize = FALSE)`
- `factors may differ from the speciﬁed nominal values by a tolerance of ±2.04%`
- `control factor array where A= 2.67, B = 1.33, C = 5.33, D = 8.0, and F = 4.8.`
- `> modyb <- lm(ybar ~ A + B + C + D + E + F + G, data = tile)`
- `> halfnorm(cfs,names(cfs), alpha=.2)`
- `> modlv <- lm(lns2 ~ A + B + C + D + E + F + G, data = tile)`
- `> halfnorm(cfs,names(cfs),alpha=0.5)`
- `Table 12.6) is Var(103.09, 101.28, ...,107.98) = 8.421, and the ln(s2) = 2.131.`
- `ences in the variance ofRT . 

The minimum variance inRT is exp(0.468) = 1.597 for run number 16, while the maximum variance is exp (4.892) = 133.219 for`
- `> mod <- transform(cont, XA = (A - 4)/2, XB = (B - 2), XC = + (C - 8)/8, XD = (D - 10)/2, XF = (F - 6)/1.2) > xvar <- transform(mod, XA2 = XA*XA, XB2 = XB*XB, XC2 = XC*XC, + XD2 = XD*XD, XF2 = XF*XF, XAB = XA*XB, XAC = XA*XC, XAD = XA*XD, + XAF = XA*XF, XBC = XB*XC, XBD = XB*XD, XBF = XB*XF, XCD = XC*XD, + XCF = XC*XF, XDF = XD*XF)`
- `> modc <- regsubsets(lns2 ~ XA + XB + XC + XD + XF + XA2 + XB2 + XC2 +`
- `+ data = xvar, nvmax = 8, nbest = 4)`

## R Functions

library(, design(, ln(, lm(, summary(, coef(, halfnorm(, names(, abs(, exp(, data(, transform(, regsubsets(, plot(, rep(, sqrt(, poly(, frame(, factor(, list(, lsmeans(, groupedData(, lmeControl(, lme(, varIdent(

