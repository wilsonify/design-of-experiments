# Summary: STAT-5309-SP19-LAB-3

## Overview

- **Source**: STAT-5309-SP19-LAB-3
- **Length**: 13,179 characters, 556 lines, 1 paragraphs
- **Sections**: 2 headings detected

## Introduction

- STAT 5309 LAB 3 **CONTENTS: Set up data frame- 1-Factor Design- Multiple comparisons-Contrasts- Power/sample size.

## Section Outline

- 1. Data: Casting of High Temperature Alloys-
- 2. Data: Detection of Phlebitis on Rabits.

## Key Concepts

STAT 5309 LAB 3 **CONTENTS: Set up data frame- 1-Factor Design- Multiple comparisons-Contrasts- Power/sample size. *DUE: Sun, Feb 17 A. PRACTICE ## --------------------Balanced data; Linear model ------------------- Data: Bacteria under package methods. log(count/cm^2) on meat samples stored in 4 packaging conditions for 9 days. Note: N= 12 observations. Factor “package” has 4 treatment levels (a =4). Each treatment level has 3 replicates ( n=3) package <- rep( c(1,2,3,4) ,each=3)) # a=4 . logcount <- c(7.66,6.98,7.80,5.26,5.44,5.80,7.41,7.33,7.04,3.51,2.91,3.66) bacteria <- data.frame(package,logcount) attach(bacteria) package <- factor(package) bacteria package logcount 1 1 7.66 2 1 6.98 3 1 7.80 4 2 5.26 5 2 5.44 6 2 5.80 7 3 7.41 8 3 7.33 9 3 7.04 10 4 3.51 11 4 2.91 12 4 3.66 Packaging Condition log(count/cm^2) Commercial plastic wrap Vacuum packaged 1% CO,40% O2, 59% N 100% CO2 7.66, 6.98, 7.80 5.26, 5.44, 5.80, 7.41, 7.33, 7.04 3.51, 2.91, 3.66 tapply(logcount,package,mean) # treament means, in a vector tapply(logcount,package,sd) #treatment standard deviations, in a vector boxplot(logcount ~ package) #Box Plot >bact.mod <- aov(logcount ~ package) #linear model >summary.aov(bact.mod) #summary.aov(), anova(mod1)give same ANOVA results Df Sum Sq Mean Sq F value Pr(>F) package 3 32.87 10.958 94.58 1.38e-06 *** Residuals 8 0.93 0.116 ---

## Key Formulas

- `Note: N= 12 observations. Factor “package” has 4 treatment levels  (a =4).  Each treatment level has 3 replicates ( n=3)`
- `package <- rep( c(1,2,3,4) ,each=3))                 # a=4 .`
- `boxplot(logcount ~ package)                #Box Plot`
- `>bact.mod <- aov(logcount ~ package)               #linear model`
- `aov(formula = logcount ~ package)`
- `>bact.mod1 <- aov(logcount ~ package -1)`
- `aov(formula = logcount ~ package - 1)`
- `Fit: aov.default(formula = logcount ~ package)`
- `options(contrasts= c("contr.treatment", "contr.poly"))  #otherwise set back to default`
- `package <- relevel(package, ref="4")`
- `Boxplot(logcount ~ package)   #  𝜇1 = 𝜇3,    𝜇2 =`
- `bact.mod2 <- aov(logcount ~ package, data=bacteria)`
- `summary.aov(bact.mod2, split=list(package=list("Tr 1 is equal to Tr 3" = 1, "Tr 2 is average of Tr 3,`
- `[ or     > cont1 <- matrix( c(1 ,0, -1, 0,  0, 1,  -1/2,  -1/2),  2,3, byrow=T]`
- `fit.contrast(bact.mod, "package", cont1,, conf.int=.95)`
- `> power.anova.test(groups=4, n=3, between.var=var(grp.means), within. var= 0.3^2, sig.level=.05, power=NULL)`
- `between.var = 3.652533 within.var = 0.09 sig.level = 0.05`
- `Note:  Given  n (as in the data set), power=NULL  power.anova.test() gives the power.`
- `Given groups=4, n=3,  𝜎=0.3 [ from summary.aov()],  power=1.[desirable power]`
- `Delta<- 1.7      # D= treatment means difference needs to be detected, from TukeyHSD`

## R Functions

log(, rep(, frame(, attach(, factor(, tapply(, boxplot(, aov(, anova(, lm(, predict(, default(, plot(, options(, relevel(, levels(, class(, matrix(, contrasts(, list(, contrast(, library(, rownames(, test(, var(

