# Summary: STAT-5309-LAB-4-B

## Overview

- **Source**: STAT-5309-LAB-4-B
- **Length**: 14,030 characters, 595 lines, 1 paragraphs
- **Sections**: 1 headings detected

## Introduction

- STAT 5309 Lab 4-B **CONTENTS: 1 BLOCKING FACTOR – 2 BLOCKING FACTORS (LATIN SQUARES) -OTHERS **Due: ___________ A.

## Section Outline

- 1 Block and 2 Factors. A data frame with 16 observations

## Key Concepts

STAT 5309 Lab 4-B **CONTENTS: 1 BLOCKING FACTOR – 2 BLOCKING FACTORS (LATIN SQUARES) -OTHERS **Due: ___________ A. PRACTICE ##-----------------------------One Treatment Factor-One Blocking Factor------------------- ##----------------------- 1-Quantitative factor + 1 blocking factor------ Data: drug. Rat Behavior. 50 observations. Rat: There are 10 rats. A factor with levels 1, 2, 3, 4, 5, 6, 7, 8, 9, 10. Dose: a factor with 5 levels: 0.0, 0.5, 1.0, 1.5, 2.0. Rate: a numeric vector head(drug) > drug rat dose rate 1 1 0 0.60 2 1 0.5 0.80 3 1 1 0.82 4 1 1.5 0.81 13 3 1 0.83 14 3 1.5 0.80 15 3 2 0.52 16 4 0 0.60 47 10 0.5 1.20 48 10 1 1.18 49 10 1.5 1.23 50 10 2 1.05 attach(drug) # rat used as a one block factor drug.mod<- aov(rate ~ rat +dose, data=drug) #Suppose no interaction between rat and dose summary.aov(drug.mod) Df Sum Sq Mean Sq F value Pr(>F) rat 9 1.6685 0.18538 22.20 3.75e-12 *** dose 4 0.4602 0.11505 13.78 6.53e-07 *** Residuals 36 0.3006 0.00835 --- Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 Note: Rat means are significiant different. Dose means are significant different.

## Key Formulas

- `drug.mod<- aov(rate ~ rat +dose, data=drug)    #Suppose no interaction between rat and dose`
- `> power.anova.test(groups=5, between.var=var (trt.means), within.var=MSE, sig.level=.05,power=.`
- `n = 3.804702 between.var = 0.0115052 within.var = 0.0081 sig.level = 0.05 power = 0.9`
- `Note:   n = 4, to have a power of 0.9`
- `> model.tables(drug.mod, type="effects")`
- `Note:  b= 10 (rats)  is a reasonable number of blocks`
- `bha.mod <- aov(y ~ block +strain *treat, data=bha)                # interaction between strain and treat`
- `bha.mod1 <- aov(y ~ strain*treat, data=bha)`
- `design.lsd <- design.lsd(treat, seed=543, serie=2)`
- `> sales.aov <- aov(sales ~ row +col +treat, data=data)`
- `Fit: aov(formula = sales ~ row + col + treat, data = data)`
- `design.graeco <- design.graeco(trt, trt2,seed=543, serie=2)`
- `design.bib <- design.bib(trt, k, seed=543, serie=2)`
- `design.bib.2 <- design.bib(trt1, k, seed=543, serie=2)`
- `design.ab <- design.ab(trt, r=3, serie=2)`

## R Functions

head(, attach(, aov(, test(, tapply(, tables(, frame(, library(, data(, anova(, plot(, str(, lsd(, names(, levels(, graeco(, bib(, ab(, size(

