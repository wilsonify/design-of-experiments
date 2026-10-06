# Summary: STAT-5309-SP19-LAB-2-a

## Overview

- **Source**: STAT-5309-SP19-LAB-2-a
- **Length**: 10,568 characters, 498 lines, 1 paragraphs
- **Sections**: 4 headings detected

## Introduction

- STAT 5309 R LAB 2 **CONTENTS: 1-FACTOR DESIGN – RESIDUALS CHECKING- TESTS.

## Section Outline

- A    B    C    D    E
- A B C D
- 1. Data: Bacteria with Packages
- 2. Data: Tensile strength of Portland Cement

## Key Concepts

STAT 5309 R LAB 2 **CONTENTS: 1-FACTOR DESIGN – RESIDUALS CHECKING- TESTS. *DUE: Thurs, FEB 7 A. PRACTICE ##--------------------Set up a dataframe; observations and levels; ---------- # Suppose a factor has 5 levels (called treatment levels or factor levels) A <- c(7, 7, 15, 11, 9) B <- c(12, 17, 12, 18, 18) C <- c(14, 18, 18, 19, 19) D <- c(19, 25, 22, 19, 23) E <- c(7, 10, 11, 15, 11) temp <- c(A,B,C,D,E) # combines A,B,C,D,E into a single column vector, length=25 temp [1] 7 7 15 11 9 12 17 12 18 18 14 18 18 19 19 19 25 22 19 23 7 10 11 15 11 # rep(): repeat a pattern ; factor(): convert characters into a factor trt <- rep(c("A", "B", "C", "D", "E"), each=5) # one column vector, “A” repeated 5 times, B repeated 5 times.. [1] A A A A A B B B B B ….. E E E E E trt <- factor(trt) #make trt into a factor data_new <- data.frame(trt, temp) #combine, as columns, converted into data frame attach(data.new) #-----------------------------Plots------------------------------ stripchart(data_new$temp ~ data_new$trt, vertical=TRUE,pch=16) trt_means <- tapply(temp, trt, mean) #tapply() calculates the treatment means lines(trt_means) ##----------------Linear models:

## Key Formulas

- `temp <- c(A,B,C,D,E)                 #  combines A,B,C,D,E into a single column vector, length=25`
- `trt <- rep(c("A", "B", "C", "D", "E"), each=5)  # one column vector, “A”  repeated 5 times, B repeated`
- `stripchart(data_new$temp ~ data_new$trt, vertical=TRUE,pch=16)`
- `data.lm <- lm(temp~trt)                #a linear model`
- `lm(formula = temp ~ trt)`
- `To find Treatment B mean, add 11.800+3.600    = 15.400 (as seen in tapply() or stripchart)`
- `trt <- relevel(trt, ref=" B")                            # B  is the control level now`
- `(a) Check if the SSTotal = SSTreatment + SS Error`
- `(b) Treament df  :   df( SSTreatment)   = a-1    (a is number of factor levels)`
- `(c)  Error df:        df(Error) = N – a  = na – a   (n is level repetition)`
- `abline(h=0)                                                     #horizontal line through 0`
- `W = 0.94816, p-value = 0.2278`
- `dwtest(data.mod, alternative="two.sided")      #can use durbin.watson()`
- `DW = 0.77192, p-value = 0.002962`
- `bartlett.test(temp ~ trt)`
- `Bartlett's K-squared = 2.0578, df = 4, p-value = 0.725`
- `kruskal.test(temp ~trt)`
- `Kruskal-Wallis chi-squared = 2.2842, df = 4, p-value = 0.`
- `LSD.test(g, "trt", MSerror, console=T)`
- `TukeyHSD(aov(temp~trt), conf.level=0.95)        # the model must be specified`

## R Functions

rep(, factor(, frame(, attach(, stripchart(, tapply(, lines(, lm(, anova(, aov(, relevel(, df(, predict(, qqnorm(, qqline(, plot(, abline(, test(, dwtest(, library(, watson(, log(

