# Summary: STAT-5309-LAB-4-A

## Overview

- **Source**: STAT-5309-LAB-4-A
- **Length**: 12,553 characters, 514 lines, 1 paragraphs
- **Sections**: 2 headings detected

## Introduction

- STAT 5309 LAB 4-A *CONTENTS: A.

## Section Outline

- S   R        L        V
- S        R        L        V

## Key Concepts

STAT 5309 LAB 4-A *CONTENTS: A. 2-FACTOR DESIGN B. 1 BLOCKING FACTOR Due: Thurs, Feb 21 A. 2-FACTOR DESIGN We consider 2 separate factors, with some interaction between the two. Data: concrete. (Compaction Effects on Asplatic Concrete Durability): Asphalt pavements suffer from water-associated deteriorations such as crackings. Research to find better pavements more resistant to deterioration. 2 factors to have an effect : (1) compact methods used to compact the specimen (2) aggregate type used in the asphalt mixture. There are 4 compact methods and 2 aggregate types, “Type” has 2 levels. “Kneading” has 4 levels. Total;u, there are 8 combinations. Each combination has 3 replicates. Totally, 24 observations. #----------------Set up data frame------------------------------------------------- type <- rep(c(1,2) ,each=12) meth <- rep(c(1,2,3,4) ,each=3,times=2) strength <- c(68,63,65,126,128,133,93,101,98,56,59,57, 71,66,66,107,110,116,63,60,59,40,41,44) concrete <- data.frame(type,meth,strength) attach(concrete) Type Kneading (4 levels) Static Regular Low Very low Basalt Silicious type <- factor(type,levels=1:2,labels=c("B","S")) meth <- factor(meth,levels=1:4,labels=c("S","R","L","V")) tapply(strength,list( type,meth),mean) S R L V B 65.33333 129 97.33333 57.33333 S 67.66667 111 60.66667 41.66667 tapply(strength,list( type,meth),sd) S R L V B 2.516611 3.605551 4.041452 1.527525 S 2.886751 4.582576 2.081666 2.081666 #-----------------------------Box plot; Interaction plot---------------------- boxplot(strength ~ type) interaction.plot(type,meth, strength) Interaction.plot(meth, type, strength) # Note: There are interactions #----------------------------- Model with Interaction------------------------------ concrete.mod

## Key Formulas

- `type <- rep(c(1,2) ,each=12) meth <- rep(c(1,2,3,4) ,each=3,times=2)`
- `type <- factor(type,levels=1:2,labels=c("B","S")) meth <- factor(meth,levels=1:4,labels=c("S","R","L","V"))`
- `boxplot(strength ~ type)`
- `concrete.mod <- aov(strength ~ type* meth)  #  interaction term`
- `𝑛(𝑛𝑢𝑚𝑏𝑒𝑟 𝑜𝑓 𝑟𝑒𝑝𝑙𝑖𝑐𝑎𝑡𝑒𝑠) = 3, 𝑙𝑒𝑣𝑒𝑙𝑠:   𝑡𝑦𝑝𝑒(𝑎= 2);   𝑚𝑒𝑡ℎ(𝑏= 4). 𝑇𝑜𝑡𝑎𝑙:  𝑁 = 𝑎∗𝑏∗𝑛= 24 𝐷𝑓(𝑡𝑦𝑝𝑒) = 𝑎−1 = 2 −1 = 1  ; 𝐷𝑓(𝑚𝑒𝑡ℎ) = 𝑏−1 = 4 −1 = 3 𝐷𝑓(𝑡𝑦𝑝𝑒: 𝑚𝑒𝑡ℎ) = (𝑎−1)(𝑏−1) = 3 𝐷𝑓(𝑅𝑒𝑠𝑖𝑑𝑢𝑎𝑙𝑠) =  𝑑𝑓(𝑆𝑆𝑇) – 𝐷𝑓(𝑡𝑦𝑝𝑒) −𝐷𝑓(𝑚𝑒𝑡ℎ) − 𝐷𝑓(𝑡𝑦𝑝𝑒: 𝑚𝑒𝑡ℎ) = 𝑎𝑏𝑛−(𝑎−1) −(𝑏−1) −(𝑎−1)(𝑏−1) = 𝑎𝑏𝑛−𝑎𝑏 = 𝑎𝑏(𝑛−1)`
- `Fit: aov(formula = strength ~ type * meth)`
- `> LSD.test(concrete.mod, c("meth", "type"), console=T)`
- `Study: concrete.mod ~ c("meth", "type")`
- `LSD <- LSD.test(concrete.mod, c("meth", "type"), console=T)`
- `#  MSE=9.5 , sigma= 3.08`
- `Note:  n=3 replicates is a satisfactory number.`
- `> estimable(concrete.mod,cont, conf.int=.95)`
- `design.rcbd <- design.rcbd(treat, r=4, seed=11)`
- `drug.mod1<- aov(rate ~ rat +dose, data=drug)    #Suppose no interaction between rat and dose`
- `MSB/MSE = 0.18538   /0.00835 =    22.20 >>1. Blocking is effective`
- `aov(formula = rate ~ rat + dose, data = drug)`
- `drug.mod2 <- aov(rate ~ dose)`
- `Note:   MSE(drug.mod2) =0.04376  , compare  MSE(drug.mod1)  =    0.00835`

## R Functions

rep(, frame(, attach(, factor(, tapply(, list(, boxplot(, plot(, aov(, lm(, anova(, test(, sqrt(, estimable(, library(, rbind(, rownames(, rcbd(, levels(, data(, head(, summary(, preferred(

