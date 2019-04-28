#####   2^2 DESIGN   ############

catalys <- read.csv(file.choose(), header=TRUE)
catalys

attach(catalys)

A <- factor(A)
B <- factor(B)


catalys.aov <- aov(reps ~ A +B +A*B)

summary(catalys.aov)

model.tables(catalys.aov, type="effects")


#### cHECK BY PLOTTING

install.packages("FrF2")
library(FrF2)

DanielPlot(catalys.aov, code=TRUE, alpha=.05, half = TRUE, faclab=NULL))

install.packages("BsMD")
library(BsMD)

Lenth <- LenthPlot(catalys.aov, alpha = 0.05, plt =FALSE, limits = TRUE)
Lenth

MEPlot(catalys.aov)

install.packages("FrF2")
library(FrF2)

MEPlot(catalys.aov)
IAPlot(catalys.aov)




##  expand.grid(): to setup a 2^k design


D <- expand.grid(BW=c(3.25, 3.75, 4.25), WL =c(4,5,6))   
D.2 <- expand.grid(A=c(-1,1), B=c(-1, 1), C=c(-1,1))
D <- rbind(D.2, D.2)
D



## gen.factorial()

install.packages("AlgDesign")
library(AlgDesign)

D.1 <- gen.factorial(levels=c(2,2,2),nVars=3, center=T, varNames=c("A", "B", "C"))
D.1                                            #generate a full factorial design



####  LAB 6(Excersises)

## Problem 1 (CH 6- Problem 5)

vibration <- read.csv(file.choose(), header=TRUE)

vibration

attach(vibration)
Size <- factor(Bit.Size)
Speed <- factor(Cutting.Speed)

names(vibration) <- c("Size", "Speed", "Vibration")
names(vibration)

vib.aov <- aov(Vibration ~ Size*Speed)
summary.aov(vib.aov)

##  1b
res <- vib.aov$residuals
qqnorm(res)
qqline(res)

IAPlot(vib.aov)

plot(res)

plot(vib.aov)
abline(h=0)
abline(h=-2.5, col="red")
abline(h=2.5, col="red")
