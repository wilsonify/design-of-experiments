
######################   LAB 3##########################
.##  data

logcount <- c(7.66, 6.98, 7.80,
              5.26, 5.44, 5.80, 
              7.41, 7.33, 7.04,
              3.51, 2.91, 3.66)

package <- rep(c("Plastic", "Vacuum", "CO", "CO2"), each=3)
bacteria <- data.frame(package, logcount)
bacteria
attach(bacteria)
package <- factor(package)


## aov model

bacteria.aov <- aov(logcount ~ package)
summary.aov(bacteria.aov)


## TukeyHSD

TukeyHSD(bacteria.aov)
plot(TukeyHSD(bacteria.aov))


## contrast

boxplot(logcount ~ package)

cont <- c(0, -1/2, -1/2, 1)
contrast <- t(cont)              # contrast must be in ROW

install.packages("gmodels")
library(gmodels)

rownames(contrast)<- c("Vacuum is average of Plastic and CO2")                 
fit.contrast(bacteria.aov, "package", contrast, conf.int=.95)



### Power of a test

gr.means <- tapply(logcount, package, mean)
gr.means

var(gr.means)

power.anova.test(groups=4, n=5, between.var=var(gr.means), within.var=0.116 , sig.level=.05, power=NULL)


##  function  Fpower1(), package  "daewr"

install.packages("daewr")
library(daewr)

rmin <- 2
rmax <- 15
sigma<- sqrt(0.116 )
alpha<- .05
Delta<- 1.75
nlev<- 4
nreps <- c(rmin:rmax)

power <- Fpower1(alpha, nlev, nreps,Delta, sigma)
power


#######################    LAB 4A  #########################

##  Problem 1

chem <- read.csv(file.choose(), header=TRUE)

chem

attach(chem)
Bolt <- factor(Bolt)    #Blocking factor
Chemist<- factor(Chemist)

##  aov model

chem.aov <- aov(Strength ~ Bolt +Chemist)
summary.aov(chem.aov)


## Problem 2

solution <- read.csv(file.choose(), header=TRUE)
solution


attach(solution)

Solution <- factor(Solution)
Days <- factor(Days)


## aov model

washing.aov <- aov(Growth ~ Days + Solution)
summary(washing.aov)


## residuals

res <- residuals(washing.aov)
res
#  check mean 0, sigma
plot(res)
abline(h=0)

#  Normal

qqnorm(res)
qqline(res)

shapiro.test(res)

## plot all


par(mfrow=c(2,2))
plot(washing.aov)




## Problem 3
