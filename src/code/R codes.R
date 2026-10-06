A <- c(7, 7, 15, 11, 9)
B <- c(12, 17, 12, 18, 18)
C <- c(14, 18, 18, 19, 19)
D <- c(19, 25, 22, 19, 23)
E <- c(7, 10, 11, 15, 11)


temp <- c(A,B,C,D,E)                 #  combines A,B,C,D,E into a single column vector, length=25
temp

[1]  7  7 15 11  9 12 17 12 18 18 14 18 18 19 19 19 25 22 19 23  7 10 11 15 11

# rep():  repeat a pattern ;  factor():  convert characters into a factor

trt <- rep(c("A", "B", "C", "D", "E"), each=5)  

trt


data.new <- data.frame(trt, temp)
data.new

trt <-factor(trt)
class(trt)

#combine, as columns,  trt and temp , converted into data frame

trt <- as.factor(trt)                                               #make trt into a factor

## Plots

attach(data.new)

stripchart(temp ~ trt, vertical=TRUE, pch=16)
means <- tapply(temp, trt, mean)
means

lines(means)


boxplot(temp~trt)

data.lm <- lm(temp ~ trt, data=data.new)

summary(data.lm)

#####   Residuals checking


##Normal

res <- data.lm$residuals
qqnorm(res)
qqline(res)

shapiro.test(res)   # Res is normal


plot(res)
abline(h=0)

## independence


install.packages("lmtest")
library(lmtest)

dwtest(data.lm, alternative="two.sided")


## test for equal variance

bartlett.test(temp~ trt)
?LSD.test

install.packages("agricolae")
library(agricolae)

MSerror <- 8.06
LSD.test(data.lm, "trt", 8.06)
LSD.test(data.lm, "trt", 8.06, console=T)

## TUkeyHSD

TukeyHSD(data.lm, conf.level=0.95)        

data.aov <- aov(temp ~ trt, data=data.new)
summary.aov(data.aov)

summary.lm(data.aov)


Tukey <-TukeyHSD(data.aov)
plot(Tukey)


## pairwise t-test

pairwise.t.test(temp,trt, p.adj="bonf")




########  EXCERSISE

logcount <-c(7.66, 6.98, 7.80,5.26, 5.44, 5.80, 7.41, 7.33, 7.04,3.51, 2.91, 3.66)

method <- rep(c("plastic","vacuum", "O2", "CO2"),each=3 )
method

package <- data.frame(method, logcount)
method <- factor(method)
attach(package)

stripchart(logcount~ method, vertical=T, pch=16)

## aov model

package.aov <- aov(logcount ~ method, data=package)

names(package.aov)

