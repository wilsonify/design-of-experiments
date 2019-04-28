
install.packages("daewr")

library(daewr)
data(COdata)
COdata

stripchart(CO ~ Eth, vertical=TRUE, pch=16)
means <- tapply(CO, Eth, mean)
lines(means)

boxplot(CO ~ Eth+Ratio)

##  aov model


COdata.aov <- aov(CO ~ Eth + Ratio +Eth*Ratio)

summary.aov(COdata.aov)

##  Residuals

plot(COdata.aov)


model.tables(COdata.aov)





###  RSM Model

install.packages("rsm")
library(rsm)

Eth.num <- as.numeric(Eth)
Ratio.num <- as.numeric(Ratio)


CO.rsm <- rsm( CO ~ SO(Eth.num, Ratio.num) + TWI(Eth.num, Ratio.num), data=COdata)
summary(CO.rsm)



y.dotdot <- mean(CO)








########   Project  


nozzle <- read.csv(file.choose(), header=TRUE)

attach(nozzle)
names(nozzle)


time <- read.csv(file.choose(), header=TRUE)

(Problem 6- Interaction Sum Squares)

