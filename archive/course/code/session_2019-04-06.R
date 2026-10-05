###########   LAB  7 : Factorial with Blocking 

block <- rep(1:2, times=16)
block

chem.bl <- data.frame(chem, block)
head(chem.bl)
attach(chem.bl)
class(A)

A <- factor(A)
B <- factor(B)
C<- factor(C)
D <- factor(D)
block <- factor(block)    

chem.aov <- aov( Yield ~ A*B*C*D + Error(block))
summary(chem.aov)


############  Fractional design


install.packages("FrF2")
library(FrF2)

des.1 <- FrF2(16, 5, generators="ABCD")
summary(des.1)



###############  LAB 7:EXERCISES . Problem #1    (data Ch6-1)



hours <- read.csv(file.choose(), header=TRUE)
hours

block <- rep(1:3, times=8)
block


data <- data.frame(hours, block)
data

data

attach(data)


Cutting.Speed<- factor(Cutting.Speed)
Tool.Geometry<-factor(Tool.Geometry)
Cutting.Angle <- factor(Cutting.Angle)
block <- factor(block)

data

##  with Error(block)
data.aov <- aov( Life.Hours~ Cutting.Speed*Tool.Geometry* Cutting.Angle+ Error(block))


###  MEPlot, DanielPlot() might not work with this the Error(block) , used above

data.aov.1 <- aov( Life.Hours~ Cutting.Speed*Tool.Geometry* Cutting.Angle)


MEPlot(data.aov.1)
DanielPlot(data.aov.1)