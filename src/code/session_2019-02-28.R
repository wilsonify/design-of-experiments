### RCBD

install.packages("agricolae")

library(agricolae)                                  
treat<- c(1,2,3,4)                                     #treatments
design.rcbd <- design.rcbd(treat, r=4) 
book.rcbd <- design.rcbd$book
levels(book.rcbd$block) <- c("carnation", "daisy", "rose", "tulip")    #change names of blocks
book.rcbd


####  package daewr

install.packages("daewr")
library(daewr)
data(drug)
drug
?drug


attach(drug)
rat <- factor(rat)
dose <- factor(dose)

##  aov model

drug.aov   <- aov(rate~ rat + dose)
summary.aov(drug.aov)

##  power

trt.means <- tapply(rate, dose, mean)
trt.means

MSE <- 0.00835

power.anova.test(groups=5,n=6, between.var=var(trt.means), within.var=MSE, sig.level=.05,power=NULL)

######  effects

model.tables(drug.aov, type="effects")

model.tables(drug.aov, type="means")

grand.mean <- mean(rate)


## data: bha

?bha
data(bha)
bha
attach(bha)

strain <- factor(strain)
treat <- factor(treat)

interaction.plot(treat,strain, y)


####  Latin Square  

library(agricolae)

treat <- c("A", "B", "C", "D")
design.lsd <- design.lsd(treat)

design.lsd$book

lsd.book <- design.lsd$book

sales <- c(10,12,15,12,8,16,8,11,15,10,13,8,14,7,10,14)

data<- data.frame(lsd.book,sales)
data
attach(data)

###  aov model

sales.aov <- aov(sales ~ row+col+ treat, data=data)
summary.aov(sales.aov)



###  LAB 5

## RSM

data(COdata)
COdata

install.packages("rsm")
library(rsm)
attach(COdata)

class(Eth)
Eth.num <- as.numeric(Eth)
Ratio.num <- as.numeric(Ratio)

class(Ratio.num)

COdata.rsm<- rsm(CO ~ SO(Eth.num, Ratio.num)+FO(Eth.num,Ratio.num), data=COdata)


summary(COdata.rsm)

contour(COdata.rsm, ~ Eth.num+Ratio.num, image=TRUE, at=xs)


##  LAB 4B


eye <- read.csv(file.choose(), header=TRUE)
eye

attach(eye)

#####


days <- rep(c(1:5), times=5)
days
batch <- rep(c(1:5), each=5)
batch
data.1 <- data.frame(batch, days)
data.1

## 


treat <- c("A","B","C","D", "E")
design  <- design.lsd(treat)
design$book
