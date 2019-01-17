# Example 1 p. 60
D <- expand.grid( BW = c(3.25, 3.75, 4.25), WL = c(4, 5, 6) )
D

# Example 2 p. 60
D <- rbind(D, D)

# Example 3 p. 61
set.seed(2591)
D <- D[order(sample(1:18)), ]
CopterDes <- D[ c( "BW", "WL" )]
CopterDes
write.csv(CopterDes, file = "CopterDes.csv", row.names = FALSE)

# Example 4 p. 65
library(daewr)
mod1 <- aov( CO ~ Eth * Ratio, data = COdata )
summary(mod1)

# Example 5 p. 66
model.tables( mod1, type = "means", se = T )

# Example 6 p. 66
c1 <- c(-1/2, 0, 1/2)
c2 <- c(.5, -1, .5)
cm <- cbind( c1, c2 )

# Example 7 p. 67
mod2 <- aov( CO ~ Eth * Ratio, contrasts = list( Eth = cm, Ratio = cm ), data = COdata)
library(gmodels)
c <- rbind('Ethanol 0.3 vs 0.1' = c(0,1,0,0,0,0,0,0,0), 'Ratio 16 vs 14' = c(0,0,0,1,0,0,0,0,0) )
estimable(mod2,c)

# Example 8 p. 67 figure 3.5
with(COdata, (interaction.plot(Eth, Ratio, CO, type = "b", pch = c(18,24,22), leg.bty = "o",
    main = "Interaction Plot of Ethanol and air/fuel ratio", xlab = "Ethanol",ylab = "CO emissions")))

# Example 9 p. 68 Figure 3.6
Ethanol <- COdata$Eth
with(COdata, (interaction.plot(Ratio, Ethanol, CO, type = "b",pch = c(18,24,22), leg.bty = "o",
    main="Interaction Plot of Ethanol and air/fuel ratio", xlab = "Ratio", ylab = "CO emissions")))

# Example 10 p. 71
library(daewr)
rmin <- 2 # smallest number of replicates
rmax <- 8 # largest number of replicates
sigma <- .32
alpha <- .05
Delta <- 1
nlev <- 16
nreps <- c(rmin:rmax)
power <- Fpower1(alpha, nlev, nreps, Delta, sigma)
options(digits = 5)
power

# Example 11 p. 71 - 72 ##
library(daewr)
rmin <- 2 # smallest number of replicates
rmax <- 4 # largest number of replicates
alpha <- .05
sigma <- .32
Delta <- 1.0
nlev <- c(4,4)
nreps <- c(rmin:rmax)
result <- Fpower2(alpha, nlev, nreps, Delta, sigma)
options(digits = 5)
result

# Example 12 p. 73
COdatam <- COdata
COdatam[18, 3] <- NA

# Example 13 p. 74
library(car)
mod2 <- lm( CO ~ Eth*Ratio, data = COdatam, contrasts = list( Eth = contr.sum, Ratio = contr.sum ))
Anova( mod2, type="III" )

# Example 14 p. 75
p <- data.frame( expand.grid( Eth = c(.1, .2, .3), Ratio = c(14,15,16) ) )
p[] <- lapply(p, factor)
p <- cbind( yhat = predict( mod2, p), p)
with(p, tapply(yhat, Ratio, mean) )

# Example 15 p. 75
library(lsmeans)
lsmeans(mod2,~ Eth)

# Example 16 p. 75
lsmeans(mod2,~Ratio)

# Example 17 p. 76
library(daewr)
data(COdata)
Cellmeans <- tapply( COdata$CO, list(COdata$Eth, COdata$Ratio), mean )
dim(Cellmeans) <- NULL
Eth <- factor(rep(c(.1, .2, .3), 3))
Ratio <- factor(rep(c(14,15,16), each=3))
cells <- data.frame( Eth, Ratio, Cellmeans )
modnr <- lm(Cellmeans ~ Eth*Ratio, data=cells )
anova(modnr)

# Example 18 p. 77
Ethc <- as.ordered(cells$Eth)
Ratioc <- as.ordered(cells$Ratio)

# Example 19 p. 77
EthLin<-contr.poly(Ethc)[Ethc,".L"]
RatioLin <-contr.poly(Ratioc)[Ratioc,".L"]
mbo <-lm(Cellmeans~Ethc + Ratioc + EthLin:RatioLin, data=cells)
anova(mbo)

# Example 20 p. 78
Pred <-predict(mbo, newdata=data.frame(Ethc, Ratioc, EthLin, RatioLin))
pred.means <- aggregate(Pred, by=list(Ethc = Ethc, Ratioc = Ratioc), "mean")
Ethanol <- pred.means$Ethc
interaction.plot(pred.means$Ratioc, Ethanol, pred.means$x, type="b", pch = c(18,24,22), leg.bty ="o",
                 xlab = "Ratio", ylab = "predicted CO emissions")

# Example 21 p. 80
library(daewr)
Tukey1df(virus)

# Example 22 p. 82
library(daewr)
data(web)
head(web)

# Example 23 p. 83
modb <- glm( cbind(signup, visitors-signup) ~ A * B * C * D, data = web, family = binomial )
anova(update(modb, .~ A+B + A:B + C + A:C + B:C + A:B:C + D + A:D+B:D + A:B:D + C:D + A:C:D + 
  B:C:D + A:B:C:D ), test = "Chisq")

# Example 24 p. 84
prop <- web$signup / web$visitors
webp <- data.frame(web,prop)
par ( mfrow = c(1,3) )
webp1 <- subset(webp, A == 1)
interaction.plot(webp1$C, webp1$D, webp1$prop, type = "l",legend=FALSE, 
  ylim = c(.015,.0275), main = "Background = 1", xlab = "Text Color", ylab = "Proportion Signing-up")
webp2 <- subset(webp, A == 2 )
interaction.plot( webp2$C, webp2$D, webp2$prop, type = "l", legend = FALSE, 
    ylim = c(.015,.0275), main = "Background = 2", xlab = "Text Color", ylab = " ")
lines( c(1.7,1.85), c(.016,.016), lty = 2)
lines( c(1.7,1.85), c(.017,.017) ,lty = 1)
text(1.3, .017, "Sign-up link ")
text(1.3, .016, "Sign-up Button" )
text(1.4, .018, "LEGEND" )
webp3 <- subset(webp, A == 3)
interaction.plot(webp3$C, webp3$D, webp3$prop, type = "l",  legend=FALSE, ylim = c(.015,.0275),
  main="Background = 3", xlab = "Text Color", ylab = " ")

# Example 25 p. 91 
library(daewr)
library(FrF2)
modv <- lm( y ~ A*B*C, data=volt, contrast=list(A=contr.FrF2, B=contr.FrF2, C=contr.FrF2))
summary(modv)

# Example 26 p. 93 produces Figure 3.13 p. 92
C_Warmup=volt$C
with(volt, (interaction.plot(A, C_Warmup, y, type = "b",
            pch = c(24,22), leg.bty = "o",
            xlab = "Temperature",ylab = "Voltage")))

# Example 27 p. 97
library(daewr)
modf <- lm( y ~ A*B*C*D, data = chem)
summary(modf)

# Example 28 p. 98 produces Figure 3.15 p. 98
library(daewr)
fullnormal(coef(modf)[-1],alpha=.025)

# Example 29 p. 99 produces Figure 3.16
library(daewr)
LGB( coef(modf)[-1], rpt = FALSE)

# Example 30 p. 100 produces Figure 3.17
library(daewr)
with(chem, (interaction.plot( A, B, y, type = "b", pch = c(18,24), 
            main = "Interaction Plot of Catalyst by Excess A",
            xlab = "Excess Reactant A", ylab = "Percent Conversion")))

# Example 31 p. 101 produces Figures 3.18 
par( mfrow = c(2,1) )
library(BsMD)
LenthPlot(modf, main = "Lenth Plot of Effects")
X <- model.matrix(modf)[ , 2:16]
y <- chem$y
Chem.BsProb <- BsProb( X = X, y = y, blk = 0, mFac = 15,
                       mInt = 1, p = 0.2, g = 2.49, ng = 1, nMod = 10)
plot( Chem.BsProb, main = "Bayes Plot of Effects" )

# Figure 3.19 (without Gap annotation)
modB<-lm(y ~ A*B*C*D, data = BoxM)
fullnormal(coef(modB)[-1],alpha=.2)

# Example 32 p. 103 also produces 3.20
library(daewr)
data(BoxM)
Gaptest(BoxM)
