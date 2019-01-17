# Figure 6.1 p. 197
library(FrF2)
design <- FrF2(8, 4, randomize = FALSE)
library(daewr)
colormap(design, mod=3)

# Example 1 p. 198
library(FrF2)
design <- FrF2( 16, 5, generators = "ABCD", randomize = FALSE)
design

# Example 2 p. 199
library(FrF2)
y <- runif(16, 0, 1)
aliases( lm( y~ (.)^4, data = design))

# Example 3 p. 201
library(FrF2)
soup <- FrF2(16, 5, generators = "ABCD", factor.names = list(Ports=c(1,3), Temp=c("Cool","Ambient"), 
                                                             MixTime=c(60,80),BatchWt=c(1500,2000), delay=c(7,1)), randomize = FALSE)
y <- c(1.13, 1.25, .97, 1.70, 1.47, 1.28, 1.18, .98, .78,
       1.36, 1.85, .62, 1.09, 1.10, .76, 2.10 )
library(DoE.base)
soup <- add.response( soup , y )
mod1 <- lm( y ~ (.)^2, data = soup)
summary(mod1)

# Example 4 p. 202 produces Figure 6.2
soupc<-FrF2(16,5,generators="ABCD",randomize=FALSE)
soupc<-add.response(soupc, y)
modc<-lm(y~(.)^2, data=soupc)
library(daewr)
LGB(coef(modc)[-1], rpt = FALSE)

# Figure 6.3 P. 203
delay <- as.numeric(sub(-1, 7, soup$delay))
temp <- soup$Temp
interaction.plot(delay, temp, soup$y, type="b",pch=c(24,18,22), leg.bty="o",
                 main="Interaction Plot for Mixing Temperature by Delay time",
                 xlab="Delay Time (days)", ylab="Average S.D. Fill Weight")

#### Use of IAPlot function to create interacton plots  #############
######         like Figuers 6.3 and 6.4              ################
## Note Delay time axis is reversed from Figures 6.3 and 6.4 ########
## due to the way it is coded in Table 6.3 and the code on p 201 ####
library(FrF2)
IAPlot(soup, sel=c(2,4,5), abbrev=7)
#####################################################################

# Figure 6.4 p. 204
BatchWt <- ((as.numeric(soup$BatchWt)-1.5)/.5)*250+1750
interaction.plot(delay, BatchWt, soup$y, type="b",pch=c(18,24,22), leg.bty="o",
                 main="Interaction Plot for Batch Weight by Delay time",
                 xlab="Delay Time (days)", ylab="Average S.D. Fill Weight")

# Example 5 p. 206
library(FrF2)
frac <- FrF2( 8, 6, generators = c("AB", "AC", "BC"))
frac

# Example 6 p. 209
library(FrF2)
des1 <- FrF2( 16, 8 )
y <- runif( 16, 0, 1 )
library(DoE.base)
generators(des1)
aliases( lm( y ~ (.)^3, data = des1) )

# Example 7 p. 211
library(FrF2)
culture <- FrF2( 16, generators = c("BCD", "ACD", "ABC", "ABD"), randomize = FALSE)
y1 <- c(5.75, 6.7, 11.12, 10.67, 4.92, 5.35, 2.81, 10.83, 6.08, 7.27, 9.68, 4.2, 
        3.9, 3.78, 11.57, 7.39 )
culture <- add.response( culture, y1 )
culture

# Example 8 p. 212
modf <- lm( y1 ~ (.)^2, data = culture)
summary(modf)

# Figure 6.5 P. 213
library(daewr)
cfs <- coef(modf)[2:16]
names <- names(cfs)
halfnorm(cfs, names, alpha = .25, refline = FALSE)

# Example 9 p. 214
culture2 <- FrF2( 16, 5, factor.names = c("B", "C", "E", "G", "H"), randomize = FALSE)
y <- c(3.37, 3.55, 3.78, 2.81, 5.53, 10.43, 5.35, 11.57, 2.93, 7.23, 3.9, 10.83, 
       11.69, 10.59, 4.92, 7.39)
culture2 <- add.response( culture2, y )
culture2

# Figure 6.6 p. 215
moda <- lm(y ~ (.)^2, data = culture2)
library(daewr)
cfs <- coef(moda)[2:16]
effects <- cfs
names <- names(cfs)
halfnorm(cfs, names, alpha = .2, refline = FALSE)

# Figure 6.7 p. 215
RiceBran <- 10*((as.numeric(culture2$B)-1.5 ) / .5) + 20
FermTime <- 12*((as.numeric(culture2$H)-1.5) / .5) + 84
interaction.plot(FermTime, RiceBran, culture2$y, type = "b", pch=c(18,24,22), leg.bty="o",
                 main="Interaction Plot for Fermentation Time and Level of Rice Bran",
                 xlab = "Fermentation Time(hrs)", ylab = "Biomass")

# Figure 6.8 p. 216
Urea <- 1*((as.numeric(culture2$C)-1.5 ) / .5) + 1
AmonSulf <- 1*((as.numeric(culture2$E)-1.5) / .5) + 1
interaction.plot(AmonSulf, Urea, culture2$y, type = "b", pch=c(18,24,22), leg.bty="o",
                 main="Interaction Plot of Urea and Ammonium Sulfate",
                 xlab = "Ammonium Sulfate(g/L)", ylab = "Biomass")

# Figure 6.9 p. 216
Urea <- 1*((as.numeric(culture2$C)-1.5 ) / .5) + 1
FermTime <- 12*((as.numeric(culture2$H)-1.5) / .5) + 84
interaction.plot(FermTime, Urea, culture2$y, type = "b", pch=c(18,24,22), leg.bty="o",
                 main="Interaction Plot of Fermentation Time and Urea",
                 xlab = "Fermentation Time(hrs)", ylab = "Biomass")


# Figure 6.10 p. 217 (left Side)
culture3 <- culture2[culture2$B == -1]
SodPhos <- 1*((as.numeric(culture3$G)-1.5 ) / .5) + 1
AmonSulf <- 1*((as.numeric(culture3$E)-1.5) / .5) + 1
interaction.plot(AmonSulf, SodPhos, culture3$y, type = "b", pch=c(18,24,22), leg.bty="o",
                 main="Rice Bran = 10g/L",
                 xlab = "Ammonium Sulfate(g/L)", ylab = "Biomass")

# Figure 6.10 p. 217 (Right Side)
culture4 <- culture2[culture2$B == 1]
SodPhos <- 1*((as.numeric(culture4$G)-1.5 ) / .5) + 1
AmonSulf <- 1*((as.numeric(culture4$E)-1.5) / .5) + 1
interaction.plot(AmonSulf, SodPhos, culture4$y, type = "b", pch=c(18,24,22), leg.bty="o",
                 main="Rice Bran = 30g/L",
                 xlab = "Ammonium Sulfate(g/L)", ylab = "Biomass")

# Example 10 p. 220
library(FrF2)
des <- FrF2(8, 6, generators = c("AB", "AC", "BC"), randomize = FALSE)
desa <- fold.design(des, columns = 'full')
desa

# Example 11 p. 227
library(FrF2)
des2 <- FrF2( 8, 7, generators = c("AB", "AC", "BC", "ABC" ), randomize=FALSE)
augm <- fold.design(des2)

A <- (as.numeric( augm$A) - 1.5 ) / .5
B <- (as.numeric( augm$B) - 1.5 ) / .5
C <- (as.numeric( augm$C) - 1.5 ) / .5
D <- (as.numeric( augm$D) - 1.5 ) / .5
E <- (as.numeric( augm$E) - 1.5 ) / .5
F <- (as.numeric( augm$F) - 1.5 ) / .5
G <- (as.numeric( augm$G) - 1.5 ) / .5
Block <- augm$fold
augmn <- data.frame(A, B ,C, D, E, F, G, Block)

# Example 12 p. 228
library(AlgDesign)
cand <- gen.factorial( levels = 2, nVar = 7, varNames = c("A","B", "C", "D", "E", "F", "G"))

Block <- rep('cand', 128)
cand <- data.frame( A=cand$A, B=cand$B, C=cand$C, D=cand$D, E=cand$E, F=cand$F, G=cand$G, Block)
all <- rbind( augmn, cand)

fr<-1:16
optim <- optFederov( ~ A + B + F + I(A*D) + I(C*F), data = all, nTrials = 24, criterion = "D",
                     nRepeats = 10, augment = TRUE, rows=fr)

newruns <- optim$design[ 17:24, ]
newruns

# Example 13 p. 230
library(FrF2)
pb( nruns = 12, randomize=FALSE)

# Example 14 p. 231
library(BsMD)
data( PB12Des, package = "BsMD" )
colnames(PB12Des) <- c("c11", "c10", "c9", "c8", "G", "F", "E", "D", "C", "B", "A")
castf <- PB12Des[c(11,10,9,8,7,6,5,4,3,2,1)]
y <- c(4.733, 4.625, 5.899, 7.0, 5.752, 5.682, 6.607, 5.818, 5.917, 5.863, 6.058, 4.809)
castf <- cbind( castf, y )

# Example 15 p. 232 creates Figure 6.12
modpb <- lm( y ~ (.), data = castf )
library(daewr)
cfs <- coef(modpb)[2:12]
names<-names(cfs)
halfnorm(cfs, names, alpha = .35, refline=FALSE)

# Figure 6.13 (a) p. 233
#Makes color map for Plackett-Burman Design
library(daewr)
data(castf)
castfr <- castf[ , c(1:7)]
colormap(castfr,mod=2)

# Figure 6.13 (b) p. 233
# Makes color map for saturated 2^(7-4) design in Figure 6.14 p. 197
library(FrF2)
design <-FrF2( 8, 7)
library(daewr)
colormap(design, mod=2)

# Example 15 pp. 234 - 235 creates Figures 6.14 and 6.15
castfr <- castf[ , c(1:7, 12)]
library(leaps)
modpbr<-regsubsets(y ~ (.)^2, data=castfr, method = "exhaustive", nvmax = 8, nbest = 4)
rs <- summary(modpbr)
plot(c(rep(1:8,each=4)), rs$adjr2, xlab="No. of Parameters", ylab="Adjusted R-square")
plot(modpbr,scale="r2")

# Example 16 p. 237
library(daewr)
OPB<-OptPB(20, 9, randomize = FALSE)
head(OPB)

# Example 17 p, 237
library(daewr)
ascr <- Altscreen(6, randomize= FALSE)
head(ascr)

# Example 18 p. 238
library(daewr)
ModelRobust()
MR8 <- ModelRobust('MR8m5g2')
head(MR8)

# Figure 6.16 (a) Alternate Screening Design p. 238
library(daewr)
ascr<-Altscreen(7)
colormap(ascr, mod=2)

# Figure 6.16 (b) Model Robust Design p. 238
library(daewr)
MR16 <- ModelRobust('MR16m7g5', randomize = FALSE)
colormap(MR16, mod=2)

# Example 19 p. 239
null <- lm( y ~ 1, data = castfr )
up <- lm( y ~ (.)^2, data = castfr )
step( null, scope = list(lower = null, upper = up), direction="forward", steps=4)

# Example  20 p. 240-241
des <- castfr[ ,c(1, 2, 3, 4, 5, 6,7 )]
y <- castfr[ ,8]
library(daewr)
trm <- ihstep( y, des )
trm <- fhstep( y, des, trm)
trm <- bstep(y, des, trm)
trm <- bstep(y, des, trm)
trm <- bstep(y, des, trm)

# Example 21 p. 244
# Find an orthogonal array 
library("DoE.base")
show.oas(factors=list(nlevels=c(3,2),number=c(5,2)))

# Example 22 p. 244
#find orthogonal array
des<-oa.design(nlevels=c(3,3,3,3,3,2,2),nruns=36,columns="min3",randomize=TRUE,seed=104)

# Example 23 p. 245
#Create 18 run near-orthogonal array
library(DoE.base)
cand<-oa.design(nlevels=c(3,3,3,3,3,2,2),nruns=36,columns="min3",seed=104)
#Use Federov algorithm to select D-optimal 18-run subset from the from candidates
# that will allow estimation of the main effects
library(AlgDesign)
optim<-optFederov(~A+B+C+D+E+F+G,cand,nRepeats=10,nTrials=18,criterion="D")
optim

# Example 24 p. 245
#Allow for specific interactions to be estimable
optim<-optFederov(~A+B+C+D+E+F+G+E:F+F:G,cand,nRepeats=10,nTrials=18,criterion="D")

# Example 25 p. 246
# This shows there is no 12-run OA for this case
show.oas(factors=list(nlevels=c(4,3,2),number=c(2,1,1)))

# create candidate design using 48 run OA
cand<-oa.design(nlevels=c(4,4,3,2),randomize=FALSE,seed=2013)
#Use Federov algorithm to select D-optimal 12-run subset from the from candidates
# that will allow estimation of the main effects
library(AlgDesign)
optim<-optFederov(~A+B+C+D,cand,nRepeats=10,nTrials=12,criterion="D",aug=FALSE)
optim

# Example 26 p. 247
library(daewr)
data(hardwood)
modh<-lm(Rating~Price+Density+Guarantee+Design,data=hardwood)
anova(modh)

# Example 27 p. 248
library(daewr)
des <- DefScreen( m = 8, c = 2, randomize = FALSE )
head(des)

# Figure 6.17 p. 249
library(daewr)
defscr <- DefScreen(m=8)
y<-runif(nrow(defscr),0,1)
test<-model.matrix(lm(y~(.)^2,data=defscr))
q <- I(test[,2:9])^2
colnames(q) <- c("A^2", "B^2", "C^2", "D^2", "E^2", "F^2", "G^2", "H^2")
defs <- data.frame(cbind(test[, 2:9],q,test[,10:37 ]))
colormap(defs,mod=1)
