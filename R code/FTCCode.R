###R Basics ######

## Program Interface

#Slide 17-Running Commands from an RScript
x<-c(1,2,3,4,5)
y<-x^2+runif(5,0,1)
data.frame(x,y)

#Slide 18-Making a Plot in R
y<-rnorm(10,100,10)
plot(y,type="o")

## R Packages 

#Slide 19-Installing an R Package 
library(DoE.base)

#Slide 24-Running a function in a loaded package (DoE.Base) 
fac.design(factor.names=list(A=c(10,20),B=c("old","new"), C=c("min","med","max")))

### Design and Analysis of Two-Level Factorials###

## Creating and Analyzing Two-Level Factorials in R

#Slide 51-Creating a two-level factorial design with R FrF2
library(FrF2)
Design.p9<-FrF2(nruns=32, nfactors=5, blocks=1, ncenter=0, replications=1,randomize=FALSE, 
                factor.names=list(Ratio=c(0.1,0.9),Gas_flow=c(40,60),Pressure=c(300,1200),Temperature=c(300,460), Power=c(10,60)))
y1<-c(1.92,3.06,1.96,3.33,1.87,2.62,1.97,2.96,1.94,3.53,2.06,3.75,1.96,3.14,2.15,
      3.43,1.95,3.16,2.01,3.43,1.88,2.14,1.98,2.81,1.97,3.67,2.09,3.73,1.98,2.99,2.19,
      3.39)
y2<-c(1.79,10.10,3.02,15.00,19.70,11.20,35.70,36.20,2.31,5.58,2.75,14.50,20.70,11.70,31.00,39.00,3.93,12.40,6.33,23.70,35.30,15.10,57.10,45.90,5.27,12.30,6.39,30.50,30.10,14.50,50.30,47.10)
Design.p9 <-add.response(Design.p9, y1, replace=FALSE)
Design.p9 <-add.response(Design.p9, y2, replace=FALSE)

#Slide 52-Creating a two-level factorial design with R FrF2
print( Design.p9, std.order=TRUE)

#Slide 55-Example analysis of a replicated 23 factorial
library(daewr)
volt

#Slide 56-Example analysis of a replicated 23 factorial
library(FrF2)
modv<-lm(y ~ A*B*C, data=volt, contrast=list(A=contr.FrF2,B=contr.FrF2, C=contr.FrF2))
summary(modv)

#Slide 57-Example analysis of a replicated 23 factorial 
C_Warmup=volt$C
with(volt, (interaction.plot(A, C_Warmup, y, type="b", pch=c(24,22), leg.bty="o",
                             xlab="Temperature", ylab="Voltage")))

#Slide 58-Example analysis of a replicated 23 factorial
library(daewr)
data(chem)
chem
class(chem$A)

#Slide 61-Example analysis of an unreplicated 24 design
modf<-lm(y~ A*B*C*D, data=chem)
summary(modf)

#Slide 62-Example analysis of an unreplicated 24 design
fullnormal(coef(modf)[-1], alpha=.025)
LGB(coef(modf)[-1], rpt=FALSE)

#Slide 63-Example analysis of an unreplicated 24 design 
library(BsMD)
LenthPlot(modf, main = "Lenth Plot of Effects")

#Slide 64-Example analysis of an unreplicated 24 design
with(chem, (interaction.plot(A, B, y, type="b", pch=c(18,24), main="Interaction Plot of Catalyst by Excess A",
                             xlab="Excess Reactant A", ylab="Percent Conversion")))

#Slide 66-Example analysis of an unreplicated design with an outlier
library(daewr)
data(BoxM)
BoxM
modB<-lm(y~A*B*C*D, data=BoxM)

#Slide 67-Example analysis of an unreplicated design with an outlier
fullnormal(coef(modB)[-1], alpha=.2)

#Slides 68-69 - Example analysis of an unreplicated design with an outlier
library(daewr)
Gaptest(BoxM)

## Blocking Two-Level Factorials

#Slide 72-Create the design with FrF2
library(FrF2)
Bdish<-FrF2(16, 4, blocks=c("ABD", "BCD"), alias.block.2fis=TRUE, randomize=FALSE)
Bdish

#Slide 73-Create the design with FrF2
y<-c(0, 0, 12, 14, 1, 0, 1, 11, 10, 2, 33, 24, 3, 5, 41, 70)
Bdish<-add.response(Bdish, response=y)
Bdish

#Slide 74-Analyze the design ignoring blocks
mudu<-lm(y~ A*B*C*D, data=Bdish)
fullnormal(coef(mudu)[-1], alpha=.1)

#Slide 75-Analyze the design accounting for blocks
dish <-lm( y ~ Blocks + A * B * C * D, data = Bdish)
effects <-coef(dish)
effects <-effects[5:19] 
effects <-effects[ !is.na(effects) ]
library(daewr)
halfnorm(effects, names(effects), alpha=.25) 

#Slide 76-An unlikely interaction
x <-as.numeric(Bdish$B)
x[x=="1"] <-"1 tbs"
x[x=="2"] <-"2 tbs"
Brand <-as.numeric(Bdish$D)
Brand[Brand==1] <-"WF"
Brand[Brand=="2"] <-"UP"
interaction.plot(x, Brand, Bdish$y, type="l" ,xlab="Soap Amount B",ylab="Average Clean Squares")

#Slides 78 - 78 -Criteria for choosing block defning contrasts
library(FrF2)
Blocked25<-FrF2(32, 5, blocks=8, alias.block.2fis=TRUE, randomize=FALSE)
summary(Blocked25)

## Restrictions on Randomization - Split-Plot Designs

#Slide 86-Create the design with FrF2
library(FrF2)
FrF2(32, 4, WPs = 8, nfac.WP= 2, factor.names= (c("A","B","C","D")))

#Slide 87-The data frame sausage is in the daewr package
library(daewr)
library(lme4)
rmod2<-lmer(ys~ A + B + A :B + (1|Block) + (1|A:B:Block) + C + D + C:D + A:C + A:D + B:C + B:D + A:B:C + 
             A:B:D + A:C:D + B:C:D + A:B:C:D, data=sausage)

#Slide 88-Analysis of the fixed Effects 
anova(rmod2)

#Slide 90-The data frame plasma is in the daewr package
library(daewr)
sol <-lm(y ~ A*B*C*D*E, data = plasma)
effects <-coef(sol)
effects <-effects[c(2:32)]
Wpeffects<-effects[c(1:4, 6:11, 16:19, 26)]
Speffects<-effects[c(5,12:15,20:25,27:31)]

#Slide 91-Analysis by normal plot of all effects is misleading
fullnormal(effects, names(effects), alpha = .10)

#Slide 92-Normal plot of whole-plot effects
fullnormal(Wpeffects, names(Wpeffects), alpha = .10)

#Slide 93-Normal plot of split-plot effects
fullnormal(Speffects, names(Speffects), alpha=.05)

### Preliminary #####

## One-Factor Designs

#Slide 101-Method of Moments Estimators
library(daewr)
mod1<-aov(weight~ batch, data=soupmx)
summary(mod1)

#Slide 105-Maximum Likelihood and REML estimators
library(daewr)
library(lme4)
mod2<-lmer(weight ~ 1 +(1|batch), data=soupmx)
summary(mod2)

## Staggered Nested Designs for Multiple Factors

#Slide 113-Method of moments estimators
library(daewr)
mod2<-aov(strength ~ lot + lot:box + lot:box:prep, data = polymer)
summary(mod2)

#Slide 114-REML estimators
library(daewr)
library(lme4)
mod3<-lmer(strength ~ 1 + (1|lot) +(1|lot:box) + (1|lot:box:prep), data=polymer)
summary(mod3)

## Graphical Methods to Check Assumptions

#Slide 116-Computing and graphing variances in R
library(daewr)
data(polymer)
y<-array( polymer$strength, c(4,30) )
sd1 <-sqrt( (y[2,] -y[1,])**2 / 2)
sd2 <-sqrt( (2/3) * ( y[3,] -(y[1,] + y[2,]) / 2)**2 )
sd3 <-sqrt( (3/4) * (y[4,] -(y[1,] + y[2,] + y[3,] )/3 )**2)
osd2 <-sort(sd2)
r<-c( 1: length(sd2))
zscore<-qnorm( ( ( r -.5 ) / length(sd2) +1 )/ 2)
plot(zscore,osd2, main="Half-normal plot of prep(box) standard deviations", xlab="Half Normal Score",
      ylab="std. due to prep within box")

## Chemistry Example

#Slide 123-Exploration Experiment 1
Batch<-c(rep(1:10,each=3, len=30))
Oven<-c(rep(1:3,10))
PoreV<-c(1.05,1.35,1.13,1.21,1.39,1.28,1.26,1.41,1.25,1.27,1.40,1.28,1.20,1.42,1.17,1.19,1.33,1.22,1.18,
         1.37,1.08,1.22,1.30,1.18,1.21,1.39,1.11,1.17,1.27,1.00)
SA<-c(172,188,164,183,193,190,182,189,183,172,183,172,171,189,171,175,180,179,165,183,163,167,169,184,
      173,186,165,156,168,155)
Exp1<-data.frame(Batch, Oven, PoreV,SA)
Exp1

#Slide 124-Analysis of Exploration Experiment 1
library(lme4)
modE1<-lmer(PoreV ~ 1 + (1|Batch), data=Exp1)
summary(modE1)

#Slide 125-Analysis of Exploration Experiment 1
library(lme4)
modE1<-lmer(SA ~ 1 + (1|Batch), data=Exp1)
summary(modE1)

#Slide 126-Residual Variability
boxplot(SA~Oven, data=Exp1, ylab="Surface Area", xlab="Oven Number")
boxplot(PoreV~Oven, data=Exp1, ylab="Pore Volume", xlab="Oven Number")

### Screening ###

## Half-Fractions of Two-Level Factorial Designs

#Slide 146-Creating the Design with FrF2 
library(FrF2)
soup <- FrF2(16, 5, generators = "ABCD", factor.names = list(A=c(1,3),B=c("Cool","Ambient"),
            C=c(60,80),D=c(1500,2000), E=c(7,1)), randomize = FALSE)
soup

#Slide 147-Adding the Responses
y <- c(1.13, 1.25, .97, 1.70, 1.47, 1.28, 1.18, .98, .78, 1.36, 1.85, .62, 1.09, 1.10, .76, 2.10 )
library(DoE.base)
soup <- add.response( soup , y )
soup

#Slide 148-Checking the Alias Pattern
mod1 <- lm( y ~ (.)^4, data = soup)
aliases(mod1)

#Slide 150-Analyzing the Data
mod2<-lm(y~(.)^2, data=soup)
summary(mod2)

#Slide 151-Half-Normal Plot of Coefficients
library(daewr)
LGB(coef(mod2)[-1], rpt=FALSE)

#Slide 152-Interpretation of Results
soup <- FrF2(16, 5, generators = "ABCD", factor.names =list(Ports=c(1,3),Temp=c("Cool","Ambient"), MixTime=c(60,80),
            BatchWt=c(1500,2000), delay=c(7,1)), randomize = FALSE)                                                           
y <- c(1.13, 1.25, .97, 1.70, 1.47, 1.28, 1.18, .98, .78, 1.36, 1.85, .62, 1.09, 1.10, .76, 2.10 )
library(DoE.base)
soup <- add.response( soup , y )
delay <- as.numeric(sub(-1, 7, soup$delay))
temp <- soup$Temp
interaction.plot(delay, temp, soup$y, type="b",pch=c(24,18,22), leg.bty="o",
                 main="Interaction Plot for Mixing Temperature by Delay time",
                 xlab="Delay Time (days)", ylab="Average S.D. Fill Weight")


## One-Quarter and Higher Fractions of Two-Level Factorial Designs

#Slide 158-Create the Design in FrF2
library(FrF2)
frac <- FrF2( 16, 6, generators = c("AB", "AC"),randomize=FALSE)
frac

#Slide 159-View the Alias Structure
y <- runif( 16, 0, 1 )
aliases( lm( y ~ (.)^3, data = frac) )

#Slide 160-Some Generators Better than Others
frac <- FrF2( 16, 6, generators = c("ABC", "BCD"),randomize=FALSE)
aliases( lm( y ~ (.)^3, data = frac) )

## Criteria for Choosing Generators for Fractional Factorial Designs

#Slide 163-FrF2 Default - Minimum Aberration Design
## maximum resolution minimum aberration design with 9 factors in 32 runs
## show design information instead of design itself
design.info(FrF2(32,9))

#Slide 164-FrF2 Option-Maximum Number of Clear Effects
## maximum number of free 2-factor interactions instead of minimum aberration
## show design information instead of design itself
design.info(FrF2(32,9,MaxC2=TRUE))

#Slide 167-Create Design with FrF2 in Coded Factor Levels
arsrm<-FrF2(8,6,generators = c("AB", "AC", "BC"), randomize=FALSE)
y<-c(69.95, 58.65, 56.25, 53.25, 94.40, 73.45, 10.0, 2.11)
library(DoE.base)
arsrm2<-add.response(arsrm,y)
arsrm2

#Slide 168-Analysis of the Data
Lmod<-lm(y ~ (.)^2,data=arsrm2)
estef<-coef(Lmod)[c(2:7,12)]
library(daewr)
LGB(estef,rpt=FALSE)
aliases(Lmod)

#Slide 172-Creating a Minimum Aberration Split-Plot Fractional Factorial with FrF2
library(FrF2)
SPFF2 <-FrF2(16,6, WPs = 4, nfac.WP = 3, factor.names = c("A","B","C","P","Q","R"))
print(SPFF2)

#Slide 173-Checking the Alias Pattern
y<-rnorm(16,0,1)
aliases(lm( y ~ (.)^3, data=SPFF2))

#Slide 176-Analysis with R
spexp <- FrF2(16,5,WPs=8,nfac.WP=3, factor.names=c("A","B","C","P","Q"),randomize=FALSE)
y<-c(18.0,21.5,27.5,17.0,22.5,15.0,19.0,22.0,13.0,-4.5,17.5,14.5,0.5,5.5,24.0,13.5)
sol<-lm( y~A*B*C*P*Q, data=spexp)
summary(sol)

#Slide 177-Separate Normal Plots of Whole-Plot and Sub-Plot Effects
library(daewr)
effects <-coef(sol)
Wpeffects <- effects[ c(2:4, 7:9, 16) ]
Speffects <- effects[ c(5:6, 10:15) ]
fullnormal(Speffects, names(Speffects), alpha=.20)
fullnormal(Wpeffects, names(Wpeffects), alpha=.10)

## Augmenting Fractional Factorial Designs to Resolve Confounding 

#Slide 179-Augmenting the IOCS Experiment
library(DoE.base)
arsrm3<-fold.design(arsrm, columns='full')
y<-c(69.95,58.65,56.25,53.25,94.4,73.45,10.0,2.11,16.2,52.85,9.05,31.1,7.4,9.9,10.85,48.75)
arsrm4<-add.response(arsrm3,y)
arsrm4

#Slide 182-Change Factors to Numeric in New Data Frame
A <- (as.numeric(arsrm3$A)-1.5)/.5
B <- (as.numeric(arsrm3$B)-1.5)/.5
C <- (as.numeric(arsrm3$C)-1.5)/.5
D <- (as.numeric(arsrm3$D)-1.5)/.5
E <- (as.numeric(arsrm3$E)-1.5)/.5
F <- (as.numeric(arsrm3$F)-1.5)/.5
Block<-arsrm3$fold
augmn<-data.frame(A,B,C,D,E,F,Block)
augmn

#Slide 183-Use Federov Algorithm in AlgDesign Package to Find 8 Additional Runs that Maximize the Determinant
library(AlgDesign)
cand<-gen.factorial(levels = 2, nVar = 6, varNames = c("A","B","C","D","E","F"))
Block<-rep('cand',64)
cand<-data.frame(A=cand$A, B=cand$B, C=cand$C, D=cand$D, E=cand$E, F=cand$F,Block)
all<-rbind(augmn, cand)
fr<-1:16
optim<-optFederov( ~ A + B + F + I(A*D) + I(C*F), data=all, nTrials =24, criterion = "D", 
                   nRepeats =10, augment=TRUE, rows=fr)
newruns<-optim$design[ 17:24, ]
newruns

## Plackett-Burman and Model Robust Screening Designs

#Slide 185-Creating a PB Design with FrF2
library(FrF2)
pb( nruns = 12, randomize=FALSE)

#Slide 187-Recall the Design from the BsMD package
data( PB12Des, package = "BsMD" )
colnames(PB12Des) <- c("c11", "c10", "c9", "c8", "G", "F", "E", "D", "C", "B", "A")
castf <- PB12Des[c(11,10,9,8,7,6,5,4,3,2,1)]
castf

#Slide 188-Analysis Shows only Factor F Possibly Signifcant
y<-c(4.733, 4.625, 5.899, 7.0, 5.752, 5.682, 6.607, 5.818, 5.917, 5.863, 6.058, 4.809)
castf<-cbind(castf,y)
modpb<-lm(y~ (.), data=castf)
library(daewr)
cfs<-coef(modpb)[2:12]
names<-names(cfs)
halfnorm(cfs, names, alpha = .35, refline=FALSE)

#Slide 191-istep, fstep, bstep Functions in daewr Package Perform this Algorithm - FG interaction rst term entered
des<-castf[ , c(1:7)]
y<-castf[ ,12]
library(daewr)
trm<-ihstep(y,des)

#Slide 193-Alternative to Plackett-Burman when 16 Runs Needed
library(daewr)
ascr <-Altscreen(nfac = 6, randomize = FALSE)
head(ascr)

#Slide 194-Alternative to Plackett-Burman when 16 Runs Needed
library(daewr)
MR8 <- ModelRobust('MR8m5g2', randomize = FALSE)
head(MR8)

## Optimization ##

## Standard Designs for Second Order Models

#Slide 210-Variance Dispersion Graph Shows UP Characteristic
library(daewr)
data(cement)
des<-cement[, 2:4]
library(Vdgraph)
Vdgraph(des)

#Slide 211-Creating a Central Composite Design in R
library(rsm)
rotd <- ccd(3, n0 = c(4,2), alpha = "rotatable", randomize = FALSE)
rotd

#Slide 212-Creating a Central Composite Design in R
library(rsm)
ccd.up<-ccd(y~x1+x2+x3,n0=c(4,2),alph="rotatable",coding=list(x1~(Temp-150)/10,
        x2~(Press-50)/5,x3~(Rate-4)/1),randomize=FALSE)
head(ccd.up)

#Slide 214-Creating a Box-Behnken Design in R
# create design with rsm
library(rsm)
bbd3 <- bbd(3,randomize=FALSE,n0=3)
library(Vdgraph)
Vdgraph(bbd3[ , 3:5])

#Slide 218-Comparing Two Designs with Vdgraph
library(rsm)
ccd.up<-ccd(y~x1+x2+x3,n0=c(4,2),alph="rotatable",coding=list(x1~(Temp-150)/10,
          x2~(Press-50)/5,x3~(Rate-4)/1),randomize=FALSE)
head(ccd.up)

#Slide 219-Comparing Two Designs with Vdgraph
library(Vdgraph)
data(D310)
D310
des<-transform(D310,Temp=10*x1+150, Press=5*x2+50,Rate=x3+4)
des

#Slide 220-Comparing Two Designs with Vdgraph
Compare2Vdg(des[, 4:6],ccd.up[, 3:5],"D310","CCD.UP")

## Non-standard Designs

#Slide 224-Create the Design with optFederov function in AlgDesign
library(daewr)
data(qsar)
library(AlgDesign)
desgn1<-optFederov(~quad(.),data=qsar,nTrials=15,center=TRUE, criterion="D",nRepeats=40)
desgn2<-optFederov(~quad(.),data=qsar,nTrials=15,center=TRUE, criterion="I",nRepeats=40)
desgn2$design

#Slide 225-Compare the D-Optimal and I-Optimal Designs for the Quadratic Model
library(Vdgraph)
Compare2FDS(desgn1$design, desgn2$design, "D-optimal", "I-optimal", mod=2)

#Slide 228-Create the Design in R
k1 <- .15; k2 <- .72; gamma0 <- 2.65; t0 <- 0.41
x <- c(seq(1:25))
dfdk1 <- c(rep(0, 25))
dfdk2 <- c(rep(0, 25))
dfdgamma0 <- c(rep(0, 25))
dfdt0 <- c(rep(0, 25))
for (i in 1:25) {
  dfdk1[i] <- -1 * gamma0 * exp(-k1 * (x[i] - t0)) *(x[i] - t0)
  dfdk2[i] <-gamma0 * exp(-k2 * (x[i] - t0)) * (x[i] - t0)
  dfdgamma0[i] <- exp(-k1 * (x[i] - t0)) - exp( -k2 * ( x[i] - t0))
  dfdt0[i] <- gamma0 * exp(-k1 * (x[i] - t0)) * k1 - gamma0 *
  exp(-k2 * (x[i] - t0)) * k2; }
grid <- data.frame(x, dfdk1, dfdk2, dfdgamma0, dfdt0)
library(AlgDesign)
desgn2<-optFederov(~-1+dfdk1+dfdk2+dfdgamma0+dfdt0,data=grid,nTrials=4,center=TRUE,
                     criterion="D",nRepeats=20)
desgn2$design

## Fitting the Response Surface Model

#Slide 230-Central Composite Design{Cement Grout
library(daewr)
data(cement)
cement

#Slide 231-Fit Linear Model-Block 1
library(rsm)
grout.lin <- rsm(y ~ SO(x1, x2, x3),data = cement, subset = (Block == 1))

#Slide 232-Fit Quadratic Model-All Data
library(daewr)
data(cement)
grout.quad <- rsm(y ~ Block + SO(x1,x2,x3), data = cement)
summary(grout.quad)

## Determining the Optimum Conditions 

#Slide 234-Contour Plots of Fitted Surface
library(rsm)
par(mfrow=c(2,2))
contour(grout.quad, ~ x1+x2+x3)

#Slide 235-Perspective Plots of Fitted Surface
par(mfrow=c(1,3))
persp(grout.quad, ~ x1+x2+x3, zlab="Work", contours=list(z="bottom"))

#Slide 239-Calculations with rsm package
ridge<-steepest(grout.quad, dist=seq(0, 1.7, by=.1),descent=FALSE)
ridge

#Slide 241-Plotting the Ridge Trace with R
par (mfrow=c(2,1))
eg.txt<-c("W/C","Rad","SNF")
plot(ridge$dist,ridge$yhat, type="l",xlab="radius",ylab="Max. Predicted")
plot(ridge$dist,seq(.10,.355,by=.015), type="n", xlab="radius", ylab="Factors")
lines(ridge$dist,ridge$WatCem,lty=1)
lines(ridge$dist,ridge$BlackL,lty=2)
lines(ridge$dist,ridge$SNF,lty=3)
legend(1.1,.31,leg.txt,lty=c(1,2,3))

## Split-Plot Response Surface Designs

#Slide 243-Fitting the Model with lme4 package
library(lme4)
library(daewr)
data(cake)
cake
mmod <- lmer(y ~ x1 +x2 +x1:x2 +x1sq + x2sq +(1|Ovenrun), data=cake)
summary(mmod)

#Slide 247-Creating a Design with daewr package
library(daewr)
EEw2s3()
EEw2s3('EE21R7WP')

## Screening to Optimization

#Slide 249-Creating a Defnitive Screening Design with daewr
library(daewr)
DefScreen(8)

#Slide 252-Analysis using ihstep, fstep in daewr package
des<-DefScreen(8)
pd<-c(5.35,4.4,12.91,3.79,4.15,14.05,11.4,4.29,3.56,11.4,10.09,5.9,9.54,4.53,3.919,
      8.1,5.35)
trm<-ihstep(pd,des)

#Slide 253-Analysis using ihstep, fstep in daewr package
trm<- fhstep(pd, des, trm)

#Slide 254-trm<- fhstep(pd, des, trm)
trm <-fhstep(pd, des, trm)

#Slide 255-Final Results
modpd<-lm(pd ~ F + A + I(A^2) + C, data=des)
summary(modpd)
library(rsm)
contour(modpd, F ~ A, at=list(C=0), nlevels = 6, main="Contour Plot of Pore Diameter with Drying Time Fixed at Mid Level",
        xlabs=c("Speed of H2O Addition (coded levels)", "Calcination Temperature (coded levels)"))