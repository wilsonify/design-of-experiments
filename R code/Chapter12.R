# Example 1 p. 509
library(DoE.base)
des.control <- oa.design(nfactors = 4, nlevels = 3,
                         factor.names = c("A","B","C","D"))
des.noise <- oa.design(nfactors = 3,nlevels = 2, nruns = 8,
                       factor.names = c("E","F","G"))
des.crossed <- cross.design( des.control, des.noise)

# Example 2 p. 512
oa.design(nfactors = 8,nlevels=c(2, 3, 3, 3, 3, 3, 3, 3),
          factor.names = c("X1", "X2", "X3", "X4", "X5", "X6", "X7"
                           ,"X8"), randomize = FALSE)

# Example 3 p. 516
library(daewr)
modyb <- lm(ybar ~ A + B + C + D + E + F + G, data = tile)
summary(modyb)

# Example 4 p. 516 and Figure 12.6
cfs <- coef(modyb)[-1]
halfnorm(cfs,names(cfs), alpha=.2)

# Example 5 p. 517 and Figure 12.7
library{daewr}
modlv <- lm(lns2 ~ A + B + C + D + E + F + G, data = tile)
summary(modlv)
cfs <- coef(modlv)[-1]
halfnorm(cfs,names(cfs),alpha=0.5)

# Example 6 p. 519
library(daewr)
data(cont)
mod <- transform(cont, XA = (A - 4)/2, XB = (B - 2), XC =
                   (C - 8)/8, XD = (D - 10)/2, XF = (F - 6)/1.2)
xvar <- transform(mod, XA2 = XA*XA, XB2 = XB*XB, XC2 = XC*XC,
                  XD2 = XD*XD, XF2 = XF*XF, XAB = XA*XB, XAC = XA*XC, XAD = XA*XD,
                  XAF = XA*XF, XBC = XB*XC, XBD = XB*XD, XBF = XB*XF, XCD = XC*XD,
                  XCF = XC*XF, XDF = XD*XF)
library(leaps)
modc <- regsubsets(lns2 ~ XA + XB + XC + XD + XF + XA2 + XB2 + XC2 +
                     XD2 + XF2 + XAB + XAC + XAD + XAF + XBC + XBD + XBF + XCD + XCF + XDF,
                   data = xvar, nvmax = 8, nbest = 4)
rs <- summary(modc)
plot(c(rep(1:8,each=4)), rs$adjr2, xlab="No. of Parameters",
     ylab="Adjusted R-square")

# Example 7 p. 521
pmod <- lm(lns2 ~ XB + XC + XD + XF + XAF + XBD, data = xvar)
summary(pmod)

# Example 8 p. 523
library(daewr)
smod<-lm(y ~ A + B + C + D + E + F + G + H + AH + BH + CH + DH +
           + EH + FH + GH, data = strungtile)
summary(smod)

# Example 9 p. 524 and Figures 12.8 - 12.9 p. 524-525
effects <- coef(smod)
effects <- effects[c(2:16)]
Wpeffects <- effects[c(1:7)]
Speffects <- effects[c(8:15)]
halfnorm(Wpeffects, names(Wpeffects), alpha=.10)
halfnorm(Speffects, names(Speffects), alpha=.25)

# Figure 12.10 p. 526
Kiln_Position<-c(rep("",16))
for (i in 1:16) {
  if (strungtile$H[i]==-1) {Kiln_Position[i]<-"Inner"}
  else 
  {Kiln_Position[i]<-"Outer"}
}
Pct_Lime<--2*as.numeric(strungtile$C)+3
aspd<-strungtile$y
interaction.plot(Kiln_Position, Pct_Lime, aspd, type="l"
                 ,main=""
                 ,xlab="H: Kiln Position",ylab="Arcsin(sqrt(proportion defective))")


# Example 10 - An expansion of that shown on p. 527 
library(daewr)
data(prodstd)
# make contrast vectors for 2 level factors
XE<-(prodstd$E-1.5)/.5
XF<-(prodstd$F-1.5)/.5
XG<-(prodstd$G-1.5)/.5
# standardize the contrasts
E<-XE/sqrt(72)
F<-XF/sqrt(72)
G<-XG/sqrt(72)
# make contrast vectors for three level factors
cont3<-contr.poly(3)
lin<-cont3[,1]
quad<-cont3[,2]
Al<-prodstd$A
Aq<-prodstd$A 
for (i in 1:72) {
  if (Al[i]==1) {Al[i]<-lin[1]; Aq[i]<-quad[1]}
  else if (Al[i]==2) {Al[i]<-lin[2]; Aq[i]<-quad[2]}
  else {Al[i]<-lin[3]; Aq[i]<-quad[3] }
} 


Bl<-prodstd$B
Bq<-prodstd$B 
for (i in 1:72) {
  if (Bl[i]==1) {Bl[i]<-lin[1]; Bq[i]<-quad[1]}
  else if (Bl[i]==2) {Bl[i]<-lin[2]; Bq[i]<-quad[2]}
  else {Bl[i]<-lin[3]; Bq[i]<-quad[3]}
}
Cl<-prodstd$C
Cq<-prodstd$C
for (i in 1:72) {
  if (Cl[i]==1) {Cl[i]<-lin[1]; Cq[i]<-quad[1]}
  else if (Cl[i]==2) {Cl[i]<-lin[2]; Cq[i]<-quad[2]}
  else {Cl[i]<-lin[3]; Cq[i]<-quad[3] }
}
Dl<-prodstd$D
Dq<-prodstd$D
for (i in 1:72) {
  if (Dl[i]==1) {Dl[i]<-lin[1]; Dq[i]<-quad[1]}
  else if (Dl[i]==2) {Dl[i]<-lin[2]; Dq[i]<-quad[2]}
  else {Dl[i]<-lin[3]; Dq[i]<-quad[3] } 
}

# standardise the contrasts
Al<-Al/sqrt(24); Aq<-Aq/sqrt(24); Bl<-Bl/sqrt(24); Bq<-Bq/sqrt(24); 
Cl<-Cl/sqrt(24); Cq<-Cq/sqrt(24); Dl<-Dl/sqrt(24); Dq<-Dq/sqrt(24); 
E<-XE/sqrt(72); F<-XF/sqrt(72); G<-XG/sqrt(72)
# create interaction contrasts
EF<-XE*XF/sqrt(72)
EG<-XE*XG/sqrt(72)
FG<-XF*XG/sqrt(72)
EFG<-XE*XF*XG/sqrt(72)
AlE<-Al*XE; BlE<-Bl*XE; ClE<-Cl*XE; DlE<-Dl*XE
AqE<-Aq*XE; BqE<-Bq*XE; CqE<-Cq*XE; DqE<-Dq*XE
AlF<-Al*XF; BlF<-Bl*XF; ClF<-Cl*XF; DlF<-Dl*XF
AqF<-Aq*XF; BqF<-Bq*XF; CqF<-Cq*XF; DqF<-Dq*XF
AlG<-Al*XG; BlG<-Bl*XG; ClG<-Cl*XG; DlG<-Dl*XG
AqG<-Aq*XG; BqG<-Bq*XG; CqG<-Cq*XG; DqG<-Dq*XG;
AlEF<-Al*XE*XF; BlEF<-Bl*XE*XF; ClEF<-Cl*XE*XF; DlEF<-Dl*XE*XF
AqEF<-Aq*XE*XF; BqEF<-Bq*XE*XF; CqEF<-Cq*XE*XF; DqEF<-Dq*XE*XF
AlEG<-Al*XE*XG; BlEG<-Bl*XE*XG; ClEG<-Cl*XE*XG; DlEG<-Dl*XE*XG
AqEG<-Aq*XE*XG; BqEG<-Bq*XE*XG; CqEG<-Cq*XE*XG; DqEG<-Dq*XE*XG
AlFG<-Al*XF*XG; BlFG<-Bl*XF*XG; ClFG<-Cl*XF*XG; DlFG<-Dl*XF*XG
AqFG<-Aq*XF*XG; BqFG<-Bq*XF*XG; CqFG<-Cq*XF*XG; DqFG<-Dq*XF*XG
AlEFG<-Al*XE*XF*XG; BlEFG<-Bl*XE*XF*XG; ClEFG<-Cl*XE*XF*XG; DlEFG<-Dl*XE*XF*XG
AqEFG<-Aq*XE*XF*XG; BqEFG<-Bq*XE*XF*XG; CqEFG<-Cq*XE*XF*XG; DqEFG<-Dq*XE*XF*XG
# Create data frame with new variables
Connector<-data.frame(Pof=prodstd$Pof,Al,Aq,Bl,Bq,Cl,Cq,Dl,Dq,E,F,G,EF,EG,FG,EFG,AlE,BlE,ClE,DlE,AqE,BqE,CqE,DqE,AlF,BlF,ClF,DlF,
                      AqF,BqF,CqF,DqF,AlG,BlG,ClG,DlG,AqG,BqG,CqG,DqG,AlEF,BlEF,ClEF,DlEF,AqEF,BqEF,CqEF,DqEF,AlEG,BlEG,
                      ClEG,DlEG,AqEG,BqEG,CqEG,DqEG,AlFG,BlFG,ClFG,DlFG,AqFG,BqFG,CqFG,DqFG,AlEFG,BlEFG,ClEFG,DlEFG,AqEFG,
                      BqEFG,CqEFG,DqEFG)

# Example 11 p. 529
prodfac <- data.frame(A = as.factor(prodstd$A), B = as.factor
                      (prodstd$B), C = as.factor(prodstd$C), D = as.factor(prodstd$D),
                      E = as.factor(prodstd$E), F = as.factor(prodstd$F), G =
                        as.factor(prodstd$G), Pof = prodstd$Pof)

# Figure 12.11 p. 529
library(daewr)
fmod<-lm(Pof ~ (.), data=Connector)
effects <- fmod$effects[-1]
fullnormal(effects)
library(car)


# Example 12 p. 529
rmod <- lm(Pof ~ A + B + C + D + E + F + G + A:G + D:E + D:F,
           data = prodfac, contrasts=list(A = contr.sum, B = contr.sum,
                                          C = contr.sum, D = contr.sum, E = contr.sum, F = contr.sum,
                                          G = contr.sum))
Anova(rmod, type="III")

# Example 13 p. 530
library(lsmeans)
lsmeans(rmod, ~ C)

# Figure 12.12 p. 530
# first make labels for factor levels
CondRH<-c(1:72)
Interference<-c(1:72)
for (i in 1:72) {
  if (prodfac$G[i]==1) {CondRH[i]<-"25%RH"}
  else 
  {CondRH[i]<-"75%RH"}
  if (prodfac$A[i]==1) {Interference[i]<-"Low"}
  else if (prodfac$A[i]==2) {Interference[i]<-"Mid"}
  else {Interference[i]<-"High"}
}
# Make Plot
interaction.plot(CondRH, Interference, prodfac$Pof, type="l"
                 ,main="",xlab="G: Conditioning Relative Humidity",ylab="Pull Off Force")


# Figure 12.13 p. 531
# makes interaction plot
# first make labels for factor levels
CondTm<-c(1:72)
AdhesivePct<-c(1:72)
for (i in 1:72) {
  if (prodfac$E[i]==1) {CondTm[i]<-"24hrs"}
  else 
  {CondTm[i]<-"120hrs"}
  if (prodfac$D[i]==1) {AdhesivePct[i]<-"Low"}
  else if (prodfac$D[i]==2) {AdhesivePct[i]<-"Mid"}
  else {AdhesivePct[i]<-"High"}
}
interaction.plot(CondTm, AdhesivePct, prodfac$Pof, type="l"
                 ,main="",xlab="E: Conditioning Time",ylab="Pull Off Force")

# Example 14 p. 534
library(daewr)
library(car)
rmod <- lm(torque ~ A + B + C + D + E + A:B + A:C + A:D + A:E,
           data = Smotor, contrasts=list(A = contr.sum, B = contr.sum,
                                         C = contr.sum, D = contr.sum, E = contr.sum))
Anova(rmod, type="III")

# Example 15 p. 534
library(lsmeans)
lsmeans(rmod, ~ C)

# Example 16 p. 535
library(lsmeans)
lsmeans(rmod, ~ A:B)

# Figure 12.14  p. 535
# makes interaction plot
# first make labels for factor levels
InsideDiameter<-c(1:18)
for (i in 1:18) {
  if (Smotor$B[i]==1) {InsideDiameter[i]<-"small"}
  else if (Smotor$B[i]==2) {InsideDiameter[i]<-"medium"}
  else {InsideDiameter[i]<-"large"}
}
interaction.plot(Smotor$A, InsideDiameter, Smotor$torque, type="l"
                 ,main="",xlab="A: Noise Factor",ylab="torque")

# Figure 12.15 p. 537
library(daewr)
data(inject)
ffdes<-inject[1:16,]
injmod<-lm(shrinkage~ (.)^2, data=ffdes)
cfs <- coef(injmod)[c(2:14,16)]
fullnormal(cfs,names(cfs),alpha=.05)

# Figure 12.16 p. 538
# makes interaction plot
# first make labels for factor levels
MoldTemp<-c(1:16)
ScrewSpeed<-c(1:16)
for (i in 1:16) {
  if (ffdes$A[i]==1) {MoldTemp[i]<-"high"}
  else {MoldTemp[i]<-"low"}
  if (ffdes$B[i]==1) {ScrewSpeed[i]<-"high"}
  else {ScrewSpeed[i]<-"low"}
}
interaction.plot(MoldTemp, ScrewSpeed, ffdes$shrinkage, type="l"
                 ,main="",xlab="A: Mold Temperature",ylab="% Shrinkage")

# Figure 12.17 and 12.18 p. 538 and 539
#Make Boxplot at low screw speed
# first subset ffdes selecting data at low screw speed
ffdesls<-subset(ffdes, B == -1)
boxplot(ffdesls$shrinkage)
# make comparative box plots on page 543
ffdesls$C[ffdesls$C == -1] <- "Low"
ffdesls$C[ffdesls$C == 1] <- "High"
boxplot(shrinkage ~ C, data=ffdesls )

# Figure 12.19 p. 542
library(daewr)
data(eptaxyb)
modyb<-lm(ybar~(.)^2,data=eptaxyb)
cfs <- coef(modyb)[c(2:15)]
fullnormal(cfs,names(cfs),alpha=.15)

# Figure 12.20 p. 543
library(daewr)
data(eptaxs2)
mods2<-lm(log(s2)~(.)^2,data=eptaxs2)
eptaxs2t <- transform(eptaxs2, lns2=log(s2))
eptaxs2t$s2<-NULL
mods2<-lm(lns2~(.)^2,data=eptaxs2t)
cfs <- coef(mods2)[c(2:15)]
fullnormal(cfs,names(cfs),alpha=.05)

# Example 17 p. 543-544
library(daewr)
data(eptaxr)
library(nlme)
# construct a groupedData object
eptaxg<-groupedData(y ~ 1|A, data=eptaxr)
#joint model of mean and variance
cnt<-lmeControl(opt="optim")
modj <- lme(y ~ D + H, data = eptaxg, weights=varIdent(form = ~ 1 | A ), control=cnt)
summary(modj)

