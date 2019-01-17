# Example  1 p. 456
library(mixexp)
SLD(3,2)

# Example 2 p. 457
library(mixexp)
SCD(4)

# Example 3 p. 457 and Figure 11.10
library(mixexp)
des<- SLD(3,3)
DesignPoints(des)

# Example 4 p. 458 and Figure 11.11 p. 459
library(daewr)
data(pest)
DesignPoints(pest)

# Example 5 p.459
library(daewr)
spc <- lm(y ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 + x1:x2:x3
          -1, data = pest)
summary(spc)

# Example 6 p. 460
library(daewr)
qm <- lm(y ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 -1,
         data = pest)
summary(qm)

# Example 7 p. 461
fm <- lm(y ~ x1 + x2 + x1:x2 + x1:x3 + x2:x3, data = pest)
summary(fm)

# Example 8 p. 461 
library(mixexp)
MixModel(pest, "y", c("x1","x2","x3"), 2)

# Example 9 p. 462
library(daewr)
data(pest)
MixturePlot(des = pest,mod = 2)

# Example 10 p. 463 and Figure 11.14 p. 464
EffPlot(des=pest,mod=2,dir=1)

# Example 11 p. 468
library(mixexp)
Xvert(4, uc=c(.188,.128,.438,.438),lc=c(.124,.064,.374,.374))

# Example 12 p. 468
library(mixexp)
Xvert(4,uc=c(.188,.128,.438,.438),lc=c(.124,.064,.374,.374), ndm=1)

# Example 13 p. 468
library(mixexp}
exvert<-Xvert(4,uc=c(.188,.128,.438,.438),lc=c(.124,.064,.374,.374), ndm=2)
library(AlgDesign)
desMix <- optFederov(~ -1 + x1 + x2+ x3 + x4 + x1:x2 + x1:x3 +
                    x1:x4 + x2:x3 + x2:x4 + x3:x4 ,data = exvert, nTrials = 12)

# Example 14 p. 470 and Figure 11.17 p. 471
library(mixexp)
des<-Xvert(3,uc=c(.8,.95,.50),lc=c(0,.10,.05),ndm=1, plot=FALSE)
library(AlgDesign)
desPolv <- optFederov(~ -1 + x1 + x2+ x3 + x1:x2 + x1:x3 + x2:x3
                      + x1*x2*x3, des ,nTrials=9)
DesignPoints(desPolv$design, x1lower = 0, x1upper = .8, x2lower =
               .1, x2upper = .95, x3lower = .05, x3upper = .50, pseudo=TRUE)

# Example 15 p. 471
library(daewr)
data(polvdat)
sqm <- lm(y ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 +
            x1:x2:x3 -1, data = polvdat)
summary(sqm)

# Example 16 p. 472 and Figure 11.18
MixturePlot(des = polvdat, mod = 3, lims=c(0,.8,.1,.95, .05,.50), constrts=TRUE, pseudo=TRUE)

# Example  17 p. 476
library(mixexp)
indes <- SLD(3, 2)
library(AlgDesign)
bdesign <- optBlock(~ -1 + x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3,
                    indes, rep(4,2), criterion = "Dpc", nRepeats = 1000)

# Example 18 p. 477
conmx <- matrix(c(1, 0, 0, 0,
                  -1, 0, 0, .1,
                  0, 1, 0, 0,
                  0,-1, 0, .1,
                  -1,-1, 0, .15),nrow=5,byrow=TRUE)
library(mixexp)
ev <- crvtave(0, conmx)
evd <- data.frame(ev)
cand <- Fillv(3, evd)
library(AlgDesign)
bdesign <- optBlock( ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 -1,
                     cand, rep(5, 2), criterion ="D", nRepeats = 1000)


# Example 19 p. 479
library(mixexp)
ev<-Xvert(3, uc=c(.45,.67,.34),lc=c(.13,.21,.20), ndm=1)
mp <- subset(ev, dimen <= 1)
mp <- rbind(mp, mp)
z <- c( rep(-1, 8), rep(1, 8))
mp <- cbind( z, mp )
mp

# Example 20 p. 481
library(mixexp)
sld <- SLD(3, 2)
id <- c( rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6))
sldl <- rbind(sld, sld, sld, sld)
sldl <- cbind(sldl, id)
facdes <- expand.grid(z1 = c(-1, 1),z2 = c(-1, 1))
id <- c(1, 2, 3, 4)
facdes <- cbind(facdes, id)
comdes <- merge(sldl, facdes, by = "id", all = TRUE)

# Example 21 p 483
library(mixexp)
sld<-SLD(4,2)
id <- rep(1, 10)
for (i in 2:32) {id <- c(id, rep(i, 10))}
sldm<-sld
for (i in 2:32) { sldm <- rbind(sldm, sld) }
sldm <- cbind(sldm, id)
facdes <- expand.grid(z1 = c(-1, 1), z2 = c(-1, 1),
                      z3=c(-1, 1), z4 = c(-1, 1), z5 = c(-1, 1))
id <- c(1:32)
facdes <- cbind(facdes, id)
cand <- merge(sldm, facdes, by = "id", all = TRUE)
library(AlgDesign)
MixPro <- optFederov( ~ x1 + x2 + x3 + x4 + x1:x2 + x1:x3 +
                        x1:x4 + x2:x3 + x2:x4 + x3:x4 + z1:z2 + z1:z3 + z1:z4 +
                        z1:z5 + z2:z3 + z2:x4 + z2:z5 + z3:z4 + z3:z5 + z4:z5 +
                        x1:z1 + x1:z2 + x1:z3 + x1:z4 + x1:z5 + x2:z1 + x2:z2 +
                        x2:z3 + x2:z4 + x2:z5 + x3:z1 + x3:z2 + x3:z3 + x3:z4 +
                        x3:z5 + x4:z1 + x4:z2 + x4:z3 + x4:z4 + x4:z5 -1,
                      cand, nTrials = 44, criterion = "D", maxIteration = 100,
                      nRepeats=10)

# Example 22 p. 485
library(daewr)
data(MPV)
modmp <- lm(y ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 +
              x1:z1 + x2:z1 + x3:z1 + x1:x2:z1 + x1:x3:z1 +x2:x3:z1 +
              x1:z2+x2:z2 + x3:z2 + x1:x2:z2 + x1:x3:z2 + x2:x3:z2 +
              x1:z1:z2 + x2:z1:z2 + x3:z1:z2 + x1:x2:z1:z2 + x1:x3:z1:z2 +
              x2:x3:z1:z2 -1, data=MPV)
summary(modmp)

# Example 23 p. 486
cosvis<-function(x) {
  x1<-x[1]
  x2<-x[2]
  x3<-1-(x1+x2)
  f<-(54.59*x1 + 5.69*x2 +7.49*x3)+
    + abs(229186.4*x1+11487.06*x2+817.8356*x3-346975*x1*x2-19419*x1*x3-12628.5*x2*x3-3657)
}
ui <- t(matrix(c(1, 0, 0, 1, -1, 0, 0, -1, 1, 1, -1, -1), 2, 6))
ci <- c(0, 0, -.0549, -.9725, .0275, -1)
constrOptim( c(.014415, .613363), cosvis, NULL, ui, ci )

# Example 24 pp. 488-489
library(daewr)
data(SPMPV)
library(lme4)
modsp <- lmer( y ~ -1 + z1:z2 +x1 +x2+ x3 + x1:x2 + x1:x3 +
                 x2:x3 + z1:x1 + z1:x2 + z1:x3 + z2:x1 + z2:x2 + z2:x3 +
                 ( 1 | wp ), data = SPMPV )
summary(modsp)

# Example 25 p. 501 Figure 11.27 p. 502
library(daewr)
data(pest)
orth <- transform(pest, w1 = sqrt(6)*(2*x1-x2-x3), w2 = 
                    + -sqrt(18)*(x2 - x3))
library(rsm)
orth.mod <- rsm(y ~ SO(w1, w2), data = orth)
contour(orth.mod, w1 ~ w2)
polygon(c( -4.3, 0, -4.30),c(-2.54777591, 4.89898, 4.89898),
        col = "white", border = "white")
abline(4.89898, ((4.89898 + 2.44944) / 4.24264))
polygon(c(4.3, 0, 4.3),c(-2.54777591, 4.89898, 4.89898), 
        col = "white", border = "white")
abline(4.89898, -((4.89898 + 2.44944) / 4.24264))

