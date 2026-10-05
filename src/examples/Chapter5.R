# Example 1 p. 146
library(daewr)
mod1 <- aov( conc ~ lab, data = Apo )
sm1 <- summary(mod1)
sm1

# Example 2 p. 147
X <- as.matrix( model.matrix(mod1) )
labB <- X[,2]
labC <- X[,3]
labD <- X[,4]
labA <- X[,1]-(labB+labC+labD)

# Example 3 p. 147
s1 <- summary(aov (labA ~ Apo$lab ))
x1 <- as.matrix( s1[[1]][1,3] )
s2 <- summary(aov( labB ~ Apo$lab ))
x2 <- as.matrix( s2[[1]][1,3] )
s3 <- summary(aov(labC ~ Apo$lab ))
x3 <- as.matrix( s3[[1]][1,3] )
s4 <- summary(aov(labD ~ Apo$lab ))
x4 <- as.matrix( s4[[1]][1,3] )
c <- x1+x2+x3+x4

# Example 4 p. 147
sigma2 <- as.matrix( sm1[[1]][2,3] )
mslab <- as.matrix( sm1[[1]][1,3] )
cat(" Mean Square for Lab = ",mslab,"\n"," Mean Square for Error = ", sigma2,"\n",
    " Expected Mean Square for Lab","\n", "Var(error)+",c,"Var(Lab)","\n")

# Example 5 p. 148
sigma2t <- (mslab - sigma2) / c
cat("Method of Moments Variance Component Estimates","\n", "Var(error)=",sigma2,
    "\n","Var(Lab)=",sigma2t,"\n")

# Example 6 p. 152
library(daewr)
library(lme4)
rmod2 <- lmer( weight ~ 1 + (1|batch), data = soupmx)
summary(rmod2)

# Example 7 p. 153
library(daewr)
library(lme4)
pr1 <- profile( fm1M <- lmer( yield ~ 1 + (1| sample), data = Naph, REML = FALSE))
confint (pr1) # 95% confidence interval on sigma

# Example 8 p. 154
nu2 <- 36:44
chiu <- qchisq(.975, nu2)
chil <- qchisq(.025, nu2)
width <- nu2 * (chiu - chil) / (chil * chiu)
halfw <- width/2
data.frame(nu2, width, halfw)

# Example 9 p. 155
alpha <- .05
rho <- 3.0
t <- rep(5:7, each = 3)
r <- rep(2:4, 3)
nu_1 <- t-1
nu_2 <- t * (r - 1)
fcrit <- qf( 1 - alpha, nu_1, nu_2 )
factor <- 1 / ( 1 + r * rho )
plimit <- factor * fcrit
power <- 1 - pf( plimit, nu_1, nu_2 )
data.frame( t, r, power)

# Example 10 p. 158
library(daewr)
modr1 <- aov( y ~ part + oper + part:oper, data = gagerr)
summary(modr1)

# Example 11 p. 158
sigma2 <- .000752
sigma2po <- (0.026885 - sigma2) / 2
sigma2o <- (0.014852 - sigma2 - 2 * sigma2po ) / 20
sigma2p <- (.160991 - sigma2 - 2 * sigma2po ) / 6
cat("Method of Moments Variance Component Estimates","\n","Var(error)=",sigma2,"\n","Var(part x oper)=",sigma2po,"\n",
    "Var(oper)=",sigma2o,"\n","Var(part)=",sigma2p,"\n")

# Example 12 p. 159
library(lme4)
modr2 <- lmer(y ~ 1 + (1|part) + (1|oper) + (1|part:oper), data = gagerr)
summary(modr2)

# Example 13 p. 161
library(daewr)
options(digits = 3)
vci(confl = .90, c1 = .05, ms1 = .01485, nu1 = 2, c2 = .05, ms2 = .02689, nu2 = 18)

# Example 14 p. 163
library(daewr)
library(lme4)
rmod3 <- lmer( calcium ~ 1 + (1|lab) + (1|sol) + (1|lab:sol), data = blood)
summary(rmod3)

# Example 15 p. 164
cellmeans <- tapply( blood$calcium, list(blood$lab, blood$sol), mean)
dim(cellmeans) <- NULL
Lab <- factor( rep(c("A", "B", "C"), 4))
Solution <- factor(rep( c( 1, 2, 3, 4), each = 3))
mod2 <- aov( cellmeans ~ Lab + Solution + Lab:Solution )
summary(mod2)

# Example 16 p. 164
library(daewr)
vci(confl = .90, c1 = .1354166, ms1 = 413, nu1 = 2, c2 = .1354166, ms2 = 104, nu2 = 6)

# Example 17 p. 166
mod2 <- aov( elasticity ~ supplier + supplier:batch + supplier:batch:sample, data = rubber)
summary(mod2)

# Example 18 p. 166
library(lme4)
modr3 <- lmer( elasticity ~ 1+ (1|supplier)  + (1|supplier:batch) + (1|supplier:batch:sample), data = rubber)
summary(modr3)

# Example 19 p. 170
library(daewr)
mod2<-aov(strength ~ lot + lot:box + lot:box:prep, data = polymer)
summary(mod2)

# Example 20 p. 172
library(lme4)
modr3 <- lmer( strength ~ 1 + (1|lot) + (1|lot:box) + (1|lot:box:prep), data = polymer)
summary(modr3)

# Example 21 p. 176
library(daewr)
mod4 <- aov( residue ~ form + tech + form:tech +  plot:form:tech, data = pesticide)
summary(mod4)

# Example 22 p. 177
c1 <- c(-.5, .5)
mod4 <- aov( residue ~ form + tech + form:tech + plot:form:tech, 
             contrasts = list( form = c1, tech = c1, plot = c1 ), data = pesticide)
c <- ('application effect' = c(0,0,1,0,0,0,0,0))
library(gmodels)
estimable(mod4,c)

# Example 23 p. 178
library(lme4)
c1 <- c( -.5, .5 )
mod5 <- lmer( residue ~ 1 + form + tech + form:tech + (1|plot:form:tech), 
            contrasts = list( form = c1, tech = c1), data = pesticide)
summary(mod5)

# Example 24 p. 178
library(lsmeans)
lsmeans(mod5, pairwise ~ tech, adjust = c("tukey"))

# Example 25 p. 179
anova(mod5)

# Figure 5.4 p. 181
library(daewr)
data(gagerr)
interaction.plot(gagerr$part, gagerr$oper, gagerr$y, type="l", 
  main = "Interaction Plot for part by Operator",  xlab="Part Number", ylab="Average measurement")

# Example 26 p. 182 produces Figure 5.5
library(daewr)
data(Naph)
s2 <- tapply( Naph$yield, Naph$sample, var )
os2 <- sort(s2)
r <- c( 1:length(s2) )
gscore <- qgamma( (r - .5 ) / length (s2), 2)
plot(gscore, os2, main = "Gamma plot of within sample
     variances", xlab = "Gamma score", ylab = "Sample Variance")

# Example 27 p. 183
library(daewr)
data(polymer)
y <- array( polymer$strength, c(4,30) )
sd1 <- sqrt( (y[2,] - y[1,])**2 / 2)
sd2 <- sqrt( (2/3) * ( y[3,] - (y[1,] + y[2,]) / 2)**2 )
sd3 <- sqrt( (3/4) * (y[4,] - (y[1,] + y[2,] + y[3,] )/3 )**2)

# Example 28 p. 184 produces Figure 5.6
osd2 <- sort(sd2)
r <- c( 1: length(sd2))
zscore <- qnorm( ( ( r - .5 ) / length(sd2) +1 )/ 2)
plot( zscore, osd2, main = "Half-normal plot of prep(box) standard deviations", 
      xlab = "Half Normal Score", ylab ="std. due to prep within box")

# Example 29 p. 187 produces Figure 5.7
library(lme4)
modr3 <- lmer( strength ~ 1 + (1|lot) + (1|lot:box) + (1|lot:box:prep), 
               data = polymer)
qqnorm( ranef(modr3)$"lot:box:prep"[[1]], main = "prep within box and lot", ylab="EBLUP",
        xlab = "Normal Score" )

