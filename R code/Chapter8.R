# Example 1 p. 310
library(AlgDesign)
sp <- expand.grid(trayT = factor( c("RoomT", "Hot")),
                  bakeT = factor( c("low", "mid", "high") ))
wp <- data.frame(short = factor( c("100%", "80%") ))
wp <- rbind(wp, wp)
splitP <- optBlock( ~ short * (trayT + bakeT +
                                 trayT:bakeT), withinData = sp, blocksizes = rep(6, 4),
                    wholeBlockData = wp)

# Example 2 p. 310
splitP$Blocks

# Example 3 p. 311
library(FrF2)
Sp <- FrF2(16, 3, WPs = 4, nfac.WP = 1,factor.names =
             list(short = c("80%", "100%"), bakeT = c("low", "high"),
                  trayT = c("low", "high")))
Sp

# Example 3 p. 313-314
library(daewr)
data(splitPdes)
library(GAD)
Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
TrayT <- as.fixed(splitPdes$trayT)
model <- aov(y ~ Short + Short%in%Batch + BakeT +
               TrayT + Short*BakeT + Short*TrayT + BakeT*TrayT +
               Short*BakeT*TrayT, data = splitPdes)
gad(model)

# Example 4 p. 314
library(lme4)
rmodel <- lmer(y ~ 1 + short + bakeT + trayT + short:bakeT +
                 short:trayT + bakeT:trayT + short:bakeT:trayT +
                 (1|short:batch), data = splitPdes)
anova(rmodel)

# Example 5 p. 315
summary(rmodel)

# Figure 8.2 p. 316
TrayT <- splitPdes$trayT
interaction.plot(splitPdes$short, TrayT , splitPdes$y, type = "l"
                 ,main = "Interaction Plot"
                 ,xlab = "Percent Shortening",ylab="Diameter")

# Example 6 p. 318
library(AlgDesign)
sp <- expand.grid(PoleStiff = factor(c("light",
                                       "medLight")), LureWgt = factor(c("light", "heavy")))
wp <- data.frame(LineWgt = factor(c("6lb", "10lb",
                                    "20lb")))
wp <- rbind(wp, wp)
splitP <- optBlock( ~ LineWgt*(PoleStiff + LureWgt +
                                 PoleStiff:LureWgt), withinData = sp, blocksizes =
                      rep(4, 6), wholeBlockData = wp)
fisherman <- factor( c(rep(1:2, each = 12)))
splitP$design <- cbind(fisherman, splitP$design)

# Example 7 p. 320
library(FrF2)
FrF2(32, 4, WPs = 8, nfac.WP = 2, factor.names = (c("A", "B",
                                                    "C", "D")))

# Example 8 p. 320
library(daewr)
library(lme4)
rmod2 <- lmer( ys ~ A + B + A:B +(1|Block) + (1|A:B:Block)+
                 C + D + C:D + A:C + A:D + B:C + B:D + A:B:C + A:B:D +
                 A:C:D + B:C:D + A:B:C:D, data = sausage)
summary(rmod2)

# Example 9 p. 321
anova(rmod2)

# Figure 8.3 p. 322
par( mfrow = c(1, 2))
Cn <- subset(sausage, C == -1)
interaction.plot(as.numeric(Cn$A), Cn$B, Cn$ys, type = "l", legend = FALSE, ylim = c(1.96,2.08), main = "C = Low Level", xlab = "Factor A", ylab = "Mean Bursting Strength")
lines( c(1.3, 1.4), c(1.965, 1.965), lty = 2)
lines( c(1.3, 1.4), c(1.975, 1.975), lty = 1)
text(1.1, 1.965, "B = low ")
text(1.1, 1.975, "B = high ")
text(1.2, 1.995, "LEGEND")
Cp <- subset(sausage, C == 1)
interaction.plot(as.numeric(Cp$A), Cp$B, Cp$ys, type = "l", legend = FALSE, ylim = c(1.96, 2.08), main = "C = High Level", xlab = "Factor A", ylab = "Mean Bursting Strength")


# Example 10 p. 322
options(digits = 5 )
library(lsmeans)
require(pbkrtest)
require(lme4)
lsmeans(rmod2, ~ A)

# Example 11 p. 322
lsmeans(rmod2, ~ C )

# Example 12 p. 324
library(daewr)
data(plasma)
sol <- lm(y ~ A*B*C*D*E, data = plasma)
effects <- coef(sol)
effects <- effects[c(2:32)]
Wpeffects <- effects[c(1:4, 6:11, 16:19, 26)]
Speffects <- effects[c(5,12:15,20:25,27:31)]

# Example 13 p. 325
summary(sol)

# Example 14 p. 326-Figures 8.4 and 8.5
library(daewr)
fullnormal(Wpeffects, names(Wpeffects), alpha = .10)
fullnormal(Speffects, names(Speffects), alpha = .05)

# Figure 8.6 p. 328
fullnormal(effects, alpha = .20)

# Example 14 p. 330
library(FrF2)
SPFF1 <- FrF2(16, 6, WPs = 4, nfac.WP = 3,
              factor.names = c("A","B","C","P","Q","R"), randomize = FALSE)

# Example 15 p. 331
SPFF2 <- FrF2(16, 6, WPs = 4, nfac.WP = 3,
              factor.names = c("A", "B", "C", "P", "Q", "R"))
y <- rnorm(16, 0, 1)
aliases(lm( y ~ (.)^3, data = SPFF2))
print(SPFF2)

# Example 16 p. 333
library(FrF2)
SPFF1 <- FrF2(16, 8, WPs = 8, nfac.WP = 4,
              factor.names=c("A", "B", "C", "D", "P", "Q", "R", "S"),
              randomize = FALSE)

# Example 17 p. 333
y <- rnorm(16, 0, 1)
aliases(lm( y ~ (.)^3, data = SPFF1))

# Example 18 p. 334
library(FrF2)
FFSP2 <- FrF2(32, 8, WPs = 16, nfac.WP = 4,
              factor.names = c("A", "B", "C", "D", "P", "Q", "R", "S"),
              randomize = FALSE)
y <- rnorm(32, 0, 1)
aliases(lm( y ~ (.)^3, data = FFSP2))

# Example 19 p. 335
library(FrF2)
FFSP3 <- FrF2(32, 8, WPs = 8, nfac.WP = 4,factor.names =
                c("A", "B", "C", "D", "P", "Q", "R", "S"), randomize = FALSE)

# Example 20 p. 336
library(FrF2)
FFSP4 <- FrF2(64, 8, WPs = 8, nfac.WP = 4,factor.names =
                c("A", "B", "C", "D", "P", "Q", "R", "S"), randomize = FALSE)

# Example 21 p. 338
library(FrF2)
spexp <- FrF2(16, 5, WPs = 8, nfac.WP = 3, factor.names =
                c("A","B","C", "P", "Q"),randomize = FALSE)
y <- c(18.0, 21.5, 27.5, 17.0, 22.5, 15.0, 19.0, 22.0,
       13.0, -4.5, 17.5, 14.5, 0.5, 5.5, 24.0, 13.5)
gear <- add.response(spexp, response = y)
gear

# Example 22 p. 339
sol <- lm( y ~ A*B*C*P*Q, data = gear)
summary(sol)

# Example 23 p. 339-Figures 8.7 and 8.8
effects <- coef(sol)
Wpeffects <- effects[ c(2:4, 7:9, 16) ]
Speffects <- effects[ c(5:6, 10:15) ]
fullnormal(Wpeffects, names(Wpeffects), alpha = .10)
fullnormal(Speffects, names(Speffects), alpha = .20)

# Figure 8.9 p. 341
Position <- (gear$C)
interaction.plot((gear$A), Position, gear$y, type = "l", xlab = "Furnace_Track", ylab = "Gear Dishing")







