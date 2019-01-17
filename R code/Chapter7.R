# Example 1 p. 263
library(daewr)
BIBsize(6, 3)

# Example 2 p. 263
library(AlgDesign)
BIB <- optBlock( ~ ., withinData = factor(1:6), blocksizes = rep(3, 10))

# Example 3 p. 264
des <- BIB$rows
dim(des) <- NULL
des <- matrix(des, nrow = 10, ncol = 3, byrow = TRUE,
              dimnames = list(c( "Block1", "Block2", "Block3", "Block4",
                                 "Block5", "Block6", "Block7", "Block8", "Block9",
                                 "Block10"), c("unit1", "unit2", "unit3")))
des

# Example 4 p. 266
library(daewr)
mod1 <- aov( score ~ panelist + recipe, data = taste)
summary(mod1)

# Example 5 p. 266
library(lsmeans)
lsmeans(mod1, pairwise ~ recipe, adjust = ("tukey"))

# Example 6 p. 268
library(daewr)
modm <- aov( pressure ~ Block + Treatment, data = BPmonitor)
library(lsmeans)
lsmeans(modm,pairwise~Treatment,adjust=("tukey"))

# Example 7 p. 270
library(agricolae)
treat <- c(1, 2, 3, 4, 5, 6)
des <- design.cyclic(treat, k = 3, r = 3)

# Example 8 p. 271
des$book

# Example 9 p. 272
library(agricolae)
treat <- c(1, 2, 3, 4, 5, 6, 7, 8)
RCD <- design.cyclic(treat, k = 4, r = 4, rowcol = TRUE,
                     seed = 1)

# Example 10 p. 273-274
library(FrF2)
Bdish <- FrF2( 16, 4, blocks = c("ABD", "BCD"),
               alias.block.2fis = TRUE, randomize = FALSE)
Bdish

# Example 11 p. 275
y <- c(0, 0, 12, 14, 1, 0, 1, 11, 10, 2, 33, 24, 3, 5, 41, 70)
Bdish <- add.response(Bdish, response = y)
dish <- lm( y ~ Blocks + A * B * C * D, data = Bdish)

# Example 12 p. 276-Figure 7.1
effects <- coef(dish)
effects <- effects[5:19]
effects <- effects[ !is.na(effects) ]
library(daewr)
halfnorm(effects, names(effects), alpha=.25)

# Figure 7.2
x <- as.numeric(Bdish$B)
x[x==1] <- "1 tbs"
x[x=="2"] <- "2 tbs"
Brand <- as.numeric(Bdish$D)
Brand[Brand==1] <- "WF"
Brand[Brand=="2"] <- "UP"
interaction.plot(x, Brand, Bdish$y, type="l" ,xlab="Soap Amount B",ylab="Average Clean Squares")

# Example 13 p. 281
library(FrF2)
Bff <- FrF2(16, 8, generators = c("BCD", "ACD", "ABC", "ABD"),
            blocks = c("AB", "AC", "AD"),randomize = FALSE)
weight <- c(0.0, 9.0, 5.35, 9.90, 4.35, 8.8, 6.8, 3.93,
            9.25, 4.9, 7.43, 2.6, 0.0, 7.
            43, 4.87, 10.2)
add.response(Bff, response = weight)

# Example 14 p. 281
mouse <- lm(weight ~ Blocks + A + B + C + D + E + F + G + H,
            data = Bff)

# Example 15 p. 281-Figure 7.3 p. 282
effects <- coef(mouse)
effects <- effects[ 9:16 ]
library(daewr)
halfnorm(effects, names(effects), alpha=.15)

# Example 16 p. 284
Blocked.design <- FrF2(32, 6, blocks = 4,
                       alias.block.2fis = TRUE, randomize = FALSE)
summary(Blocked.design)

# Example 17 p. 284
Blocked.design <- FrF2(32, 6, blocks = 4,
                       randomize = FALSE)
summary(Blocked.design)

# Example 18 p. 285
library(AlgDesign)
Blockdes <- gen.factorial(3, nVars = 2,
                          center = FALSE, varNames = c( "A", "B" ))
Block <- 1+mod((Blockdes$A -1 )+(Blockdes$B - 1), 3)
Blockdes <- cbind(Block, Blockdes)

# Example 19 p. 286
library(AlgDesign)
Blockdes <- gen.factorial(3, nVars = 4, factors = "all",
                          varNames = c("A", "B", "C", "D"))
Blockdes <- optBlock( ~ A + B + C + D + A:B + A:C +
                        A:D+B:C + B:D + C:D, withinData = Blockdes,
                      blocksizes = c(rep(9, 9)), criterion = "D")

# Example 20 p. 288
library(DoE.base)
Mixfac <- fac.design(nlevels = c(2, 2, 3, 3),factor.names =
                       c("A", "B", "C", "D"), randomize = FALSE)

# Example 21 p. 288
library(daewr)
blk1 <- mod(as.numeric(Mixfac$A) + as.numeric(Mixfac$B), 2) + 1
blk2 <- mod(as.numeric(Mixfac$C) + as.numeric(Mixfac$D), 3) + 1
Block <- as.factor((blk1 - 1) * 3 + blk2 )

# Example 22 p. 289
BlMixfac <- cbind(Block,Mixfac)
BlMixfac <- BlMixfac[order(BlMixfac$Block), ]
BlMixfac

# Example 23 p. 290
library(DoE.base)
Mixfac <- fac.design(nlevels = c(2, 2, 2, 3, 3),
                     factor.names = c("b1", "b2", "c1", "A", "c2"),
                     randomize = FALSE)

# Example 24 p. 290
library(daewr)
blk1 <- mod(as.numeric(Mixfac$b1) + as.numeric(Mixfac$b2) +
              as.numeric(Mixfac$c1) ,2) + 1
blk2 <- mod(as.numeric(Mixfac$A) + as.numeric(Mixfac$c2),
            3) +1

# Example 25 p. 290
Block <- as.factor((blk1 - 1) * 3 + blk2 )
B <- (as.numeric(Mixfac$b1) - 1) * 2 + as.numeric(Mixfac$b2)
C <- (as.numeric(Mixfac$c1) - 1) * 3 + as.numeric(Mixfac$c2)
BlMixfac<-cbind(Block, A = Mixfac$A, B = as.factor(B), as.factor(C))
BlMixfac <- BlMixfac[order(Block), ]

# Example 26 p. 291
BlMixfac

# Example 27 p. 292
library(AlgDesign)
des <- gen.factorial(levels = c(3, 4, 6), factors = 'all',
                     varNames = c("A", "B", "C"))
bdes <- optBlock( ~ A + B + C + A:B + A:C + B:C, withinData =
                    des, blocksizes = c(rep(12, 6)), criterion = "D")

# Example 28 p. 293
bdes$Blocks

# Example 29 p. 293
Block <- c(rep(1:6, each = 12))
bdesign <- cbind(Block, bdes$design)

# Example 30 p. 294
library("DoE.base")
show.oas(factors = list(nlevels = c(4, 6, 2, 3),
                        number = c(1, 1, 3, 3)))

# Example 31 p. 294
library("DoE.base")
fnames =c ("A", "B", "C", "D", "E", "F", "G", "Block")
BlockOA <- oa.design(nlevels = c(4, 6, 2, 2, 2, 3, 3, 3),
                     factor.names = fnames, seed=104, nruns = 72)
BlockOA <- BlockOA[order(BlockOA$Block), ]

# Example 32 p. 294
library(DoE.base)
library(AlgDesign)
fnames <- c("A", "B", "C", "D", "E", "F", "G")
cand <- oa.design(nlevels = c(4, 6, 2, 2, 2, 3, 3),
                  factor.names = fnames, randomize = TRUE, seed = 104,
                  nruns = 72)
bdes <- optBlock( ~ A + B + C + D + E + F + G,
                  withinData = cand, blocksizes = c(rep(8, 3)),criterion = "D")

# Example 33 p. 295
bdes$Blocks

# Example 34 p. 296
library(AlgDesign)
Blockdes <- gen.factorial(2, nVars = 2, factors = "all",
                          varNames = c("A","B"))
Blockdes <- optBlock( ~ A + B + A:B, withinData = Blockdes,
                      blocksizes = c(rep(2,6)), criterion = "D")

# Example 35 p. 297
desf <- gen.factorial(c(2, 2, 3), factors = "all",
                      varNames = c("A", "B", "C"))
Blockdes <- optBlock( ~ A*B*C, withinData = desf,
                      blocksizes = c(rep(4, 12)),criterion = "D")

# Example 36 p. 298
des23 <- gen.factorial(c(4, 3), factors = "all",
                       varNames = c("A", "B"))
Blockdes <- optBlock( ~ A*B, withinData = des23,
                      blocksizes = c(rep(6, 4)),criterion = "D")

# Example 37 p. 298
library(daewr)
library(car)
modf <- lm(rating ~ Block + A + B + A:B, data = apple,
           contrasts = list(A = contr.sum, B = contr.sum,
                            Block = contr.sum))

# Example 38 p. 299
Anova(modf,type="III")

# Example 39 p. 299
library(lsmeans)
lsmeans(modf, pairwise ~ A, adjust = ("tukey"))



