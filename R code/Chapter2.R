# Example 1 p. 18
set.seed(7638)
f <- factor( rep ( c(35, 40, 45 ), each = 4))
fac <- sample ( f, 12 )
eu <- 1:12
plan <- data.frame ( loaf = eu, time = fac )
write.csv( plan, file = "Plan.csv", row.names = FALSE )

# Example 2 p. 23
bread <- read.csv("Plan.csv")

# Example 3 p. 24
rm(bread)
library(daewr)
mod0 <-lm( height ~ time, data = bread )
summary (mod0)

# Example 4 p. 25
library(gmodels)
fit.contrast (mod0, "time", c(1, -1, 0) )

# Example 5 p. 27
mod1 <- aov( height ~ time, data = bread )
summary( mod1 )

# Example 6 p. 29 produces Figure 2.4
par( mfrow = c(2,2) )
plot( mod1, which = 5 )
plot( mod1, which = 1 )
plot( mod1, which = 2 )
plot( residuals(mod1) ~ loaf, main = "Residuals vs Exp. Unit", font.main = 1, data = bread)
abline( h=0, lty = 2 )

# Example 7 p. 32
library(MASS)
bc <- boxcox(mod1)
lambda <- bc$x[ which.max( bc$y ) ]
lambda

# Example 8 p. 33
tbread <- transform(bread, theight = height^(-.5050505) )
mod2 <- aov( theight ~ time, data = tbread )
summary(mod2)

# Figure 2.7 p. 34
par( mfrow = c(2,2) )
plot( mod2, which = 5 )
plot( mod2, which = 1 )
plot( mod2, which = 2 )
plot( residuals(mod2) ~ loaf, main = "Residuals vs Exp. Unit", font.main = 1, data = tbread)
abline( h=0, lty = 2 )

# Example 9 p. 35
with( bread, { std <- sqrt( tapply( height, time, var) )
 weights <- rep( 1/std, each = 4 ) 
 mod3 <- lm( height ~ time, weights = weights, data = bread )
 anova( mod3 )
})

# Example 10 p. 37
library(daewr)
library(MASS)
modf <- polr( score ~ method, weight = count, data = teach )
modr <- polr( score ~ 1, weight = count, data = teach )
anova(modf, modr)

# Figure 2.8
meth <- cut(as.numeric(teach$method), c(-Inf, 1, 2, 3), labels = c('Method 1', 'Method 2', 'Method 3'))
class <- cbind(teach[ , 3:4], meth)
library(lattice)
barchart(count ~ score | meth, data = class, layout = c(1,3), horizontal = FALSE, xlab = "Score", col = "grey")

# Example 11 p. 40
library(daewr)
rmin <- 2 # smallest number of replicates
rmax <- 6 # largest number of replicates
alpha <- rep(0.05, rmax - rmin +1)
sigma <- sqrt(2.1)
nlev <- 3
nreps <- rmin:rmax
Delta <- 3
power <- Fpower1(alpha, nlev, nreps, Delta, sigma)  
power

# Example 12 p. 42 
library(daewr)
mod4 <- aov( yield ~ treat, data = sugarbeet )
con <- matrix( c(1, -1/3, -1/3, -1/3, 0, 1, -1, 0, 0, 0, 1, -1 ), 4, 3 )
L <- t(con)
rownames(L) <- c("-fertilizer effect", "-plowed vs broadcast", "-January vs April")
L


# Example 13 p. 42 
options( digits = 3)
library(gmodels)
fit.contrast(mod4, "treat", L)



# Example 14 p. 43
contrasts(bread$time) <- contr.poly(3)
contrasts(bread$time)

# Example 15 p. 43
mod3 <- aov( height ~ time, bread )
summary.lm(mod3)

# Example 16 p. 44
mod4 <- aov( yield ~ treat, data = sugarbeet )
mod4.tukey <- TukeyHSD( mod4, ordered = T )
mod4.tukey

# Example 17 p. 45
library(agricolae)
compare <- SNK.test( mod4, "treat",  alpha = 0.05 )
print(compare)

# Example 18 p. 46
summary(sugarbeet)

# Example 19 p. 47
library(multcomp)
sugar.dun <- glht(mod4, linfct = mcp( treat = "Dunnett"),
alternative = "greater")
summary(sugar.dun)



