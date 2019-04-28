install.packages("faraway")
library(faraway)

data(savings)
savings

mod.lm <- lm(sr ~ ., data=savings)

summary(mod.lm)

dim(savings)

### coefficients

ones <- rep(1, times=50)
X <- cbind(ones, pop15, pop75,     dpi,  ddpi)
XtX <- t(X) %*% X

XtX.inv  <- solve(XtX)

beta <- XtX.inv  %*% t(X) %*% sr
beta


mod.lm$coefficients

####   standard errors of coefficients


var.beta <- 3.803*XtX.inv
diag(var.beta)


#####   LAB 8

angle <- rep(c(15, 20, 25), each=3)
speed <- rep(c(125, 150, 175), times=3)
rep1 <- cbind(angle, speed)
rep2 <- cbind(angle, speed)

rep <- rbind(rep1, rep2)

y <- c(-2, -3, 2, 0, 1, 4, -1, 5, 0, -1, 0, 3, 2, 3, 6, 0, 6, -1)

cutting <-data.frame(rep, y)
cutting

attach(cutting)
install.packages("rsm")
library(rsm)

cutting.rsm <- rsm( y ~ SO(angle, speed), data=cutting)
summary(cutting.rsm)
