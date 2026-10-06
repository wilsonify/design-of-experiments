height <- c(132,151,162,139,166,147,122)
weight <- c(48,49,66,53,67,52,40)
gender <- c("male","male","female","female","male","female","male")
data <- data.frame( cbind(height, weight, gender))
data
data$height
data$gender

class(data$gender)
levels(data$gender)

gender
ls()
rm(list=ls())

x <- rnorm(100, 0,1)
x           
min(x)
max(x)

## histogram
hist(x)
## check normal
qqnorm(x)
qqline(x)

## Plot normal curve

z <- seq(-4.0,+4.0, by=.1)
z
y <- (1/sqrt(2*pi))*exp((-1/2)*z^2)
plot(z,y,col="red", pch=16)

curve((1/sqrt(2*pi))*exp((-1/2)*x^2), from=-4, to=4)


hz <- dnorm(z, 0,1)
plot(z, hz, pch=15, cex=.5)

##Sum of 2 standard normal is normal N(mu1+mu2, sigma2 +sigma2)

Z1 <- rnorm(100, 0,1)
Z2 <- rnorm(100, 0,1)
hist(Z1)
hist(Z2)
Z <- Z1 + Z2

hist(Z)
X <- Z1^2 + Z2^2

hist(X)

## density curve Chi square

curve(dchisq(x, 5), from=.1, to=50)

## sensity F distribution

curve(df(x, 5, 10), from=.1, to=50)
