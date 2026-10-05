logcount <-c(7.66, 6.98, 7.80, 5.26, 5.44, 5.80, 7.41, 7.33, 7.04,             3.51, 2.91, 3.66)
package <- rep(c("Plastic","Vacuum", "1%CO", "100%CO2"),each=3)



bacteria

package <- as.character(package)
package
bacteria <- data.frame(cbind(package, logcount))


package <- factor(package)

####  aov

bacteria.aov <- aov(logcount ~ package)


summary.lm(bacteria.aov)

summary.aov(bacteria.aov)


####################################



strength <- c(3129, 3000, 2865, 2890, 3200, 3300, 2975, 3150,2800, 2900, 2985, 3050,
              2600, 2700, 2600, 2765)

mixing <- rep(c(1,2,3,4), each=4)
mixing

cement <- data.frame(cbind(mixing, strength))
cement

mixing <- as.factor(mixing)

attach(cement)


MSerror <- 12826
install.packages("agricolae")
library(agricolae)

LSD.test(cement.aov, "mixing", MSerror)
result <-LSD.test(cement.aov, "mixing", MSerror, console=T)

plot(result)

#########################################################

tv <- read.csv(file.choose(), header=TRUE)

tv

attach(tv)

Coating.Type <- as.factor(Coating.Type)

## a

tv.aov <- aov(Conductivity ~ Coating.Type)
summary.aov(tv.aov)

## b
mean(Conductivity)   # estimate population mean

means <- tapply(Conductivity, Coating.Type, mean)

grand.means <- rep(137.9375, time=4)
grand.means

effects <- means -grand.means
effects

##  c


coating.4 <- Conductivity[c(13:16)]

summary.aov(tv.aov)

LSD.test(tv.aov, "Coating.Type", 19.69)
result <-LSD.test(tv.aov, "Coating.Type", 19.69, console=T)


stripchart(Conductivity~ Coating.Type, vertical=TRUE, pch=16)
