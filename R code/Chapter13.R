# Example 1 p. 564
library(daewr)
des <- DefScreen( m = 8)
des

# Example 2 p. 565
pd <- c(5.35, 4.4, 12.91, 3.79, 4.15, 14.05,
        11.4, 4.29, 3.56, 11.4, 10.09, 5.9, 9.54,
        4.53,3.919,8.1,5.35)
trm <- ihstep( pd, des )

# Example 2 p. 565
trm <- fhstep( pd, des, trm)

# Example 3 p. 565
trm <- fhstep( pd, des, trm)

# Figure 13.5 p. 566
modpd<-lm(pd ~ F + A + I(A^2) + C, data=des)
summary(modpd)
contour(modpd, F ~ A, at=list(C=0), nlevels = 6, main="Contour Plot of Pore Diameter with Drying Time Fixed at Mid Level",
        xlabs=c("Speed of H2O Addition (coded levels)", "Calcination Temperature (coded levels)"))