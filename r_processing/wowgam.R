# install.packages("ISLR")

library(ISLR)
attach(Wage)

library(gam)

gam.m3 = gam(wage ~ s(year, 4) + s(age,5) + education, data=Wage)

par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
