library(faraway)
library(ggplot2)
library(lmtest)
library(MASS)
library(Amelia)

?sat
data(sat)
sat
lmod1 <- lm(total ~ expend + ratio + salary + takers, sat)
summary(lmod1)

# Checking Normaility 
qqnorm(residuals(lmod1),ylab = "Residuals", main ="")
qqline(residuals(lmod1))
  #Checking using Shapiro Test
  shapiro.test(residuals(lmod1))
  