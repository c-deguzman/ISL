# ISL - Ch3 Q8

LoadLibraries=function()
{
  library(MASS)
  library(ISLR)
  library(car)
  print("Libraries have been loaded")
}

LoadLibraries()

# a)
lm.fit = lm(mpg~horsepower, data=Auto)

summary(lm.fit)


#   i) There is a relationship between predictor and response - suggested by p-value of <2e-16


summary(lm.fit)$r.sq

#   ii) ~60% of variance is explained by horsepower alone, suggesting a strong relationship
#   iii) value is -0.16, therefore negative


predict(lm.fit, data.frame(horsepower = c(98)), interval="confidence", level = 0.95)
predict(lm.fit, data.frame(horsepower = c(98)), interval="prediction", level = 0.95)

#   iv) as calculated above, predicted mpg is 24.47, with
#       95% confidence interval of [23.97, 24.96]
#       95% prediction interval of [14.81, 34.12]


# b)
plot(Auto$horsepower, Auto$mpg)
abline(lm.fit, lwd=3, col="red")

# c)
par(mfrow=c(2,2))
plot(lm.fit)

# Residuals look suspisciously curved
# qqplot looks as if it has some deviation espeically in the tails
# spread-location - clear non-horizontal line
#     hints at heteroscedasticity (unequal variance)
# Leverage - visually, no large values - all within cook's distance lines

# Summarily, suggests underlying relation is not really linear
