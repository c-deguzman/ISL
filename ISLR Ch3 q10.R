# ISL - Ch3 Q10

LoadLibraries=function()
{
  library(MASS)
  library(ISLR)
  library(car)
  print("Libraries have been loaded")
}

LoadLibraries()

attach(Carseats)

# a)
lm.fit = lm(Sales~Price + Urban + US, data=Carseats)
summary(lm.fit)

# b)
# More sales the lower the price
# urban carsets sell a bit less (no stat. significance)
# More sales if the carset is from the US

# c)
contrasts(Carseats$US)
contrasts(Carseats$Urban)
# Sales = 13.04 - 0.054 * Price - 0.02 * Urban_i + 1.2 * US_i
# where Urban_i = 1 if Urban == Yes and 0 otherwise
# where US_i= 1 if US == Yes and 0 otherwise

# d)
# Evidence to reject the null hypothesis for:
# Price, US
# (based on t-statistic p-values)

# e)
lm.fit=lm(Sales~Price + US, data=Carseats)
summary(lm.fit)


#f)
lm.fit_full=lm(Sales~Price + Urban + US, data=Carseats)
summary(lm.fit_full)

lm.fit_part=lm(Sales~Price + US, data=Carseats)
summary(lm.fit_part)

# lm.fit_part and lm.fit_full have equal R^2
# lm.fit_part has a slightly lower RSE than lm.fit_full
# Overall the models fit fairly poorly by ~0.2 R^2

# g)
confint(lm.fit_part, "Price", level = 0.95)
confint(lm.fit_part, "USYes", level = 0.95)


# h)
par(mfrow=c(2,2))
plot(lm.fit_part)

# From residuals vs leverage, there appears to be a point outside the 0.5 demarkation of
# high leverage (by Cook's distance lines)
# From Residuals vs Fitted, does not appear to be significant outliers
