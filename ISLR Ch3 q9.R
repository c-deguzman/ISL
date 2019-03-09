# ISL - Ch3 Q9

LoadLibraries=function()
{
  library(MASS)
  library(ISLR)
  library(car)
  print("Libraries have been loaded")
}

LoadLibraries()

attach(Auto)

# a)
plot(Auto)

# b)
# Remove "name" column because it is qualitative
cor(Auto[, -which(names(Auto) == "name")])

# c)
lm.fit=lm(mpg~.-name, data=Auto)
summary(lm.fit)

# From p-values listed under the summary - appears a few
#   relationships between predictors, namely:
#     - displacement
#     - weight
#     - year
#     - origin
# In general it appears that newer cars tend to improve mpg
# Further, the weight of the car significantly negatively effects mpg

# d)
par(mfrow=c(2,2))
plot(lm.fit)

# From diagnostics produced:
#   - appears to be systematic deviation in residuals for higher values
#     also appears to be a distinct curve to the residuals
#   - Normal qq demonstrates another systematic deviation from the line for higher quartiles
#     with points much more sperad out than expected
#   - Pretty good scale location suggests homoscedasicity (equal variance)
#   - Data point 14 appears to have significant leverage, though this point does not have a significant cook's distance score


# e)

names(Auto)

lm.fit=lm(mpg~.+weight:horsepower-name, data=Auto)
summary(lm.fit)

lm.fit=lm(mpg~.+horsepower:cylinders-name, data=Auto)
summary(lm.fit)

lm.fit=lm(mpg~.+year:cylinders-name, data=Auto)
summary(lm.fit)

# Experimenting with interaction terms:
# (stat.) significant synergy between weight and horsepower in it's effect on mpg (stronger, heavier cars  -> slightly more mpg?)
# (stat.) significant synergy between horsepower and cylinders (bigger cylinders -> more mpg?)
# (stat.) significant synergy between year and cylinders (less efficient cylinders?)


# f)

names(Auto)

lm.fit=lm(mpg~.-name+log(weight), data=Auto)
summary(lm.fit)

lm.fit=lm(mpg~.-name+sqrt(horsepower), data=Auto)
summary(lm.fit)

lm.fit=lm(mpg~.-name+I(horsepower^2), data=Auto)
summary(lm.fit)

#Experimenting with transformations of the terms
# (stat. + real.) significance to log of weight on mpg
# (stat. + real.) significance of sqrt of horsepower on mpg
# (stat. + real.) significance of horsepower^2 on mpg

# Evident there is a non-linear relationship between horsepower and weight and mpg