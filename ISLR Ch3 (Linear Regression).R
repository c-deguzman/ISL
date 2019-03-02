LoadLibraries=function()
{
  library(MASS)
  library(ISLR)
  library(car)
  print("Libraries have been loaded")
}

LoadLibraries()

fix(Boston)
names(Boston)

lm.fit=lm(medv~lstat, data=Boston)

#summary(lm.fit)
# reveals that the l-stat T-statistic p-value is <2e-16

confint(lm.fit)

predict(lm.fit)


#Confidence intervals try to guess an average y value for an x value
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="confidence")

#Prediction intervals try to guess a point based off the regression
# these must be wider than confidence intervals because they must additionally accommodate for the error term variance
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="prediction")


# Hints at non-linearity through its curvature
plot(Boston$lstat, Boston$medv)
abline(lm.fit, lwd=3, col="red")

par(mfrow=c(2,2))
plot(lm.fit)


# Further evidence of non-linearity
plot(predict(lm.fit), residuals(lm.fit))

#studentized residuals are fit through removing outlier residuals (accordint to t-statistic)
plot(predict(lm.fit), rstudent(lm.fit))


# Consider points of high leverage
# Note: hatvalues sounds like it would refer to fitted values, but it doesn't
#   hatvalue refers to the projection (hat) matrix
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


# -------------------------------------------------
#       Multiple Linear Regression

#Fit a multiple linear regression against lstat and age
lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)

#Fit a multiple linear regression against ALL predictors
lm.fit=lm(medv~., data=Boston)
summary(lm.fit)

#Consider goodness of fit:
# R^2
summary(lm.fit)$r.sq

#Residual Standard Error
summary(lm.fit)$sigma

#Consider variance inflation factor - detect collinearity
#    Note: vif uses the R^2 regression of a predictor against the remaining predictors
#       a model with collinear predictors will have a higher possible variance
#   Furhter note: ideally this should be less than 10 for a given factor
vif(lm.fit)

# ------------------------------------------------
#       Interaction Terms

# Interaction terms are useful for discovering synergies


# Shorthand note - (used in accordance with hierarchial principle)
# a*b == a+b+a:b 
lm.fit=lm(medv~lstat*age, data=Boston)
summary(lm.fit)


# --------------------------------------------------
# Non-linear Transformations of the Predictors


lm.quadractic_fit=lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.quadratic_fit)


lm.linear_fit=lm(medv~lstat, data=Boston)

# Compare the fits - preform hypothesis test
# Null: models fit equally well
# Alt: quadratic_fit is better
anova(lm.linear_fit, lm.quadratic_fit)


# Check the performance based on the four fundamental graphs
par(mfrow=c(2,2))
plot(lm.quadratic_fit)


# preform polynomial regression
lm.fifth_degree_fit=lm(medv~poly(lstat,5), data=Boston)
summary(lm.fifth_degree_fit)


# preform regression on logarithmic transformation
lm.log_fit=lm(medv~log(lstat), data=Boston)
summary(lm.log_fit)



# --------------------------------------------------
# Qualitative Predictors

fix(Carseats)
names(Carseats)


# About dataset: "Shelveloc" is indicator of quality of shelving location
#    think slotting fees in supermarkets


lm.fit=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)

attach(Carseats)

#Discern what dummy values R has created to accommodate for qualitative data
#Note: contrasts can be changed, though there is no statistical significance to this
#   it is done purely from an interpretability standpoint
contrasts(ShelveLoc)
