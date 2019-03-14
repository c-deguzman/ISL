# ISL - Ch3 Q11

# Problem Environment initialization
set.seed(1)
x = rnorm(100)
y = 2*x+rnorm(100)

# a)

# linear regression without intercept
lm.fit = lm(y~x+0)
summary(lm.fit)

# significant evidence by t-statistic that the coefficient is ~1.9939
#   very close to the actual value of two

# confidence interval by standard error does not include 0, we reject the null hypothesis



# b)

lm.fit = lm(x ~ y + 0)
summary(lm.fit)
# significant evidence by t-statistic that the coefficient is ~0.3911
#   close to actual value of 0.5


# c)

# estimated coefficients are roughly reciprocals
# t-statistic is the exact same

# f)
lm.fit = lm(y~x)
summary(lm.fit)

lm.fit = lm(x~y)
summary(lm.fit)

# Once again, numerically t-values are the same

