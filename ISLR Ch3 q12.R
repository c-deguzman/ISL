# ISL - Ch3 Q12

# a)

# Examine the given terms in (3.38) - clearly the values will be the same if the sum of squares of x and y are equal

set.seed(1)
x = rnorm(100)


# b)
y = 95 * x + rnorm(100)

lm.fit = lm(y ~ x + 0)
summary(lm.fit)

lm.fit = lm(x ~ y + 0)
summary(lm.fit)


# c)

# take re-ordering of points and apply transformation that will not alter sum of squares
y = abs(sample(x, replace = FALSE, 100))

lm.fit = lm(y ~ x + 0)
summary(lm.fit)

lm.fit = lm(x ~ y + 0)
summary(lm.fit)

