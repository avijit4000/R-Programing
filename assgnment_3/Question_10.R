library(MASS)
data(Boston)
Boston
?Boston
pairs(Boston)
pairs(Boston[, 1:6])  # First six predictors
pairs(Boston[, 7:12]) # Next six predictors
pairs(Boston[, 13:14]) # Last two predictors


pairs(Boston[, c("crim", "zn", "indus", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat", "medv")])

cor(Boston)
apply(Boston, 2, range)

high_crime <- Boston[Boston$crim > quantile(Boston$crim, 0.95), ]
high_tax <- Boston[Boston$tax > quantile(Boston$tax, 0.95), ]
high_ptratio <- Boston[Boston$ptratio > quantile(Boston$ptratio, 0.95), ]
summary(high_crime)
summary(high_tax)
summary(high_ptratio)
charles_river <- sum(Boston$chas == 1)

charles_river

