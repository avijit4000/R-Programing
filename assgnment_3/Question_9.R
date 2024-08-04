library(ISLR)
data(Auto)
head(Auto)

sapply(Auto[, sapply(Auto, is.numeric)], range)


sapply(Auto[, sapply(Auto, is.numeric)], function(x) c(mean = mean(x), sd = sd(x)))

Auto_subset <- Auto[-(10:85), ]

# Range of each quantitative predictor in the subset
subset_ranges <- sapply(Auto_subset[, sapply(Auto_subset, is.numeric)], range)
subset_means_sds <- sapply(Auto_subset[, sapply(Auto_subset, is.numeric)], function(x) c(mean = mean(x), sd = sd(x)))

subset_ranges
subset_means_sds

pairs(Auto[, sapply(Auto, is.numeric)])
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles Per Gallon (mpg)", main = "MPG vs. Horsepower")
plot(Auto$weight, Auto$mpg, xlab = "Weight", ylab = "Miles Per Gallon (mpg)", main = "MPG vs. Weight")
boxplot(Auto$mpg ~ Auto$cylinders, xlab = "Cylinders", ylab = "Miles Per Gallon (mpg)", main = "MPG vs. Cylinders")