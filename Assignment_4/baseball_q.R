getwd()
data <- read.csv("baseballu.txt", sep = "", strip.white = T)
data
#"baseball.csv", sep = "", strip.white = TRUE
str(data)
subsetdata <- subset(data, at_bats >= 100)
dim(subsetdata)
summary(data)
?read.csv(baseball)

## Question_39
plot(subsetdata$homeruns, subsetdata$bat_ave,
     xlab = "Home Runs",
     ylab = "Batting Average",
     main = "Home Runs vs Batting Average", col = "blue")


## Question_40
#Ans. A correlation between batting average and home runs is seen in informal data. Though there isn't much of a correlation, players with higher batting averages typically hit more home runs.
  
## Question_41 
#Ans: Players with higher batting averages have a wide range in how many home runs they hit. The quantity of home runs varies greatly even among players with higher averages, suggesting that a higher batting average does not always translate into more home runs.

## Question_42 
#Ans-It's possible that there are issues with the homoscedasticity assumption (constant variance of the residuals). The scatter plot indicates that the variance of home run counts is not constant because it displays a large spread of home run counts across various batting averages. The validity of standard regression assumptions may be impacted by this.

## Question_43
# Perform linear regression
model <- lm(homeruns ~ bat_ave, data = subsetdata)

# Standardized residuals
standardized_residuals <- rstandard(model)

# Normal probability plot
qqnorm(standardized_residuals)
qqline(standardized_residuals)

# Residuals vs. Fitted values
plot(fitted(model), residuals(model),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0

## Question_44
# Adding a small value to avoid log(0)
subsetdata$ln_homeruns <- log(subsetdata$homeruns + 1)

# Perform linear regression on ln(homeruns)
model_lns <- lm(ln_homeruns ~ bat_ave, data = subsetdata )

# Standardized residuals
standardized_residuals_ln <- rstandard(model_lns)

# Normal probability plot
qqnorm(standardized_residuals_ln)
qqline(standardized_residuals_ln)

## Question_45
# Residuals vs Fitted plot
plot(model_ln$fitted.values, standardized_residuals_ln)
abline(h = 0, col = "green")

#The dots in the scatter plot appear randomly scattered around horizontal axis (fitted values).
#There is no clear pattern or systematic structure in the residuals.
#This lack of pattern suggest that the constant variance assumption (homoscedasticity) is not violated.
#Absence of funnel shapes or trends supports the assumption that residuals have constant variance.

## Question_46

model_ln

## Question_47

summary(model_lns)
# Regression Equation:
# The natural logarithm of the home runs  is equal to -1.226 plus 13.161 times the batting average.
# The Regression Equation:
# ln(homeruns) = -1.226 + 13.161(bat_ave)
# from the equation indicates that with every 1 unit increase in batting average, the log of home runs is willbe increased by 13.1608
# in this  we are taking  b0,b1

## Question_48
# The y-intercept is the expected value of ln(home runs) when the batting average is zero.

## Question_49
# The slope indicates the expected change in ln(home runs) for a one-unit increase in batting average.

## Question_50
# Estimate ln(home runs) for batting average of 0.300
ln_homeruns_estimate <- predict(model_lns, newdata = data.frame(bat_ave = 0.300))

# Convert back to home runs
homeruns_estimate <- exp(ln_homeruns_estimate) - 1
homeruns_estimate

## Question_51
summary(model_ln)$sigma

## Question_52
summary(model_ln)$r.squared*100

## Question_53
summary(model_ln)$coefficients


## Question_54
confint(model_ln, level = 0.95)

## Question_55
exp(predict(model_ln, newdata = data.frame(bat_ave = 0.300), interval = "confidence"))-1

## Question 56
exp(predict(model_ln, newdata = data.frame(bat_ave = 0.300), interval = "prediction"))-1

# Question 57: Standardized residuals

std_res <- rstandard(model_ln)
which(std_res < -3| std_res > 3)
# one outlier at the index 135

# Question 58: List high leverage points

lev <- hatvalues(model_ln)
lev2 <- unname(lev)
which(lev2 >0.028)

# Question 59: List influential observations according to Cook's distance
cooks_dis <- cooks.distance(model_ln)
which(cooks_dis>1)

# There have no influenced point in given baseball data








