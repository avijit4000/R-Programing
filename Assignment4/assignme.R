library(dplyr)
library(ggplot2)

#Suppose we are interested in whether there is a relationship between batting average (X) 
#and number of home runs (Y) a player hits. Some fans might argue, for example, that 
#those who hit lots of home runs also tend to make a lot of strikes outs so that their batting 
#average is lower. Let us check it out, using a regression of the number of home runs 
#against the player's batting average (hits divided by at bats). Because baseball batting 
#averages tend to be highly variable for low number of at bats, we restrict our data set to 
#those players who has at least 100 at bats for the 2002 season. This leaves us with 209 
#players.

data <- read.csv("baseballu.txt", sep = "", strip.white = T)
data
str(data)
subsetdata <- subset(data, at_bats >= 100)
dim(subsetdata)
summary(data)

#39. Construct a scatter plot of home runs versus batting average.
plot(subsetdata$homeruns, subsetdata$bat_ave,
     xlab = "Home Runs",
     ylab = "Batting Average",
     main = "Home Runs vs Batting Average", col = "blue")

#40. Informally, is there evidence of a relationship between the variables? 

basebal <- baseball_2002[baseball_2002$bats<=100,]
basebal

#41. What would you say about the variability of the number of home runs, for those with higher batting averages? 


#42. Refer to the previous exercise. Which regression assumption might this presage difficulty for? 


#43. Perform a regression of home runs on battling average. Obtain a normal probability plot of the standardization residuals from this regression. Does the normal probability plot indicate acceptable normality. Construct a plot of the residuals versus the fitted values (fitted values refers to Vs). What pattern do you see? What does this indicate regarding the regression assumptions? 
model <- lm(homeruns ~ bat_ave, data = subsetdata)

standardized_residuals <- rstandard(model)

qqnorm(standardized_residuals)
qqline(standardized_residuals)

plot(fitted(model), residuals(model),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0

#44. Take a natural log of home runs, and perform a regression of In home runs on batting overage. Obtain a normal probability plot of the standardized residuals from this regression. Does the normal probability plot indicate acceptable normality? 

subsetdata$ln_homeruns <- log(subsetdata$homeruns + 1)
subsetdata$ln_homeruns

model_ln <- lm(ln_homeruns ~ bat_ave, data = subsetdata )

standardized_residuals_ln <- rstandard(model_ln)
qqnorm(standardized_residuals_ln)
qqline(standardized_residuals_ln)

#45. Construct a plot of the residuals versus the fitted values. Do you see strong evidence that the constant variance assumption has been violated? (Remember to avoid the Rorschach effect.) therefore conclude that the assumptions are validated, 
plot(model_ln$fitted.values, standardized_residuals_ln)
abline(h = 0, col = "green")

#46. Write the population regression equation for our model. Intercept the meeting of /jJ and 
model_ln

#47. State the regression equation (from the regression results) in words and numbers. 

summary(model_ln)

# The Regression Equation:
# The natural logarithm of the home runs  is equal to -1.226 plus 13.161 times the batting average.
# The Regression Equation:
# ln(homeruns) = -1.226 + 13.161(bat_ave)
# from the equation indicates that with every 1 unit increase in batting average, the log of home runs is willbe increased by 13.1608
#48. Interpret the value of the y-intercept bo. 

#49.Interpret the value of the slope bl. 
 
#50.Estimate the number of home runs (not In home runs) for a player with a batting average of 0.300. 
ln_homeruns_estimate <- predict(model_ln, newdata = data.frame(bat_ave = 0.300))
ln_homeruns_estimate


homeruns_estimate <- exp(ln_homeruns_estimate) - 1
homeruns_estimate

#51. What is the size of the typical error in predicting the number of home runs, based on the player's batting average? 
summary(model_ln)$sigma

#52. What percentage of the variability in the In home runs does batting average account for? 
summary(model_ln)$r.squared*100

#53. Perform the hypothesis test for determining whether a linear relationship exist between the variables. 
summary(model_ln)$coefficients

# 54. Construct and interpret a 95% confidence interval for the unknown true slope of the regression line. 
confint(model_ln, level = 0.95)

#55. Construct and interpret a 95% confidence interval for the mean number of home runs for all players who had a batting average of 0.300. 

exp(predict(model_ln, newdata = data.frame(bat_ave = 0.300), interval = "confidence"))-1


#56. Construct and interpret a 95% prediction interval for a randomly chosen player with a 0.300 batting average. Is this prediction interval useful? 

exp(predict(model_ln, newdata = data.frame(bat_ave = 0.300), interval = "prediction"))-1

#57. List all the outliers. Mention the value of all the variables for the outliers. 
std_ress <- rstandard(model_ln)
which(std_ress < -3| std_ress > 3)

#58. List the high leverage points. 
lev1 <- hatvalues(model_ln)
lev2 <- unname(lev1)
which(lev2 >0.028)

#59. List the influential observations, according to Cook's distance,

cook_dis <- cooks.distance(model_ln)
which(cook_dis>1)

# There have no influenced point in given baseball data
