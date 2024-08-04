# Load necessary packages
library(dplyr)
library(ggplot2)

# Sample data
baseball_2002 <- read.csv("baseball.csv")
baseball_2002


# Calculate batting average
baseball_2002 <- baseball_2002 %>%mutate(batting_average = hits / at_bats)

# Scatter plot
ggplot(baseball_2002, aes(x = batting_average, y = home_runs)) + geom_point() +
  labs(title = "Scatter Plot of Home Runs vs Batting Average",
       x = "Batting Average",
       y = "Number of Home Runs") +
  theme_minimal()


# Perform simple linear regression
model <- lm(home_runs ~ batting_average, data = baseball_2002)

# Summary of the regression model
summary(model)


# Normal probability plot of standardized residuals
qqnorm(rstandard(model))
qqline(rstandard(model), col = "blue")


# Residuals vs. Fitted values plot
plot(model$fitted.values, rstandard(model),
     xlab = "Fitted Values",
     ylab = "Standardized Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")


# Log-transform home runs
baseball_2002 <- baseball_2002 %>%
  mutate(log_home_runs = log(home_runs + 1))

# Perform regression with log-transformed home runs
log_model <- lm(log_home_runs ~ batting_average, data = baseball_2002)

# Summary of the regression model
summary(log_model)

# Normal probability plot of standardized residuals
qqnorm(rstandard(log_model))
qqline(rstandard(log_model), col = "blue")

# Residuals vs. Fitted values plot
plot(log_model$fitted.values, rstandard(log_model),
     xlab = "Fitted Values",
     ylab = "Standardized Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")


# Population regression equation
coefficients <- summary(log_model)$coefficients
intercept <- coefficients[1, 1]
slope <- coefficients[2, 1]

regression_eq <- paste("log(Home Runs + 1) = ", round(intercept, 3), " + ", round(slope, 3), " * Batting Average")
regression_eq


# Scatter plot of home runs versus batting average
ggplot(baseball_2002, aes(x = batting_average, y = home_runs)) +
  geom_point() +
  labs(title = "Scatter Plot of Home Runs vs Batting Average",
       x = "Batting Average",
       y = "Number of Home Runs") +
  theme_minimal()

install.packages("ggplot2")
library(ggplot2)

# Scatter plot of home runs versus batting average
ggplot(baseball_2002, aes(x = batting_average, y = home_runs)) +
  geom_point() +
  labs(title = "Scatter Plot of Home Runs vs Batting Average",
       x = "Batting Average",
       y = "Number of Home Runs") +
  theme_minimal()
































