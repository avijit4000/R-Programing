# Load necessary packages
library(ggplot2)

# Example data frame
baseball_2002 <- data.frame(
  player = c("Player A", "Player B", "Player C", "Player D", "Player E"),
  batting_average = c(0.301, 0.287, 0.315, 0.275, 0.298),
  home_runs = c(32, 25, 39, 20, 30)
)

# Perform simple linear regression
model <- lm(home_runs ~ batting_average, data = baseball_2002)

# Summary of the regression model
summary(model)
