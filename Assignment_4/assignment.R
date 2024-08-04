library(dplyr)
library(ggplot2)



set.seed(123)
baseball_2002 <- read.csv("baseball.csv")
baseball_2002




baseball_2002 <- baseball_2002 %>% mutate(batting_average = hits / bats)
baseball_2002


filtered_data <- baseball_2002 %>% filter(bats >= 100)
filtered_data 


ggplot(filtered_data, aes(x = batting_average, y = home_runs)) + geom_point() + 
  labs(title = "Scatter Plot of Home Runs vs Batting Average", x = "Batting Average", y = "Number of Home Runs") +
  theme_minimal()



model <- lm(home_runs ~ batting_average, data = filtered_data)


summary(model)


ggplot(filtered_data, aes(x = batting_average, y = home_runs)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Home Runs vs Batting Average with Regression Line",
       x = "Batting Average",
       y = "Number of Home Runs") +
  theme_minimal()


ggplot(filtered_data, aes(x = batting_average, y = home_runs)) +
  geom_point() +
  labs(title = "Scatter Plot of Home Runs vs Batting Average",
       x = "Batting Average",
       y = "Number of Home Runs") +
  theme_minimal()

model <- lm(home_runs ~ batting_average, data = filtered_data)


summary(model)




# Read the data with correct delimiter
data <- read.csv("baseball.csv", sep = "\t", strip.white = TRUE)

# Display the structure of the data to inspect the column names
str(data)

# Display the column names to identify the correct names
print(colnames(data))

# Manually correct the column names if necessary
corrected_colnames <- c(
  "firstname", "lastname", "age", "team", "games", 
  "at_bats", "runs", "hits", "doubles", "triples", 
  "home_runs", "RBIs", "walks", "strikeouts", 
  "bat_avg", "on_base_pct", "slugging_pct", 
  "stolen_bases", "caught_stealing"
)

# Assign the corrected column names to the data
colnames(data) <- corrected_colnames

# Display the corrected column names to verify
print(colnames(data))

# Subset the data where at_bats is greater than or equal to 100
subsetdata <- subset(data, at_bats >= 100)

# Display dimensions of the subset data
dim(subsetdata)

# Summary of the subset data
summary(subsetdata)





# Read the data with correct delimiter
data <- read.csv("baseball.csv", sep = "\t", strip.white = TRUE)

# Display the structure of the data to inspect the column names
str(data)

# Display the column names to identify the correct names
print(colnames(data))

# Check the number of columns
num_columns <- length(colnames(data))
print(num_columns)

# Manually correct the column names if necessary
# Adjust this list to match the number of columns and their intended names
corrected_colnames <- c(
  "firstname", "lastname", "age", "team", "games", 
  "at_", "bats", "runs", "hits", "dou", 
  "bles", "trip", "les_h", "omeru", "ns_RBIs", 
  "walks", "strikeo", "uts", 
  "bat_ave_on_base_pct_slugging_pct_stolen_bases_caught_stealing"
)

# Assign the corrected column names to the data if the number of columns match
if (length(corrected_colnames) == num_columns) {
  colnames(data) <- corrected_colnames
} else {
  stop("Number of columns does not match the number of provided names.")
}

# Display the corrected column names to verify
print(colnames(data))

# Manually combine columns if needed (for example at_ and bats into at_bats)
data$at_bats <- data$at_ + data$bats

# Subset the data where at_bats is greater than or equal to 100
subsetdata <- subset(data, at_bats >= 100)

# Display dimensions of the subset data
dim(subsetdata)

# Summary of the subset data
summary(subsetdata)



########################
# Read the data with correct delimiter
data <- read.csv("baseball.csv", sep = "\t", strip.white = TRUE)

# Display the structure of the data to inspect the column names
str(data)

# Display the column names to identify the correct names
print(colnames(data))

# Check the number of columns
num_columns <- length(colnames(data))
print(num_columns)

# Inspect the first few rows to understand the data structure
head(data)

# Manually correct the column names if necessary
corrected_colnames <- c(
  "firstname", "lastname", "age", "team", "games", 
  "at_", "bats", "runs", "hits", "dou", 
  "bles", "trip", "les_h", "omeru", "ns_RBIs", 
  "walks", "strikeo", "uts", 
  "bat_ave_on_base_pct_slugging_pct_stolen_bases_caught_stealing"
)

# Assign the corrected column names to the data if the number of columns match
if (length(corrected_colnames) == num_columns) {
  colnames(data) <- corrected_colnames
} else {
  stop("Number of columns does not match the number of provided names.")
}

# Display the corrected column names to verify
print(colnames(data))

# Combine 'at_' and 'bats' into 'at_bats' if needed
data$at_bats <- data$at_ + data$bats

# Subset the data where at_bats is greater than or equal to 100
subsetdata <- subset(data, at_bats >= 100)

# Display dimensions of the subset data
dim(subsetdata)

# Summary of the subset data
summary(subsetdata)


subsetdata <- data$bats >= 100
subsetdata
#################


# Read the data with correct delimiter
data <- read.csv("baseballu.txt", sep = "", strip.white = T)
data
# Display structure of the data to inspect column names and structure
str(data)

# Assuming you've corrected column names as discussed earlier...

# Create subset based on condition
subsetdata <- subset(data, bats >= 100)

# Display subsetdata
print(subsetdata)

# Inspect the column names to find the correct name
colnames(data)
# Assuming the correct name is 'at_bats', not 'bats'
subsetdata <- data[data$at_bats >= 100,]

subsetdata













































