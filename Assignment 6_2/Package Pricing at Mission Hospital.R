data <- read.csv("Mission_Hospital.csv")
library(dplyr)
data
Q1. 
# Check missing values in the data
any(is.na(data))
missing_values <- colSums(is.na(data))
missing_values


Q2.
# fill missing values in numerical columns with mean
fill_with_mean <- function(x) { x[is.na(x)] <- mean(x, na.rm = TRUE) return(x) }

# fill missing values in categorical columns with mode
fill_with_mode <- function(x) { mode_value <- as.character(stats::mode(x, na.rm = TRUE))x[is.na(x)] <- mode_value
  return(x)
}

#  numerical and categorical columns  Separate
numerical_columns <- sapply(data, is.numeric)
numerical_columns
number_numerical_columns <- sum(sapply(data, is.numeric))
number_numerical_columns

categorical_columns <- sapply(data, is.factor) | sapply(data, is.character)
categorical_columns
number_categorical_columns <- sum(sapply(data, is.factor) | sapply(data, is.character))
number_categorical_columns

msing_values <- colSums(is.na(data))
columns_missing_values <- msing_values[msing_values > 0]
columns_missing_values



# Fill missing values in numerical columns with mean
data[, numerical_columns] <- lapply(data[, numerical_columns], fill_with_mean)

# Fill missing values in categorical columns with mode
data[, categorical_columns] <- lapply(data[, categorical_columns], fill_with_mode)

print(data)

names(data)
new_data <- data[,-c(1,12)]
colSums(is.na(new_data))/nrow(new_data)

install.packages("Hmisc")
library(Hmisc)


new_data$BP_LOW = as.numeric(impute(new_data$BP_LOW, mean))
new_data$BP_HIGH = as.numeric(impute(new_data$BP_HIGH, mean))
new_data$UREA = as.numeric(impute(new_data$UREA, mean))
new_data$HB = as.numeric(impute(new_data$HB, mean))
new_data$CREATININE  = as.numeric(impute(new_data$CREATININE , mean))
new_data$KEY_COMPLAINTS._CODE = as.character(impute(new_data$KEY_COMPLAINTS._CODE , mode))
names(data)
colSums(is.na(new_data))/nrow(new_data)

sum(is.na(new_data$KEY_COMPLAINTS._CODE))
class(new_data$KEY_COMPLAINTS._CODE)
class(new_data$CREATININE)



#  categorical values to dummy variable converting
str(new_data)
install.packages("fastDummies")
library(fastDummies)
Hospital_dumy <- dummy_cols(new_data)
head(Hospital_dumy)
str(Hospital_dummy)
Hospitalmreg <- dummy_cols(new_data, remove_first_dummy = TRUE,remove_selected_columns = TRUE)
head(Hospitalmreg)
names(Hospitalmreg)
str(Hospitalmreg)



Q4. 
model1 <- lm(TOTAL_COST_TO_HOSPITAL~1, data = Hospitalmreg)
summary(model1)

model2 <- lm(TOTAL_COST_TO_HOSPITAL~., data = Hospitalmreg)
summary(model2)
Q5. 

par(mfrow=c(2,2))
plot(model2)

Q6.
model2_log <- lm(log(TOTAL_COST_TO_HOSPITAL)~., data = Hospitalmreg)
summary(model2_log)

par(mfrow=c(2,2))
plot(model2_log)



ck <- cooks.distance(model2_log)
which(ck>1)

summary(model2_log)
Q8.
# final model

x_model <- step(model1, scope = list(lower = model1, upper = model2), direction = "both", trace = TRUE, test = "F")



res_test<- predict(model, testing, type = "response")
length(res_test)

predicted_type<- ifelse(res_test>0.5,"Positive","Negative")


library(pROC)
roc_curve<-roc(testing$type, res_test, pring.auc=TRUE, plot=TRUE)
