## Question_1
library(readxl)
data <- read_excel("UV6696-XLS-ENG.xlsx", sheet = 2)
data1 <- read_excel("UV6696-XLS-ENG.xlsx", sheet = 2)

str(data)
colnames(data)


names(data)[names(data) == "Customer Age (in months)"] <- "Age"
names(data)[names(data) == "Churn (1 = Yes, 0 = No)"] <- "Churn"
names(data)[names(data) == "CHI Score Month 0"] <- "CHI_Month0"
names(data)[names(data) == "CHI Score 0-1"] <- "CHI_Score_01"
names(data)[names(data) == "Support Cases Month 0"] <- "supportcases0"
names(data)[names(data) == "Support Cases 0-1"] <- "supportcases01"
names(data)[names(data) == "SP Month 0"] <- "spmonth0"
names(data)[names(data) == "Logins 0-1"] <- "logins"
names(data)[names(data) == "Blog Articles 0-1"] <- "Blog"
names(data)[names(data) == "Views 0-1"] <- "views0_1"
names(data)[names(data) == "Days Since Last Login 0-1"] <- "Days_Since_Last_Login_0_1"
names(data)[names(data) == "SP 0-1"] <- "SP_01"

library(dplyr)


data <- data %>%
  mutate(age_group = case_when(
    data$Age < 6 ~ "less_than_6",
    data$Age >= 6 & data$Age <= 14 ~ "between_6_and_14",
    data$Age > 14 ~ "more_than_14"
  ))


churn_counts <- data %>%
  group_by(age_group) %>%
  summarise(
    churned = sum(Churn),
    total = n()
  )

less_than_6_churned <- churn_counts$churned[churn_counts$age_group == "less_than_6"]
less_than_6_total <- churn_counts$total[churn_counts$age_group == "less_than_6"]

between_6_and_14_churned <- churn_counts$churned[churn_counts$age_group == "between_6_and_14"]
between_6_and_14_total <- churn_counts$total[churn_counts$age_group == "between_6_and_14"]

more_than_14_churned <- churn_counts$churned[churn_counts$age_group == "more_than_14"]
more_than_14_total <- churn_counts$total[churn_counts$age_group == "more_than_14"]

prop_test_1 <- prop.test(
  x = c(between_6_and_14_churned, more_than_14_churned),
  n = c(between_6_and_14_total, more_than_14_total),alternative = "greater"
)

print(prop_test_1)

prop_test_2 <- prop.test(
  x = c(between_6_and_14_churned, less_than_6_churned),
  n = c(between_6_and_14_total, less_than_6_total),alternative = "greater"
)
print(prop_test_2)
# Based on the prop test we reject the null hypothesis. So the assumption of Wall was correct.

##Question2
set.seed(123)
library(caret)
library(pROC)
library(dplyr)
library(pROC)
library(randomForest)
library(ipred)
colnames(data1)[which(names(data1) == "Churn (1 = Yes, 0 = No)")] <- "Churn"
trainIndex <- createDataPartition(data1$Churn, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]
trainData$Churn <- as.factor(trainData$Churn)
testData$Churn <- as.factor(testData$Churn)
logistic_model <- glm(Churn ~ ., data = trainData, family = binomial)
summary(logistic_model)
with(logistic_model,null.deviance-deviance)
with(logistic_model,df.null-df.residual)
with(logistic_model,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=FALSE))
##a
# the model is significant 
# if reject the null hypothesis since p value  7.541624e-34
#b) the significant factors are
#ID 
#Customer Age (in months)
#CHI Score Month 0`
#CHI Score 0-1
#Days Since Last Login 0-1

#c)
probabilities <- predict(logistic_model, newdata = testData, type = "response")
roc <- roc(testData$Churn, probabilities)
print(auc(roc))

## d
threshold <- seq(0, 1, by = 0.01)
accuracy <- sapply(threshold, function(t) {
  prediction <- ifelse(probabilities > t, 1, 0)
  mean(prediction == testData$Churn)
})
best_threshold <- threshold[which.max(accuracy)]
print(best_threshold)  # 0.33


## Question_3
bagging <- randomForest(Churn ~ ., data = trainData, ntree = 5000)
bagging_prediction <- predict(bagging, newdata = testData)
conf_matrix <- confusionMatrix(bagging_prediction, testData$Churn)
baggingaccuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy of the bagging model:", round(baggingaccuracy, 4)))


## Question _4
random_forestl <- randomForest(Churn ~ ., data = trainData, ntree = 5000)
rf_predictions <- predict(random_forestl, newdata = testData)
rf_conf_matrix <- confusionMatrix(rf_predictions, testData$Churn)
rf_accuracy <- rf_conf_matrix$overall['Accuracy']
print(paste("Accuracy of the random forest model:", round(rf_accuracy, 4)))