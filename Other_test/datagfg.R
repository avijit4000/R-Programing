x <- c(rep(1:20)) 
y <- x * 2
f <- lm(x ~ y)
f 
library(caret)
library(tidyverse)
install.packages("datarium")
data("marketing", package = "datarium")


head(marketing)
marketing

# R program to implement
# validation set approach

# setting seed to generate a 
# reproducible random sampling
set.seed(123)

# creating training data as 80% of the dataset
random_sample <- createDataPartition(marketing $ sales, p = 0.8, list = FALSE)

random_sample <- createDataPartition(marketing$sales, p = 0.8, list = FALSE)



# generating training dataset
# from the random_sample
training_dataset <- marketing[random_sample, ]

# generating testing dataset
# from rows which are not 
# included in random_sample
testing_dataset <- marketing[-random_sample, ]

# Building the model

# training the model by assigning sales column
# as target variable and rest other columns
# as independent variables
model <- lm(sales ~., data = training_dataset)

# predicting the target variable
predictions <- predict(model, testing_dataset)

# computing model performance metrics
data.frame( R2 = R2(predictions, testing_dataset $ sales),
            RMSE = RMSE(predictions, testing_dataset $ sales),
            MAE = MAE(predictions, testing_dataset $ sales))

x <- c(2, 4, 6, 8) 
y <- c(1, 3, 5, 7) 
x
y
f <- lm(y ~ x)
install.packages("dplyr")
library(dplyr)
summary(mtcars)

mtcars


install.packages("caTools")


install.packages("ROCR")


library(caTools)
library(ROCR)


split <- sample.split(mtcars, SplitRatio = 0.8)
split


train_reg <- subset(mtcars, split == "TRUE")
test_reg <- subset(mtcars, split == "FALSE")


train_reg
test_reg


logistic_model <- glm(vs ~ wt + disp,
                      data = train_reg,
                      family = "binomial")

logistic_model


summary(logistic_model)


predict_reg <- predict(logistic_model,
                       test_reg, type = "response")

predict_reg

predict_reg <- ifelse(predict_reg >0.5, 1, 0)
predict_reg
table(test_reg$vs, predict_reg)


missing_classerr <- mean(predict_reg != test_reg$vs)
print(paste('Accuracy =', 1 - missing_classerr))

ROCPred <- prediction(predict_reg, test_reg$vs)

ROCPer <- performance(ROCPred, measure = "tpr",
                      x.measure = "fpr")

ROCPer


auc <- performance(ROCPred, measure = "auc")
auc

auc <- auc@y.values[[1]]

auc

install.packages("rpart")
