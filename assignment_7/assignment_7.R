data<-read.csv("VM_traindata.csv")
data

#  Check the overall significance of the model
model <- lm(Relative.Strength.in.the.segment ~ Sales.Value....Mn + Profit.. + Joint.Bid...WSES.Portion, data = data)
summary(model)

#  b) Check the individual significance of variables and answer the following questions

#  Q.1 Does winning improve with better relative strength perception of the segment?
#   Relative.Strength.in.the.segment (to Intercept in model output).

#  Q.2 Are we winning more large deals than small deals?
 # Variable: Sales.Value....Mn
 # Estimate: -0.0937948
 # p-value: 0.312 (not significant)
 # The p-value for Sales.Value....Mn is 0.312, which is not significant. This suggests that there is no significant evidence to support that we are winning more large deals compared to small deals based on the sales value.

#  Q.3 Dors low margin bring more wins?
# Variable: Profit..
# Estimate: 0.0008713
# p-value: 0.962 (not significant)
# the p-value for Profit.. is 0.962, which is not significant. This indicates that there is no significant evidence that low margins bring more wins.

#  Q.4 Is it favourable if WSFS bundle its product with others?
# Variable: Joint.Bid...WSES.Portion
# Estimate: -0.0190979
# p-value: 0.237 (not significant)
# the p-value for Joint.Bid...WSES.Portion is 0.237, which is not significant. Therefore, there is no significant evidence that bundling WSFS products with others is favorable.


# Calculate AUROC of the model
data$Win <- ifelse(data$Relative.Strength.in.the.segment > 70, 1, 0)

# logistic regression model
model <- glm(Win ~ Sales.Value....Mn + Profit.. + Joint.Bid...WSES.Portion, data = data, family = binomial)
predicted_probabilities <- predict(model, type = "response")

if (!require(pROC)) install.packages("pROC")
library(pROC)
# AUROC Calculate
roc_curve <- roc(data$Win, predicted_probabilities)
auc(roc_curve)
# Plot ROC curve
plot(roc_curve, main = "ROC Curve")



# Part 2: Optimization Model (VMS data shared) Optimize the Marketing Spends with following constraints:
# Marketing Costs to be reduced by 50% The sales pipeline shouldn't go below 50% of the total pipeline value
# Expected value of sales to be at least 50% of the total expected sales Expected value of profits to be at least 50% of the total expected profits
# Expected value of FY15 revenues to be at least 50% of the total expected FY15 revenues Marketing spend allocated on the opportunity to be 6% of the sales value

if (!require(lpSolve)) {
  install.packages("lpSolve")
}

# Define variables
n <- nrow(data)
TotalMarketingCosts <- sum(data$MarketingCost)
TotalPipelineValue <- sum(data$PipelineValue)
TotalExpectedSales <- sum(data$ExpectedSales)
TotalExpectedProfits <- sum(data$ExpectedProfits)
TotalFY15Revenues <- sum(data$FY15Revenues)

# Define the objective function coefficients (minimize marketing costs)
objective <- data$MarketingCost

# Define the constraint matrix and the right-hand side of the constraints
constraints <- rbind(
  data$MarketingCost,                      
  data$PipelineValue,                      
  data$ExpectedSales,                     
  data$ExpectedProfits,                   
  data$FY15Revenues,                      
  data$SalesValue * 0.06                  
)

rhs <- c(
  0.5 * TotalMarketingCosts,               
  0.5 * TotalPipelineValue,                
  0.5 * TotalExpectedSales,                
  0.5 * TotalExpectedProfits,             
  0.5 * TotalFY15Revenues,                 
  rep(0, n)                                
)

#  the direction Define of the constraints
direction <- c("<=", ">=", ">=", ">=", ">=", rep("<=", n))

# the LP problem Solve 
result <- lp(
  "min",
  objective,
  constraints,
  direction,
  rhs,
  all.int = FALSE
)

# Output of the results
print(result$solution)
print(result$objval)



