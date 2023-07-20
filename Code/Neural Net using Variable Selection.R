library("tidyverse")
library("car")
library("SSBtools")
library("ggplot2")
library('moments')
library('corrr')
library('dplyr')
library('neuralnet')
library('pROC')
library('caret')

# This code provides the steps to build a neural net model from a reduced set of variables from the Credit Approval data set.

# Read the data
credit_data <- read.csv("clean_dataset.csv")

# Create a new dataframe called `New_credit_data`.
New_credit_data <- credit_data %>%
  select(Debt,YearsEmployed,PriorDefault,Employed,CreditScore,Income,Approved)
head(New_credit_data)

# Scale the 'Debt', 'YearsEmployed', 'CreditScore', and 'Income' variables
New_credit_data$Debt <- scale(New_credit_data$Debt)
New_credit_data$YearsEmployed <- scale(New_credit_data$YearsEmployed)
New_credit_data$CreditScore <- scale(New_credit_data$CreditScore)
New_credit_data$Income <- scale(New_credit_data$Income)
head(New_credit_data)

# Split data into training and test set
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(New_credit_data), replace=TRUE, prob=c(0.7, 0.3))
train <- New_credit_data[sample,]
test <- New_credit_data[!sample,]

# Train neural net on `mydata`
model <- neuralnet(Approved~., data=train, hidden=2, linear.output=FALSE)

# Make predictions on the test set
pred <- predict(model, newdata=test[,1:6])

# Create a confusion matrix to test the performance of the neural net
predApproved <- factor(ifelse(pred >= 0.5,"1","0"))
confusionMatrix(data=as.factor(test$Approved), reference=predApproved, positive="1")

# Create an ROC curve and calculate the AUC
credit_roc <- roc(test$Approved, pred)
plot(credit_roc, print.auc=TRUE)