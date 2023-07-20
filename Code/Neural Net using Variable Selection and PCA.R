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

# This code provides the steps to build a neural net model from a principal component analysis which is built from a reduced set of variables from the Credit Approval data set.

# Read the data
credit_data <- read.csv("clean_dataset.csv")

# Create a new dataframe called `New_credit_data_nn`.
New_credit_data <- credit_data %>%
  select(Debt,YearsEmployed,PriorDefault,Employed,CreditScore,Income,Approved)
head(New_credit_data)

# Scale the 'Debt', 'YearsEmployed', 'CreditScore', and 'Income' variables
New_credit_data$Debt <- scale(New_credit_data$Debt)
New_credit_data$YearsEmployed <- scale(New_credit_data$YearsEmployed)
New_credit_data$CreditScore <- scale(New_credit_data$CreditScore)
New_credit_data$Income <-scale(New_credit_data$Income)
head(New_credit_data)

# Split data into training and test set
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(New_credit_data), replace=TRUE, prob=c(0.7, 0.3))
train <- New_credit_data[sample,]
test <- New_credit_data[!sample,]

# Perform PCA on training set
PCA_data <- prcomp(train, scale=TRUE, center=TRUE)
summary(PCA_data)

# Select principal components that explain at least 90% of the variance in the data
var_explained = PCA_data$sdev^2 / sum(PCA_data$sdev^2)
var_explained
which(cumsum(var_explained) >= 0.9)

# The first 5 components explain at least 90% of the data. Create new data frame called `mydata_nn` using first 5 components
mydata <- data.frame(Class=train[,"Approved"], PCA_data$x[,1:5])
head(mydata)

# Train neural net on `mydata_nn`
model <- neuralnet(Class~., data=mydata, hidden=2, linear.output=FALSE)

# Plot the neural net
plot(model, rep='best')

# Make predictions on the test set
# First transform the test_nn data
test.p <- predict(PCA_data, newdata=test)

# Make predictions
pred <-predict(model, newdata=data.frame(test.p[,1:5]))

# Create a confusion matrix to test the performance of the neural net
predApproved <- factor(ifelse(pred >= 0.5,"1","0"))

confusionMatrix(data=as.factor(test$Approved), reference=predApproved, positive="1")