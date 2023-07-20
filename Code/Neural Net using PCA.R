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

# This code provides the steps to transform and reduce the dimensionality of the Credit Approval data set in order to build a neural net model.

# Read the data
credit_data <- read.csv("clean_dataset.csv")

# Create a new dataframe called `New_credit_data`. We do not need to normalize the skewed data to build a neural net model.
New_credit_data <- credit_data %>%
  select(Gender,Age,Debt,Married,BankCustomer,Ethnicity,YearsEmployed,PriorDefault,Employed,CreditScore,DriversLicense,Income,Approved,Citizen) %>%
  mutate(Citizen_by_birth=ifelse(Citizen=='ByBirth',1,0)) %>%
  mutate(Citizen_by_others=ifelse(Citizen=='ByOtherMeans',1,0)) %>%
  mutate(Citizen_temp=ifelse(Citizen=='Temporary',1,0))%>%
  mutate(W= ifelse(Ethnicity=='White',1,0)) %>%
  mutate(B=ifelse(Ethnicity=='Black',1,0)) %>%
  mutate(A=ifelse(Ethnicity=='Asian',1,0)) %>%
  mutate(L=ifelse(Ethnicity=='Latino',1,0)) %>%
  mutate(O=ifelse(Ethnicity=='Other',1,0))
head(New_credit_data)

# Remove 'Ethnicity' and 'Citizen' variables
New_credit_data <- subset(New_credit_data, select=-c(Ethnicity,Citizen))

# Scale the 'Age', Debt', 'YearsEmployed', 'CreditScore', and 'Income' variables
New_credit_data$Age <- scale(New_credit_data$Age)
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
var_explained = PCA_data_nn$sdev^2 / sum(PCA_data_nn$sdev^2)
which(cumsum(var_explained) >= 0.9)

# The first 12 components explain at least 90% of the data. Create new data frame called `mydata_nn` using first 12 components
mydata <- data.frame(Class=train[,"Approved"], PCA_data$x[,1:13])
#head(mydata)

# Train neural net on `mydata_nn`
model <- neuralnet(Class~., data=mydata, hidden=2, linear.output=FALSE)

# Make predictions on the test set
# First transform the test_nn data
test.p <- predict(PCA_data, newdata=test)

# Make predictions
pred <-predict(model, newdata=data.frame(test.p[,1:13]))

# Create a confusion matrix to test the performance of the neural net
predApproved <- factor(ifelse(pred >= 0.5,"1","0"))
confusionMatrix(data=as.factor(test$Approved), reference=predApproved, positive="1")

# Create an ROC curve and calculate the AUC
credit_roc <- roc(test$Approved, pred)
plot(credit_roc, print.auc=TRUE)