library("tidyverse")
library("car")
library("SSBtools")
library("ggplot2")
library('moments')
library('corrr')
library('dplyr')
library('neuralnet')

# This code provides the steps to transform and reduce the dimensionality of the Credit Approval dataset in order to build a neural net model.

# Read the data
credit_data <- read.csv("clean_dataset.csv")

# Check for missing values in the dataset
any(is.na(credit_data))

# Create a new dataframe called `New_credit_data_nn`
New_credit_data_nn <- credit_data %>%
  select(Gender,Age,Debt,Married,BankCustomer,Ethnicity,YearsEmployed,PriorDefault,Employed,CreditScore,DriversLicense,Income,Approved,Citizen) %>%
  mutate(Citizen_by_birth=ifelse(Citizen=='ByBirth',1,0)) %>%
  mutate(Citizen_by_others=ifelse(Citizen=='ByOtherMeans',1,0)) %>%
  mutate(Citizen_temp=ifelse(Citizen=='Temporary',1,0))%>%
  mutate(W= ifelse(Ethnicity=='White',1,0)) %>%
  mutate(B=ifelse(Ethnicity=='Black',1,0)) %>%
  mutate(A=ifelse(Ethnicity=='Asian',1,0)) %>%
  mutate(L=ifelse(Ethnicity=='Latino',1,0)) %>%
  mutate(O=ifelse(Ethnicity=='Other',1,0))
head(New_credit_data_nn)

# Remove 'Ethnicity' and 'Citizen' variables, and scale the 'Income' variable
New_credit_data_nn <- subset(New_credit_data_nn, select=-c(Ethnicity,Citizen))
New_credit_data_nn$Income <-scale(New_credit_data_nn$Income)
head(New_credit_data_nn)

# Split data into training and test set
set.seed(10)
sample <- sample(c(TRUE, FALSE), nrow(New_credit_data_nn), replace=TRUE, prob=c(0.7, 0.3))
train_nn <- New_credit_data_nn[sample,]
test_nn <- New_credit_data_nn[!sample,]

# Perform PCA on training set
PCA_data_nn <- prcomp(train_nn, scale=TRUE, center=TRUE)
summary(PCA_data_nn)

# Select principal components that explain at least 90% of the variance in the data
var_explained_nn = PCA_data_nn$sdev^2 / sum(PCA_data_nn$sdev^2)
which(cumsum(var_explained_nn) >= 0.9)

# The first 13 components explain at least 90% of the data. Create new data frame called `mydata_nn` using first 13 components
mydata_nn <- data.frame(Class=train_nn[,"Approved"], PCA_data_nn$x[,1:13])

# Train a neural net on `mydata_nn`
model_nn <- neuralnet(Class~., data=mydata_nn, hidden=c(4,2), linear.output=FALSE)

# Plot the neural net
plot(model_nn, rep='best')

# Make predictions on the test set
test.p_nn <- predict(PCA_data_nn, newdata=test_nn)

# Create a confusion matrix to test the performance of the neural net
pred_nn <-predict(model_nn, newdata=data.frame(test.p_nn[,1:13]))
predApproved_nn <- factor(ifelse(pred_nn >= 0.5,"1","0"))
table(test_nn$Approved, predApproved_nn)
