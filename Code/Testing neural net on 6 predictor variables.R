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

# Read the data
credit_data <- read.csv("clean_dataset.csv")

# Create a new dataframe called `df`. We do not need to normalize the skewed data to build a neural net model.
df <- credit_data %>%
  select(Debt,YearsEmployed,PriorDefault,Employed,CreditScore,Income,Approved)
head(df)
            
# Scale the continuous variables, Debt, YearsEmployed, and Income
df$Debt <- scale(df$Debt)
df$CreditScore <- scale(df$CreditScore)
df$Income <- scale(df$Income)
head(df)

# Split data into training and test set
set.seed(3)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7, 0.3))
train <- df[sample,]
test <- df[!sample,]

# Train 6 neural nets on `mydata_nn` to see effect of different numbers of layers and neurons
model1 <- neuralnet(Approved~., data=train, hidden=2, linear.output=FALSE)
model2 <- neuralnet(Approved~., data=train, hidden=4, linear.output=FALSE)
model3 <- neuralnet(Approved~., data=train, hidden=6, linear.output=FALSE)
#model4 <- neuralnet(Approved~., data=train, hidden=c(4,2), linear.output=FALSE)
#model5 <- neuralnet(Approved~., data=train, hidden=c(6,4), linear.output=FALSE)
#model6 <- neuralnet(Approved~., data=train, hidden=c(6,2), linear.output=FALSE)

# The models with 2 layers did not converge on a solution, so we will just stick to looking at 1 layer, with 2, 4, or 6 nodes
# Make predictions on the test set
pred1 <-predict(model1, newdata=test)
pred2 <-predict(model2, newdata=test)
pred3 <-predict(model3, newdata=test)


# Create a confusion matrix to test the performance of the neural net
predApproved1 <- factor(ifelse(pred1 >= 0.5,"1","0"))
predApproved2 <- factor(ifelse(pred2 >= 0.5,"1","0"))
predApproved3 <- factor(ifelse(pred3 >= 0.5,"1","0"))

confusionMatrix(data=as.factor(test$Approved), reference=predApproved1, positive="1")
confusionMatrix(data=as.factor(test$Approved), reference=predApproved2, positive="1")
confusionMatrix(data=as.factor(test$Approved), reference=predApproved3, positive="1")

#The best accuracy was obtained from the model with 2 nodes, so we will progress with this model.