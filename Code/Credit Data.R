rm(list=ls())
library("tidyverse")
library("car")
install.packages("SSBtools")
library("SSBtools")
library("ggplot2")
credit_data <- read.csv('/Users/Owner/Downloads/archive (1)/clean_dataset.csv',header= TRUE)
any(is.na(credit_data))
summary(credit_data)
###Data prepping . Converting category variables into factor for easier graphing. 
Gender_f <-factor(credit_data$Gender)
Approval_f <- factor(credit_data$Approved)
Married_f <-factor(credit_data$Married)
Bank_f <-factor(credit_data$BankCustomer)
Default_f <-factor(credit_data$PriorDefault)
Employed_f <-factor(credit_data$Employed)
Driver_license_f<-factor(credit_data$DriversLicense)


## Understanding the underlying variables. 
##1. Age. Basic scatterplot for age to show the range between 13-80 years old. 
summary(credit_data$Age)
plot(credit_data$Age)
hist(credit_data$Age)
ggplot(credit_data,aes(Age))+geom_histogram(bins=10,boundary=10)
plot(density(credit_data$Age),col='red',main="Age Distribution",xlbabs='Age')
log_age <-log(credit_data$Age)
hist(log_age)
##skew to the left- majority of applicant is working class, under the age of 50. Log_age fix the issue
log_age <-log(credit_data$Age)
hist(log_age)
##2. Gender
ggplot(credit_data)+geom_bar(mapping=aes(x=Gender_f,fill=Approval_factor))
##number of applicant is skew toward gender "1"?? confirm men or women 
##3. Debt ratio 
ggplot(credit_data,aes(Debt))+geom_histogram(bins=10)
ggplot(credit_data,mapping=aes(x=Debt,y=Age,color=Approval_factor))+geom_point()+scale_color_manual(values=c("1"="Red","0"="Black"))
hist(credit_data$Debt)
log_debt <-log(credit_data$Debt)
hist(log_debt)
## younger age with higher debt
##4. Married status           
ggplot(credit_data,mapping=aes(x=Married_f,y=Age))+geom_boxplot()
## 5: Bank Customer 
ggplot(credit_data)+geom_bar(mapping=aes(x=Bank_f,fill=Approval_factor))
## 6: Industry: 
unique(credit_data$Industry)
##7: ethnicity 
ggplot(credit_data)+geom_bar(mapping=aes(x=Ethnicity,fill=Approval_factor))
## 8: Years of employment
hist(credit_data$YearsEmployed)
log_hist_emp <-log(credit_data$YearsEmployed)
hist(log_hist_emp)
##Credit score
hist(credit_data$CreditScore)
log_creditscore <-log(credit_data$CreditScore)
hist(log_creditscore)
##9:Income
hist(credit_data$Income)
log_income <-log(credit_data$Income)
hist(log_income)

## Look like the 5 continuous datas: age, debt, credit score, income and years of employment need log or some sort of transformation
## elaborate w/ team on continuing repeating show the regression on each factor to the approval decision 

# subset data to remove category variable to test for correlation 
sub_credit_data <- credit_data[,-c(6,7,14,13,16)]
df <- cor(sub_credit_data)
df
install.packages("corrplot")
library("corrplot")
corrplot(df,type="upper",order="hclust")
## Noticing "married" and "bank customer" are very highly correlated. 


# Running a basic linear model regression to quickly check 1: statistically significant factors. 2 : multicollineary. 
##exclude Zipcode .. should I exclude industry as well??? 
credit_data <-credit_data[,-14]
model_init <-glm(credit_data$Approved~.,data=credit_data,family='binomial')
summary(model_init)
## Significant factors: Priordefault,employed, citizen,creditscore
vif(model_init)



## modifying the categorial variable 
credit_data <- credit_data %>%
  mutate(Citizen_by_birth=ifelse(Citizen=='ByBirth',1,0)) %>%
  mutate(W=ifelse(Ethnicity=='White',1,0)) %>%
  mutate(B=ifelse(Ethnicity=='Black',1,0)) %>%
  mutate(A=ifelse(Ethnicity=='Asian',1,0)) %>%
  mutate(L=ifelse(Ethnicity=='Latino',1,0)) %>%
  mutate(O=ifelse(Ethnicity=='Other',1,0))
# selecting all columns except Industry and Zipcode - Zipcode is fake data.
credit_data_selected <- credit_data %>%
  select(Gender,Age,Debt,Married,BankCustomer,YearsEmployed,PriorDefault,Employed,CreditScore,DriversLicense,Income,Approved,Citizen_by_birth,W,B,A,L,O)

# splitting data into training and testing 
set.seed(1)
sample <- sample(c(TRUE,FALSE),nrow(credit_data_selected),replace=TRUE,prob=c(0.7,0.3))
train <- credit_data_selected[sample,]
test <-credit_data_selected[!sample,]




