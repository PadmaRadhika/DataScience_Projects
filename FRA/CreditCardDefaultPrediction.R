#Before reading the file in R, creation of new features such as pay% = PayAmt/Bill_Amt for each month for the last three months.

#Loading relevant libraries for current session
 
library(caTools)
library(caret)
library(ROCR)
library(corrplot)
library(car)

setwd("~/Desktop/Data")

#Reading the file in R

creditrisk=read.csv("CreditCardDefault.csv",header = TRUE)
View(creditrisk)
#Checking for missing values and omitting for the purpose of this case study
sum(is.na(creditrisk))
creditrisk1=na.omit(creditrisk)
sum(is.na(creditrisk1))

#checking the dimension and proportion of dependent variable
dim(creditrisk1)
prop.table(table(creditrisk1$default.payment.next.month))

#Creating Default as factor type variable and dummy variables for categorical independent variables
creditrisk1$Default=as.factor(creditrisk1$default.payment.next.month)
creditrisk1$Gender=ifelse(creditrisk1$SEX=="1",1,0)
creditrisk1$Education1=ifelse(creditrisk1$EDUCATION=="1",1,0)
creditrisk1$Education2=ifelse(creditrisk1$EDUCATION=="2",1,0)
creditrisk1$Married=ifelse(creditrisk1$MARRIAGE=="1",1,0)
creditrisk1$Fico=ifelse(creditrisk1$FICO=="<500",1,0)

#Checking the structure of dataset
str(creditrisk1)
 
#Partitioning data in train and test in 80:20 ratio
set.seed(123)
spl=sample.split(creditrisk1$default.payment.next.month,SplitRatio = 0.8)
train=subset(creditrisk1,spl==TRUE)
dim(train)
test=subset(creditrisk1,spl==FALSE)
dim(test)

#Checking distribution of dependent variable in train and test
prop.table(table(train$Default))
prop.table(table(test$Default))

#checking structure of train and getting rid of unnecessary variables for logistic model build
str(train)
train.new=train[,-c(1,3:5,7:12,16,17)]
test.new=test[,-c(1,3:5,7:12,16,17)]

#verifying the column names and attaching train data
names(train.new)
attach(train.new)

#checking for multicollinearity
correlation=corrplot(cor(train.new[,-6]),method = "circle",type = "upper")

#building the base model with all the variables
model=glm(Default~LIMIT_BAL+AGE+Pay.1+Pay.2+Pay.3+Gender+Education1+Education2+Married+Fico,data=train.new,family=binomial("logit"),maxit=100)
summary(model)

#Removing Education 1 to see if it impacts Education2 and makes in significant  
model1=glm(Default~LIMIT_BAL+AGE+Pay.2+Pay.3+Gender+Education1 + Education2+Married+Fico,data=train.new,family=binomial)
summary(model1)


model2=glm(Default~LIMIT_BAL+AGE+Pay.2+Pay.3+Gender+Married+Fico,data=train.new,family=binomial)
summary(model2)

#Removing all the insignificant variables
model3=glm(Default~LIMIT_BAL+Pay.2+Pay.3+Fico+AGE+Gender,data=train.new,family=binomial)
summary(model3)

model4=glm(Default~LIMIT_BAL+Pay.2+Pay.3+Fico+AGE,data=train.new,family=binomial)
summary(model4)


#Checking the VIF score for all the variables as per model2
vif(model4)

#Prediction using test data(You should do it on train set first)
predTest=predict(model4,newdata = test.new,type = "response")
head(test.new)

#Creation of confusion matrix to assess model performance measures
tab.LR=table(test.new$Default,predTest > 0.4)
tab.LR
sum(diag(tab.LR))/sum(tab.LR)

### Bill Amount/Payment Amount 
#1 Bill amount 0, Payment amount 0 1 PD 0
#2 Bill amount is finite, Payment amount 0 0 PD 1
#3 Bill amount is 0, Payment amount is finite 1 PD 0

#data$Ratio = data$Bill_Amount/data$Payment_Amount
