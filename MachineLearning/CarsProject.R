#=============================================================================
# Data Analysis - Cars
#=============================================================================
#Environment Set up and Data Import
#Set up working Directory
rm(list=ls())
set.seed(248)
setwd("C:/Users/Radhika/Desktop/R Programming/Project_MachineLearning")
getwd()
carsData <- read.csv("Cars.csv", header=T)
#convert the categorical variable Gender into numeric and factor data
carsData$Gender = as.factor(carsData$Gender)
carsData$Gender = as.numeric(carsData$Gender)
#There are three types of transport data in the given data set
#As we are going to predict whether employee is going to use Car as the transport
#Convert car type to 1 and remaining two types of transport to 0
#convert the categorical variable Transport into numeric and factor data
carsData$Transport = as.numeric(carsData$Transport)
carsData$Transport[carsData$Transport == 1] = 0
carsData$Transport[carsData$Transport == 2] = 1
carsData$Transport[carsData$Transport == 3] = 0
carsData$Transport = as.factor(carsData$Transport)
str(carsData)
#Check the proportion of Transport data
prop.table(table(carsData$Transport))
# In the above data, car is the minority class and other transport are the 
# Majority class
#Split the data into Train and Test data sets
library(caTools)       
split = sample.split(carsData$Transport, SplitRatio = 0.7)
cars_train = subset(carsData, split == T)
cars_test = subset(carsData, split == F)
#Check the proportion of Transport data in train and test
prop.table(table(cars_train$Transport))
prop.table(table(cars_test$Transport))
#In Training data set, the majority class is 91.7% and minority is 0.82%
#In Test data set, the majority class is 91.2% and minority is 0.87%
#logistic regression
LogRmodel  = glm(Transport ~., data = cars_train, family = binomial)
LogRpredTest = predict(LogRmodel, newdata = cars_test, type = "response")
tabLogR = table(cars_test$Transport, LogRpredTest > 0.5)
tabLogR
sum(diag(tabLogR))/sum(tabLogR)
# By applying logistic regression, this model is 99.2% accurate
#KNN
library(class)
predKNNmodel = knn(train = cars_train[,1:8], test = cars_test[,1:8], cl = cars_train[,9], k = 3, prob = T)
tabKNN = table(cars_test$Transport, predKNNmodel)
tabKNN
sum(diag(tabKNN))/sum(tabKNN)
# KNN also shows 99.2% accuracy to predict using this model.
install.packages("e1071")
library(e1071)
NBmodel = naiveBayes(Transport ~., data = cars_train)
NBpredTest = predict(NBmodel, newdata = cars_test)
tabNB = table(cars_test$Transport, NBpredTest)
tabNB
sum(diag(tabNB))/sum(tabNB)
#Using, naive model, this model is 98.4% accurate
#Bagging
install.packages("xgboost")
install.packages("caret")
install.packages("ipred")
install.packages("rpart")
library(xgboost)     
library(caret)       
library(ipred)
library(rpart)
BAGmodel <- bagging(as.numeric(Transport) ~.,data=cars_train, control=rpart.control(maxdepth=10, minsplit=50))
BAGpredTest = predict(BAGmodel, cars_test)
tabBAG = table(cars_test$Transport,BAGpredTest > 0.5)
tabBAG
sum(diag(tabBAG))/sum(tabBAG)
#Bagging model shows the model is 91.2% accurate
#Boosting
features_train<-as.matrix(cars_train[,1:8])
label_train<-as.matrix(cars_train[,9])
features_test<-as.matrix(cars_test[,1:8])
XGBmodel = xgboost(
  data = features_train,
  label = label_train,
  eta = .001,
  max_depth = 5,
  min_child_weight = 3,
  nrounds = 10,
  nfold = 5,
  objective = "binary:logistic",  # for regression models
  verbose = 0,               # silent,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)
XGBpredTest = predict(XGBmodel, features_test)
tabXGB = table(cars_test$Transport, XGBpredTest>0.5)
tabXGB
sum(diag(tabXGB))/sum(tabXGB)
#Boosting shows, model is 98.4% accurate
#SMOTE
install.packages("DMwR")
library(DMwR)
table(carsData$Transport)
smote.train<-subset(carsData, split == T)
smote.test<-subset(carsData, split == F)
table(smote.train$Transport)
balanced.data <- SMOTE(Transport ~., smote.train, k = 5)
table(balanced.data$Transport)
smote_features_train<-as.matrix(balanced.data[,1:8])
smote_label_train<-as.matrix(balanced.data$Transport)

smote.xgb.fit <- xgboost(
  data = smote_features_train,
  label = smote_label_train,
  eta = 0.001,
  max_depth = 5,
  nrounds = 10,
  nfold = 5,
  objective = "binary:logistic",  # for regression models
  verbose = 0,               # silent,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

smote_features_test<-as.matrix(smote.test[,1:8])
smote.test$smote.pred.class <- predict(smote.xgb.fit, smote_features_test)

tabSmt = table(smote.test$Transport,smote.test$smote.pred.class>=0.5)
tabSmt
sum(diag(tabSmt))/sum(tabSmt)
#Smote Test also shows the model is 99.2% accurate
