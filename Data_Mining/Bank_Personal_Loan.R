#=============================================================================
# Data Analysis - Bank Personal Loan
#=============================================================================
#Environment Set up and Data Import
#Set up working Directory
setwd("C:/Users/Radhika/Desktop/R Programming/Project_DataMining")
getwd()
#
#Import the required packages and install them
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ROCR")
#install.packages("caret")
#install.packages("ModelMetrics")
#install.packages("corrplot")

library(readxl)
library(tidyverse)
library(dplyr)
library(ROCR)
library(caret)
library(ModelMetrics)
library(corrplot)

#Read the input file
bank_data <- read_excel("Bank_Personal_Loan_Modelling.xlsx", sheet = "Bank_Personal_Loan_Modelling")
attach(bank_data)
view(bank_data)
sum(is.na(bank_data))
#Find the internal structure of the data
str(bank_data)
#Find the descriptive statistics of the data
summary(bank_data)
#Remove target variable before scaling the data
temp_data <-  bank_data %>% select (-`Personal Loan`)
str(temp_data)
new_bank_data = scale(temp_data[,-1])
summary(new_bank_data)
new_bank_data = as.data.frame(new_bank_data)
str(new_bank_data)

cor.mat <- cor(new_bank_data)
corrplot(cor.mat, type = "lower", method = "number")

### Experience is highly correlated with Age
## For now we decide to remove Experience
new_bank_data <-  new_bank_data %>% select (-`Experience (in years)`)
dim(new_bank_data)
names(new_bank_data)
view(new_bank_data)
#Add the Target variable that was removed while scaling the data
new_bank_data = cbind(new_bank_data, `Personal Loan` = bank_data$`Personal Loan`)
str(new_bank_data)
### Check the proportion of data in bank_data

nrow(subset(new_bank_data, `Personal Loan` == 1))/nrow(new_bank_data)

set.seed(1000)
train_indx <- sample(c(1:nrow(new_bank_data)), round(nrow(new_bank_data) * 0.7,0), replace = FALSE)
train_bank_data <- new_bank_data[train_indx, ]
test_bank_data <- new_bank_data[-train_indx, ]
sum(is.na(train_bank_data))
sum(is.na(test_bank_data))
dim(train_bank_data)
dim(test_bank_data)

train.pos <- subset(train_bank_data, `Personal Loan` == 1)
nrow(train.pos)

train.neg <- subset(train_bank_data, `Personal Loan` == 0)
nrow(train.neg)

dim(train.pos)
dim(train.neg)

set.seed(200)    ## Set the seed

## Take the sample subset from the major class (here negative)
train.neg.sub_idx <- sample(c(1:nrow(train.neg)), nrow(train.pos), replace = FALSE)
train.new <- train.neg[train.neg.sub_idx,]
dim(train.new)
sum(is.na(train.new))
train.new <- rbind(train.new, train.pos)  ## Merge the negative and positive cases 
dim(train.new)

train.new <- train.new[sample(1:nrow(train.new)),]

### Now check the proportion of Attrition in the sample
## in train_data
nrow(subset(train_bank_data, `Personal Loan` == 1))/nrow(train_bank_data)
## in train.new
nrow(subset(train.new, `Personal Loan` == 1))/nrow(train.new)

###CART Model
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
r.ctrl = rpart.control(minsplit = 100, minbucket = 10, cp=0, xval=10)
DTModel = rpart(`Personal Loan`~.,data=train.new, method="class", control = r.ctrl)
rpart.plot(DTModel)
attributes(DTModel)
DTModel$cptable
ptree = prune(DTModel, 0.0218750, "CP")
ptree$variable.importance
#CART Validation on Train Data
predTrain = predict(ptree, newdata = train.new)
view(predTrain)
predDT = predict(ptree, newdata = test_bank_data)
view(predDT)
#
library(ROCR)
#Validation on Train data
DTpredROC1 = ROCR::prediction(predTrain[,2], train.new$`Personal Loan`)
perf1 = performance(DTpredROC1, "tpr", "fpr")
plot(perf1)
as.numeric(performance(DTpredROC1, "auc")@y.values)
#
#Validation on Test Data
DTpredROC = ROCR::prediction(predDT[,2], test_bank_data$`Personal Loan`)
perf = performance(DTpredROC, "tpr", "fpr")
plot(perf)
as.numeric(performance(DTpredROC, "auc")@y.values)
#Decile code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

train.new$deciles = decile(predTrain[,2])
predTrain[,2]
test_bank_data$deciles = decile(predDT[,2])
predDT[,2]
#KS on Train
install.packages("ineq")
library(ineq)
#KS on Train
KS = max(attr(perf1, 'y.values')[[1]]-attr(perf1, 'x.values')[[1]])
KS
#KS on Test
KS = max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS
#Gini For Train
gini = ineq(predTrain[,2], type = "Gini")
gini
#Gini for Test
gini = ineq(predDT[,2], type="Gini")
gini
#RandomForest
install.packages("randomForest")
library(randomForest)
names(train.new)
#sum(is.na(train.new$`Family members`))
train.new =  train.new %>% select (-`deciles`)
test_bank_data =  test_bank_data %>% select (-`deciles`)
colnames(train.new) = c("Age", "Income", "ZipCode", "FamilyMembers", "CCAvg", "Education", "Mortgage", "SecutitiesAccount", "CDAccount", "Online", "CreditCard", "PersonalLoan")
colnames(test_bank_data) = c("Age", "Income", "ZipCode", "FamilyMembers", "CCAvg", "Education", "Mortgage", "SecutitiesAccount", "CDAccount", "Online", "CreditCard", "PersonalLoan")
RFmodel = randomForest(`PersonalLoan`~ ., data=train.new[,-12], mtry = 3, nodesize = 10, ntree = 100, importance = TRUE)
print(RFmodel)
names(test_bank_data)
view(test_bank_data)
RFmodel1 = randomForest(PersonalLoan ~ ., data=test_bank_data[,-12], mtry = 3, nodesize = 10, ntree = 100, importance = TRUE)
print(RFmodel1)

#Validate RF Model on Train Data
predRF1 = predict(RFmodel, newdata = train.new)
predRF1.round = round(predRF1, 0)
predRF1.round
table(train.new$PersonalLoan, predRF1.round)
mean(train.new$PersonalLoan==predRF1.round)

RFPredROC1 = ROCR::prediction(predRF1, train.new$PersonalLoan)
perf1 = performance(RFPredROC1,"tpr","fpr")
plot(perf1)
as.numeric(performance(RFPredROC1, "auc")@y.values)
train.new$deciles = decile(predRF)

#Validate RF Model on Test Data
predRF = predict(RFmodel, newdata = test_bank_data)
predRF.round = round(predRF, 0)
predRF.round
table(test_bank_data$PersonalLoan, predRF.round)
mean(test_bank_data$PersonalLoan==predRF.round)

RFPredROC = ROCR::prediction(predRF, test_bank_data$PersonalLoan)
perf = performance(RFPredROC,"tpr","fpr")
plot(perf)
as.numeric(performance(RFPredROC, "auc")@y.values)
test_bank_data$deciles = decile(predRF)

#KS on Train
install.packages("ineq")
library(ineq)

#KS on Train
KS = max(attr(perf1, 'y.values')[[1]]-attr(perf1, 'x.values')[[1]])
KS
#KS on Test
KS = max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS
#Gini For Train
gini = ineq(predRF1, type = "Gini")
gini
#Gini For Test
gini = ineq(predRF, type = "Gini")
gini
#Neural Network model
install.packages("neuralnet")
library(neuralnet)

names(train.new)
attach(train.new)
attach(test_bank_data)
#NN Built with Train data
NNmodel = neuralnet(formula = PersonalLoan ~ Age+Income+ZipCode+FamilyMembers+CCAvg+Education+Mortgage+SecutitiesAccount+CDAccount+Online+CreditCard, data = train.new, hidden = 2,err.fct = "sse",
                    linear.output = FALSE,
                    lifesign = "full",
                    lifesign.step = 10,
                    threshold = 0.01,
                    stepmax = 2000)
plot(NNmodel)

#NN built with Test data
NNmodel1 = neuralnet(formula = PersonalLoan ~ Age+Income+ZipCode+FamilyMembers+CCAvg+Education+Mortgage+SecutitiesAccount+CDAccount+Online+CreditCard, data = test_bank_data, hidden = 2,err.fct = "sse",
                    linear.output = FALSE,
                    lifesign = "full",
                    lifesign.step = 10,
                    threshold = 0.01,
                    stepmax = 2000)
plot(NNmodel1)
str(train.new)
names(test_bank_data)
#For Train data
predNN = neuralnet::compute(NNmodel, train.new)
predNN
predNN.round = round(predNN$net.result, 0)
predNN.round

table(train.new$PersonalLoan, predNN.round[,1])
mean(train.new$PersonalLoan==predNN.round)
library(ROCR)
NNpredROC = ROCR::prediction(predNN.round[,1], train.new$PersonalLoan)
perf = performance(NNpredROC, "tpr", "fpr")
plot(perf)
as.numeric(performance(NNpredROC, "auc")@y.values)
#For Test data
predNN1 = neuralnet::compute(NNmodel, test_bank_data)
predNN1
predNN1.round = round(predNN1$net.result, 0)
predNN1.round

table(test_bank_data$PersonalLoan, predNN1.round[,1])
mean(test_bank_data$PersonalLoan==predNN1.round)
library(ROCR)
NNpredROC1 = ROCR::prediction(predNN1.round[,1], test_bank_data$PersonalLoan)
perf1 = performance(NNpredROC1, "tpr", "fpr")
plot(perf1)
as.numeric(performance(NNpredROC, "auc")@y.values)
## deciling Train data
train.new$deciles <- decile(predNN.round[,1])

library(ineq)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## deciling Test data 
test_bank_data$deciles <- decile(predNN1.round[,1])

library(ineq)
KS <- max(attr(perf1, 'y.values')[[1]]-attr(perf1, 'x.values')[[1]])
KS
#Gini for Train data
gini = ineq(predNN.round[,1], type="Gini")
gini
#Gini for Test data
gini = ineq(predNN1.round[,1], type="Gini")
gini

