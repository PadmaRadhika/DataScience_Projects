#=============================================================================
# Data Analysis - Credit Risk
#=============================================================================
#Environment Set up and Data Import
#Set up working Directory
setwd("C:/Users/Radhika/Desktop/R Programming/Project_FRA")
getwd()
#
rm(list = ls(all.names = TRUE))
#install.packages("readxl")
library(readxl)
#Read the input file
companydefault=read_excel("raw-data.xlsx", sheet = "raw data")
attach(companydefault)
summary(companydefault)
#Check for null values
sum(is.na(companydefault))
#Replace null values with 0
companydefault[is.na(companydefault)] <- 0
sum(is.na(companydefault))
summary(companydefault)
#Checking outliers
boxplot(`PBT as % of total income`)
boxplot(`Current ratio (times)`)
#check the data types of the variables
str(companydefault)
length(companydefault)
#Convert the character type variables to numeric type variables
companydefault$`Creditors turnover`[companydefault$`Creditors turnover`=="NA"]<-"0"
companydefault$`Creditors turnover` = as.numeric(companydefault$`Creditors turnover`)
companydefault$`Debtors turnover`[companydefault$`Debtors turnover`=="NA"]<-"0"
companydefault$`Debtors turnover` = as.numeric(companydefault$`Debtors turnover`)
companydefault$`Finished goods turnover`[companydefault$`Finished goods turnover`=="NA"]<-"0"
companydefault$`Finished goods turnover` = as.numeric(companydefault$`Finished goods turnover`)
companydefault$`WIP turnover`[companydefault$`WIP turnover`=="NA"]<-"0"
companydefault$`WIP turnover` = as.numeric(companydefault$`WIP turnover`)
companydefault$`Raw material turnover`[companydefault$`Raw material turnover`=="NA"]<-"0"
companydefault$`Raw material turnover` = as.numeric(companydefault$`Raw material turnover`)
companydefault$`Shares outstanding`[companydefault$`Shares outstanding`=="NA"]<-"0"
companydefault$`Shares outstanding` = as.numeric(companydefault$`Shares outstanding`)
companydefault$`PE on BSE`[companydefault$`PE on BSE`=="NA"]<-"0"
companydefault$`PE on BSE` = as.numeric(companydefault$`PE on BSE`)
companydefault$`Equity face value`[companydefault$`Equity face value`=="NA"]<-"0"
companydefault$`Equity face value` = as.numeric(companydefault$`Equity face value`)
str(companydefault)
#Univariate Analysis
#Converting Networth Next Year to be the dependant variable
companydefault$`Networth Next Year` = ifelse(companydefault$`Networth Next Year` > 0,0,1)
summary(companydefault$`Networth Next Year`)
names(companydefault)[names(companydefault) == "Networth Next Year"] <- "Default - 1"
names(companydefault)
#BiVariate Analysis
plot(`PBT as % of total income`, `Total income` )
#check the percentage of 0's and 1's
prop.table(table(companydefault$`Default - 1`))
#as per the result, % of negative networth data is only 6.8% of total data
#hence, we need to balance the data
#Creating new columns of Profitability ratio, Liquidity Ratio and CompanySize
ROE.Profitability <- companydefault$`Profit after tax`/companydefault$`Total assets`
leverageRatio <- companydefault$`Capital employed`/companydefault$`Total assets`
companySize <- log(companydefault$`Total assets`)
companydefault = cbind(companydefault, ROE.Profitability)
companydefault = cbind(companydefault, leverageRatio)
companydefault = cbind(companydefault, companySize)
str(companydefault)
length(companydefault)
#balance the data
#install.packages("DMwR")
library(DMwR)
attach(companydefault)
names(companydefault)
companydefault = as.data.frame(companydefault)
companydefault$`Default - 1` = as.factor(companydefault$`Default - 1`)
summary(companydefault$`Default - 1`)
str(companydefault$`Default - 1`)
default.smote = SMOTE(`Default - 1` ~., data=companydefault)
#companydefault$`Default - 1` = as.numeric(companydefault$`Default - 1`)
summary(companydefault$`Default - 1`)
prop.table(table(default.smote$`Default - 1`))

library(caTools)
library(caret)
library(ROCR)
library(corrplot)
library(car)
#checking for multicollinearity
#remove the number column and dependant variable column while
#checking multicollinearity
default.smote.cor = default.smote[,-1]
default.smote.cor = default.smote.cor[,-1]
length(default.smote.cor)
names(default.smote.cor)
#default.smote.cor$Debt.to.capital.leverage = round(default.smote.cor$Debt.to.capital.leverage, digits = 2)
#default.smote.cor = default.smote.cor[,-51]
str(default.smote.cor)

correlation=corrplot(cor(default.smote.cor),method = "number",type = "lower")
highlyCorrelated <- findCorrelation(cor(default.smote.cor), cutoff=0.7)
print(highlyCorrelated)
highlyCorCol <- colnames(default.smote.cor)[highlyCorrelated]
highlyCorCol
#Remove the highly correlated columns from the data set
newCompanyDefault <- default.smote[, -which(colnames(default.smote) %in% highlyCorCol)]
length(newCompanyDefault)
#remove the number column from the new data set 
#as it is not required for generating a model
newCompanyDefault = newCompanyDefault[,-1]
str(newCompanyDefault)
#Run the logistic regression model on the data
model=glm(newCompanyDefault$`Default - 1`~.,data = newCompanyDefault,family = binomial)
summary(model)
vif(model)
#removing columns of `Total capital`, `Shares outstanding` as
#their VIF values are > 4
#newCompanyDefault = newCompanyDefault[, -7]
#newCompanyDefault = newCompanyDefault[, -18]
names(newCompanyDefault)
#Run the logistic regression model on the new data after 
#removing columns of VIF values > 4
model1=glm(newCompanyDefault$`Default - 1`~ `Change in stock`+`Profit after tax`
           +`PAT as % of net worth`+`Other income`
           +`Total term liabilities / tangible net worth`+`Contingent liabilities / Net worth (%)`
           +`Contingent liabilities`+`Quick ratio (times)`+`Creditors turnover`
           +`Debtors turnover`+`Finished goods turnover`+`WIP turnover`
           +`Raw material turnover`+`Adjusted EPS`+`PE on BSE`+ROE.Profitability
           +leverageRatio+companySize,data = newCompanyDefault,family = binomial)
summary(model1)
vif(model1)
#Predit the train data model
predTrain=predict(model1, newdata = newCompanyDefault,type = "response")

#Creation of confusion matrix to assess model performance measures
tab.LR= table(newCompanyDefault$`Default - 1`,predTrain>0.3)
tab.LR
sum(diag(tab.LR))/sum(tab.LR)

tab.LR= table(newCompanyDefault$`Default - 1`,predTrain>0.2)
tab.LR
sum(diag(tab.LR))/sum(tab.LR)

tab.LR= table(newCompanyDefault$`Default - 1`,predTrain>0.5)
tab.LR
sum(diag(tab.LR))/sum(tab.LR)
#Preparing test data
#Creating new columns as in Raw data
testData=read_excel("validation_data.xlsx", sheet = "valdata")
attach(testData)
summary(testData)
sum(is.na(testData))
testData[is.na(testData)] <- 0
sum(is.na(testData))
summary(testData)
str(testData)
#Converting character type variables to numeric type variables
testData$`Creditors turnover`[testData$`Creditors turnover`=="NA"]<-"0"
testData$`Creditors turnover` = as.numeric(testData$`Creditors turnover`)
testData$`Debtors turnover`[testData$`Debtors turnover`=="NA"]<-"0"
testData$`Debtors turnover` = as.numeric(testData$`Debtors turnover`)
testData$`Finished goods turnover`[testData$`Finished goods turnover`=="NA"]<-"0"
testData$`Finished goods turnover` = as.numeric(testData$`Finished goods turnover`)
testData$`WIP turnover`[testData$`WIP turnover`=="NA"]<-"0"
testData$`WIP turnover` = as.numeric(testData$`WIP turnover`)
testData$`Raw material turnover`[testData$`Raw material turnover`=="NA"]<-"0"
testData$`Raw material turnover` = as.numeric(testData$`Raw material turnover`)
testData$`Shares outstanding`[testData$`Shares outstanding`=="NA"]<-"0"
testData$`Shares outstanding` = as.numeric(testData$`Shares outstanding`)
testData$`PE on BSE`[testData$`PE on BSE`=="NA"]<-"0"
testData$`PE on BSE` = as.numeric(testData$`PE on BSE`)
testData$`Equity face value`[testData$`Equity face value`=="NA"]<-"0"
testData$`Equity face value` = as.numeric(testData$`Equity face value`)
str(testData)
length(testData)
#Creating new ratios of Profitability, liverage and company size
ROE.Profitability <- testData$`Profit after tax`/testData$`Total assets`
leverageRatio <- testData$`Capital employed`/testData$`Total assets`
companySize <- log(testData$`Total assets`)
testData = cbind(testData, ROE.Profitability)
testData = cbind(testData, leverageRatio)
testData = cbind(testData, companySize)
str(testData)
length(testData)
#removing highly correlated columns from test data
newTestData <- testData[, -which(colnames(testData) %in% highlyCorCol)]
length(newTestData)
names(newTestData)
#removing number column from test data
newTestData = newTestData[,-1]
#Predicting test data with train model
predTest=predict(model1, newdata = newTestData,type = "response")
predTest
#Creation of confusion matrix to assess model performance measures
tab.LR1= table(newTestData$`Default - 1`,predTest>0.3)
tab.LR1
sum(diag(tab.LR1))/sum(tab.LR1)

tab.LR1= table(newTestData$`Default - 1`,predTest>0.2)
tab.LR1
sum(diag(tab.LR1))/sum(tab.LR1)

tab.LR1= table(newTestData$`Default - 1`,predTest>0.5)
tab.LR1
sum(diag(tab.LR1))/sum(tab.LR1)

library(ROCR)
ROCRpred = prediction(predTest, newTestData$`Default - 1`)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf)
#Plot ROC curve
plot(perf, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7))
#Sort the data in descending order based on probability of default 
#and then divide into 10 dociles based on probability & check 
#how well the model has performed
library(dplyr)
dim(newTestData)
decileData = newTestData
decileData
decileData = cbind(decileData,predTest)
df1 = mutate(decileData, quantile_rank = ntile(decileData$predTest,10))
str(df1)
View(df1[order(-df1$quantile_rank),])
