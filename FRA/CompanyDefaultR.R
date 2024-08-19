#Loading relevant libraries for current session

library(caTools)   
library(car)
library(caret)
library(ROCR)
library(corrplot)
library(car)

setwd("~/Desktop/Data")

companydefault=read.csv("CompanyDefault.csv",header = TRUE)
View(companydefault)
#Checking for missing values and omitting for the purpose of this case study
sum(is.na(companydefault))

#checking the dimension and proportion of dependent variable
dim(companydefault)
attach(companydefault)
prop.table(table(companydefault$Default))

#Checking the structure of dataset
str(companydefault)

#Creating Default as factor type variable and dummy variables for categorical independent variables
companydefault$Default=as.factor(companydefault$Default)

#Partitioning data in train and test in 65:35 ratio
set.seed(123)
spl=sample.split(companydefault$Default,SplitRatio = 0.65)
train=subset(companydefault,spl==TRUE)
dim(train)
test=subset(companydefault,spl==FALSE)
dim(test)

#Checking distribution of dependent variable in train and test
prop.table(table(train$Default))
prop.table(table(test$Default))

#checking for multicollinearity
#cor(train[,-c(1,3)])
#correlation=corrplot(cor(train[,-c(1,3)]),method = "circle",type = "upper")

#building the base model with all the variables
model=glm(Default~Total_assets+PAT_as_._of_total_income+
            PBDITA_as_._of_total_income+PBT_as_._of_total_income+Cash_profit_as_._of_total_income+
            Current_ratio+Debt_to_equity_ratio,data = train,family = binomial)
summary(model)
vif(model)

model1=glm(Default~Total_assets+PAT_as_._of_total_income+
            PBDITA_as_._of_total_income+PBT_as_._of_total_income+Cash_profit_as_._of_total_income+
            Debt_to_equity_ratio,data = train,family = binomial)
summary(model1)

vif(model1)

#Removing variables that have very high VIF to remove multicolliniearity
model2=glm(Default~Total_assets+
             PBDITA_as_._of_total_income+Cash_profit_as_._of_total_income+
             Debt_to_equity_ratio,data = train,family = binomial)
summary(model2)

vif(model2)

#Removing either Cash_profitas_._of_total_income or PBDITA_as_._of_total_income makes other insignificant as well
model3=glm(Default~Total_assets+ Debt_to_equity_ratio,data = train,family = binomial)
summary(model3)
vif(model3)

#Prediction using test data for Model2
predTest=predict(model3, newdata = test,type = "response")


#Creation of confusion matrix to assess model performance measures
tab.LR= table(test$Default,predTest>0.3)
tab.LR
sum(diag(tab.LR))/sum(tab.LR)
library(ROCR)
ROCRpred = prediction(predTest, test$Default)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf)

plot(perf, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7)) 

