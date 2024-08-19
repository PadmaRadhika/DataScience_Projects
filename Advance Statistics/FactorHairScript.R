#=============================================================================
# Data Analysis - Factor-Hair
#=============================================================================
#Environment Set up and Data Import
#Set up working Directory
setwd("C:/Users/Radhika/Desktop/R Programming/Project_AdvanceStatistics_Module2")
getwd()
#
#Read the input file
hair = read.csv("Factor-Hair-Revised.csv")
attach(hair)
#Find the internal structure of the data
str(hair)
#Find the descriptive statistics of the data
summary(hair)
#Find the column names of the data
names(hair)
#Column 1 is an ID column, coulumns 2 to 12 are the independant variables
#and column 13 is the dependant variable. Since we are going to find the 
#correlation between independant variables, Discard column 1 and 13.
hair1=hair[,2:12]
#Get the correlation matrix of all independant variables to test
#the multicollinearity between the independant variables.
matrix=cor(hair1)
#Display the correlation matrix
matrix
# Install packages "car" and "corrplot" to visualize the correlation between
# the variables diagramatically.
install.packages("car")
library(car)
#vif(m1)
install.packages("corrplot")
library(corrplot)
#Display the correlation diagram
corrplot(matrix, type="upper", method="number") 
#
#Load "psych" package to use bartlett test to check if there is a need
# for dimension reduction in the given data set.
library(psych)
# Bartlett test gives a p-value and if the p-value is < 0.05, it confirms the
# scope for dimension reduction in the data set.
cortest.bartlett(matrix, 100)
# The above test results in p-value of 1.79337e-96 which is < 0.05.
# Hence, we need to perform Factor analysis for reducing the factors to dimensions.
#
#Perform KMO Test to measure the Sampling adequacy.
# KMO Test Specifies if overall MSA > 0.5, the sample size is good enough
# for performing FActor Analysis.
KMO(matrix)
# KMO Test results in overall MSA = 0.65, hence, the given sample size is
# good enough for performing Factor Analsis.
#
# Find out the eigen values to identify the Principal factors which are 
# contributing the most.
eigenvector = eigen(matrix)
# Extract the eigen values and display them.
eigenValues = eigenvector$values
eigenValues
# The first four values of the eigen values are > 1 which are contributing the
# most to derive the customer satisfaction. As per the Kaiser rule, select these
# variables as the principal factors.
# Visualize the principal factors in the scree plot.
plot(eigenValues, xlab="Factors", ylab = "Eigen Values", col="red", pch=20)
lines(eigenValues, col="blue")
#
#Apply the Factor Analysis with the selected 4 principal factors and
#check the factor scores associated with each principal factor
fa1=fa(r=hair1, nfactors = 4, rotate="none", fm="pa")
print(fa1)
#Print the Factor Analysis diagram 
fa.diagram(fa1)
#
#As per the diagram, the factors doesn't seem to be mapped properly.
#Hence, rotate using varimax and check the factor analysis scores and diagram.
fa2=fa(r=hair1, nfactors = 4, rotate="varimax", fm="pa")
print(fa2)
#After rotation, the factors seem to be mapped to the principal factors
#properly with commonality between the factors.
fa.diagram(fa2)
# Get the attributes of the derived factors and print the scores associated
# with the new derived factors.
attributes(fa2)
fa2$scores
# Now bind the dependant variable customer satisfaction with the newly derived
# independant variables using their scores.
finalData = cbind(hair[,13], fa2$scores)
#Print the above matrix with headers. As the headers doesn't seem to be meaningful,
#assign some meaningful names to the derived factors.
head(finalData)
#Map the column names with meaningful names.
colnames(finalData) = c("Customer Satisfaction", "Customer Service", "Product Marketing", 
                        "Product Support", "Product Line Pricing")
head(finalData)
class(finalData)
#Since the final data is in matrix form, convert it into a data frame.
finalData = as.data.frame(finalData)
finalData
#Now, split the data into two sets, Train and Test to predict the model with new data.
#In this case, we build the model with Training data and predict the model with Test data.
install.packages("caTools")
library(caTools)
set.seed(300)
#Split the data into 70:30
spl = sample.split(finalData$`Customer Satisfaction`, SplitRatio = 0.7)
Train = subset(finalData, spl==T)
Test = subset(finalData, spl==F)
dim(Train)
dim(Test)
plot(Train[c(2,3,4,5)])
#Apply regression to the train data and check the R Square value.
m2 = lm(`Customer Satisfaction` ~., data = Train)
#
# Plot regression model
plot(m2, col="blue")
# Plot Histogram of residuals to check for normality 
hist(m2$residuals, col="blue")
summary(m2)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-1.52566 -0.48763  0.04769  0.43246  1.44382 
#
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             6.92249    0.07699  89.917  < 2e-16 ***
#  `Customer Service`      0.56394    0.07452   7.567 1.58e-10 ***
#  `Product Marketing`     0.67726    0.07606   8.904 6.44e-13 ***
#  `Product Support`       0.09038    0.08404   1.075    0.286    
#`Product Line Pricing`  0.55484    0.08576   6.470 1.40e-08 ***
 # ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.6454 on 66 degrees of freedom
#Multiple R-squared:  0.7366,	Adjusted R-squared:  0.7206 
#F-statistic: 46.13 on 4 and 66 DF,  p-value: < 2.2e-16
#
#The above result shows that the adjusted R squared value is 0.7206
#Which says, there is 72.06% variation present in Customer Satisfation dependant variable
#by the four independant variables. Also, the p-values of 3 independant variables
#is much less than 0.05 meaning that the three independant variables are highly significant.
#Hence, the regression model is robust and statistically significant.
#
#variance inflation factor (or VIF), which measures how much the variance of a 
#regression coefficient is inflated due to multicollinearity in the model.
vif(m2)
#`Customer Service`    `Product Marketing`  `Product Maintenance` `Product Line Pricing` 
#1.016756               1.025960               1.033948               1.028339 
#In the above result, the VIF values of regression coefficients are less than 5 which
#shows the coefficients are not multicollinear.
#
#Predict the model with test data by calculating R-squared value
pred = predict(m2, newdata = Test)
#
#SST
SST = sum((Test$`Customer Satisfaction` - mean(Train$`Customer Satisfaction`))^2)
SST
#SSR
SSE = sum((pred - Test$`Customer Satisfaction`)^2)
SSE
#SSR
SSR = sum((pred - mean(Train$`Customer Satisfaction`))^2)
SSR
SSR/SST
#The above result gives an R Square value of 0.6080142, which is in the range of +/-15 of 
#Training R-Squared value.
#Hence, this model is statistically significant.
