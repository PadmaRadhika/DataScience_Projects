#=============================================================================
# Data Analysis - Golf
#=============================================================================
#Environment Set up and Data Import
#Set up working Directory
setwd("C:/Users/Radhika/Desktop/R Programming/Project_Module1")
getwd()
#
#Read the input file
golf = read.csv("Golf.csv")
attach(golf)
#Find the internal structure of the data
str(golf)
#Find the descriptive statistics of the data
summary(golf)
#Test if the two samples are normally distributed
#Draw a box plot for comparing Current Ball and New Ball samples
boxplot(CurrentBall, NewBall, horizontal = TRUE,names = c("Current Ball", "New Ball"), 
        main="Comparative box plot of New Ball and Current ball Data")
# As per the box plot, both the samples are independent of each other
# As per the box plot, the mean and median are almost same for both the groups
# Hence, we can assume that each group data is normally distributed
# Draw histogram for each group to visualize the distribution

# Draw historgram for Current Ball group
hist(CurrentBall)
# Draw historgram for New Ball group
hist(NewBall)
#As per the histograms, data seems to be normally distributed

#Check if the samples have same variance using F test
# Use var.test function to check the equality of variances.
var.test(CurrentBall, NewBall, alternative = "two.sided")
# Null Hypothesis, H0 = Variance of Current Ball = Variance of New Ball
# Alternate Hypothesis Ha = Variance of Current Ball != Variance of New Ball
#data:  CurrentBall and NewBall
#F = 0.78219, num df = 39, denom df = 39, p-value = 0.4465
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.413701 1.478906
#sample estimates:
#  ratio of variances 
#0.7821924 
# Since p-Value of 0.4465 is > 0.05, we fail to reject the null hypothesis
# Hence both the variances are equal.
#
#The assumptions of 2 sample t test are:
# a) The data continuous
# b) The data follow the normal distribution
# c) The variances of two samples are equal
# d) The two samples are independant
# e) Both the samples are simple random samples from their respective populations
#    meaning, each individual in the population has an equal probability of being
#    selected in the sample.
#
#With the above tests, we can say that the given data met the required assumptions
#of 2-Sample t test, hence, we use 2 sample t test to compae the driving distances
#of Current and New Ball data of golf data set.
#
# Calculate the p-value using 2 sample t test to compare the driving distances of
# Current and new golf ball data in the given data set
# Null Hypothesis H0= Mu New - Mu Curr = 0
# Alternate Hypothesis Ha = Mu New - Mu Curr != 0
t.test(CurrentBall, NewBall, alternative = "two.sided", mu=0, paired=FALSE, 
       var.equal=TRUE, conf.level = 0.95)
#
# Result:
#data:  CurrentBall and NewBall
#t = 1.3284, df = 78, p-value = 0.1879
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -1.383958  6.933958
#sample estimates:
# mean of x mean of y 
#270.275   267.500 
#
#Since p-value 0.1879 > 0.05, we fail to reject the null hypothesis
# Hence, we conclude that, there is no considerable difference between the two 
# means of data to prove that the new coating has effect on the driving distances
# of golf balls.
# 
#95 percent confidence interval for the difference between the means of the
#population is: -1.383958  6.933958
#
#Calculate the 95% confidence Interval for the population of Current Ball
t.test(CurrentBall)
#95 percent confidence interval:267.4757  273.0743
#
#Calculate the 95% confidence Interval for the population of New Ball
t.test(NewBall)
#95 percent confidence interval:264.3348  270.6652
#
#Calculate the Standard Deviation of Current Ball Data
sd(CurrentBall)
#Calculate the Standard Deviation of New Ball Data
sd(NewBall)
#Find the Standard Deviation of difference of the two means of the data
SD=sd(NewBall - CurrentBall)
SD
# Find the true difference in the means of two samples
delta = mean(CurrentBall) - mean(NewBall)
delta
#Calculate the power of t test with the default value of beta=0.1(probability of type II error)
power.t.test(power = 0.9, delta = delta, sd=9.343, sig.level = 0.05, type = "two.sample", 
             alternative = "two.sided" )
#
#Two-sample t test power calculation 
#
#n = 516.4575
#delta = 2.775
#sd = 13.74397
#sig.level = 0.05
#power = 0.9
#alternative = two.sided
#
# The power of the test is the probability that we make the right decision
# when the null is not correct.
#The above result shows that, we need a sample size of 517 (rounded to the next
# whole number) to make the right decision about the golf balls.
shapiro.test(CurrentBall)
shapiro.test(NewBall)
