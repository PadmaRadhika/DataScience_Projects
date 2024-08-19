#=============================================================================
# Data Analysis - Cafe Great Transaction Data Set
#=============================================================================
#Environment Set up and Data Import
#Set up working Directory
setwd("C:/Users/Radhika/Desktop/R Programming/Project_MRA")
getwd()
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
#Read data from the given excel sheet
cafe_data <- read_excel("Cafe Great Transaction Data set.xlsx", sheet = "Sheet2")
attach(cafe_data)
str(cafe_data)
summary(cafe_data)
#Install the required packages for performing market basket analysis
install.packages("arules")
install.packages("arulesViz")
install.packages("datasets")
library(arules) 
library(arulesViz)
library(datasets) 
#Convert the data frame into Transactions
Agg.RTxn <- split(cafe_data$`Item Desc`,cafe_data$`Bill Number`)
Agg.RTxn2 = list()
for(i in 1:length(Agg.RTxn)){
  Agg.RTxn2[[i]]=unique(Agg.RTxn[[i]])}
Txns = as(Agg.RTxn2,"transactions")
Txns
inspect(Txns[10])

# Create an item frequency plot for the top 20 items
?itemFrequencyPlot
itemFrequencyPlot(Txns,topN=20,type="absolute")

# Get the rules 
rules <- apriori(Txns, parameter = list(supp = 0.0001, conf = 0.8)) 
rules

# Show the top 5 rules
summary(rules)
inspect(rules[1:5])

#The number of rules generated: 11 

# Sorting to get the most likely rules

rules<-sort(rules, by="confidence", decreasing=TRUE)
rules

# Top 5 relevant rules
inspect(rules[1:11])
