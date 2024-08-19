#=============================================================================
# Data Analysis - Gas Time Series
#=============================================================================
#Environment Set up and Data Import
#Set up working Directory
setwd("C:/Users/Radhika/Desktop/R Programming/Project_TimeSeries")
getwd()
#
#Import the required packages and install them
install.packages("tseries")
library('tseries')
install.packages("forecast")
library("forecast")
#read the data from gas object from forecast package
data("gas")
#Write the data into a CSV file and modify the csv file manually
#To show the yearwise data and read the data from CSV file into gasData object
write.csv(gas, file="gas.csv")
gasData = read.csv("gas.csv")
#Convert the data into Time Series data
gas_ts = ts(gasData[,3], start = c(1956,1), frequency = 12)
#Plot Time Series data
plot(gas_ts)
#Time series plot shows both trend and seasonality in the data
#Draw month plot to check the seasonality of data and to show the periodicity
monthplot(gas_ts)
#Decompose the data to calculate the seasonality component using smoothing
decompGas = stl(gas_ts, s.window = "p")
#Adjust data by removing the seasonality component
#And plot the sesaonality
deseasonal_demand=seasadj(decompGas)
plot(decompGas)
#Check whether the series is stationary or not using Augmented Dickey-Fuller Test
#H0: Data is not Stationary
#Ha: Data is Stationary
adf.test(gas_ts)
#As p-value: 0.2764 > 0.05, Null hypothesis is accepted
#That is Data is not stationary.
#
#Check the autocorrelation plots
acf(gas_ts, lag = 50)
#ACF plot shows the data is not stationary and there exists trend and seasonality
#between the lags
#Check PACF plot to see the correlation between the variable and its lags
pacf(gas_ts, lag = 50)
#As the PACF plot suggests, difference the series in order of 1
#to remove the trend and seasonality from data
count_d1 = diff(gas_ts, differences = 1)
count_d1
plot(count_d1)
#Plot shows that series is now having constant mean
#Test for Stationary 
#H0: Data is not Stationary
#Ha: Data is stationary
adf.test(count_d1)
#p-Value : 0.01 < 0.05, hence reject the null hypothesis and 
# Accept the alternate hypothesis, now the series is Stationary
#Draw the ACF and PACF plots to confirm the stationary series
acf(count_d1, main="ACF for Differenced Series")
pacf(count_d1, main="PACF for Differenced Series")
# As per the plots, there exists correlation between the lags and sesonality
#Divide the Stationary series into Train and Test Data
gasTrain = window(ts(deseasonal_demand[c(1:465)]))
gasTest= window(ts(deseasonal_demand), start=466)
#Use auto.arima function to automatically generate optimal (p,d,q) order
#To forecast the model.
fit<-auto.arima(gasTrain, seasonal=FALSE)
fit
#It suggests the order to be ARIMA(1,1,5) with drift
#To Evaluate the fitted model, Draw the ACF and PACF plots of model residuals
tsdisplay(residuals(fit), lag.max=45, main='Auto ARIMA Model Residuals')
fit1<-arima(gasTrain, order=c(1,1,5))
fit1
tsdisplay(residuals(fit1), lag.max=45, main='Auto ARIMA Model Residuals')
#
#Ljung box test to check whether the residuals are independant or not
#H0: Residuals are independent
#Ha: Residuals are not independent
library(stats)
Box.test(fit1$residuals)
#p-value: 0.9879 > 0.05, hence residuals are independant.
#Normality of the residuals
checkresiduals(fit1)
#Forecasting with ARIMA model
fcast <- forecast(fit1)
#plot the forecast model
plot(fcast)
#Test Accuracy of the forecast
accuracy(fcast, gasTest)
#High value of MAPE 53.71842 shows that the model is not adequate for future
#Predictions. Hence, the series is further needs to be evaluated to get
#the better model. Also, the accuracy test shows, the MAPE values of both
#Train and Test data are not matching. Hence, the model needs to be further
#evaluated.