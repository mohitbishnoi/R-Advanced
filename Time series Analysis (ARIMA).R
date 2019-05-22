#Time Series Analysis Using ARIMA Model
#Time Series is used to foreast a numeric variables based on date.
#Here dependent variable is numeric variables and independent variable is date

setwd("F:/my work/R Material/Time Series Analysis class/TIME SERIES ANALYSIS/NYBIRTH.txt")

skirtsseries<- read.table("SKIRTS.txt",skip=5)
skirtsseries
View(skirtsseries)
help(ts)

#There are few pre-requiste before apply ARIMA on data
#1. Transform data into Time series format - use ts()
#2. Data should have constant mean value  - use diff()
#3. There should not be any seasonal effect on data -  use decompose()

library(astsa)
#install.packages("forecast")
library(forecast) # load the "forecast" R library

#skirtsseries - it is a yearly data,
skirtsseries1<-as.matrix(skirtsseries)
skirtsseries1
plot(skirtsseries1)
mean(skirtsseries1)

skirtsseriesdiff1 <- diff(skirtsseries1, differences=1,na.rm=T)
skirtsseriesdiff1
plot.ts(skirtsseriesdiff1)
mean(skirtsseriesdiff1)

skirtsseriesdiff2 <- diff(skirtsseries1, differences=2,na.rm=T)
skirtsseriesdiff2

plot.ts(skirtsseriesdiff2)
mean(skirtsseriesdiff2)

#
kings<- read.table("KINGS.txt",skip=3)
kingstimeseries<- ts(kings)
View(kingstimeseries)

plot(kingstimeseries)
#or plot.ts(kingstimeseries)
kingstimeseries2<-diff(kingstimeseries,differences = 1)
plot.ts(kingstimeseries2)
mean(kingstimeseries2)

#Monthly data
births<-read.table("nybirth.txt")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
plot.ts(birthstimeseries)

births_decompose<-decompose(birthstimeseries)
plot(births_decompose)
#Decompose data into three part - trends, seasonal, randomness
birthstimeseriesseasonallyadjusted <- 
  birthstimeseries - births_decompose$seasonal

plot(birthstimeseriesseasonallyadjusted)


birthstimeseriesseasonallyadjustedDiff1<-diff(birthstimeseriesseasonallyadjusted,differences = 1)
plot.ts(birthstimeseriesseasonallyadjustedDiff1)


ts_data1<-birthstimeseriesseasonallyadjustedDiff1
ts_data1
plot.ts(ts_data1)


#Kings data - forecasting age of king

kingstimeseries1<-as.matrix(kingstimeseries)
plot(kingstimeseries1)
plot.ts(kingstimeseries1)
kingstimeseries2<-diff(kingstimeseries1,differences = 1)
plot.ts(kingstimeseries2)

#ARMA(p,d,q)
#correlogram acf
acf(kingstimeseries2,lag.max = 20) 
acf(kingstimeseries2,plot=FALSE,lag.max = 20)
#as number of points above dotted blue line is 1, q=1

#partial correlogram pacf
pacf(kingstimeseries2,lag.max=20)
#as number of points above dotted line is 3, p=3

#ARIMA model is combination of two models - AR and MA
#ARIMA model has three arguments (p,d,q)
#AR - auto regressive model (p,d) here q is assumed as 0
#MA - moving average model  (d,q) here p is assumed as 0

#Best model is whose sum of arguments is least.
#ARMA(3,1,1) - 5
#AR(3,1,0)   - 4
#MA(0,1,1)   - 2

# a model whose sum of argument is least will be selected
auto.arima(kingstimeseries1)

#ARIMA(p,d,q) where parameters p, d, and q are non-negative integers, 
#p is the order (number of time lags) of the autoregressive model, 
#d is the degree of differencing (the number of times the data have had past values subtracted),
#and q is the order of the moving-average model.

#X_t - mu = Z_t - (theta * Z_t-1), 
#where X_t is the stationary time series we are studying (the first differenced series of
#ages at death of English kings), mu is the mean of time series X_t, 
#Z_t is white noise with mean zero and constant variance, and 
#theta is a parameter that can be estimated.

kingstimeseriesarima <- arima(kingstimeseries1, order=c(0,1,1))
kingstimeseriesarima
arima(c(0,1,1))

auto.arima(kings)

kingstimeseriesforecasts <- predict(kingstimeseries1, 5)

pred<-kingstimeseriesforecasts$mean[1:5]
new<-c(kings,kingstimeseriesforecasts)

plot.forecast(kingstimeseriesforecasts)

#make a correlogram of the forecast errors for our ARIMA(0,1,1) model for the ages at
#death of kings, and perform the Ljung-Box test for lags 1-20
acf(kingstimeseriesforecasts$residuals, lag.max=20)
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")

plot.ts(kingstimeseriesforecasts$residuals) # make time plot of forecast errors



