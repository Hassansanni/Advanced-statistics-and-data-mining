#Load necessary packages
library(readxl)
library(tidyverse)
library(forecast)
library(TTR)
#Load and preprocess data (Norway gdp data extracted from the larger dataset)
dataset <- read_excel("C:\\Users\\Fonis\\Documents\\timedata.xlsx")
View(dataset)
#Train-test split
train_time <- head(dataset$GDP, -5)
test <- tail(dataset$GDP, 5)
#create and plot a time series
gdpnorway <- ts(train_time, start = c(1973))
plot.ts(gdpnorway)
#smoothing using the moving average to estimate the trend
gdpnorway_sma5 <- SMA(gdpnorway, n = 5)
plot.ts(gdpnorway_sma5)
#it shows no seasonality
plot.ts(decompose(gdpnorway))
#due to lack of seasonality, we forecast using holtwinters
norwayhw <- HoltWinters(gdpnorway, gamma = FALSE)
plot(norwayhw)
#forecasting with specific values
# Set initial values for level and trend
l_start <- mean(gdpnorway)
b_start <- (tail(gdpnorway, 1) - head(gdpnorway, 1)) / length(gdpnorway)
# Fit Holt-Winters model with specified initial values
hw_model <- HoltWinters(gdpnorway, gamma = FALSE, l.start = l_start, b.start = b_start)
# Print the model
print(hw_model)
hw_model$SSE
plot(hw_model)
#forecasting
forecasting <- forecast(hw_model, h=5)
plot(forecasting)
print(forecasting)
test
#Diagnosing 
acf(forecasting$residuals, lag.max = 20, na.action = na.pass)
Box.test(na.omit(forecasting$residuals), lag= 20, type = "Ljung-Box")
plot.ts(forecasting$residuals)
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
forecasting$residuals <- forecasting$residuals[!is.na(forecasting$residuals)]
plotForecastErrors(forecasting$residuals)
#Trying Arima model because HoltWinter did not work
plot.ts(gdpnorway)
#it appears to be non-stationary, so I difference
gdpnorway1 <- diff(gdpnorway, differences = 2)
plot.ts(gdpnorway1)
#difference was done twice, hence d= 2, ext we confirm value of p and q
acf(gdpnorway1, lag.max=20) # plot a correlogram
acf(gdpnorway1, lag.max=20, plot=FALSE)
pacf(gdpnorway1, lag.max=20) # plot a partial correlogram
pacf(gdpnorway1, lag.max=20, plot=FALSE)
arima_model <- arima(gdpnorway1, order = c(1, 2, 0))
gdpnorway_forecast <- forecast(arima_model, h=5)
print(gdpnorway_forecast)
acf(gdpnorway_forecast$residuals, lag.max = 20)
Box.test(gdpnorway_forecast$residuals, lag = 20, type = "Ljung-Box")
# Plotting forecast errors for ARIMA model
plot.ts(gdpnorway_forecast$residuals)
plotForecastErrors(gdpnorway_forecast$residuals)
