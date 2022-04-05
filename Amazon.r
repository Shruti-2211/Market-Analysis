#Step 1: Loading and Graphing Datasets
#Install required packages
library(forecast)
library(quantmod)
library(tseries)
library(timeSeries)
library(xts)

#variable plug-ins
stock.name = "AMZN"
start.date = '2017-01-01'
endifference.date = '2021-01-01'

#pull data from Yahoo finance
amazon.stock.data <- getSymbols(c(stock.name), src = 'yahoo', from = start.date, 
                         to = endifference.date, auto.assign = FALSE)

#View the data and analyze summary
View(amazon.stock.data)
summary(amazon.stock.data)
class(amazon.stock.data)
plot(amazon.stock.data)

#Find number of missing entries
sum(is.na(amazon.stock.data))

#Replace missing value 
amazon.stock.data$Open[is.na(amazon.stock.data$Open)] <- median(amazon.stock.data$Open, na.rm = TRUE)
amazon.stock.data$High[is.na(amazon.stock.data$High)] <- median(amazon.stock.data$High, na.rm = TRUE)
amazon.stock.data$Low[is.na(amazon.stock.data$Low)] <- median(amazon.stock.data$Low, na.rm = TRUE)
amazon.stock.data$Close[is.na(amazon.stock.data$Close)] <- median(amazon.stock.data$Close, na.rm = TRUE)

#Check for null value
any(is.na(amazon.stock.data))

#Chart Time Series
chartSeries(amazon.stock.data, theme = "white", name = c(stock.name)[1])
addBBands()

#Pull close price series at column4 of Yahoo Finance dataset
price = amazon.stock.data[,4]

#Step 2 : Decomposing Data into data components
#Decompose data
amazon.stock.data.ts <- ts(price, start = 2017-01-02, frequency = 120)
amazon.stock.data.de <- decompose(amazon.stock.data.ts)
plot(amazon.stock.data.de)

#Calculate data change value for every point
amazon.data.change <-  OpCl(amazon.stock.data)
head(amazon.data.change)
plot(amazon.data.change)

#Step 3 : Smoothing the Data
par(mfrow = c(2,2))

#Logarithmic Returns
log.price <- log(price)
plot(log.price, type = 'l', xlab = 'Time', ylab = 'Log(Price)', 
     main = 'Logarithmic Price Returns')

#Square root values
sqrt.price = sqrt(price)
plot(sqrt.price, type = 'l', xlab = 'Time', ylab = 'Sqrt(Price)', 
     main = 'Square Root Price Returns')

#Logarithmic Price Returns Difference 
difference.log.price <- diff(log(price), lag = 1)
difference.log.price <- difference.log.price[!is.na(difference.log.price)]
plot(difference.log.price, type = 'l', xlab = 'Time', ylab = 'Log(Price)', 
     main = 'Logarithmic Price Returns Difference')

#Square Root Price Returns Difference 
difference.sqrt.price <- diff(sqrt(price), lag = 1)
difference.sqrt.price <- difference.sqrt.price[!is.na(difference.sqrt.price)]
plot(difference.sqrt.price, type = 'l', xlab = 'Time', ylab = 'Sqrt(Price)', 
     main = 'Square Root Price Returns Difference')

#Step 4 : Performing the ADF (Augmented Dickey Fuller) Test
print(adf.test(log.price))
print(adf.test(sqrt.price))
print(adf.test(difference.log.price))
print(adf.test(difference.sqrt.price))

#Step 5: Creating Correlograms
par(mfrow = c(1,2))

#ACF and PACF for log data
acf(difference.log.price, main = 'ACF for Logarithmic Price Returns')
pacf(difference.log.price, main = 'PACF for Logarithmic Price Returns')

par(mfrow = c(1,2))

#ACF and PACF for square root data
acf(difference.sqrt.price, main = 'ACF for Square Root Price Returns')
pacf(difference.sqrt.price, main = 'PACF for Square Root Price Returns')

#Step 6 : Programming a Fitted Forecast

#Initialize real log returns via xts
real.return <- xts(0, as.Date("2020-11-25", "%Y-%m-%d"))
#Initialize forecasted returns via Dataframe
forecast.return <- data.frame(forecasted = numeric())
split = floor(nrow(difference.log.price) * (2.9/3))
for(s in split:(nrow(difference.log.price) - 1)){
  difference.log.price.training <- difference.log.price[1:s,]
  difference.log.price.testing <- difference.log.price[(s + 1) : nrow(difference.log.price), ]

  fit = arima(difference.log.price.training, order = c(2, 0, 2), include.mean = FALSE)
  summary(fit)
  
  arima.forecast = forecast(fit, h = 25)
  summary(arima.forecast)
  
  Box.test(fit$residuals, lag = 1, type = 'Ljung-Box')
  
  forecast.return = rbind(forecast.return, arima.forecast$mean[1])
  colnames(forecast.return) = c("Forecasted")
  return.series = difference.log.price[(s + 1), ]
  real.return = c(real.return, xts(return.series))
  rm(return.series)
}

real.return = real.return[-1]
forecast.return = xts(forecast.return, index(real.return))
plot(real.return, type = 'l', main = 'Actual Returns (Black) VS Forecast Returns (Red)')
lines(forecast.return, lwd = 2, col = 'red')

amazon.stock.comparison <- merge(real.return, forecast.return)
amazon.stock.comparison$Accuracy <- sign(amazon.stock.comparison$real.return) == 
                                      sign(amazon.stock.comparison$Forecasted)
View(amazon.stock.comparison)
Accuracy.percentage <- sum(amazon.stock.comparison$Accuracy == 1) * 100 / length(amazon.stock.comparison$Accuracy) 
print(Accuracy.percentage)

#-----------------------------------------------------------------------------------------------------------------------------------------------

#Step 5 : Programming a Fitted Forecast
difference.log.price.training <- difference.log.price[1:(0.9*length(difference.log.price))]  
difference.log.price.testing <- difference.log.price[(0.9*length(difference.log.price)+1) : length(difference.log.price)]  
auto.arima(difference.log.price.training)
fit <- arima(difference.log.price.training, order = c(2, 0, 2))
print(fit)
prediction <- predict(fit, n.ahead = (length(difference.log.price) - (0.9*length(difference.log.price))))$pred
print(prediction)
test.forecast <- forecast(fit, h = 25)
head(test.forecast,4)
plot(test.forecast, main = 'Arima Forecast for Amazon Stock')
accuracy(prediction, difference.log.price.testing)

#Lower the value of Root mean square error better is the accuracy of the model.

