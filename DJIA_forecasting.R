#Load libraries#
packages =  c("ggplot2", "dplyr", "tidyr", "data.table", 'corrplot', 'gridExtra', 'forecast', 'tseries', 'TSA', 'tibble', 'TTR', 'xts', 'dygraphs', 'assertthat')

my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}

#read the data#
DJIA <- read.csv("C:/Users/svech/Downloads/all_stocks_2006-01-01_to_2018-01-01.csv/all_stocks_2006-01-01_to_2018-01-01.csv")
summary(DJIA)
# NA's i.e) missing values observed # esp dates : 93426 / 93612 : need to impute
str(DJIA)
#date is listed as a factor, need to convert it to date structure

#clean the data#
DJIA[is.na(DJIA)] <- 0
DJIA$Date <- as.Date(DJIA$Date, format = "%Y-%m-%d")
summary(DJIA) # no more NA's

length(DJIA$Date)
str(DJIA)


#EDA
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
p1 = ggplot(DJIA, aes(Open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p2 = ggplot(DJIA, aes(High)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p3 = ggplot(DJIA, aes(Low)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p4 = ggplot(DJIA, aes(Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

library(gridExtra)
grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)

install.packages("dplyr")
library(dplyr)
counts <- DJIA %>% count(Name)
#filter data where High >100 
tmp <- filter(DJIA,High > 100)
length(unique(tmp$Name))
tmp$Name[1:5]
install.packages("assertthat")
library(assertthat)
sample_num = 5
assert_that(sample_num < length(unique(tmp$Name)))
sample_ticker <- as.character(sample(tmp$Name, sample_num))

is.ts(DJIA)
DJIATS <- as.ts(DJIA)

#Identify a sample of company interested in
sample_ticker <- c(sample_ticker, 'GOOGL') 
candidate_ticker <- unique(sample_ticker)
candidate_ticker <- c("IBM", "BA", "AAPL", "GS", "GOOGL")
candidate_num <- length(candidate_ticker)
stock_list <- vector(mode="list", length=candidate_num)
names(stock_list) <- candidate_ticker
i = 1
for (ticker in candidate_ticker){
  stock_list[[i]] <- filter(DJIA, Name == ticker)
  # print(stock_list[[i]])
  i <- i+1
  # print(ticker)
}
str(stock_list)

xts_list <- vector(mode="list", length=candidate_num)
ts_list <- vector(mode="list", length=candidate_num)

names(xts_list) = candidate_ticker
names(ts_list) = candidate_ticker

# use [[]] to access single element of a list in R, versus printing the entire thing , it may alternatively be used
#for partial matching as well #
install.packages("xts")
library(xts)
for (ticker in candidate_ticker){
  stock = stock_list[[ticker]]
  xts = xts(stock$Close, order.by=stock$Date)
  attr(xts, 'frequency') <- length(xts)/12
  ts = as.ts(xts, start = c(2006))
  xts_list[[ticker]] <- xts
  ts_list[[ticker]] <- ts
}
#the data.table below contains the values of 'High' for the 5 companies of interest/date#
xts_table= do.call(cbind, xts_list)
install.packages("dygraphs")
library(dygraphs)
dygraph(xts_table, xlab = "Time", ylab = "High value", main = "Time Series") %>%
  # dySeries(labels.default()) %>%
  # dyOptions(colors = c("red")) %>%
  dyRangeSelector()

#Time series use case : Google
#“Dickey-Fuller test” to determine stationarity
xts = xts_list[['GOOGL']]
ts = ts_list[['GOOGL']]
library(tseries)
adf.test(xts, alternative = "stationary", k = 0)

#Decomposing time series to separate trends and irregular components to get rid of biases due to seasonality (irregular)
tscomponents_add <- decompose(ts, type = "additive")
tscomponents_mul <- decompose(ts, type = "multiplicative")
plot(tscomponents_add, col = "red")
plot(tscomponents_mul, col = "blue")

#seasonality variation seems to be constant over time

#Use diffferncing to eliminate the effect of trend#
xtsdiff1 <- diff(xts, differences=1)
tsdiff1 <- diff(ts, differences=1)
plot.xts(xtsdiff1, col = "blue")

# check if differencing helps stationarize it#
adf.test(tsdiff1, alternative = "stationary", k = 0)

# Try to fit an ARIMA model ie) find p,q , d=1 (usually differencing over one lag suffices)
#Check correlations instead of covariances because they are not skewed for scale
install.packages("TSA")
library(TSA)
library(dplyr)
xtsdiff1[is.na(xtsdiff1)] <- 0
acf(xtsdiff1,lag.max = 60) 
pacf(xtsdiff1)

#Model trials3
AR1 <- arima(xtsdiff1,order = c(1,0,0))
plot(residuals(AR1))
AR1fit <- xtsdiff1 - residuals(AR1)

MA1 <- arima(xtsdiff1, order = c(0,0,1))
plot(residuals(MA1))
MA1fit <- xtsdiff1 - residuals(MA1)

drift <- arima(xtsdiff1,order=c(0,0,0))


#try autoarima - different combinations
install.packages("forecast")
library(forecast)
tsarima240 <- auto.arima(head(xts, -240), max.p = 3, max.q = 3, max.d = 3) 
print(tsarima240)
#forecasting using fit model
tsforecasts240 <- forecast(tsarima240, h = 240)
print(tsforecasts240)
xts_test <- tail(xts, 240)
write.csv(xts_test,file = "forecasts_orig")

#save forecasts to a tabl
write.csv(tsforecasts240,file = "forecasts_Google")
install.packages("ggfortify")
library(ggfortify)
autoplot(tsforecasts240)

accuracy(tsforecasts240, head(tail(xts, 240), 240))

ggplot(data.frame(residuals = tsforecasts240$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..),
      col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
