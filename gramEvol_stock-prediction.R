library(gramEvol)
library(quantmod)
library(tidyquant)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(TTR)
library(dplyr)
library(reshape2)
library(forecast)
library(Metrics)
#library(rvest)
#1 data import & analysis
#

#setSymbolLookup(QQQ='yahoo',SPY='google')

myStock = tq_get("CL", from = "2015-01-01",to = "2020-01-01", get= "stock.prices")
#myStock = myStock %>% tq_transmute(select = open:volume, mutate_fun = to.period, period = "weeks")

  #lineChart(BAC, up.col = "black", dn.col = "red", theme = "white")
  #addSMA(n = c(100))

# show price movement 
ggplot(data = myStock, aes(x= date, y = close)) +
  geom_line( aes(color = "black")) +
  geom_ma(ma_fun = SMA, n = 100, aes(color = "red")) + # Plot 100-day SMA
  geom_ma(ma_fun = SMA, n = 20, aes(color = "blue")) +
  ggtitle("Closing prices") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  scale_color_manual(labels = c("Price", "EMA20","EMA100"), values = c("black", "blue", "red")) +
  theme_bw()

# calculate returns 
myReturn = myStock %>%
tq_transmute(select = "close", mutate_fun = periodReturn, period = "daily", col_rename = "returns")


# and log
myLog = myStock %>%
  tq_transmute(select = "close", mutate_fun = periodReturn,type = "log", period = "daily", col_rename = "lg_returns")


# returns
gplt1 = ggplot(data = myReturn, aes(x = date, y = returns)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(x = "Date", y = "Returns") +
  ggtitle("Returns") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.3,0.3,0.05),labels = scales::percent) +
  #scale_fill_discrete(name="Returns",breaks=c("myReturn.retruns", "myLog.lg_returns"),labels=c("normal", "log")) +
  theme_bw()

gplt2 = ggplot(data = myLog, aes(x = date, y = lg_returns)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(x = "Date", y = "Lg - returns") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.3,0.3,0.05),labels = scales::percent) +
  #scale_fill_discrete(name="Returns",breaks=c("myReturn.retruns", "myLog.lg_returns"),labels=c("normal", "log")) +
  theme_bw()
grid.arrange(gplt1, gplt2, nrow=2)


# distribution of returns
gplt1 = ggplot(data = myLog,aes(x = lg_returns)) +
  geom_histogram(binwidth = 0.01) +
  labs(x = "Returns") +
  ggtitle("Return distributions") +
  scale_x_continuous(breaks = seq(-0.5,0.6,0.05),labels = scales::percent) +
  theme_bw()
gplt2 = ggplot(data = myReturn,aes(x = returns)) +
  geom_histogram(binwidth = 0.01) +
  labs(x = "lg - Returns") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(-0.5,0.6,0.05),labels = scales::percent) +
  theme_bw()
grid.arrange(gplt1, gplt2, nrow=2)


#check normality
#par(mfrow=c(2,1))
qqnorm(myReturn$returns)              
qqline(myReturn$returns)             
qqnorm(myLog$lg_returns)           
qqline(myLog$lg_returns)            

#Cumulative returns
plot(as.Date(myReturn$date),cumprod(1+ myReturn$returns),type="l",lwd=1, col="black",
     xlab="Date",ylab="Return",main="BAC - Cumulative product of weekly returns")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dashed")
abline(h=1, col="red", lty="dashed")


#decompose timeseries
myTs = ts(myStock$close, start = c(2015, as.numeric(format(myStock$date[1], "%j"))),frequency = 253)
#plot(decompose(ts(myStock$close,frequency = 253),type="multiplicative"))
plot(decompose(myTs,type="additive"))


#AFC
par(mfrow=c(3,1))
acf(myStock$close,lag.max=100)
acf(myLog$lg_returns,lag.max=20)
acf(myReturn$returns,lag.max=20)
par(mfrow=c(1,1))

########################GE

#init data
myDataTotal = myStock$close #define dataset

lagData = data.frame(x5=lag(myDataTotal,5), x4=lag(myDataTotal,4), x3=lag(myDataTotal,3), 
                     x2=lag(myDataTotal,2), x1=lag(myDataTotal,1), myDataTotal)
names(lagData) = c('x5','x4','x3',"x2","x1",'x')
lagData = lagData[-c(1:(length(lagData)-1)),]


nrows = length(lagData[,1])-round(length(lagData[,1])*0.10,0)
myDataFuture = lagData[-c(1:nrows),]
myDataTrain = lagData[c(1:nrows),]
  
#####

newRules = list(expr = grule(op(expr, expr), func(expr), var),
                 func = grule(sin, cos, log),
                 op = grule('+', '-', '*', '/', '^'),
                 var = grule(geData$x5, geData$x4,geData$x3, geData$x2, geData$x1)) 

newGram = CreateGrammar(newRules)


# Fitness function (RMSE)
newFitFunc = function(expr) {
  result = eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (rmse(geData$x , result))
}


geData = myDataTrain
ge = GrammaticalEvolution(newGram, newFitFunc, terminationCost = 0.001, iterations = 1000, max.depth = 3, disable.warnings=T)#, monitorFunc = print)
ge



# 
trainGe = eval(ge$best$expressions)

date = myStock$date[c(1:nrows)]
df1 = data.frame(date, myDataTrain$x, trainGe)
df2 = melt(df1, id.vars='date')
ggplot(data = df2, aes(x=date, y=value, col=variable)) +
  geom_line() +
  ggtitle("") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  theme_bw()

# Then look at the performance over the test data


geData = myDataTrain[length(myDataTrain[,1]-4),1:5]
predictions = c()

for(i in 1:round(length(lagData[,1])*0.10,0)){
  predictions[i] = eval(ge$best$expressions)
  for (cols in (1:(length(geData)-1))){
    geData[cols] = geData[cols+1]
  }
  geData[cols+1] = predictions[i]
}
#predictions

date = myStock$date[-c(1:(nrows+5))]
df1 = data.frame(date, myDataFuture$x, predictions)
df2 = melt(df1, id.vars='date')
ggplot(data = df2, aes(x=date, y=value, col=variable)) +
  geom_line() +
  ggtitle("Predictive outcome - GE") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  theme_bw()

ge1_rmse = rmse(predictions,myDataFuture$x)
########

myStockweekly = myStock %>% tq_transmute(select = open:volume, mutate_fun = to.period, period = "weeks")
myDataTotal = myStockweekly$close #define dataset

lagData = data.frame(x3=lag(myDataTotal,3), x2=lag(myDataTotal,2), x1=lag(myDataTotal,1), myDataTotal)
names(lagData) = c('x3',"x2","x1",'x')
lagData = lagData[-c(1:(length(lagData)-1)),]

nrows = length(lagData[,1])-round(length(lagData[,1])*0.10,0)
myDataFuture = lagData[-c(1:nrows),]
myDataTrain = lagData[c(1:nrows),]

newRules = list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos, sqrt, log),
                op = grule('+', '-', '*', '/', '^'),
                var = grule(geData$x3^n, geData$x2^n, geData$x1^n),
                n = grule(1,2)) #geData$x5^n, geData$x4^n, 
newGram = CreateGrammar(newRules)

geData = myDataTrain
ge = GrammaticalEvolution(newGram, newFitFunc, terminationCost = 0.001, iterations = 1000,disable.warnings=T) #,monitorFunc = print)
ge

# see matching of Ge-formula with actual data
trainGe = eval(ge$best$expressions)

date = myStockweekly$date[c(1:nrows)]
df1 = data.frame(date, myDataTrain$x, trainGe)
df2 = melt(df1, id.vars='date')
ggplot(data = df2, aes(x=date, y=value, col=variable)) +
  geom_line() +
  ggtitle("") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  theme_bw()

#test formula on unseen data
geData = myDataFuture
trainGe = eval(ge$best$expressions)

date = myStockweekly$date[-c(1:(nrows+3))]
df1 = data.frame(date, myDataFuture$x, trainGe)
df2 = melt(df1, id.vars='date')
ggplot(data = df2, aes(x=date, y=value, col=variable)) +
  geom_line() +
  ggtitle("") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  theme_bw()

#see predictive power
geData = myDataTrain[length(myDataTrain[,1]-2),1:3]
predictions = c()

for(i in 1:round(length(lagData[,1])*0.10,0)){
  predictions[i] = eval(ge$best$expressions)
  for (cols in (1:(length(geData)-1))){
    geData[cols] = geData[cols+1]
  }
  geData[cols+1] = predictions[i]
}

date = myStockweekly$date[-c(1:(nrows+3))]
df1 = data.frame(date, myDataFuture$x, predictions)
df2 = melt(df1, id.vars='date')
ggplot(data = df2, aes(x=date, y=value, col=variable)) +
  geom_line() +
  ggtitle("Predictive outcome - GE") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  theme_bw()

ge2_rmse = rmse(predictions,myDataFuture$x)

###############
# daily
myDataTotal = myStockweekly$close #define dataset

#apply SMAs
lagData = data.frame(x3=SMA(myDataTotal,20), x2=SMA(myDataTotal,10), x1=SMA(myDataTotal,5), myDataTotal)
names(lagData) = c('x3',"x2","x1",'x')
lagData = lagData[-c(1:19),]

nrows = length(lagData[,1])-round(length(lagData[,1])*0.10,0)
myDataFuture = lagData[-c(1:nrows),]
myDataTrain = lagData[c(1:nrows),]

newRules = list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos, sqrt, log),
                op = grule('+', '-', '*', '/', '^'),
                var = grule(geData$x3^n, geData$x2^n, geData$x1^n),
                n = grule(1,2)) #geData$x5^n, geData$x4^n, 
newGram = CreateGrammar(newRules)

geData = myDataTrain
ge = GrammaticalEvolution(newGram, newFitFunc, terminationCost = 0.001, iterations = 1000,disable.warnings=T) #,monitorFunc = print)
ge

trainGe = eval(ge$best$expressions)

date = myStockweekly$date[c(1:nrows)]
df1 = data.frame(date, myDataTrain$x, trainGe)
df2 = melt(df1, id.vars='date')
ggplot(data = df2, aes(x=date, y=value, col=variable)) +
  geom_line() +
  ggtitle("") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  theme_bw()

#test formula on unseen data
geData = myDataFuture
trainGe = eval(ge$best$expressions)

date = myStockweekly$date[-c(1:(nrows+19))]
df1 = data.frame(date, myDataFuture$x, trainGe)
df2 = melt(df1, id.vars='date')
ggplot(data = df2, aes(x=date, y=value, col=variable)) +
  geom_line() +
  ggtitle("") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.position = c(0.2, 0.1)) 

#see predictive power
geData = myDataTrain[length(myDataTrain[,1]-2),1:3]
predictions = c()

for(i in 1:round(length(lagData[,1])*0.10,0)){
  predictions[i] = eval(ge$best$expressions)
  for (cols in (1:(length(geData)-1))){
    geData[cols] = geData[cols+1]
  }
  geData[cols+1] = predictions[i]
}

date = myStockweekly$date[-c(1:(nrows+19))]
df1 = data.frame(date, myDataFuture$x, predictions)
df2 = melt(df1, id.vars='date')
ggplot(data = df2, aes(x=date, y=value, col=variable)) +
  geom_line() +
  ggtitle("Predictive outcome - GE") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.position = c(0.2, 0.1)) 

ge3_rmse = rmse(predictions,myDataFuture$x)
###############
#holt-winters 

myDataTotal = myStockweekly$close #define dataset
nrows = length(myDataTotal) - round(length(myDataTotal)*0.10,0)
myDataFuture = myDataTotal[-c(1:nrows)]
myDataTrain = myDataTotal[c(1:nrows)]

myTs = ts(myDataTrain, start = c(2015, as.numeric(format(myStock$date[1], "%j"))),frequency = 52)

hw = HoltWinters(myTs)
plot(hw)

hw_forecast = predict(hw, n.ahead = round(length(myDataTotal)*0.10,0), prediction.interval = F, level = 0.95)
delta = length(myDataTrain)-length(hw$fitted[,1]) 
date = myStockweekly$date[-c(1:delta)]
df1 = data.frame(date, myStockweekly$close[-c(1:delta)], c(hw$fitted[,1],hw_forecast))
df2 = melt(df1, id.vars='date')
ggplot(data = df2, aes(x=date, y=value, col=variable)) +
  geom_line() +
  geom_vline(xintercept = date[length(hw$fitted[,1])], linetype="dotted") +
  ggtitle("Closing prices") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Price") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.position = c(0.2, 0.15)) 


hw_rmse = rmse(myDataFuture, hw_forecast)

#auto.arima

model = auto.arima(myTs)

summary(model)
ar_forecast = forecast(model,round(length(myDataTotal)*0.10,0))
#plot(ar_forecast)
arima_rmse = rmse(myDataFuture, ar_forecast$mean)



RMSE = c(ge1_rmse,ge2_rmse,ge3_rmse,hw_rmse,arima_rmse)
names = c("daily prices - Ge","weekly prcies - Ge","daily SMA - GE","weekly prices - HW","weekly prices autoArima")
data.frame(names,RMSE)