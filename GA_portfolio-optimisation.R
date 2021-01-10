library("quantmod")
#library("ggplot2")
library("GA")
library("rvest")
#library("ggpubr")
library("tidyquant")
library("fPortfolio")
library("PerformanceAnalytics")
library("dplyr")
library("xts")

max_iteration = 100
#get stock inf of DOW stocks
getStocks = tq_index("DOW") %>% as.data.frame()
getStocks = subset(getStocks,symbol!="DOW")  # delete rows with no stock data for period --> "DOW"
myStocks = gsub(pattern = '\\.', '-', getStocks$symbol) # change: BRK.B -> BRK-B (yahoo uses '-')
orgWeights = getStocks$weight/sum(getStocks$weight) # get initial weightings - with rebalancing

#myStocks = c("MMM","ABT","ABBV","ABMD","ACN","ATVI","ADBE","AMD","AAP","AES")

stock_data = function(StartP,EndP) {
Stocks = lapply(myStocks, function(sym) {
  weeklyReturn(na.omit(getSymbols(sym, from=StartP, to=EndP, auto.assign=FALSE)))
})
return(Stocks)
}

Stocks = stock_data(StartP = "2016-01-01",EndP = Sys.Date())
myDataTotal = do.call(merge.xts, Stocks)
myDataTotal = as.data.frame(myDataTotal, col.names = myStocks); names(myDataTotal) = c(myStocks) 
myDataTotal = myDataTotal[sapply(myDataTotal, function(myDataTotal) !any(is.na(myDataTotal)))]

#create two datasets: train and test, define ratio
nrows = length(myDataTotal[,1])-round(length(myDataTotal[,1])*0.25,0)
myDataFuture = myDataTotal[-c(1:nrows),]
myData = myDataTotal[c(1:nrows),]



#myData+1

#ggdensity(myData$ABBV)
#ggqqplot(myData$ABBV)
#shapiro.test(myData$ABBV) 

# really slow for GA
# portReturns = function(x,myData) {
#   myReturns = as.vector(Return.portfolio(myData, weights = x))
#   return(myReturns)
# }

################################################################
################################################################



#sum(portReturns(x=x,myData))

# portReturns = function(x,myData) {
#   myReturns = 0
#   for (i in 2:length(x)) {
#     myReturns = myReturns + t(x) %*% myData[,i]
#   }
#   return(myReturns)
# }

portReturns = function(x,myData) {
  myReturns = 0
  for (i in 1:length(x)) {
    myReturns = myReturns + myData[,i] * x[i]
  }
  return(myReturns)
}

portRisk = function(x,myData) {
    myRisk = sqrt(t(x) %*% cov(myData) %*% x)
  return(myRisk)
}


sharpe = function(x) {
  myReturns = portReturns(x,myData = myData)
  myRisk = portRisk(x,myData = myData)
  return(sum((myReturns/myRisk)))
}

# sharpe optimisation
objSharpe = function(x) {
    x = x/sum(x)
    return(sharpe(x=x))
    #return (-sharpe(x=x) + 100*(sum(x != 0.00)/length(x))) #penalizing larger portfolios
}

GA = ga(type="real-valued", fitness = objSharpe ,lower = rep(0,ncol(myData)), upper = rep(1,ncol(myData)), 
        popSize=150,pmutation=1/ncol(myData),pcrossover = 0.8,maxiter = max_iteration, run=200,
        parallel=T, monitor=T, seed = 1)

#plot(GA)
summary(GA)
sum(summary(GA)$solution)
normalWeights = summary(GA)$solution/sum(summary(GA)$solution)
optiWeights = as.vector(normalWeights)

#as.data.frame(cbind(names(myData),optiWeights)) # names()= c("Ticker","Optimal Weights") 

optiReturns = 1 + portReturns(optiWeights,myData = myData)

orgReturns = 1 + portReturns(orgWeights,myData = myData)

balWeights = rep(1/length(myData),ncol(myData))
balReturns = 1 + portReturns(balWeights,myData = myData)

# create randomised portfolios
randReturnsFunc = function(x,myData) {
  myRands = list()
  for (i in 1:x){
    xRand = runif(length(myData), min=0, max=1)
    xRand = xRand/sum(xRand)
    myRands[[i]] = 1 + portReturns(x = xRand, myData = myData)
  }
  return(myRands)
}
#randReturns = randReturnsFunc(x=1000,myData=myData)

#myColor = terrain.colors(100) #myColor[i]

plot(as.Date(rownames(myData)),cumprod(optiReturns),type="l",lwd=2, col="black",
     xlab="Return",ylab="Date",main="Return of different portfolios, train data")
for(i in 1:length(randReturnsFunc(x=100,myData=myData))) lines(as.Date(rownames(myData)),
                                                                cumprod(randReturnsFunc(x=100,myData=myData)[[i]]),
                                                                col = "lightgrey", lwd=0.5, lty  = "dashed")
lines(as.Date(rownames(myData)),cumprod(orgReturns),type="l",lwd=2, col="red")
lines(as.Date(rownames(myData)),cumprod(balReturns),type="l",lwd=2, col="darkblue")
legend("topleft",inset = .05,legend = c("Sharpe","Index","Balanced","Random"),
       text.col = c("black","red","darkblue","grey"),cex=0.75)


plot(as.Date(rownames(myDataFuture)),cumprod(1 + portReturns(optiWeights,myData = myDataFuture)),type="l",lwd=2, col="black",
     xlab="Return",ylab="Date",main="Return of different portfolios, test data", ylim= c(0.95,1.25))
for(i in 1:length(randReturnsFunc(x=100,myData=myDataFuture))) lines(as.Date(rownames(myDataFuture)),
                                                                cumprod(randReturnsFunc(x=100,myData=myDataFuture)[[i]]),
                                                                col = "lightgrey", lwd=0.5, lty  = "dashed")
lines(as.Date(rownames(myDataFuture)),cumprod(1 + portReturns(orgWeights,myData = myDataFuture)),type="l",lwd=2, col="red")
lines(as.Date(rownames(myDataFuture)),cumprod(1 + portReturns(balWeights,myData = myDataFuture)),type="l",lwd=2, col="darkblue")
legend("topleft",inset = .05,legend = c("Sharpe","Index","Balanced","Random"),
       text.col = c("black","red","darkblue","grey"),cex=0.75)

############################### building efficent frontier
##### min var optimisation
objMin = function(x) {
  x = x/sum(x)
  return(sum(portRisk(x=x,myData = myData)))
  #return (-sharpe(x=x) + 100*(sum(x != 0.00)/length(x))) #penalizing larger portfolios
}

GA = ga(type="real-valued", function(x)-objMin(x),lower = rep(0,ncol(myData)), upper = rep(1,ncol(myData)), 
        popSize=150,pmutation=1/ncol(myData),pcrossover = 0.8,maxiter = max_iteration, run=200,
        parallel=T, monitor=T, seed = 1)
summary(GA)

normalWeights = summary(GA)$solution/sum(summary(GA)$solution)
minWeights = as.vector(normalWeights)
minReturns = 1 + portReturns(minWeights,myData = myData)


##### max return optimisation
objMax = function(x) {
  x = x/sum(x)
  return(sum(portReturns(x=x,myData = myData)))
  #return (-sharpe(x=x) + 100*(sum(x != 0.00)/length(x))) #penalizing larger portfolios
}

GA = ga(type="real-valued", fitness = objMax ,lower = rep(0,ncol(myData)), upper = rep(1,ncol(myData)), 
        popSize=150,pmutation=1/ncol(myData),pcrossover = 0.8,maxiter = max_iteration, run=200,
        parallel=T, monitor=T, seed = 1)
summary(GA)

normalWeights = summary(GA)$solution/sum(summary(GA)$solution)
maxWeights = as.vector(normalWeights)
maxReturns = 1 + portReturns(maxWeights,myData = myData)



# plot different portfolios 
plot(as.Date(rownames(myData)),cumprod(optiReturns),type="l",lwd=2, col="black",
     xlab="Return",ylab="Date",main="Return of different portfolios, train data")
lines(as.Date(rownames(myData)),cumprod(orgReturns),type="l",lwd=2, col="red")
lines(as.Date(rownames(myData)),cumprod(balReturns),type="l",lwd=2, col="darkblue")
lines(as.Date(rownames(myData)),cumprod(minReturns),type="l",lwd=2, col="green")
lines(as.Date(rownames(myData)),cumprod(maxReturns),type="l",lwd=2, col="orange")
legend("topleft",inset = .05,legend = c("Sharpe","Index","Balanced","MinVar","MaxReturn"),
       text.col = c("black","red","darkblue","green","orange"),cex=0.75)

# test dataset
plot(as.Date(rownames(myDataFuture)),cumprod(1 + portReturns(optiWeights,myData = myDataFuture)),type="l",lwd=2, col="black",
     xlab="Return",ylab="Date",main="Return of different portfolios, test data")
lines(as.Date(rownames(myDataFuture)),cumprod(1 + portReturns(orgWeights,myData = myDataFuture)),type="l",lwd=2, col="red")
lines(as.Date(rownames(myDataFuture)),cumprod(1 + portReturns(balWeights,myData = myDataFuture)),type="l",lwd=2, col="darkblue")
lines(as.Date(rownames(myDataFuture)),cumprod(1 + portReturns(minWeights,myData = myDataFuture)),type="l",lwd=2, col="green")
lines(as.Date(rownames(myDataFuture)),cumprod(1 + portReturns(maxWeights,myData = myDataFuture)),type="l",lwd=2, col="orange")
legend("topleft",inset = .05,legend = c("Sharpe","Index","Balanced","MinVar","MaxReturn"),
       text.col = c("black","red","darkblue","green","orange"),cex=0.75)

# total dataset
plot(as.Date(rownames(myDataTotal)),cumprod(1 + portReturns(optiWeights,myData = myDataTotal)),type="l",lwd=2, col="black",
     xlab="Return",ylab="Date",main="Return of different portfolios, total data")
lines(as.Date(rownames(myDataTotal)),cumprod(1 + portReturns(orgWeights,myData = myDataTotal)),type="l",lwd=2, col="red")
lines(as.Date(rownames(myDataTotal)),cumprod(1 + portReturns(balWeights,myData = myDataTotal)),type="l",lwd=2, col="darkblue")
lines(as.Date(rownames(myDataTotal)),cumprod(1 + portReturns(minWeights,myData = myDataTotal)),type="l",lwd=2, col="green")
lines(as.Date(rownames(myDataTotal)),cumprod(1 + portReturns(maxWeights,myData = myDataTotal)),type="l",lwd=2, col="orange")
legend("topleft",inset = .05,legend = c("Sharpe","Index","Balanced","MinVar","MaxReturn"),
       text.col = c("black","red","darkblue","green","orange"),cex=0.75)


## sharpe  with risk seeking values: lambda = 4
portRisk = function(x,myData) {
  myRisk = sqrt(t(x) %*% cov(myData) %*% x)
  return(myRisk)
}

GAseeking = ga(type="real-valued", fitness = objSharpe ,lower = rep(0,ncol(myData)), upper = rep(1,ncol(myData)), 
        popSize=150,pmutation=1/ncol(myData),pcrossover = 0.8,maxiter = max_iteration, run=200,
        parallel=T, monitor=T, seed = 1)

summary(GAseeking)

normalWeights = summary(GAseeking)$solution/sum(summary(GAseeking)$solution)
seekingWeights = as.vector(normalWeights)
seekingReturns = 1 + portReturns(maxWeights,myData = myData)



## sharpe  with risk averse values: lambda = 1/4
objSharpeAverse = function(x) {
  x = x/sum(x)
  return(sharpe(x=x,lambda = 1/500))
  #return (-sharpe(x=x) + 100*(sum(x != 0.00)/length(x))) #penalizing larger portfolios
}

GAaverse = ga(type="real-valued", fitness = objSharpeAverse ,lower = rep(0,ncol(myData)), upper = rep(1,ncol(myData)), 
        popSize=150,pmutation=1/ncol(myData),pcrossover = 0.8,maxiter = max_iteration, run=200,
        parallel=T, monitor=T, seed = 1)
summary(GAaverse)

normalWeights = summary(GAaverse)$solution/sum(summary(GAaverse)$solution)
averseWeights = as.vector(normalWeights)
averseReturns = 1 + portReturns(maxWeights,myData = myData)


# calculate & plot efficient frontier
PlotEff = function(myDataSource){
  effFrontier = portfolioFrontier(as.timeSeries(myDataSource), constraints = "LongOnly")
  plot(effFrontier,c(1)) # c(1,2,5)
  points(x=portRisk(optiWeights,myData = myDataSource)/length(portRisk(optiWeights,myData = myDataSource)),
         y=sum(portReturns(optiWeights,myData = myDataSource)/length(portReturns(optiWeights,myData = myDataSource))),
         col="black",pch=8,cex=1)
  points(x=portRisk(orgWeights,myData = myDataSource)/length(portRisk(orgWeights,myData = myDataSource)),
         y=sum(portReturns(orgWeights,myData = myDataSource)/length(portReturns(orgWeights,myData = myDataSource))),
         col="red",pch=16,cex=1)
  points(x=portRisk(balWeights,myData = myDataSource)/length(portRisk(balWeights,myData = myDataSource)),
         y=sum(portReturns(balWeights,myData = myDataSource)/length(portReturns(balWeights,myData = myDataSource))),
         col="darkblue",pch=16,cex=1)
  points(x=portRisk(minWeights,myData = myDataSource)/length(portRisk(minWeights,myData = myDataSource)),
         y=sum(portReturns(minWeights,myData = myDataSource)/length(portReturns(minWeights,myData = myDataSource))),
         col="green",pch=16,cex=1)
  points(x=portRisk(maxWeights,myData = myDataSource)/length(portRisk(maxWeights,myData = myDataSource)),
         y=sum(portReturns(maxWeights,myData = myDataSource)/length(portReturns(maxWeights,myData = myDataSource))),
         col="orange",pch=16,cex=1)
  points(x=portRisk(seekingWeights,myData = myDataSource)/length(portRisk(seekingWeights,myData = myDataSource)),
         y=sum(portReturns(seekingWeights,myData = myDataSource)/length(portReturns(seekingWeights,myData = myDataSource))),
         col="salmon",pch=16,cex=1)
  points(x=portRisk(averseWeights,myData = myDataSource)/length(portRisk(averseWeights,myData = myDataSource)),
         y=sum(portReturns(averseWeights,myData = myDataSource)/length(portReturns(averseWeights,myData = myDataSource))),
         col="maroon",pch=16,cex=1)
  legend("right",title = deparse(substitute(myDataSource)), legend = c("Sharpe","Index","Balanced","MinVar","MaxReturn","seekingRisk","averseRisk"),
         bty = "n", lwd = 1, cex = 0.75, col = c("black","red","darkblue","green","orange","salmon","maroon"), lty = c(rep(NA,7)), pch = c(8, rep(16,6)))
}
#select dataset
PlotEff(myDataSource = myData)
PlotEff(myDataSource = myDataFuture)


# train & test datasets
DataFrameEff = function(myDataSource){
  Riskmeasures = c(portRisk(optiWeights,myData = myDataSource)/length(portRisk(optiWeights,myData = myDataSource)),
                   portRisk(orgWeights,myData = myDataSource)/length(portRisk(orgWeights,myData = myDataSource)),
                   portRisk(balWeights,myData = myDataSource)/length(portRisk(balWeights,myData = myDataSource)),
                   portRisk(minWeights,myData = myDataSource)/length(portRisk(minWeights,myData = myDataSource)),
                   portRisk(maxWeights,myData = myDataSource)/length(portRisk(maxWeights,myData = myDataSource)),
                   portRisk(seekingWeights,myData = myDataSource)/length(portRisk(seekingWeights,myData = myDataSource)),
                   portRisk(averseWeights,myData = myDataSource)/length(portRisk(averseWeights,myData = myDataSource)))
  Returnmeasures = c(sum(portReturns(optiWeights,myData = myDataSource)/length(portReturns(optiWeights,myData = myDataSource))),
                     sum(portReturns(orgWeights,myData = myDataSource)/length(portReturns(orgWeights,myData = myDataSource))),
                     sum(portReturns(balWeights,myData = myDataSource)/length(portReturns(balWeights,myData = myDataSource))),
                     sum(portReturns(minWeights,myData = myDataSource)/length(portReturns(minWeights,myData = myDataSource))),
                     sum(portReturns(maxWeights,myData = myDataSource)/length(portReturns(maxWeights,myData = myDataSource))),
                     sum(portReturns(seekingWeights,myData = myDataSource)/length(portReturns(seekingWeights,myData = myDataSource))),
                     sum(portReturns(averseWeights,myData = myDataSource)/length(portReturns(averseWeights,myData = myDataSource))))
                     
  data.frame("PF" = c("Sharpe","Index","Balanced","MinVar","MaxReturn","seekingRisk","averseRisk"), 
             "Risk" = Riskmeasures, "Return" = Returnmeasures, stringsAsFactors = FALSE)
}

DataFrameEff(myDataSource = myData) #train data
DataFrameEff(myDataSource = myDataFuture) #test data


######

#rm(list=ls())
