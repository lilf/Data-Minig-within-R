# Chapter3  Predicting Stock Market Returns
#objects:
#(1)use R to analyze data stored in a database 
#(2)handle prediction problems with a time ording(time series)
#(3)difficulties of translating model predicitons into decisions and actions in real-world applicaitons

# 3.1 Problem Description and Objectives
# efficient markets hypothes is that several researcher hold the opion that ther is no space to obtain profits in a consistent way.


#3.2 The Available Data
library(DMwR)
data(GSPC)
GSPC
head(GSPC)
dim(GSPC)
str(GSPC)

#3.2.1 Handling Time-Dependent Data in R
# each observation of our dataset has a time tag attached to it .   time series data
# a time series is a set of ordered observations of a variable Y   
# eg: y1, y2, ……, yt-1, yt, yt+1,……,yn
##目前处理与时间有关的数据有秷的包" zoo和xts
#从技术的角度来说，xts对象也是zoo对象
library(xts)
#POSIXct与POSIXlt表示的是日历中的日期与时间，精确到秒
?xts
#xts可用于由原始数据创建xts即时间序列的对象
x1 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01"), len = 100, by = "day"))
x2 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01"), len = 100, by = "day"))
x3 <- xts(rnorm(3), as.Date(c("2005-01-01",
                              "2005-01-10", "2005-01-12")))
#取数
x1[as.POSIXct("2000-01-04")]
x1["20000102"]
x1["2000-04"]
x1["2000-03-27/"]
#取从初始到结束点的数据
x1["2000-02-26/2000-03-03"]
#取到截止日期的所有数据
x1["/20000103"]
?POSIXct
?POSIXlt  
###########################################
#POSIXct与POSIXlt是Date-Time Class
###########################################
?zoo
###########################################
# zoo可处理irregular time series
###########################################

##########################################################
x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
x.Date
x <- zoo(rnorm(5), x.Date)
plot(x)
time(x)
##########################################################


#多维时间序列
mts.vals <- matrix(round(rnorm(25), 2), 5, 5)
#paste函数将名称进行组合
colnames(mts.vals) <- paste('ts', 1:5, sep = '')
mts <- xts(mts.vals, as.POSIXct(c('2003-01-01', '2003-01-04', '2003-01-05', '2003-01-06', '2003-02-16')))
mts
#访问某些行列的数据
#访问2003-01, ts2、ts5的数据
mts["2003-01", c("ts2", "ts5")]

# index()与time()可用于从xts对象中抽取出time tages的信息
# coredata()用于获取时间序列的数据值
index(mts)
time(mts)
coredata(mts)

#数据获取
#可从csv格式中读取或使用tseries包中的get.hist.quote()函数下载quotes加入到zoo中
install.packages("tseries")
library(tseries)
GSPC <- as.xts(get.hist.quote("^GSPC", start = "1970-01-02",
                              quote = c("Open", "High", "Low", "Close", "Volume", "AdjClose")))


?get.hist.quote
#get.hist.quote从www中下载指定的financial数据，返回的是一个zoo类型的对象
#可从 http://finance.yahoo.com/ 或 http://www.oanda.com/lang/cns/ 处下载
head(GSPC)

#另外一种从Web中获取quotes data的方法是，在quantmod中使用getSymbols()
install.packages("quantmod") 
library(quantmod)
?getSymbols
##########################################
# getSymbols是从多个来源load与manage数据
##########################################
getSymbols("^GSPC", src = "yahoo")
head(GSPC)
tail(GSPC)
# getSymbols("^GSPC", src = "yahoo", from = "1970-01-01", to = "2009-09-15")
colnames(GSPC) <- c("Open", "High", "Low", "Close", "Volume", "AdjClose")

?setSymbolLookup

setSymbolLookup(IBM = list(name = "IBM", src = "yahoo", USDEUR = list(name = 'USD/EUR', src = 'oanda')))
getSymbols(c('IBM', 'USDEUR'))
head(IBM)
head(USDEUR)
#setSymbolLookup() 在R位置会话中创建与管理Symbol的默认查找位置，用于getSymbols调用
#getSymbolLookup()
#saveSymbolLookup()
#loadSymbolLookup()用于不同R的session中保存与加载setting

#######################################
#从myswl中获取数据
#######################################
#window环境下
library(RODBC)
ch <- odbcConnect("QuotesDSN", uid = "myusername", pwd = "mypassword")
allQuotes <- sqlFetch(ch, "gspc")
GSPC <- xts(allQuotes[, -1], order.by = as.Date(allQuotes[, 1]))
head(GSPC)
odbcClose(ch)
#sqlFecth()与sqlFecthMore()

#linux环境下
library(DBI)
library(RMySQL)
drv <- dbDriver("MySQL")
ch <- dbConnect(drv, dbname = "Quotes", "myusername", "mypassword")
allQuotes <- dbGetQuery(ch, "select * from gspc")
GSPC <- xts(allQuotes[, -1], order.by = as.Date(allQuotes[, 1]))
head(GSPC)
dbDisconnect(ch)
dbUnloadDriver(drv)

setSymbolLookup(GSPC = list(name = 'gspc', src = 'mysql', 
                            db.fields = c('Index', 'Open', 'High', 'Low', 'Close', 'Volume', 'AdjClose'),
                            user = 'xpto', password = 'ypto', dbname = 'Quotes'))
getSymbols('GSPC')


#3.3 
# our goals: have a good forecasts of the future price of the S&P 500 index so that profitable orders can be placed on time
# questions: (1) which of the daily quotes (2)for which time in the future

#What to Predict
#当前的增副为p%，用模型来预测一下接下来的k天里的情况,动态获取
#正值买进，负值卖出
# mean(Pi) = (Ci + Hi + Li) / 3, Ci、Hi及Li分别代表第i天的close,high, low quotes 
# Vi是与今天相隔k天的的k百分比变动的均值的集合
# Vi = {(mean(P(i+j) - Ci)) / Ci}^k 
# indicator variable

?NROW
###########################################
# NROW处理的是vector或1-column的matrix
###########################################


T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
  v <- apply(HLC(quotes), 1, mean)
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for (x in 1:n.days)
    r[, x] <- Next(Delt(v, k = x), x)
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x == tgt.margin]))
  if (is.xts(quotes))
    xts(x, time(quotes))
  else
    x
}
?HLC
#HLC提前并转换OHLC时间序列
?Next
#Next将所有的Time Series向前推进一个
?Delt
#Delt计算Percent Change

last(GSPC, "3 months")
?candleChart
#candleChart创建Financial Charts
candleChart(last(GSPC, "3 months"), theme = "white", TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
?newTA
#newTA创建indicator或content用于画图
addT.ind <- newTA(FUN = T.ind, col = 'red', legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()

?ATR
# TR是High-Low-CLose序列的变动的一个度量标准
myATR <- function(x) ATR(HLC(x))[, "atr"]
?SMI
#SMI是一个momentum indicator，与每天的location相关
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
?ADX
#
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myAroon <- function(x) aroon(x[, c("High", "Low")])$oscillator
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High", "Low")]))[, 1]
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[, 2]
myMACD <- function(x) MACD(Cl(x))[, 2]
myMFI <- function(x) MFI(x[, c("High", "Low", "Close")], x[, "Volume"])
mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[, 1]


#首先使用数据来创建一个random forest，用于训练
head(GSPC)
data(GSPC)
install.packages("randomForest")
library(randomForest)
?specifyModel
#specifyModel创建一个可重用的model
data.model <- specifyModel(T.ind(GSPC) ~ Delt(CL(GSPC), k = 1:10)+
                             myATR(GSPC) + mySMT(GSPC) + myADX(GSPC) + myAroom(GSPC) +
                             myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) +
                             CMD(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
                             myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC))+
                             mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
set.seed(1234)
?buildModel
#创建一个model，并模型方法与quantmod对象关联起来
rf <- buildModel(data.model, method = 'randomForest',
                 training.per = c(start(GSPC), index(GSPC["1999-12-31"])), ntree = 50, importance = T)
?modelData
#modelData用于从一个quantmod对象中抽取出数据集，用于创建model
ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM), k = 1:3))
data <- modelData(ex.model, data.window = c("2009-01-01", "2009-08-10"))
head(data)

?myFavouriteModellingTool
m <- myFavouriteModellingTool(ex.model@model.formula, as.data.frame(data))

varImpPlot(rf@fitted.model, type = 1)

#importance获得的是对于每个变量的综合得分
imp <- importance(rf@fitted.model, type = 1)
#然后根据阈值过滤出将在模型中使用的变量的名字
rownames(imp)[which(imp > 10)]
#可获得最终的数据集
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC), k = 1) +
                             myATR(GSPC) + myADX(GSPC) + myEMV(GSPC) + myVolat(GSPC) + 
                             myMACD(GSPC) + mySAR(GSPC) + runMean(Cl(GSPC)))
#The Prediction Tasks
#Our real goal: predict the correct trading signal at any time t
# Two paths to obtain predictins for the correct trading signal
# first alternative one：将T作为一个目标变量，根据预测信息，尝试着从models中预测出这个value
#这是一个多元回归分析，需将predictions转换为signals
# book包中的trading.signals()函数可作于将数值型的T值转化为具有's'、'h'及'b'的因素型数据

#第二种预测方法：直接使用考虑使用the signals进行预测
#对于每天d,使用一个目标变量'correct'信号
#如何获取signals
#使用T indicator及相同的thresholds

#Tdata.train用于train阶段的数据
Tdata.train <- as.data.frame(modelData(data.model, data.window = c('197001-02', '1999-12-31')))
#Tdata.eval用于evaluation阶段的数据
#na.omit可用于data frame尾部的NAs的数据
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window = c('2000-01-01', '2009-09-15'))))
Tform <- as.formula('T.ind.GSPC ~ .')
#trading.signals()函数可用于生成signals


#Evaluation Criteria
