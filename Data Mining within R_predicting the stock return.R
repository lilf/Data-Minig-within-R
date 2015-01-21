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
# eg: y1, y2, ����, yt-1, yt, yt+1,����,yn
##Ŀǰ������ʱ���йص������ж��İ�" zoo��xts
#�Ӽ����ĽǶ���˵��xts����Ҳ��zoo����
library(xts)
#POSIXct��POSIXlt��ʾ���������е�������ʱ�䣬��ȷ����
?xts
#xts��������ԭʼ���ݴ���xts��ʱ�����еĶ���
x1 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01"), len = 100, by = "day"))
x2 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01"), len = 100, by = "day"))
x3 <- xts(rnorm(3), as.Date(c("2005-01-01",
                              "2005-01-10", "2005-01-12")))
#ȡ��
x1[as.POSIXct("2000-01-04")]
x1["20000102"]
x1["2000-04"]
x1["2000-03-27/"]
#ȡ�ӳ�ʼ�������������
x1["2000-02-26/2000-03-03"]
#ȡ����ֹ���ڵ���������
x1["/20000103"]
?POSIXct
?POSIXlt  
###########################################
#POSIXct��POSIXlt��Date-Time Class
###########################################
?zoo
###########################################
# zoo�ɴ���irregular time series
###########################################

##########################################################
x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
x.Date
x <- zoo(rnorm(5), x.Date)
plot(x)
time(x)
##########################################################


#��άʱ������
mts.vals <- matrix(round(rnorm(25), 2), 5, 5)
#paste���������ƽ������
colnames(mts.vals) <- paste('ts', 1:5, sep = '')
mts <- xts(mts.vals, as.POSIXct(c('2003-01-01', '2003-01-04', '2003-01-05', '2003-01-06', '2003-02-16')))
mts
#����ĳЩ���е�����
#����2003-01, ts2��ts5������
mts["2003-01", c("ts2", "ts5")]

# index()��time()�����ڴ�xts�����г�ȡ��time tages����Ϣ
# coredata()���ڻ�ȡʱ�����е�����ֵ
index(mts)
time(mts)
coredata(mts)

#���ݻ�ȡ
#�ɴ�csv��ʽ�ж�ȡ��ʹ��tseries���е�get.hist.quote()��������quotes���뵽zoo��
install.packages("tseries")
library(tseries)
GSPC <- as.xts(get.hist.quote("^GSPC", start = "1970-01-02",
                              quote = c("Open", "High", "Low", "Close", "Volume", "AdjClose")))


?get.hist.quote
#get.hist.quote��www������ָ����financial���ݣ����ص���һ��zoo���͵Ķ���
#�ɴ� http://finance.yahoo.com/ �� http://www.oanda.com/lang/cns/ ������
head(GSPC)

#����һ�ִ�Web�л�ȡquotes data�ķ����ǣ���quantmod��ʹ��getSymbols()
install.packages("quantmod") 
library(quantmod)
?getSymbols
##########################################
# getSymbols�ǴӶ����Դload��manage����
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
#setSymbolLookup() ��Rλ�ûỰ�д��������Symbol��Ĭ�ϲ���λ�ã�����getSymbols����
#getSymbolLookup()
#saveSymbolLookup()
#loadSymbolLookup()���ڲ�ͬR��session�б��������setting

#######################################
#��myswl�л�ȡ����
#######################################
#window������
library(RODBC)
ch <- odbcConnect("QuotesDSN", uid = "myusername", pwd = "mypassword")
allQuotes <- sqlFetch(ch, "gspc")
GSPC <- xts(allQuotes[, -1], order.by = as.Date(allQuotes[, 1]))
head(GSPC)
odbcClose(ch)
#sqlFecth()��sqlFecthMore()

#linux������
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
#��ǰ������Ϊp%����ģ����Ԥ��һ�½�������k��������,��̬��ȡ
#��ֵ�������ֵ����
# mean(Pi) = (Ci + Hi + Li) / 3, Ci��Hi��Li�ֱ������i���close,high, low quotes 
# Vi����������k��ĵ�k�ٷֱȱ䶯�ľ�ֵ�ļ���
# Vi = {(mean(P(i+j) - Ci)) / Ci}^k 
# indicator variable

?NROW
###########################################
# NROW��������vector��1-column��matrix
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
#HLC��ǰ��ת��OHLCʱ������
?Next
#Next�����е�Time Series��ǰ�ƽ�һ��
?Delt
#Delt����Percent Change

last(GSPC, "3 months")
?candleChart
#candleChart����Financial Charts
candleChart(last(GSPC, "3 months"), theme = "white", TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
?newTA
#newTA����indicator��content���ڻ�ͼ
addT.ind <- newTA(FUN = T.ind, col = 'red', legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()

?ATR
# TR��High-Low-CLose���еı䶯��һ��������׼
myATR <- function(x) ATR(HLC(x))[, "atr"]
?SMI
#SMI��һ��momentum indicator����ÿ���location���
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


#����ʹ������������һ��random forest������ѵ��
head(GSPC)
data(GSPC)
install.packages("randomForest")
library(randomForest)
?specifyModel
#specifyModel����һ�������õ�model
data.model <- specifyModel(T.ind(GSPC) ~ Delt(CL(GSPC), k = 1:10)+
                             myATR(GSPC) + mySMT(GSPC) + myADX(GSPC) + myAroom(GSPC) +
                             myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) +
                             CMD(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
                             myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC))+
                             mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
set.seed(1234)
?buildModel
#����һ��model����ģ�ͷ�����quantmod�����������
rf <- buildModel(data.model, method = 'randomForest',
                 training.per = c(start(GSPC), index(GSPC["1999-12-31"])), ntree = 50, importance = T)
?modelData
#modelData���ڴ�һ��quantmod�����г�ȡ�����ݼ������ڴ���model
ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM), k = 1:3))
data <- modelData(ex.model, data.window = c("2009-01-01", "2009-08-10"))
head(data)

?myFavouriteModellingTool
m <- myFavouriteModellingTool(ex.model@model.formula, as.data.frame(data))

varImpPlot(rf@fitted.model, type = 1)

#importance��õ��Ƕ���ÿ���������ۺϵ÷�
imp <- importance(rf@fitted.model, type = 1)
#Ȼ�������ֵ���˳�����ģ����ʹ�õı���������
rownames(imp)[which(imp > 10)]
#�ɻ�����յ����ݼ�
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC), k = 1) +
                             myATR(GSPC) + myADX(GSPC) + myEMV(GSPC) + myVolat(GSPC) + 
                             myMACD(GSPC) + mySAR(GSPC) + runMean(Cl(GSPC)))
#The Prediction Tasks
#Our real goal: predict the correct trading signal at any time t
# Two paths to obtain predictins for the correct trading signal
# first alternative one����T��Ϊһ��Ŀ�����������Ԥ����Ϣ�������Ŵ�models��Ԥ������value
#����һ����Ԫ�ع�������轫predictionsת��Ϊsignals
# book���е�trading.signals()���������ڽ���ֵ�͵�Tֵת��Ϊ����'s'��'h'��'b'������������

#�ڶ���Ԥ�ⷽ����ֱ��ʹ�ÿ���ʹ��the signals����Ԥ��
#����ÿ��d,ʹ��һ��Ŀ�����'correct'�ź�
#��λ�ȡsignals
#ʹ��T indicator����ͬ��thresholds

#Tdata.train����train�׶ε�����
Tdata.train <- as.data.frame(modelData(data.model, data.window = c('197001-02', '1999-12-31')))
#Tdata.eval����evaluation�׶ε�����
#na.omit������data frameβ����NAs������
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window = c('2000-01-01', '2009-09-15'))))
Tform <- as.formula('T.ind.GSPC ~ .')
#trading.signals()��������������signals


#Evaluation Criteria