#################################################
# Detecting Fraudulent Transactions
#################################################
library(DMwR)
head(sales)
str(sales)
summary(sales)

#######################
# exploring dataSet
#######################
nlevels(sales$ID)
nlevels(sales$Prod)

#####################
# Quant与Val都为NA的
#####################
length(which(is.na(sales$Quant) & is.na(sales$Val)))
#####################
#也可用这样计算
#####################
sum(is.na(sales$Quant) & is.na(sales$Val))

########################################################################
# the distribution of the values in the inspection column
########################################################################
table(sales$Insp)/nrow(sales)*100

totS <- table(sales$ID)
totP <- table(sales$Prod)
barplot(totS, main = 'Transactions per salespeople', names.arg = '',
        xlab = 'SalesPeople', ylab = 'Amount')
barplot(totP, main = 'Transactions per product', names.arg = '',
        xlab = 'Products', ylab = 'Amount')


sales$Uprice <- sales$Val/sales$Quant
summary(sales$Uprice)

#######################################################
# the top most expensive and cheap products
#######################################################
attach(sales)
?aggregate
upp <- aggregate(Uprice, list(Prod), median, na.rm = T)
topP <- sapply(c(T, F), function(o)
  upp[order(upp[,2], decreasing = o)[1:5], 1])
colnames(topP) <- c('Expensive', 'Cheap')
topP


######################################################################
# confirm the different price distribution of the top products
######################################################################
tops <- sales[Prod %in% topP[1, ], c("Prod", "Uprice")]
tops
tops$Prod <- factor(tops$Prod)
boxplot(Uprice ~ Prod, data = tops, ylab = 'Uprice', log = 'y')

###################################################################################
# which salesPeople are the ones who bring more(less) money to the company
###################################################################################
vs <- aggregate(Val, list(ID), sum, na.rm = T)
scoresSs <- sapply(c(T, F), function(o)
  vs[order(vs$x, decreasing = o)[1:5], 1])
colnames(scoresSs)<- c('Most', 'Least')
scoresSs

####################################################################################
# the distribution of the unit prices of the cheapest and most expensive products
####################################################################################
sum(vs[order(vs$x, decreasing = T)[1:100], 2])/sum(Val, na.rm = T) * 100
sum(vs[order(vs$x, decreasing = F)[1:2000], 2])/sum(Val, na.rm = T) * 100


#############################################################
# analysis the quantity of each product
#############################################################
qs <- aggregate(Quant, list(Prod), sum, na.rm = T)
scoresPs <- sapply(c(T, F), function(o)
  qs[order(qs$x, decreasing = o)[1:5], 1])
colnames(scoresPs) <- c('Most', 'Least')
scoresPs
sum(as.double(qs[order(qs$x, decreasing = T)[1:100], 2]))/ 
  sum(as.double(Quant), na.rm = T)*100
sum(as.double(qs[order(qs$x, decreasing = F)[1:4000], 2]))/
  sum(as.double(Quant), na.rm = T) * 100

############################################
# the number of outlines of each product
############################################
out <- tapply(Uprice, list(Prod = Prod), function(x) length(boxplot.stats(x)$out))
out
out[order(out, decreasing = T)[1:10]]
sum(out)/nrow(sales) * 100

######################################################
# Unknown Values
######################################################

####################################################################
# total number of transactions per salesperson and product
####################################################################
totS <- table(ID)
totP <- table(Prod)

####################################################################
# the salesperson and products in the problemitic  transactions
####################################################################
nas <- sales[which(is.na(Quant) & is.na(Val)), c('ID', 'Prod')]
length(nas$ID)

#############################################################################################################
# we now obtain the salespeople with a larger proportion of transactions with unknowns on both Val and Quant
#############################################################################################################
propS <- table(nas$ID)/totS * 100
propS[order(propS, decreasing = T)[1:10]]
#############################################################################
# It seems reasonable to delete these transactions from the perspective of 
#salespeople as they represent a small proportion of their transactions
#############################################################################

########################################
# for the products
########################################
propP <- table(nas$Prod)/totP * 100
propP[order(propP, decreasing = T)[1:10]]
#######################################################
#in summary,it is better to remove all transactions
# with unknown values on quantity and the value 
#######################################################

detach(sales)
sales <- sales[-which(is.na(sales$Quant) & is.na(sales$Val)), ]


###########################################################
# analyze the remaining reports with unknown values 
#in either the quantity or the transaction
###########################################################
nnasQp <- tapply(sales$Quant, list(sales$Prod), function(x) sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp, decreasing = T)[1:10]]

#####################################
# delete problemtic transactions
#####################################
sales <- sales[!sales$Prod %in% c('p2442', 'p2443'),]

########################################
# updatea the levels of the column Prod
########################################
nlevels(sales$Prod)
sales$Prod <- factor(sales$Prod)
nlevels(sales$Prod)

#################################################
# exploring if there are salespeople with all 
# transactionswith unknown quantity
#################################################
nnasQs <- tapply(sales$Quant, list(sales$ID), function(x) sum(is.na(x)))
propNAsQs <- nnasQs/table(sales$ID)
propNAsQs[order(propNAsQs, decreasing = T)[1:10]]

#######################################################################
# analysis of the transaction with an unknown value in the Val column
#######################################################################
nnasVp <- tapply(sales$Val, list(sales$Prod), function(x) sum(is.na(x)))
propNAsVp <- nnasVp/table(sales$Prod)
propNAsVp[order(propNAsVp, decreasing = T)[1:10]]

###################################################################
# with respect to salesperson, the numbers are as follows:
###################################################################
nnasVs <- tapply(sales$Val, list(sales$ID), function(x) sum(is.na(X)))
propNAsVs <- nnasVs/table(sales$ID)
propNAsVs[order(propNAsVs, decreasing = T)[1:10]]

########################################################
# from now on,we have removed all the reports that had 
# insufficient inforation to be subject to a fill-in 
# strategy
########################################################


###################################################################
# for the remaining unknown values,we will apply a method 
# based on the assumption that transactions of the same products
# should have a similar unit price
###################################################################
tPrice <- tapply(sales[sales$Insp != 'fraud', 'Uprice'], list(sales[sales$Insp != 'fraud', 'Prod']), median, na.rm = T)

###################################################
#  fills in all remaining unknown values
###################################################
noQuant <- which(is.na(sales$Quant))
sales[noQuant, 'Quant'] <- ceiling(sales[noQuant, 'Val']) / tPrice[sales[noQuant, 'Prod']]
noVal <- which(is.na(sales$Val))
sales[noVal, 'Val'] <- sales[noVal, 'Quant'] * tPrice[sales[noVal, 'Prod']]

#####################################
# recalculate the Uprice column
#####################################
sales$Uprice <- sales$Val / sales$Quant

save(sales, file = 'salesClean.Rdata')

load('salesClean.Rdata')
sales


##################################################
# few transactions of some products
##################################################
attach(sales)
notF <- which(Insp != 'fraud')
ms <- tapply(Uprice[notF], list(Prod = Prod[notF]), function(x) {
  bp <- boxplot.stats(x)$stats
  c(median = bp[3], iqr = bp[4] - bp[2])
})
ms <- matrix(unlist(ms), length(ms), 2, byrow = T, dimnames = list(names(ms), c('median', 'iqr')))

?plot
par(mfrow = c(1, 2))
plot(ms[, 1], ms[, 2], xlab = 'Mdeian', ylab = 'IQR', main = '')
plot(ms[, 1], ms[, 2], xlab = 'Median', ylab = 'IQR', main = '', col = 'grey', log = 'xy')
smalls <- which(table(Prod) < 20)
points(log(ms[smalls, 1]), log(ms[smalls, 2]), pch = '+')


dms <- scale(ms)
smalls <- which(table(Prod) < 20)
prods <- tapply(sales$Uprice, sales$Prod, list)
prods
similar <- matrix(NA, length(smalls), 7, dimnames = list(names(smalls), c("Simil", 'ks.stat', 'ks.p', 'mdeP', 'igrP', 'medS', 'iqrS')))
for (i in seq(along = smalls)) {
  d <- scale(dms, dms[smalls[i], ], FALSE)
  d <- sqrt(drop(d^2 %*% rep(1, ncol(d))))
  stat <- ks.test(prods[[smalls[i]]], prods[[order(d)[2]]])
  similar[i, ] <- c(order(d)[2], stat$statistic, stat$p.value, ms[smalls[i], ], ms[order(d)[2], ])
}

head(similar)

levels(Prod)[similar[1, 1]]

nrow(similar[similar[, 'ks.p'] >= 0.9, ])
sum(similar[, 'ks.p'] >= 0.9)

save(similar, file = 'similarProduct.Rdata')


###############################################
# 4.3 defining the data mining tasks
###############################################

#########################################################
# task: 哪个transaction reports是被高度怀疑为fraudulent
#########################################################

# 准确地说，是obtain an outlier ranking for a set of observations

##########################
# 针对问题的不同approaches
##########################

# 主要的problem：dataSet中有一列Insp，包含了对之前inspection的信息，【Insp取值主要有：fraudulent、OK与unkn】
# 而经过上文的分析可知，unkn占大部分【代表着Insp的值不确定是OK还是fraud】；
# 我们需要的是一个probabilistic classification【模型不仅不能预测 类别，而且可以知道它的相关概率】


######################################
# unsupervised techniques
######################################
# 对于Insp为unkn的那些transaction，仅有关于该transaction的descriptors
# 无监督式的方法【a descriptive data mining task】： learn some 'concept' without the help of a ''teacher
# Clustering： 尝试着寻找observations中的"natural" groupings
# 有监督式的方法【predictive task】：提前分好类

######################################
# semi-supervised techniques
######################################
# it is costly to find labeled data
# 需要domain experts
# semi-supervised classification methods：在ublabeled cases提供的信息下面，提升standard supervised classification algorithms的performance
# semi-supervised clustering methods: 在生成groups的过程中，利用labeled data的信息，
#【same label in the same groups(must-link constrainsts); keep cases with different labels in different groups(cannot-link constraints)】


###############################################
# self-training
###############################################
# obtain a classification model with the given labeled data
# next step： use this model to classify the unlabeled data
# the cases for which the model has higer confidence on the classification are added together with the predicted label to the inital training set

# transductive support vector machines(TSVMs)


###############################################
# evaluation criteria
###############################################
# how to evaluate the models
# 给定一个transaction reports,每个model都会生成关于这些transaction reprots的ranking

###############################################################################
# a successful model： 对已知的frauds的transaction的排名是处于top positons的
###############################################################################
#在预测较少数量的稀有events的时候【本例中即中Insp为frauds的】 precison与recall已经足够了


######################################################
# lift Charts【增益图】 与 Precision/Recall曲线
######################################################

# Precision/Recall【PR】曲线从precison和recall统计量的角度来展现模型的performance

# ROCR包中有关于evaluate binary classifiers的function   【PR curves】
# 使用模型的predictions来获得object的prediction   【prediction()】
# 将result传给performance（）获得evaluation metrics
# 将performance（）的result传给plot（），可获得不同的performance cirves

####### PRcurve（）function在
PRcurve <- function(preds, trues, ...) {
  require(ROCR, quietly = T)
  pd <- prediction(preds, trues)
  pf <- performance(pd, 'prec', 'rec')
  pf@y.values <- lapply(pf@y.values, function(x) rev(cummax(rev(x))))
  plot(pf, ...)
}


###############################################
# 准确率（precious）与召回率（recall）
# 评价二元分类模型的方法
# ROCR包中包含了评估binary classifiers的fn
# PR曲线 
# prediction（）函数
# performance（）可莸evaluation metrics
###############################################
library(ROCR)
data(ROCR.simple)
str(ROCR.simple)
#head(ROCR.simple)
pre <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pre, 'prec', 'rec')
plot(perf)

##############################################
# 采用precision内插值法消除sawtooth的影响
##############################################
PRcurve <- function(preds, trues, ...) {
  require(ROCR, quietly = T)
  pd <- prediction(preds, trues)
  pf <- performance(pd, 'prec', 'rec')
  pf@y.values <- lapply(pf@y.values, function(x) rev(cummax(rev(x)))) 
  plot(pf, ...)
}
PRcurve(ROCR.simple$predictions, ROCR.simple$labels)

# 采用模型获得test集中的每个observation的outlier score 【0， 1】， 值越大，越有较大的可能性认为observation为fraud
# 一个Confusion Matrix

#                                 Predictions value
#                            Fraud                OK
# True value     Fraud         1                  3       4        
#                OK            1                  2       3
#                              2                  5       7

# Prec = 1/(1+1) = 0.5
# Rec =  1/(1+2) = 0.333
# RPP【rate of positive predictions】 = 1/7  


#######################################
# evaluate outlier ranking models 
# 设定threshold值
# Confusion Matrix 
#######################################



###############################################################
# lift charts
# 更强调recall，x轴是rate of positive predictions【RPP】,
# 即model预测的一个positive class的概率
###############################################################
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, 'lift', 'rpp')
plot(perf, main = 'Lift Chart')


# cumulative recall chart【就RPP供献的recall vaules】
# CRchgart（）已在ROCR包中实现

CRchart <- function(preds, trues, ...) {
  require(ROCR, quietly = T)
  pd <- prediction(preds, trues)
  pf <- performance(pd, 'rec', 'rpp')
  plot(pf, ...)
}
CRchart(ROCR.simple$predictions, ROCR.simple$labels, main = 'Cumulative Recall Chart')


###############################################
# Normalized Distance to Typical Price
###############################################
avgNDTP <- function(toInsp, train, stats) {
  if (missing(train) && missing(stats))
    stop('Provide either the training data or the product stats')
  if (missing(stats)) {
    notF <- which(train$Insp != 'fraud')
    stats <- tapply(train$Uprice[notF], 
                    list(Prod = train$Prod[notF]),
                    function(x) {
                      bp< boxplot.stats(x)$stats
                      c(median = bp[3], iqr = bp[4]- bp[2])
                    })
    stats <- matrix(unlist(stats),
                    length(stats), 2, byrow = T,
                    dimnames = list(names(stats), c('median', 'iqr')))
    stats[which(stats[, 'iqr'] == 0), 'iqr'] <- 
      stats[which(stats[, 'iqr'] == 0), 'median']
  }
  mdtp <- mean(abs(toInsp$Uprice - stats[toInsp$Prod, 'median'])/stats[toInsp$Prod, 'iqr'])
  return(mdtp)
}


############################################
# Experimental Methodology
############################################
#选择Hold out方法作为experimental comparisons【随机地将dataSet分割成两部分，通常是(70%/30%的比例，一部分用于获得模型，另一部分用于test them)
# 这个过程可重复多次，以确保可靠的result】
# 额外的一个问题是：不同类型的reports的distributions不平衡【使用层次抽样的】
# 从bags中随机选择不同classes的observations

# holdOut（）查用于实现这种抽样
# 

#############################################################
# holdOut函数对给定的dataSet进行holdout experiment
#############################################################
evalOutlierRanking <- function(testSet, rankOrder, Threshold, statsProds) {
  ordTS <- testSet[rankOrder, ]
  N <- nrow(testSet)
  nF <- if (Threshold < 1) as.integer(Threshold * N) else Threshold
  cm <- table(c(rep('fraud', nF), rep('ok', N-nF)), ordTS$Insp)
  prec <- cm['fraud', ' fraud']/sum(cm['fraud', ])
  rec <- cm['fraud', 'fraud']/sum(cm[, 'fraud'])
  AVGndtp <- avgNDTP(ordTS[nF, ], stats = statsProds)
  return (c(Precision = prec, Recall = rec, avgNDTP = AVGndtp))
}



################################################
# Obtaining Outlier Rankings
################################################
# 使用不同的models获得outlier rankings， 对于每个results使用一个70%/30%的层次hold-out方案


##############################################################
# unsupervised approaches
##############################################################

###################################################
# the modified Box Plot Rule
###################################################
#box plot rule：outliers是连续型变量且近似服从normal distribution
# NDTP的优势：是一个unitless metric且可将不同的products混合在一起，对于所有的test cases，生成了一个全局性的ranking

BPrule <- function(train, test) {
  notF <- which(train$Insp != 'fraud')
  ms <- tapply(train$Uprice[notF], list(Prod = train$Prod[notF]),
               function(x) {
                 bp <- boxplot,stats(x)$stats
                 c(median = bp[3], iqr = bp[4] - bp[2])
               })
  ms <- matrix(unlist(ms), length(ms), 2, byrow = T, dimnbames = list(names(ms), c('median', 'iqr')))
  ms[which(ms[, 'iqr'] == 0), 'iqr'] <- ms[which(ms[, 'iqr'] == 0), 'median']
  ORscore <- abs(test$Uprice- ms[test$Prod, 'median'])/ ms[test$Prod, 'iqr']
  return (list(rankOrder = order(ORscore, decreasing = T), rankScore = ORscore))
}


notF <- which(sales$Insp != 'fraud')
globalStats <- tapply(sales$Uprice[notF],
                      list(Prod = sales$Prod[notF]),
                      function(x) {
                        bp <- boxplot.stats(x)$stats
                        c(median = bp[3], iqr = bp[4] - bp[2])
                      })
globalStats <- matrix(unlist(globalStats),
                      length(globalStats), 2, byrow = T,
                      dimnames = list(names(globalStats), c('median', 'iqr')))
globalStats[which(globalStats[, 'iqr'] == 0), 'iqr'] <- 
  globalStats[which(globalStats[, 'iqr'] == 0), 'median']


################################################################
# 返回evaluation statistic的值，包含predicted与true值等attribute
################################################################
ho.BPrule <- function (form, train, test, ...) {
  res <- BPrule(train, test)
  structure(evalOutlierRanking(test, res$rankOrder, ...),
            itInfo = list(preds = res$rankScore, 
                          trues = ifelse(test$Insp == 'fraud', 1, 0)))
}

bp.res <- holdOut(learner("ho.BPrule",
                          pars = list(Threshold = 0.1, 
                                      statsProds = globalStats)),
                  dataset(Insp ~., sales),
                  hldSettings(3, 0.3, 1234, T),
                  itsInfo = TRUE
)
summary(bp.res)

par(mfrow = c(1, 2))
info <- attr(bp.res, 'itsInfo')
PTs.bp <- aperm(array(unlist(info), dim = c(length(info[[1]]), 2, 3)),
                c(1, 3, 2))
PRcurve(PTs.bp[, , 1], PTs.bp[, , 2],
        main = 'PR curve', avg = 'vertical')
CRchart(PTs.bp[, , 1], PTs.bp[, , 2], 
        main = 'Cumulative Recall curve', avg = 'vertical')



#####################################################
# Local Outlier Factors(LOF)
#####################################################
# outlier ranking： local outlier factor（LOF）系统是一个state-of-the-art outlier ranking 方法
# 这个system的主要观点是：对于每一个case，通过estimate它与local neighborhood的关系，来反应其isolation degress
# 是以observatuib的local density为基础的，cases中density非常低的被认为是outliers，density的estimate是通过cases之间的distances来衡量的

# (1) concept of core distance： 点与其第k个最近neighbor的距离
# (2) concept of reachability distance [p1, p2]：到p1的最大的core distance及两者间的距离
# (3) local reachability distance of a point：到其k个neighbors的可达距离的均值
# LOF计算的是local reachability distance
# LOF 算法d lofaxtor（）函数中已实现





# 然而，我们的dataset中包含很多nominal variables
# 解决方案：
# (1) 更换LOF的source code，让它成为一个mixed-mode distance, 计算distance fn函数很多，cluster包中的daisy()
# (2) re-coding nominal variables，observation都continuous varibales
# (3) handling each product individually, 本例中【需要考虑到salespeople的information】


# changing the outlier factors into a 0..1 scale, SoftMax()可实现
# lofactor（）生成的是Inf与NaN值
# 接下来使用hold-out来获得evalution metrics的estimates

ho.LOF <- function (form, train, test, k, ... ) {
  ntr <- nrow(train)
  all <- rbind(train, test)
  N <- nrow(all)
  ups <- split(all$Uprice, all$Prod)
  r <- list(length = ups)
  for ( u in seq(along=ups))
    r[[u]] <- if (NROW(ups[[u]]) > 3)
      lofactor(ups[[u]], min(k, NROW(ups[[u]] %/% 2)))
  else if (NROW(ups[[u]])) rep(0, NROW(ups[[u]]))
  else NULL
  all$lof <- vector(length = N)
  split(all$lof, all$Prod) <- r
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
    SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
  structure(evalOutlierRanking(test, order(all[(ntr+1):N, 'lof'],
                                           decreasing = T),...),
            itInfo = list(preds = all[(ntr+1):N, 'lof'],
                          trues = ifelse(test$Insp = 'fraud', 1, 0))
  )
}

lof.res <- holdOut(learner('ho.LOF',
                           pars = list(k = 7, Threshold = 0.1, 
                                       statsOrids = globalStats)),
                   dataset(Insp ~., sales),
                   hldSettings(3, 0.3. 1234, T),
                   itsInfo = TRUE)

summary(lof.res)

par(mfrow = c(1, 2))
info <- attr(lof.res, 'itsInfo')
PTs.lof <- aperm(array(unlist(info), dim = c(length(info[[1]], 2, 3)), c(1, 3, 2)))
PRcurve(PTs.bp[,,1], PTs.bp[,, 2],
        main = 'PR curve', lty =1, xlim = c(0, 1), ylim = c(0, 1),
        avg = 'vertical')
PRcurve(PTs.lof[,,1], PTs.lof[,,2],
        add = T, lty =2,
        avg ='vertical')
legend('topright', c('BPrule', 'LOF'), lty = c(1, 2))
CRchart(PTs.bp[,,1], PTs.bp[,,2],
        main = 'Cumulative Recall curve', lty = 1, xlim =c(0, 1), ylim = c(0, 1),
        avg = 'vertical')
CRchart(PTs.lof[,,1], PTs.lof[,,2],
        add = T, lty =2,
        avg ='vertical')
legend('bottomright', c('BPrule', 'LOF'), lty = c(1, 2))


############################################
# Clustering-based Outlier Rankdings(ORh)
############################################
# clustering algorithm


# ORh method：分层聚类算法，获得给定数据的dendrogram【系统树图】
# 将这些树以不同高度的levels可生成不同的clusterings， 在最低的层的level上，有很多groups
# 算法的下一步：先前两个groups中的哪些应该进行merge

# 基础stats中的hclust()实现该思想的多种clustering方法

# 思想：outlier对merge持有resistance的态度
ho.ORh <- function(form, train, test, ...) {
  ntr <- nrow(train)
  all <- rbind(train, test)
  N <- nrow(all)
  ups <- split(all$Uprice, all$Prod)
  r <- list(length = ups)
  for (u in seq(along = ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3)
                outliers.ranking(ups[[u]])$prob.outliers      # outliers.ranking() function
              else if (NROW(ups[[u]])) rep(0, NROW(ups[[u]]))
              else NULL
  all$orh <- vector(length = N)
  split(all$orth, all$Prod) <- r
  all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))] <-
    SoftMax(all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))])
  structure(evalOutlierRanking(test, order(all[(ntr+1):N, 'orh'],
                                           decreasing = T),...),
            itInfo = list(preds = all[(ntr+1):N, 'orh'],
                          trues = ifelse(test$Insp=='fraud', 1, 0))
            )
}

###############################################
# 使用holdOut对Rh method进行评估
###############################################
orh.res <- holdOut(learner('ho.ORh',
                           pars = list(Threshold = 0.1,
                                       statsProds = globalStats)),
                   dataset(Insp ~., sales),
                   hldSettings(3, 0.3, 1234, T),
                   itsInfo = TRUE)
summary(orh.res)

par(mfrow = c(1, 2))
info <- attr(orh.res, 'itsInfo')
PTs.orh <- aperm(array(unlist(info), dim = c(length(info[[1]]), 2, 3)),
                 c(1, 3, 2))
PRcurve(PTs.bp[,,1], PTs.bp[,,2],
        main = 'PR curve', lty = 1, xlim = c(0, 1), ylinm = c(0, 1),
        avg = 'vertical')
PRcurve(PTs.lof[,,1], PTs.lof[,,2],
        main = 'PR curve', lty = 1, xlim = c(0, 1), ylim = c(0, 1), 
        avg = 'vertical')
PRcurve(PTs.lof[,,1], PTs.lof[,,2],
        add = T, lty = 2,
        avg = 'vertical')
PRcurve(PTs.orh[,,1], PTs.orh[,,2],
        add = T, lty = 1, col = 'grey',
        avg = 'vertcial')
legend('topright', c('BPrule', 'LOF', 'ORh'),
       lty = c(1, 2, 1), col = c('balck', 'black', 'grey'))
CRchart(PTs.bp[,,1], PTs.bp[,,2],
        main = 'Cumulative Recall curve', lty = 1, xlim = c(0, 1), ylim = c(0, 1),
        avg = 'vertical')

CRchart(PTs.lof[,,1], PTs.lof[,,2],
        add = T, lty =2,
        avg = 'vertical')
CRchart(PTs.orh[,,1], PTs.orh[,,2],
        add = T, lty =2,
        avg = 'vertical')
CRchart(PTs.orh[,,1], PTs.orh[,,2],
        add = T, lty = 1, col = 'grey',
        avg = 'vertical')
legend('bottomright', c('BPrule', 'LOF', 'ORh'),
       lty = c(1, 2, 1), col = c('black', 'blcak', 'grey'))




###########################################
# supervised approaches
###########################################

# given our goal: obtaining a ranking for a set of transaction reports
# we will have to constrain the selection of models
# use only systems that are able to produce probabilistic classifications

##############################################
# the class imbalacne problem
##############################################
# a very imbalanced proportion of normal and fraudulent reports

# several techniques developed with the purpose of helping the learning algorithms overcome
# the problems raised by class imbalance.

# group in two families:
# (1) methods that bias the learning process by using specific evaluation metrics that 
# are more sensitive to minority class examples
# (2) sampling methods that manipulate the training data to change the class distribution
#    several sampling methods：
#   under-sampling methods: select a small part of the majority class examples and add 
#                           them to the minority class classes, building a dataset with 
#                           with a more balanced class distribution,
#   over-sampling methods: using some process to replicate the minority class examples
#  SMOTE method： artificially generate new examples of the minority class using the nearest 
#                 neighbors of these cases  【已在SMOTE()中实现了， 生成一个更balanced分布的新数据集】

data(iris)
data <- iris[, c(1, 2, 5)]
data$Species <- factor(ifelse(data$Species == 'setosa', 'rare', 'common'))
library(DMwR)
newData <- SMOTE(Species ~ ., data, perc.over = 600)
?SMOTE
table(newData$Species)

par(mfrow = c(1, 2))
plot(data[, 1], data[, 2], pch = 19 + as.integer(data[, 3]), main = 'Original Data')
plot(newData[ 1], newData[, 2], pch = 19 + as.integer(newData[, 3]), main = "SMOTE'd Data")


####################################################
# Naive Bayes
####################################################
# naiveBayes() 包：e1071实现了该算法；包klaR也实现了，且包括一些可视化的函数

nb <- function(train, test) {
  require(e1071, quietly = T)
  sup <- which(train$Insp != 'unkn')
  data <- train[supp, c('ID', 'Prod', 'Uprice', 'Insp')]
  data$Insp <- factor(data$Insp, levels = c("ok", 'fraud'))
  model <- naiveBayes(Insp ~ ., data)
  preds <- predict(model, test[, c("ID", "Prod", "Uprice", "Insp")], type = 'raw')
  return(list(rankOrder = order(preds[, "fraud"], decreasing = T), rankScore = preds[, "fraud"]))
}

ho.nb <- function(form, train, test, ...) {
  res <- nb(train, test)
  structure(evalOutlierRanking(test, res$rankOrder, ...), 
            itInfo = list(preds = res$rankScore,
                          trues = ifelse(test$Insp == 'fraud', 1, 0)))
}

nb.res <- holdOut(learner('ho.nb', 
                          pars = list(Threshold = 0.1,
                                      statsProds = globalStats)),
                  dataset(Insp ~ . , sales),
                  hldSettings(3, 0.3 1234, T),
                  itsInfo = TRUE)

summary(nb.res)

par(mfrow = c(1, 2))
info <- attr(nb.res, 'itsInfo')
PTs.nb <- aperm(array(unlist(info), dim = c(lenght(info[[1]]), 2, 3)),
                      c(1, 3, 2))
PRcurve(PTs.nb[,,1], PTs.nb[,,2],
        main = 'PR curve', lty = 1, xlim = c(0, 1), ylim =c(0, 1),
        avg = 'vertical')

PRcurve(PTs.orh[,,1], PTs.orh[,,2],
        add = T, lty = 1, col = 'grey',
        avg = 'vertical')
legend('toprihgt', c('NaiveBayes', 'ORh'),
       lty = 1, col = c('black', 'grey'))
CRchart(PTs.nb[,,1], PTs.nb[,,2],
        main = 'Cumulative Recall curve', lty = 1, xlim =c(0, 1), ylim = c(0, 1),
        avg = 'vertical')
CRchart(PTs.orh[,,1], PTs.orh[,,2],
        add = T, lty = 1, col = 'grey',
        avg = 'vertical')
legend('bottomright', c('NaiveBayes', 'ORh'),
       lty = 1, col = c('black', 'grey'))


### poor performance of the Naive Bayes may be the class imbalance of this problem
# apply the Naive Bayes classifier using a training set obtained using SMOTE
nb.s <- function(trian, test) {
  require(e1071, quietly = T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup, c("ID", "Prod", "Uprice", "Insp")]
  data$Insp <- factor(data$Insp, levels = c("ok", "fraud"))
  newData <- SMOTE(Insp ~ ., data, perc.over = 700)
  model <- naiveBayes(Insp ~ ., newData)
  preds <- predict(model, test[, c("ID", "Prod", "Uprice", "Insp")], type = 'raw')
  return(list(rankOrder = order(preds[, "fraud"], decreasing = T), rankScore = preds[, "fraud"]))
}

# obtain the hold-out estimates for SMOTE'd version of Naive Bayes
ho.nbs <- function(form, train, test, ...) {
  res <- nb.s(train, test)
  structure(evalOutlierRanking(test, res$rankOrder, ...),
            itInfo = list(preds = res$rankScore,
                          trues = ifelse(test$Insp == 'fraud', 1, 0)))
}

nbs.res <- holdOut(learner('ho.nbs', 
                           pars = list(Threshold = 0.1,
                                       statsProds = globalStats)),
                   dataset(Insp ~ ., sales),
                   hldSettings(3, 0.3, 1234, T),
                   itsInfo = TRUE)
summary(nbs.res)

par(mfro = c(1, 2))
info <- attr(nbs.res, 'itsInfo')
PTs.nbs <- aperm(array(unlist(info), dim = c(length(info[[1]]), 2, 3)),
                       c(1, 3, 2))
PRcurve(PTs.nb[,,1], PTs.nb[,,2],
        main = 'PR curve', lty = 1, xlim = c(0, 1), ylim = c(0, 1),
        avg = 'vertical')
PRcurve(PTs.nbs[,,1], PTs.nbs[,,2],
        add = T, lty = 2,
        avg = 'vertical')
PRcurve(PTs.orh[,,1], PTs.orh[,,2],
        add = T, lty = 1, col = 'grey'
        avg = 'vertical')
legend('topright', c('NaiveBayes', 'smoteNaiveBayes', 'ORh'),
       lty = c(1, 2, 1), col = c('black', 'black', 'grey'))
CRchart(PTs.nb[,,1], PTs.nb[,,2],
        main = 'Cumulatie Recall curve', lty = 1, xlim = c(0, 1), ylim = c(0, 1),
        avg = 'vertical')
CRchart(PTs.nbs[,,1], PTs.nbs[,,2], 
        add = T, lty = 2,
        avg = 'vertical')
CRchart(PTs.orh[,,1], PTs.orh[,,2],
        add = T, lty = 1, col = 'grey',
        avg = 'vertical')
legend('bottomright', c('NaiveBayes', 'smoteNaiveBayes', 'ORh'),
       lty = c(1, 2, 1), col = c('balck', 'black', 'grey'))


###############################################
# Adaboost Algorithm
###############################################
# a learning algorightm belongs to the classs of ensemble models
# Adaboost： using a weighting sche a
# the base learner is used on different distribution of the training data.
# the result is a set of base models obtained on different training samples.
# the ensemble can be used t oobtain predictions for test cases of the original problem.
# the predictions are obtained by a weighted average of the predictions of the individual base models.

# the case weighting schema used by AdaBoost is interesting from the perspective of learning with 
# imbalance class distributions.

# AdaBoost.M1是AdaBoost方法的一个实例，在adabag包中已实现，但这些models中的predict方法不能返回probabilities

# RWeka包中的AdaBoost.M1分类模型使用Weka实现了这个算法，predict方法可获得probabilistic classification
# 因此，可用于解决我们的问题：outlier ranking

#  WOW()可用于check哪些parameters可用于Weka的学习算法

library(RWeka)
WOW(AdaBoostM1)

data(iris)
idx <- sample(150, 100)
model <- AdaBoostM1(Species ~ ., iris[idx, ], control = Weka_control(I = 100))
preds <- predict(model, iris[-idx, ])
head(preds)

table(preds, iris[-idx, 'Species'])

prob.preds <- predict(model, iris[-idx, ], type = 'probability')
head(prob.preds)


ab <- function(train, test) {
  require(RWeka, quietly = T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup, c('ID', 'Prod', 'Uprice', 'Insp')]
  data$Insp <- factor(data$Insp, levels = c('ok', 'fraud'))
  model <- AdaBoostM1(Insp ~ ., data, control = Weka_control(I = 100))
  preds <- predict(model, test[, c('ID', 'Prod', 'Uprice', 'Insp')], type = 'probability')
  return(list(rankOrder = order(preds[, 'fraud'], decresaing = T), rankScore = preds[, 'fraud']))  
}

ho.ab <- function(form, trian, test, ...) {
  res <- ab(train, test)
  structure(evalOutlierRanking(test, res$rankOrder, ...),
            itInfo = list(preds = res$rankScore,
                          trues = ifelse(test$Insp == 'fraud', 1, 0)))
}

library(DMwR)
?holdOut
?learner
ab.res <- holdOut(learner('ho.ab',
                          pars = list(Threshold = 0.1,
                                      statsProds = globalStats)),
                  dataSet(Insp ~ ., sales),
                  hldSettings(3, 0.3, 1234, T),
                  itsInfo = TRUE)
summary(ab.res)


# the PR and cumulative recall curves
par(mfrow = c(1, 2))
info <- attr(ab.res, 'itsInfo')
?attr
?aperm
PTs.ab <- aperm(array(unlist(info), dim = c(length(ifno[[1]]), 2, 3)), c(1, 3, 2))
PRcurve(PTs.nb[,,1], PTs.nb[,,2], main = 'PR curve', lty = 1, xlim = c(0, 1), ylim = c(0, 1), avg = 'vertical')
PRcurve(PTs.orh[,,1], PTs.orh[,,2], add = T, lty = 1, col = 'grey', avg = 'vertical')
PRcurve(PTs.ab[,,1], PTs.ab[,,2], add = T, lty = 2, avg = 'vertical')
legend('topright', c('NaivBayes', 'ORh', 'AdaBoostM1'), lty = c(1, 1, 2), col = c('black', 'grey', 'black'))
CRchart(PTs.nb[,,1], PTs.nb[,,2], main = 'Cumulative Recall curve', lty = 1, xlim = c(0, 1), ylim = c(0, 1), avg = 'vertical')
CRchart(PTs.orh[,,1], PTs.orh[,.,2], add = T, lty =1, col = 'grey', avg = 'vertical')
CRchart(PTs.ab[,,1], PTs.ab[,,2], add = T, lty =2, avg = 'vertical')
legend('bottomright', c('NaiveBayes', 'ORh', 'AdaBoostM1'), lty = c(1, 1, 2), col = c('black', 'grey', 'black'))



###########################################
# Semi-Supervised Approaches 
###########################################
# use both inspected and non-inspected reports to obtain a classification model 
# to detect fraudulent reports

# we need some form of semi-supervised classificaion method
# Self-training是一个较为常用 的semi-supervised classification
# self-train： building an initial classifier using the given labeled cases
# this classifier is used to predict the lables of the unlabeled cases in the given training set
# cases with higer confidence in the predicted label will be added tgo the labeled set,together with predicted labels
# with larger dataset, a new classifier is obtained and so on.
# the iterative process is continued until some criteria are met
# the last classifer is the result of the learning process

# the self-training methods has three relevant parameters:
# (1) the base learner
# (2) the threshold on the confidence of the classifications determining which cases should be added to the new training set
# (3) the criteria to decide when to terminate the self-training process

# SelfTrain() ：可用于从一个包含labeled和unlabeled的case的training set中获得probabilistic classifier来获得model
install.packages("e1071")
library(e1071)
data(iris)
idx <- sample(150, 100)
tr <- iris[idx, ]
ts <- iris[-idx, ]
nb <- naiveBayes(Species ~ ., tr)
table(predict(nb, ts), ts$Species)

trST <- tr
nas <- sample(100, 90)
trST[nas, "Species"] <- NA
func <- function(m, d) {
  p <- predict(m, d, type = 'raw')
  data.frame(cl = colnames(p)[apply(p, 1, which.max)], p = apply(p, 1, max))
}
nbSTbase <- naiveBayes(Species ~ ., trST[-nas, ])
table(predict(nbSTbase, ts), ts$Species)

nbST <- SelfTrain(Species ~ ., trST, learner('naiveBayes', list()), "func")
# 参数
# thrConf： 设定unlabeled case加入 到labled set中所需的probability
# maxIts：定义self-training迭代次数的最大值
# percFull：当给定的dataset其labeled set所到达某一个百分比的时候，该过程应当被停止
table(predict(nbST, ts), ts$Species)


pred.nb <- function(m, d) {
  p <- predict(m, d, type = 'raw')
  data.frame(cl = colnames(p)[apply(p, 1, which.max)], p = apply(p, 1, max))
}
nb.st <- function(train, test) {
  require(e1071, quietly = T)
  train <- train[, c('ID', 'Prod', 'Uprice','Insp')]
  train[which(trian$Insp == 'unkn', 'Insp')] <- NA
  train$Insp <- factor(train$Insp, levels = c('ok', 'fraud'))
  model <- SelfTrain(Insp ~ ., train, 
                     learner('naiveBayes', list()), 'pred.nb')
  preds <- predict(model, test[, c('ID', 'Prod', 'Uprice', 'Insp')], type = 'raw')
  return(list(rankOrder = order(preds[, 'fraud'], decreasing = T), rankScore = preds[, 'fraud']))
}

ho.nb.st <- function(form, train, test, ...) {
  res <- nb.st(train, test)
  structure(evalOutlierRanking(test, res$rankOrder, ...),
            itInfo = list(preds = res$rankScore, trues = ifelse(test$Insp == 'fraud', 1, 0)))
}

nb.st.res <- holdOut(learner('ho.nb.st',
                             pars = list(Threshold = 0.1,
                                         statsProds = globalStats)),
                     dataset(Insp ~ . , sales),
                     hldSettings(3, 0.3, 1234, T),
                     itsInfo = TRUE)

summary(nb.st.res)

par(mfrow = c(1, 2))
info <- attr(nb.st.res, 'itsInfo')
PTs.nb.st <- aperm(array(unlist(info), dim = c(length(info[[1]]), 2, 3)),
                   c(1, 3, 2))
PRcurve(PTs.nb[,,1], PTs.nb[,,2],
        main = 'PR curve', lty = 1, xlim = c(0, 1), ylim = c(0, 1),
        avg = 'vertical')
PRcurve(PTs.orh[,,1], PTs.orh[,,2],
        add = T, lty = 1, col = 'grey',
        avg = 'vertical')
PRcurve(PTs.nb.st[,,1], PTs.nb.st[,,2],
        add = T, lty = 2,
        avg = 'vertical')
legend('topright', c('NaivBayes', 'ORh', 'NaiveBayes- ST'),
       lty = c(1, 1, 2), col = c('black', 'grey', 'black'))
CRchart(PTs.nb[,,1], PTs.nb[,,2],
        main = 'Cumulative Recall curve', lty = 1, xlim = c(0, 1), ylim = c(0, 1),
        avg = 'vertical')
CRchart(PTs.orh[,,1], PTs.orh[,,2],
        add = T, lty = 1, col = 'grey',
        avg = 'vertical')
CRchart(PTs.nb.st[,,1], PTs.nb.st[,,2],
        add = T, lty = 2, 
        avg = 'vertical')
legend('bottomright', c('NaiveBayes', 'ORh', 'NaiveBayes-ST'),
       lty = c(1, 1, 2), col = c('black', 'grey', 'black'))



###########################################################
# self-trianing approach with the AdaBoost.M1 algorithm
###########################################################
pred.ada <- function(m, d) {
  p <- predict(m, d, type = 'probability')
  data.frame(cl = colnames(p)[apply(p, 1, which.max)],
             p = apply(p, 1, max))
}

ab.st <- function(train, test) {
  require(RWeka, quietly = T)
  train <- train[, c('ID', 'Prod', 'Uprice', 'Insp')]
  trian[which(train$Insp == 'unkn'), 'Insp'] <- NA
  trian$Insp <- factor(train$Insp, levels = c('ok', 'fraud'))
  model <- SelfTrain(Insp ~ . , train, 
                     leaner('AdaBoostM1',
                            list(control = Weka_control(I = 100))),
                     'pred.ada')
  preds <- predict(model, test[, c('ID', 'Prod', 'Uprice', 'Insp')],
                   type = 'probability')
  return(list(rankOrder = order(preds[, 'fraud'], decreasing = T), 
              rankScore = preds[, 'fraud']))
}

ho.ab.st <- function(form, train, test, ...) {
  res <- ab.st(train, test)
  structure(evalOutlierRanking(test, res$rankOrder, ....),
            itInfo = list(preds = res$rankScore, 
                          trues = ifelse(test$Insp == 'fraud', 1, 0)))
}

ab.st.res <- holdOut(learner('ho.ab.st', 
                             pars = list(Threshold = 0.1,
                                         statsProds = globalStats)),
                     dataset(Insp ~ . , sales),
                     hldSettings(3, 0.3, 1234, T),
                     itsInfo = TRUE)
summary(ab.st.res)

par(mfrow = c(1, 2))
info <- attr(ab.st.res, "itsInfo")
PTs.ab.st <- aperm(array(unlist(info), dim = c(length(info[[1]]),
                                               2, 3)), c(1, 3, 2))
PRcurve(PTs.ab[ , , 1], PTs.ab[ , , 2], main = 'PR curve', 
        lty = 1, xlim = c(0, 1), ylim = c(0, 1), avg = 'vertical')
PRcurve(PTs.orh[, , 1], PTs.orh[, , 2], add = T, lty = 1,
        col = 'grey', avg = 'vertical')
PRcurve(PTs.ab.st[, , 1], PTs.ab.st[, , 2], add = T, lty = 2,
        avg = 'vertical')
legend('topright', c('AdaBoostM1', 'ORh', ' AdaBoostM1-ST'),
       lty = c(1, 1, 2), col = c('black', 'grey', 'black'))

CRchart(PTs.ab[ , , 1], PTs.ab[ , , 2], main = "Cumulative Recall curve",
        lty = 1, xlim = c(0, 1), ylim = c(0, 1), avg = 'vertical')
CRchart(PTs.orh[ , , 1], PTs.orh[ , , 2], add = T, lty = 1, 
        col = 'grey', avg = 'vertical')
CRchart(PTs.ab.st[, , 1], PTs.ab.st[, , 2], add = T, lty = 2, avg = 'vertical')
legend('bottomright', c('AdaBoostM1', "ORh", "AdaBoostM1-ST"),
       lty = c(1, 1, 2), col = c('black', 'grey', 'black'))






