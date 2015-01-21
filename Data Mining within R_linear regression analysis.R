# Chapter2
###################################################
# Linear Regression
# Regression Tree
# k-fold cross validation

# Random Forest (内含有k-fold交叉验证)
###################################################
R.version
install.packages('DMwR')
install.packages('plyr')
install.packages('RODBC')
library(DMwR)
head(algae)
str(algae)
#eg

#第一步
#缺乏较为全面的信息，首先来看一下数据集的statistical properities
summary(algae)

#查看数据的分布，正态、偏态；峰度，峭度
#峰度：表示概率密度分布曲线在平均值处峰值高低的特征数，反映了尾部的厚度；峰度是描述分布形态的陡缓程度，峰度为3表示与正态分布相同，峰度小于出表示正态分布陡峭，大于3表示比正态分布平坦。
#峭度：反应振动分布特性的数值统计量；当k=3时分布曲线具有正常峰度即零峭度；当k>3时，分布曲线具有正峭度；当k<3时，分布曲线具有负峭度。
hist(algae$mxPH, prob = T)  #prob = T时，画出的图是频率分布；若为F时，是频数分布
#分布近似正态分布，需进一步验证，使用normal Q-Q plots
#qq.plot()在car包中
install.packages("car")
library(car)

#密度曲线与Q-Q图
par(mfrow=c(1,2))
hist(algae$mxPH, prob  = T, xlab='', main = "Histogram of maximum pH value", ylim = 0:1) #画hist图,prob = T时，为概率分布图；否则，为频次分布图
lines(density(algae$mxPH, na.rm = T))  #增加一条密度曲线
rug(jitter(algae$mxPH))   # 给plot的x轴增加一个度rug
?rug   
?jitter
qq.plot(algae$mxPH, main="Normal QQ plot of maximum pH")
par(mfrow=c(1,1))


#algae中的oPO4
str(algae$oPO4)
#快速获得分布情况
boxplot(algae$oPO4, ylab="Orthophosphate(oPO4)")
#在坐标轴上画出出分位数
rug(jitter(algae$oPO4), side = 2)
abline(h = mean(algae$oPO4, na.rm = T), lty = 2)

#奇异点的处理
plot(algae$NH4, xlab = "")
abline(h = mean(algae$NH4, na.rm = T), lty = 1)
abline(h = mean(algae$NH4, na.rm = T) + sd(algae$NH4, na.rm = T), lty = 2)
abline(h = median(algae$NH4, na.rm = T), lty = 3)
identify(algae$NH4)   #可用来标记奇异点

plot(algae$NH4, xlab = "")
clicked.lines <- identify(algae$NH4)
algae[clicked.lines, ]

algae[algae$NH4 > 19000, ]
algae[!is.na(algae$NH4) & algae$NH4 > 19000,] #取algae$NH4中非空且大于19000的记录


###############################################################################
# 考察factor类型的variable，对response产生的影响，从图表方面来说
###############################################################################
library(lattice)
bwplot(size ~ a1, data = algae, ylab='River Size', xlab='Algal A1')  # plot a1 for each value of size
??bwplot

install.packages("Hmisc")
library(Hmisc)
bwplot(size ~ a1, data = algae, panel = panel.bpplot, probs = seq(.01, .49, by = .01), datadensity = TRUE, ylab = 'River Size', xlab = 'Algal A1')


######################################################
# 对factor类型的数据进行离散化处理，生成continuous数据
######################################################
#equal.count()这种函数用来把数据划分为若干个区间，可以指定区间的个数以及区间之间的重叠程度
?stripplot
library(Hmisc)
minO2 <- equal.count(na.omit(algae$mnO2), number = 4, overlap=1/5)
stripplot(season ~ a3|minO2, data = algae[!is.na(algae$mnO2),])



#######################################################
#Unknown Values
#######################################################

########################################################
#处理方法：
# (1)直接删除
# (2)寻找unknown与其他项之间的关系
# (3)用其他相类似的案例进行填充
# (4)使用能够处理缺失值的工具
########################################################
#(1)直接删除
library(DMwR)
data(algae)
nrow(algae[!complete.cases(algae),])  #存在为空的记录的条数 
# complete.cases   判断cases是否有missing values
algae <- na.omit(algae) #过滤掉为空的记录
?na.omit
#####################################################################################
# 处理objects中的missing values
# na.fail: 若object不包含任何missing values，则返回object;否则返回一个知识信息
# na.omit：返回移除有missing values的object后的objects
# na.pass：对missing values不进行任何处理，返回所有的objects
#####################################################################################
algae <- algae[-c(62,199),] #过滤62与199的记录
algae
apply(algae, 1, function (x) sum(is.na(x)))  #统计每行记录中为na的数目 

# 需要考察那些有较多NAs的行


library(DMwR)
data(algae)
?manyNAs
############################################################
# manyNAs()获得dataframe中有较高数量的unknown values的行号
# nORp用于设定前多少前多少的行号
############################################################
manyNAs(algae, 0.2) 

########################################
# 去除algae中有较多NAs的行
########################################
algae <- algae[-manyNAs(algae), ]


################################################
#（2）进行替换
# 使用出现最为频繁的values对Unknowns进行填充
################################################
#用最可能的Values对unknowns进行填充

########################################################
# 最简单、快速的方法：使用统计中的集中趋势指标进行填充
# statistic of centrality eg：mean、median、mode,etc  慎用，
# 较为不好的方法，速度较，只是一个供选的strategy
########################################################

# 对于符合正态分布的数据，可使用mean（）来进行填充
# 对于偏态数据，使用median（）
#在计算data的skewed分布后，找出outliners，再进行相应的统计分析

#eg:
algae[48,"mxPH"] <- mean(algae$mxPH, na.rm = T)

#对为NA的数据进行统一替换
algae[is.na(algae$Chla), "Chla"] <- median(algae$Chla, na.rm = T)

?centralImputation 

#centralImputation用相应的centrality来填充所有data frame中的NA值，其中的centrality采用函数centralvalue（）计算得到
??centralvalue   ##当变量为numeric时，返回给定数据集的median;为factor时，返回mode; other时，先转换为factor，然后返回mode.

data(algae)
algae <- algae[-manyNAs(algae),]
algae <- centralImputation(algae)
nrow(is.na(algae))


##############################################################
#（3）通过寻找相关关系，对unknown values进行填充
#较好的方法是寻找变量之间的关系
##############################################################
library(DMwR)
data(algae)
cor(algae[,4:18], use = "complete.obs")
#cor函数得到的是变量之间的关系，use="complete.obs"设置去除为NA的观察那些变量

#通过cor函数处理得到的关系中，易读性较差，可采用symnum（）函数进行处理
?symnum   #可视化结构形的matrices,   eg：correlation、sparse或logcial
symnum(cor(algae[,4:18], use = "complete.obs"))

#本例中可以看到变量NH4与NO3（0.72）， PO4与oPO4（above 0.9）之间存在关系
#寻找变量之间的关系
data(algae)
algae <- algae[-manyNAs(algae),] #移除为NA的行  
lm(PO4 ~ oPO4, data = algae)
# 线型关系
#Coefficients:
#(Intercept)         oPO4  
#42.897        1.293  
algae[28, "PO4"] <- 42.897 + 1.293 * algae[28, "oPO4"]

#当为空的数较多时，可写函数来处理
data(algae)
algae <- algae[-manyNAs(algae),]
fillPO4 <- function(oP) {
  if(is.na(oP))
    return(NA)
  else return(42.897 + 1.293 * oP)
}
algae[is.na(algae$PO4), "PO4"] <- sapply(algae[is.na(algae$PO4),"oPO4"], fillPO4)



######################################################
# unknown values与nominal variables的关系
######################################################
#nominal variables时
#使用conditioned histograms达到目标
library(DMwR)
data(algae)
head(algae)
str(algae)
histogram(~algae$mxPH|algae$season, data = algae)
histogram(~mxPH|season, data = algae) #season为factor类型的，统计各level下的mxPH
?histogram
#结果展示有点乱，可通过重新设定来达到结果
algae$season <- factor(algae$season,levels = c("spring","summer","autumn","winter"))

#使用several名义变量
histogram(~mxPH|size*speed, data = algae)

stripplot(size~mxPH|speed, data = algae, jitter = T)
###############################################################
# 适用于dataset中有较少数目的nominal variables的情况
# 有太多的combination的情况下，冗余较多
###############################################################


##########################################################
#（4）探索相似的cases，用于填充unknown values
##########################################################
library(DMwR)
data(algae)
algae <- algae[-manyNAs(algae),]

#计算两个case的距离 ，采用Euclidean距离来计算
?knnImputation  #对于NA值用最近的邻居进行填充

algae <- knnImputation(algae, k = 10)
algae
nrow(algae[!complete.cases(algae),])
rm(algae)


########################################################################
#2.6 获得预测模型
#goals：作预测，发现变量之间的关系
#针对本例中的algae，采用两种模型：
#multiple linear regression and regression trees （回归问题的较好的方法）
# 线性回归不能处理包含NAs的cases
# regression trees可以处理包含NAs的cases
########################################################################
library(DMwR)
algae <- algae[-manyNAs(algae),]
clean.algae <- knnImputation(algae, k = 10)
lm.a1 <- lm(a1~., data = clean.algae[,1:12])
lm.a1$df.residual # 可用于求残差的自由度
summary(lm.a1)
summary(lm.a1)$sigma   # 用于求残差的标准差
summary(lm.a1)$sigma^2  #用于求残差的方差
summary(lm.a1)$df # 用于求自由度
######################################################
# Estimate：估计值
# Std.Error：标准误(样本的标准误) s/n^0.5
# t-value： Estimate/Std.Error
# Pr(>t)：进行检验： H0=0，接受还是拒绝原假设
# Residual standard error：求残差的标准误，可先救出残差的标准差（或方差），及自由度
#拟合优度（Goodness of fit）：可用R-squared来判断，值越大越好
# Multiple R-squared:
# Adjusted R-squared:
# F-statistic：用于判断target variable与任何一个explanatory vairbale是no dependence的
# 即：β1 = β2 = β3....= βm = 0
######################################################


plot(lm.a1)  #可用于理解model performance
#################################################################################
# 其中
# 残差的3个假设【正态性、等方差性、 独立/不相关性】
#（1）正态性：服从~N(0, δ^2) 即sigma-squared
# (2) 同方差性：残差的方差为一个常量 【the variance of the residuals is constant】；
#      若方差为常量 ，则说明residauls为同方差；
#     对应的曲线为一条近似的直线；否则residual为异方差
# (3) 独立性/不相关性：残差之间互不相关
#    【the residuals are not correlated with one another即independent】；，
#     若残差存在相关性， 需对模型进行更改。
#

# Residuals vs Fitted： 残差与拟合值相关性越小越好
# Normal Q-Q: 理论分位数-标准残差， 需符合正态分布    # 
# Scale-location： 标准化的残差-fitted values，若为一条平等于x轴的直线，则说明同方差
# Residuals vs Leverage： 残差间是相关性检验，若存在相关性，需对模型进行调整
#################################################################################




#####################################################################
# simplifying regression models
# backward elimination
#####################################################################
#为了简化模型，
# 使用anova（）函数来对模型进行variance（方差）分析
anova(lm.a1)
#可移除模型中对模型偏差影响最小的项【lest contributes to the reduction of the fitting error of the model】

################################################################
#update函数可用于对已存在的线型模型进行微调
################################################################
lm2.a1 <- update(lm.a1, .~.- season)
summary(lm2.a1)
############################################
# R^2提升不是很大
############################################
anova(lm.a1, lm2.a1)
#############################################
# 使用F-test来分析两模型间的variance
#############################################


######################################################
# 使用backward elimination方法来对lm.a1进行处理
######################################################
final.lm <- step(lm.a1)
###############################################################
# 选取AIC值小的,
# AIC: 【Akaike Information Criterion】避免模型的过度拟合
###############################################################
?step
summary(final.lm)
############################################################################################
# 用该模型解释的variance的比例还不是很明显，
# 通常这种情况下linearity assumption【线性假设】对于这次的问题，是不充分的
############################################################################################




##回归树 Regression Trees 需用到包rpart
install.packages("rpart")
library(rpart)
library(DMwR)
data(algae)
algae <- algae[-manyNAs(algae), ]
rt.a1 <- rpart(a1 ~ ., data = algae[, 1:12])  #自动筛选出最相关的变量，并不会出现所有的变量，
rt.a1
#########################################################################################
# 一个回归对是对解释变量的级层式的logical tests
# 基于树的模型自动地选择出relevant变量， 因此，不是所有的variables都会出现在trees中
# 从root node读取tree，用1来标识【信息： node_number(节点数)  devariance  预测值的平均值】
# 目标变量的在叶子节点处的均值即为tree的prediction
# 
#########################################################################################
?rpart    #rpart函数在初始阶段树是在不断增加的，但当满足一些criteria时，就停止增长（偏离方差低于阀值）

#########################################################
# obtain a graphical representation of the tree
# 可对tree使用plot()和text()
# 为获得graphs with nice setups
# 使用prettyTree()函数
#########################################################
prettyTree(rt.a1) #prettyTree()可对得到的树形结果进行展示
summary(rt.a1)
################################################
# summary可生许多关于trees的许多test信息
################################################

##################################################################################
# 树的生成一般有两步：
# (1) a large tree is grown
# (2) 根据statistcial estimation，删除bottom nodes， 进行减枝【可避免overfitting】
# spurious relatinships
##################################################################################

#############################################################################################################
# rpart()可用于生成树，grows of the tree，只有当某些criteria满足时，才停止
# 停止的条件：
# (1) the decrease in the deviance goes below a certain threshold;
# (2) the number of samples in the nodes is less than another threshold
# (3) the tree depth exceeds another value

# 这些thresholds是通过如下参数控制的：
# cp，minsplit，maxdepth
# cp: complexity parameter
#    【不能降低所有的lack of fit by a factor of cp的split将不会attempt】
#     例如：按anova来split，在每一步中的cp，其R-square必须increase
# minsplit:
# maxdepth: competitor splits在output中，确定哪一个split被选择及接下来的第二个、第三个等是很有用的

# overfitting problem： 需要进行后减枝操作

# rpart包中实现了一个减枝方法, cost complexity pruning
# 该方法使用参数cp用来计算树中的第一个节点
# 减枝方法通过estimate  cp的值，以确保predictive accuracy与树的size的compromise 

# 用rpart（）函数来生成一个一棵树的sub-trees，然后estimate它们的predictive performance 
# 可使用printcp（）函数来获得对应的信息
#############################################################################################################
?printcp
printcp(rt.a1)  #以表格的形式显示出与rpart对象相拟合的cp值

###############################################################
# cp value
# nsplit
# rel value: relative value；相对于根节点来说的
# xerror：
# xstd：

# R的内部使用了了 ten-fold cross-validation
###############################################################

# 评估方法
################################################################################################################
# 选择出rel value最小的，可避免over fitting 问题，本例中最低的estimated relative error（0.67733）
# 或者 根据 1-SE 规则，选择出最佳的tree
#           可以评估cross-validation error estimates('xerror')及它们的standard deviations('xstd'column)
# 本例中1-SE树中最小的树的error ：0.67733 + 0.10892 = 0.78625
# 第2棵树的1 test，其estimated error是0.78625


#若prefer第2棵树而不是R所建议的，可以指定cp value来获得tree
#################################################################################################################
rt2.a1 <- prune(rt.a1, cp = 0.08)
rt2.a1


#####################################################################
# 本书中提供了reparXse（）：可设定se的值，来自动的处理上述过程
# reparXse函数中集成了tree growth与tree post-pruning
# se 默认值为 1，post-pruning过程中使用SE rule中standared errors
# cp 默认为0,控制initial tree的growth的停止criteria
# minsplit 默认值为6， 控制inital tree的growth的stop criterial
# verbose ： function的verbosity
#####################################################################
rt.a1 <- rpartXse(a1 ~ ., data = algae[, 1:12])
rt.a1
?rpartXse

################################################################################################
# interactive pruning of a tree
# 使用 snip.rpart（）
# 分两步：
# （1）indicating the number of the nodes at which your want to prune the tree
#  (2) 
################################################################################################
first.tree <- rpart(a1 ~ ., data = algae[, 1:12])
first.tree
mytree <- snip.rpart(first.tree, c(4, 7))

##############################################
# 交互式地来剪枝树
##############################################
prettyTree(first.tree)
snip.rpart(first.tree)



#########################################################################################
# Model Evaluation and Selection
#########################################################################################

# which one we should use for obtaining the predicitons for the seven algae of 140 test samples
# some perference criteria over the space of possible models


#模型的预测绩效，交互性，计算效率 
#####################################################################
#the predictive performance of regression models   
# 模型的预测值与实际值进行比较，计算出平均的误差，
# eg：the mean absolute error(MAE)


# 还有eg：
# model interpretability
# model computational efficiency
# 均可用于large data mining problems
#####################################################################


###########################################################################
#step1  获得个案中的模型的预测值，使用predict（）函数； 获得了模型的预测值
###########################################################################
lm.predictions.a1 <- predict(final.lm, clean.algae) #因存在缺失值，故使用clean.algae数据框
library(DMwR)
data(algae)
algae
rt.predictions.a1 <- predict(rt.a1, algae) 
rt.predictions.a1

#######################################################################
#step2 误差测量: 计算出它们的平均绝对值误差 mean absolute error
#######################################################################
mae.a1.lm <- mean(abs(lm.predictions.a1 - clean.algae[,'a1']))   
#因linear regression不能处理为NA的数据，故需使用clean.algae这个dataset来计算
mae.a1.lm
mae.a1.rt <- mean(abs(rt.predictions.a1 - algae[, 'a1']))
mae.a1.rt


#######################################################################
#setp3 误差测量的另一种方法：平均误 mean squared error(MSE)
#######################################################################
mse.a1.lm <- mean((lm.predictions.a1 - clean.algae[, 'a1'])^2)
mse.a1.lm
mse.a1.rt <- mean((rt.predictions.a1 - algae[, 'a1'])^2)
mse.a1.rt
###################################################################################################
#上述两种方法不能评估出好坏
# 没有将target variable放在same units进行评估，因此从用户的角度来说，less interpretable

# 另外的一种方法，normalized mean squared error（NMSE）
# 采用nomalized mean squared error(NMSE)可以解决这一问题
###################################################################################################
nmse.a1.lm <- mean((lm.predictions.a1 - clean.algae[,'a1'])^2)/mean((mean(clean.algae[,'a1'])-clean.algae[,'a1'])^2)
nmse.a1.lm
nmse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2)
nmse.a1.rt
##############################################################
# NMSE是一个unit-less error measure，值的范围为[0, 1]
# 若模型比这个simple baseline predictor 预测的更好，则NMSE < 1
# NMSe值越小越好
# NMSE  > 1时，说明model比simple predicting的均值要差
##############################################################


# regr.eval()计算的是评估回归模型统计指标的值【regression evaluation statistics】
?regr.eval

library(DMwR)
regr.eval(algae[,"a1"], rt.predictions.a1, train.y = algae[, "a1"])  #可用于计算出对应的系数
# mae         mse        rmse        mape        nmse        nmae 

##############################################
#可对预测值的误差误差进行可视化
##############################################
old.par <- par(mfrow = c(1,2))
plot(lm.predictions.a1, clean.algae[,'a1'], main = "Linera Model", xlab = "Predictions", ylab = "True Values")
abline(0, 1, lty =2)

plot(rt.predictions.a1, algae[,'a1'], main="Regression Tree", xlab="Predictons", ylab="True Values")
abline(0, 1, lty=2)

par(old.par)


####################################################
# we can check which is the sample number where a 
# particulary bad prediction is made with the funciton
# identify()
####################################################
plot(lm.predictions.a1, clean.algae[,'a1'], main = "Linera Model", xlab = "Predictions", ylab = "True Values")
abline(0, 1, lty =2)
algae[identify(lm.predictions.a1, clean.algae[,'a1']),]


####################################################################################
# 从图中可以看出，
# linear model的predictions中有些为负的，
# 这是没有意义的，predicts > = 0
# 根据业务知识，可使用一个minimum value来improve linear model performance
####################################################################################
sensible.lm.predictins.a1 <- ifelse(lm.predictions.a1 < 0, 0,lm.predictions.a1)
regr.eval(clean.algae[,'a1'], lm.predictions.a1, stats = c('mae', 'mse'))
regr.eval(clean.algae[, 'a1'], sensible.lm.predictins.a1, stats = c('mae', 'mse'))

##########################################################################
# 根据计算结果，可知regression tree计算出的140个samples有较低的NMSE
# 需进一步对于 一个新的test samples，哪个模型更好一些
##########################################################################



##############################################################################################
# 为选择一个model，需获得对于unseen data更reliable estimates结果的models
# k-fold cross-validation（k-fold CV）：是小数据集中，获得reliable estimates较常用的方法
##############################################################################################
# k-折交叉验证
# 定义：先将数据集分为k段，然后取k-1段为训练样本，余下的一个作为验证样本，
# 通过训练样本得到模型，然后用验证样本进行验证，对参数进行调整，将这一过程
# 重复k。所得到的k个参数的均值即为所求。k通常取值为10
# 这样样本既参与了训练，又参与了测试。得到预测误差平方和率最低的参数既为要求的
#   http://blog.sina.com.cn/s/blog_4c98b960010009up.html


####################################################################################################
#作预测的一般过程
#（1）构建可供选择的所有模型
#（2）select the evaluation metrics that will be used to compare the models
#（3）choose the experimental methodology for obtaining reliable estiamtes of these metrics
####################################################################################################


#################################################################################
# DMwR包中提供了experimentComparison（）函数，用于模型的selection/comparison
# train + test + evaluate
#################################################################################
?experimentalComparison
############################################################################
# 对Learning Systems中的experimental comparisons
# experimentalCompariosn中的setts参数可以用来设置采取的实验手段 
# 提供了很多experimetnal methodologies:
# 包括：
# cross validation                【crossValidation】
# leave one out cross validation  【loocv】
# hold-out                        【holdOut】   
# monte carlo simulations         【monteCarlo】
# bootstrap                       【bootstrap】
############################################################################
?resp
# resp： 获得prediction问题中的target variable values，本例中即为algae[,'a1]的值

#######################################################
# learing and testing phase of our models
#######################################################
cv.rpart <- function(form, train, test, ...) {
  m <- rpartXse(form, train, ...)
  p <- predict(m, test)
  mse <- mean((p - resp(form, test))^2)
  c(nmse = mse/mean((mean(resp(form, train))-resp(form, test))^2))
}

cv.lm <- function(form, train, test, ...) {
  m <- lm(form, train, ...)
  p <- predict(m, test)
  p <- ifelse(p < 0, 0, p)
  mse <- mean((p - resp(form, test))^2)
  c(nmse = mse / mean(mean(resp(form, train))-resp(form, test))^2)
}

############################################
# cross-validation comparison
############################################
?dataset
# dataset类中包禽了predictive task中所有的信息，
# 在task类的基础上加入了predictive task的data
?variants


?experimentalComparison
?dataset
?variants
# variants: learning system中不同variants的生成，为learner提供parameters，
# 返回一系列的learner对象，每个都包含variants的不同组合.
res <- experimentalComparison(
  c(dataset(a1 ~ ., clean.algae[, 1:12], 'a1')),
  c(variants('cv.lm'),
    variants('cv.rpart', se = c(0, 0.5, 1))),
  cvSettings(3, 10, 1234))
summary(res)
plot(res)

################################################################
# experimentalComparison（）函数指定了每个model variant一个label
# 查看种个参数的设定，可使用getVariant（）函数
################################################################
?getVariant
getVariant('cv.rpart.v1', res)

######################################################################################
# carry out a similar comparative experiment for all seven prediction
# tasks we are facing at the same time.
######################################################################################
DSs <- sapply(names(clean.algae)[12:18], 
              function(x, names.attrs) {
                f <- as.formula(paste(x, "~ ."))
                dataset(f, clean.algae[, c(names.attrs, x)], x)
              },
              names(clean.algae)[1:11])
DSs


names(clean.algae)[12:18]
names(clean.algae)[1:11]
?sapply
?dataset
?names

res.all <- experimentalComparison(
  DSs,
  c(variants('cv.lm'),
    variants('cv.rpart', se = c(0, 0.5, 1))),
  cvSettings(5, 10, 1234))


plot(res.all)

##################################################
# check which is the best model for each problem,
# 查使用bestScores（）函数
##################################################
bestScores(res.all)

#################################################################################
# output表明： 除了algae1外，其他的结果disappointing
# 结果的variability说明：this might be a good candicate for an ensemble approach
# ensembles：通过生成若干可供选择的modes,然后将它们的predictions进行combining，
# 可以overcome individual models的limitations
#################################################################################
# eg：随机游走森林, randomForest()
library(randomForest)
cv.rf <- function(form, train, test) {
  m <- randomForest(form, train, ...)
  p <- predict(m, test)
  mse <- mean((p - resp(form, test))^2)
  c(nmse = mse/ mean((mean(resp(form, train)) - resp(form, test))^2))
}
?randomForest
#################################################################
# 随机森林不需交叉验证，其结果已近似于k-fold cross validation
# 
#################################################################
?bestScores

res.all <- experimentalComparison(
  DSs,
  c(variants('cv.lm'),
    variants('cv.rpart', se = c(0, 0.5, 1)),
    variants('cv.rf', ntree = c(200, 500, 700))),
  cvSettings(5, 10, 1234))

?variants

?randomForest

bestScores(res.all)
######################################################################################
# bestScores得出值越小越好
# 且bestScores（）并不有反应出these best models与alternatives 之间的显著统计性
# 用另外一个random sample，可以相同的confidence得出结果么？
# compAnalysis（）提供了相应的信息
# 该函数是在model与alternatives之间进行Wilcoxon tests
######################################################################################
compAnalysis(res.all, against = 'cv.rf.v3', datasets = c('a1', 'a2', 'a4', 'a6'))


###############################################
# Predictions for the Seven Algae
###############################################
# 最优模型的选择：获得模型的NMSE的无偏估计
# main goal： 根据140个test samples获得7个predictions


##################################################################
# regression trees： 包含了处理unknown values的方法
##################################################################

####  obtain seven models
bestModelsNames <- sapply(bestScores(res.all),
                          function(x) x['nmse', 'system'])

learners <- c(rf = 'randomForest', rpart = 'rpartXse')
funcs <- learners[sapply(strsplit(bestModelsNames, '\\.'), function(x) x[2])]
parSetts <- lapply(bestModelsNames,
                   function(x) getVariant(x, res.all)@pars)


#####################################################
# strsplit分隔字符串
#####################################################
?strsplit
strsplit(bestModelsNames, '\\.') 


bestModels <- list()
for(a in 1:7) {
  form <- as.formula(paste(names(clean.algae)[11+a], '~ .'))
  c(list(form, clean.algae[, c(1:11, 11+a)]), parSetts[[a]])
}




# 使用experimentalComparison对各模型进行对比
# bestScores可用于获得对比实验中最好的结果
??bestScores
# randomForest中的randomForest()函数，
# compAnalysis（）可检测出下一个随机数据，可以多大的置信提供相同的结果；
# compAnalysi（）中包含了一系列的模型与可供选择方案之间的Wilcoxon检验



rm(list = ls())

















