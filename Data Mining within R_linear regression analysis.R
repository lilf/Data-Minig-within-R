# Chapter2
###################################################
# Linear Regression
# Regression Tree
# k-fold cross validation

# Random Forest (�ں���k-fold������֤)
###################################################
R.version
install.packages('DMwR')
install.packages('plyr')
install.packages('RODBC')
library(DMwR)
head(algae)
str(algae)
#eg

#��һ��
#ȱ����Ϊȫ�����Ϣ����������һ�����ݼ���statistical properities
summary(algae)

#�鿴���ݵķֲ�����̬��ƫ̬����ȣ��Ͷ�
#��ȣ���ʾ�����ܶȷֲ�������ƽ��ֵ����ֵ�ߵ͵�����������ӳ��β���ĺ�ȣ�����������ֲ���̬�Ķ����̶ȣ����Ϊ3��ʾ����̬�ֲ���ͬ�����С�ڳ���ʾ��̬�ֲ����ͣ�����3��ʾ����̬�ֲ�ƽ̹��
#�Ͷȣ���Ӧ�񶯷ֲ����Ե���ֵͳ��������k=3ʱ�ֲ����߾���������ȼ����Ͷȣ���k>3ʱ���ֲ����߾������Ͷȣ���k<3ʱ���ֲ����߾��и��Ͷȡ�
hist(algae$mxPH, prob = T)  #prob = Tʱ��������ͼ��Ƶ�ʷֲ�����ΪFʱ����Ƶ���ֲ�
#�ֲ�������̬�ֲ������һ����֤��ʹ��normal Q-Q plots
#qq.plot()��car����
install.packages("car")
library(car)

#�ܶ�������Q-Qͼ
par(mfrow=c(1,2))
hist(algae$mxPH, prob  = T, xlab='', main = "Histogram of maximum pH value", ylim = 0:1) #��histͼ,prob = Tʱ��Ϊ���ʷֲ�ͼ������ΪƵ�ηֲ�ͼ
lines(density(algae$mxPH, na.rm = T))  #����һ���ܶ�����
rug(jitter(algae$mxPH))   # ��plot��x������һ����rug
?rug   
?jitter
qq.plot(algae$mxPH, main="Normal QQ plot of maximum pH")
par(mfrow=c(1,1))


#algae�е�oPO4
str(algae$oPO4)
#���ٻ�÷ֲ����
boxplot(algae$oPO4, ylab="Orthophosphate(oPO4)")
#���������ϻ�������λ��
rug(jitter(algae$oPO4), side = 2)
abline(h = mean(algae$oPO4, na.rm = T), lty = 2)

#�����Ĵ���
plot(algae$NH4, xlab = "")
abline(h = mean(algae$NH4, na.rm = T), lty = 1)
abline(h = mean(algae$NH4, na.rm = T) + sd(algae$NH4, na.rm = T), lty = 2)
abline(h = median(algae$NH4, na.rm = T), lty = 3)
identify(algae$NH4)   #��������������

plot(algae$NH4, xlab = "")
clicked.lines <- identify(algae$NH4)
algae[clicked.lines, ]

algae[algae$NH4 > 19000, ]
algae[!is.na(algae$NH4) & algae$NH4 > 19000,] #ȡalgae$NH4�зǿ��Ҵ���19000�ļ�¼


###############################################################################
# ����factor���͵�variable����response������Ӱ�죬��ͼ��������˵
###############################################################################
library(lattice)
bwplot(size ~ a1, data = algae, ylab='River Size', xlab='Algal A1')  # plot a1 for each value of size
??bwplot

install.packages("Hmisc")
library(Hmisc)
bwplot(size ~ a1, data = algae, panel = panel.bpplot, probs = seq(.01, .49, by = .01), datadensity = TRUE, ylab = 'River Size', xlab = 'Algal A1')


######################################################
# ��factor���͵����ݽ�����ɢ������������continuous����
######################################################
#equal.count()���ֺ������������ݻ���Ϊ���ɸ����䣬����ָ������ĸ����Լ�����֮����ص��̶�
?stripplot
library(Hmisc)
minO2 <- equal.count(na.omit(algae$mnO2), number = 4, overlap=1/5)
stripplot(season ~ a3|minO2, data = algae[!is.na(algae$mnO2),])



#######################################################
#Unknown Values
#######################################################

########################################################
#����������
# (1)ֱ��ɾ��
# (2)Ѱ��unknown��������֮��Ĺ�ϵ
# (3)�����������Ƶİ����������
# (4)ʹ���ܹ�����ȱʧֵ�Ĺ���
########################################################
#(1)ֱ��ɾ��
library(DMwR)
data(algae)
nrow(algae[!complete.cases(algae),])  #����Ϊ�յļ�¼������ 
# complete.cases   �ж�cases�Ƿ���missing values
algae <- na.omit(algae) #���˵�Ϊ�յļ�¼
?na.omit
#####################################################################################
# ����objects�е�missing values
# na.fail: ��object�������κ�missing values���򷵻�object;���򷵻�һ��֪ʶ��Ϣ
# na.omit�������Ƴ���missing values��object���objects
# na.pass����missing values�������κδ������������е�objects
#####################################################################################
algae <- algae[-c(62,199),] #����62��199�ļ�¼
algae
apply(algae, 1, function (x) sum(is.na(x)))  #ͳ��ÿ�м�¼��Ϊna����Ŀ 

# ��Ҫ������Щ�н϶�NAs����


library(DMwR)
data(algae)
?manyNAs
############################################################
# manyNAs()���dataframe���нϸ�������unknown values���к�
# nORp�����趨ǰ����ǰ���ٵ��к�
############################################################
manyNAs(algae, 0.2) 

########################################
# ȥ��algae���н϶�NAs����
########################################
algae <- algae[-manyNAs(algae), ]


################################################
#��2�������滻
# ʹ�ó�����ΪƵ����values��Unknowns�������
################################################
#������ܵ�Values��unknowns�������

########################################################
# ��򵥡����ٵķ�����ʹ��ͳ���еļ�������ָ��������
# statistic of centrality eg��mean��median��mode,etc  ���ã�
# ��Ϊ���õķ������ٶȽϣ�ֻ��һ����ѡ��strategy
########################################################

# ���ڷ�����̬�ֲ������ݣ���ʹ��mean�������������
# ����ƫ̬���ݣ�ʹ��median����
#�ڼ���data��skewed�ֲ����ҳ�outliners���ٽ�����Ӧ��ͳ�Ʒ���

#eg:
algae[48,"mxPH"] <- mean(algae$mxPH, na.rm = T)

#��ΪNA�����ݽ���ͳһ�滻
algae[is.na(algae$Chla), "Chla"] <- median(algae$Chla, na.rm = T)

?centralImputation 

#centralImputation����Ӧ��centrality���������data frame�е�NAֵ�����е�centrality���ú���centralvalue��������õ�
??centralvalue   ##������Ϊnumericʱ�����ظ������ݼ���median;Ϊfactorʱ������mode; otherʱ����ת��Ϊfactor��Ȼ�󷵻�mode.

data(algae)
algae <- algae[-manyNAs(algae),]
algae <- centralImputation(algae)
nrow(is.na(algae))


##############################################################
#��3��ͨ��Ѱ����ع�ϵ����unknown values�������
#�Ϻõķ�����Ѱ�ұ���֮��Ĺ�ϵ
##############################################################
library(DMwR)
data(algae)
cor(algae[,4:18], use = "complete.obs")
#cor�����õ����Ǳ���֮��Ĺ�ϵ��use="complete.obs"����ȥ��ΪNA�Ĺ۲���Щ����

#ͨ��cor���������õ��Ĺ�ϵ�У��׶��Խϲ�ɲ���symnum�����������д���
?symnum   #���ӻ��ṹ�ε�matrices,   eg��correlation��sparse��logcial
symnum(cor(algae[,4:18], use = "complete.obs"))

#�����п��Կ�������NH4��NO3��0.72���� PO4��oPO4��above 0.9��֮����ڹ�ϵ
#Ѱ�ұ���֮��Ĺ�ϵ
data(algae)
algae <- algae[-manyNAs(algae),] #�Ƴ�ΪNA����  
lm(PO4 ~ oPO4, data = algae)
# ���͹�ϵ
#Coefficients:
#(Intercept)         oPO4  
#42.897        1.293  
algae[28, "PO4"] <- 42.897 + 1.293 * algae[28, "oPO4"]

#��Ϊ�յ����϶�ʱ����д����������
data(algae)
algae <- algae[-manyNAs(algae),]
fillPO4 <- function(oP) {
  if(is.na(oP))
    return(NA)
  else return(42.897 + 1.293 * oP)
}
algae[is.na(algae$PO4), "PO4"] <- sapply(algae[is.na(algae$PO4),"oPO4"], fillPO4)



######################################################
# unknown values��nominal variables�Ĺ�ϵ
######################################################
#nominal variablesʱ
#ʹ��conditioned histograms�ﵽĿ��
library(DMwR)
data(algae)
head(algae)
str(algae)
histogram(~algae$mxPH|algae$season, data = algae)
histogram(~mxPH|season, data = algae) #seasonΪfactor���͵ģ�ͳ�Ƹ�level�µ�mxPH
?histogram
#���չʾ�е��ң���ͨ�������趨���ﵽ���
algae$season <- factor(algae$season,levels = c("spring","summer","autumn","winter"))

#ʹ��several�������
histogram(~mxPH|size*speed, data = algae)

stripplot(size~mxPH|speed, data = algae, jitter = T)
###############################################################
# ������dataset���н�����Ŀ��nominal variables�����
# ��̫���combination������£�����϶�
###############################################################


##########################################################
#��4��̽�����Ƶ�cases���������unknown values
##########################################################
library(DMwR)
data(algae)
algae <- algae[-manyNAs(algae),]

#��������case�ľ��� ������Euclidean����������
?knnImputation  #����NAֵ��������ھӽ������

algae <- knnImputation(algae, k = 10)
algae
nrow(algae[!complete.cases(algae),])
rm(algae)


########################################################################
#2.6 ���Ԥ��ģ��
#goals����Ԥ�⣬���ֱ���֮��Ĺ�ϵ
#��Ա����е�algae����������ģ�ͣ�
#multiple linear regression and regression trees ���ع�����ĽϺõķ�����
# ���Իع鲻�ܴ�������NAs��cases
# regression trees���Դ�������NAs��cases
########################################################################
library(DMwR)
algae <- algae[-manyNAs(algae),]
clean.algae <- knnImputation(algae, k = 10)
lm.a1 <- lm(a1~., data = clean.algae[,1:12])
lm.a1$df.residual # ��������в�����ɶ�
summary(lm.a1)
summary(lm.a1)$sigma   # ������в�ı�׼��
summary(lm.a1)$sigma^2  #������в�ķ���
summary(lm.a1)$df # ���������ɶ�
######################################################
# Estimate������ֵ
# Std.Error����׼��(�����ı�׼��) s/n^0.5
# t-value�� Estimate/Std.Error
# Pr(>t)�����м��飺 H0=0�����ܻ��Ǿܾ�ԭ����
# Residual standard error����в�ı�׼�󣬿��Ⱦȳ��в�ı�׼��򷽲�������ɶ�
#����Ŷȣ�Goodness of fit��������R-squared���жϣ�ֵԽ��Խ��
# Multiple R-squared:
# Adjusted R-squared:
# F-statistic�������ж�target variable���κ�һ��explanatory vairbale��no dependence��
# ������1 = ��2 = ��3....= ��m = 0
######################################################


plot(lm.a1)  #����������model performance
#################################################################################
# ����
# �в��3�����衾��̬�ԡ��ȷ����ԡ� ����/������ԡ�
#��1����̬�ԣ�����~N(0, ��^2) ��sigma-squared
# (2) ͬ�����ԣ��в�ķ���Ϊһ������ ��the variance of the residuals is constant����
#      ������Ϊ���� ����˵��residaulsΪͬ���
#     ��Ӧ������Ϊһ�����Ƶ�ֱ�ߣ�����residualΪ�췽��
# (3) ������/������ԣ��в�֮�以�����
#    ��the residuals are not correlated with one another��independent������
#     ���в��������ԣ� ���ģ�ͽ��и��ġ�
#

# Residuals vs Fitted�� �в������ֵ�����ԽСԽ��
# Normal Q-Q: ���۷�λ��-��׼�в �������̬�ֲ�    # 
# Scale-location�� ��׼���Ĳв�-fitted values����Ϊһ��ƽ����x���ֱ�ߣ���˵��ͬ����
# Residuals vs Leverage�� �в��������Լ��飬����������ԣ����ģ�ͽ��е���
#################################################################################




#####################################################################
# simplifying regression models
# backward elimination
#####################################################################
#Ϊ�˼�ģ�ͣ�
# ʹ��anova������������ģ�ͽ���variance���������
anova(lm.a1)
#���Ƴ�ģ���ж�ģ��ƫ��Ӱ����С���lest contributes to the reduction of the fitting error of the model��

################################################################
#update���������ڶ��Ѵ��ڵ�����ģ�ͽ���΢��
################################################################
lm2.a1 <- update(lm.a1, .~.- season)
summary(lm2.a1)
############################################
# R^2�������Ǻܴ�
############################################
anova(lm.a1, lm2.a1)
#############################################
# ʹ��F-test��������ģ�ͼ��variance
#############################################


######################################################
# ʹ��backward elimination��������lm.a1���д���
######################################################
final.lm <- step(lm.a1)
###############################################################
# ѡȡAICֵС��,
# AIC: ��Akaike Information Criterion������ģ�͵Ĺ������
###############################################################
?step
summary(final.lm)
############################################################################################
# �ø�ģ�ͽ��͵�variance�ı��������Ǻ����ԣ�
# ͨ�����������linearity assumption�����Լ��衿������ε����⣬�ǲ���ֵ�
############################################################################################




##�ع��� Regression Trees ���õ���rpart
install.packages("rpart")
library(rpart)
library(DMwR)
data(algae)
algae <- algae[-manyNAs(algae), ]
rt.a1 <- rpart(a1 ~ ., data = algae[, 1:12])  #�Զ�ɸѡ������صı�����������������еı�����
rt.a1
#########################################################################################
# һ���ع���ǶԽ��ͱ����ļ���ʽ��logical tests
# ��������ģ���Զ���ѡ���relevant������ ��ˣ��������е�variables���������trees��
# ��root node��ȡtree����1����ʶ����Ϣ�� node_number(�ڵ���)  devariance  Ԥ��ֵ��ƽ��ֵ��
# Ŀ���������Ҷ�ӽڵ㴦�ľ�ֵ��Ϊtree��prediction
# 
#########################################################################################
?rpart    #rpart�����ڳ�ʼ�׶������ڲ������ӵģ���������һЩcriteriaʱ����ֹͣ������ƫ�뷽����ڷ�ֵ��

#########################################################
# obtain a graphical representation of the tree
# �ɶ�treeʹ��plot()��text()
# Ϊ���graphs with nice setups
# ʹ��prettyTree()����
#########################################################
prettyTree(rt.a1) #prettyTree()�ɶԵõ������ν������չʾ
summary(rt.a1)
################################################
# summary�����������trees������test��Ϣ
################################################

##################################################################################
# ��������һ����������
# (1) a large tree is grown
# (2) ����statistcial estimation��ɾ��bottom nodes�� ���м�֦���ɱ���overfitting��
# spurious relatinships
##################################################################################

#############################################################################################################
# rpart()��������������grows of the tree��ֻ�е�ĳЩcriteria����ʱ����ֹͣ
# ֹͣ��������
# (1) the decrease in the deviance goes below a certain threshold;
# (2) the number of samples in the nodes is less than another threshold
# (3) the tree depth exceeds another value

# ��Щthresholds��ͨ�����²������Ƶģ�
# cp��minsplit��maxdepth
# cp: complexity parameter
#    �����ܽ������е�lack of fit by a factor of cp��split������attempt��
#     ���磺��anova��split����ÿһ���е�cp����R-square����increase
# minsplit:
# maxdepth: competitor splits��output�У�ȷ����һ��split��ѡ�񼰽������ĵڶ��������������Ǻ����õ�

# overfitting problem�� ��Ҫ���к��֦����

# rpart����ʵ����һ����֦����, cost complexity pruning
# �÷���ʹ�ò���cp�����������еĵ�һ���ڵ�
# ��֦����ͨ��estimate  cp��ֵ����ȷ��predictive accuracy������size��compromise 

# ��rpart��������������һ��һ������sub-trees��Ȼ��estimate���ǵ�predictive performance 
# ��ʹ��printcp������������ö�Ӧ����Ϣ
#############################################################################################################
?printcp
printcp(rt.a1)  #�Ա������ʽ��ʾ����rpart��������ϵ�cpֵ

###############################################################
# cp value
# nsplit
# rel value: relative value������ڸ��ڵ���˵��
# xerror��
# xstd��

# R���ڲ�ʹ������ ten-fold cross-validation
###############################################################

# ��������
################################################################################################################
# ѡ���rel value��С�ģ��ɱ���over fitting ���⣬��������͵�estimated relative error��0.67733��
# ���� ���� 1-SE ����ѡ�����ѵ�tree
#           ��������cross-validation error estimates('xerror')�����ǵ�standard deviations('xstd'column)
# ������1-SE������С������error ��0.67733 + 0.10892 = 0.78625
# ��2������1 test����estimated error��0.78625


#��prefer��2����������R������ģ�����ָ��cp value�����tree
#################################################################################################################
rt2.a1 <- prune(rt.a1, cp = 0.08)
rt2.a1


#####################################################################
# �������ṩ��reparXse���������趨se��ֵ�����Զ��Ĵ�����������
# reparXse�����м�����tree growth��tree post-pruning
# se Ĭ��ֵΪ 1��post-pruning������ʹ��SE rule��standared errors
# cp Ĭ��Ϊ0,����initial tree��growth��ֹͣcriteria
# minsplit Ĭ��ֵΪ6�� ����inital tree��growth��stop criterial
# verbose �� function��verbosity
#####################################################################
rt.a1 <- rpartXse(a1 ~ ., data = algae[, 1:12])
rt.a1
?rpartXse

################################################################################################
# interactive pruning of a tree
# ʹ�� snip.rpart����
# ��������
# ��1��indicating the number of the nodes at which your want to prune the tree
#  (2) 
################################################################################################
first.tree <- rpart(a1 ~ ., data = algae[, 1:12])
first.tree
mytree <- snip.rpart(first.tree, c(4, 7))

##############################################
# ����ʽ������֦��
##############################################
prettyTree(first.tree)
snip.rpart(first.tree)



#########################################################################################
# Model Evaluation and Selection
#########################################################################################

# which one we should use for obtaining the predicitons for the seven algae of 140 test samples
# some perference criteria over the space of possible models


#ģ�͵�Ԥ�⼨Ч�������ԣ�����Ч�� 
#####################################################################
#the predictive performance of regression models   
# ģ�͵�Ԥ��ֵ��ʵ��ֵ���бȽϣ������ƽ������
# eg��the mean absolute error(MAE)


# ����eg��
# model interpretability
# model computational efficiency
# ��������large data mining problems
#####################################################################


###########################################################################
#step1  ��ø����е�ģ�͵�Ԥ��ֵ��ʹ��predict���������� �����ģ�͵�Ԥ��ֵ
###########################################################################
lm.predictions.a1 <- predict(final.lm, clean.algae) #�����ȱʧֵ����ʹ��clean.algae���ݿ�
library(DMwR)
data(algae)
algae
rt.predictions.a1 <- predict(rt.a1, algae) 
rt.predictions.a1

#######################################################################
#step2 ������: ��������ǵ�ƽ������ֵ��� mean absolute error
#######################################################################
mae.a1.lm <- mean(abs(lm.predictions.a1 - clean.algae[,'a1']))   
#��linear regression���ܴ���ΪNA�����ݣ�����ʹ��clean.algae���dataset������
mae.a1.lm
mae.a1.rt <- mean(abs(rt.predictions.a1 - algae[, 'a1']))
mae.a1.rt


#######################################################################
#setp3 ����������һ�ַ�����ƽ���� mean squared error(MSE)
#######################################################################
mse.a1.lm <- mean((lm.predictions.a1 - clean.algae[, 'a1'])^2)
mse.a1.lm
mse.a1.rt <- mean((rt.predictions.a1 - algae[, 'a1'])^2)
mse.a1.rt
###################################################################################################
#�������ַ��������������û�
# û�н�target variable����same units������������˴��û��ĽǶ���˵��less interpretable

# �����һ�ַ�����normalized mean squared error��NMSE��
# ����nomalized mean squared error(NMSE)���Խ����һ����
###################################################################################################
nmse.a1.lm <- mean((lm.predictions.a1 - clean.algae[,'a1'])^2)/mean((mean(clean.algae[,'a1'])-clean.algae[,'a1'])^2)
nmse.a1.lm
nmse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2)
nmse.a1.rt
##############################################################
# NMSE��һ��unit-less error measure��ֵ�ķ�ΧΪ[0, 1]
# ��ģ�ͱ����simple baseline predictor Ԥ��ĸ��ã���NMSE < 1
# NMSeֵԽСԽ��
# NMSE  > 1ʱ��˵��model��simple predicting�ľ�ֵҪ��
##############################################################


# regr.eval()������������ع�ģ��ͳ��ָ���ֵ��regression evaluation statistics��
?regr.eval

library(DMwR)
regr.eval(algae[,"a1"], rt.predictions.a1, train.y = algae[, "a1"])  #�����ڼ������Ӧ��ϵ��
# mae         mse        rmse        mape        nmse        nmae 

##############################################
#�ɶ�Ԥ��ֵ����������п��ӻ�
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
# ��ͼ�п��Կ�����
# linear model��predictions����ЩΪ���ģ�
# ����û������ģ�predicts > = 0
# ����ҵ��֪ʶ����ʹ��һ��minimum value��improve linear model performance
####################################################################################
sensible.lm.predictins.a1 <- ifelse(lm.predictions.a1 < 0, 0,lm.predictions.a1)
regr.eval(clean.algae[,'a1'], lm.predictions.a1, stats = c('mae', 'mse'))
regr.eval(clean.algae[, 'a1'], sensible.lm.predictins.a1, stats = c('mae', 'mse'))

##########################################################################
# ���ݼ���������֪regression tree�������140��samples�нϵ͵�NMSE
# ���һ������ һ���µ�test samples���ĸ�ģ�͸���һЩ
##########################################################################



##############################################################################################
# Ϊѡ��һ��model�����ö���unseen data��reliable estimates�����models
# k-fold cross-validation��k-fold CV������С���ݼ��У����reliable estimates�ϳ��õķ���
##############################################################################################
# k-�۽�����֤
# ���壺�Ƚ����ݼ���Ϊk�Σ�Ȼ��ȡk-1��Ϊѵ�����������µ�һ����Ϊ��֤������
# ͨ��ѵ�������õ�ģ�ͣ�Ȼ������֤����������֤���Բ������е���������һ����
# �ظ�k�����õ���k�������ľ�ֵ��Ϊ����kͨ��ȡֵΪ10
# ���������Ȳ�����ѵ�����ֲ����˲��ԡ��õ�Ԥ�����ƽ��������͵Ĳ�����ΪҪ���
#   http://blog.sina.com.cn/s/blog_4c98b960010009up.html


####################################################################################################
#��Ԥ���һ�����
#��1�������ɹ�ѡ�������ģ��
#��2��select the evaluation metrics that will be used to compare the models
#��3��choose the experimental methodology for obtaining reliable estiamtes of these metrics
####################################################################################################


#################################################################################
# DMwR�����ṩ��experimentComparison��������������ģ�͵�selection/comparison
# train + test + evaluate
#################################################################################
?experimentalComparison
############################################################################
# ��Learning Systems�е�experimental comparisons
# experimentalCompariosn�е�setts���������������ò�ȡ��ʵ���ֶ� 
# �ṩ�˺ܶ�experimetnal methodologies:
# ������
# cross validation                ��crossValidation��
# leave one out cross validation  ��loocv��
# hold-out                        ��holdOut��   
# monte carlo simulations         ��monteCarlo��
# bootstrap                       ��bootstrap��
############################################################################
?resp
# resp�� ���prediction�����е�target variable values�������м�Ϊalgae[,'a1]��ֵ

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
# dataset���а�����predictive task�����е���Ϣ��
# ��task��Ļ����ϼ�����predictive task��data
?variants


?experimentalComparison
?dataset
?variants
# variants: learning system�в�ͬvariants�����ɣ�Ϊlearner�ṩparameters��
# ����һϵ�е�learner����ÿ��������variants�Ĳ�ͬ���.
res <- experimentalComparison(
  c(dataset(a1 ~ ., clean.algae[, 1:12], 'a1')),
  c(variants('cv.lm'),
    variants('cv.rpart', se = c(0, 0.5, 1))),
  cvSettings(3, 10, 1234))
summary(res)
plot(res)

################################################################
# experimentalComparison��������ָ����ÿ��model variantһ��label
# �鿴�ָ��������趨����ʹ��getVariant��������
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
# ��ʹ��bestScores��������
##################################################
bestScores(res.all)

#################################################################################
# output������ ����algae1�⣬�����Ľ��disappointing
# �����variability˵����this might be a good candicate for an ensemble approach
# ensembles��ͨ���������ɿɹ�ѡ���modes,Ȼ�����ǵ�predictions����combining��
# ����overcome individual models��limitations
#################################################################################
# eg���������ɭ��, randomForest()
library(randomForest)
cv.rf <- function(form, train, test) {
  m <- randomForest(form, train, ...)
  p <- predict(m, test)
  mse <- mean((p - resp(form, test))^2)
  c(nmse = mse/ mean((mean(resp(form, train)) - resp(form, test))^2))
}
?randomForest
#################################################################
# ���ɭ�ֲ��轻����֤�������ѽ�����k-fold cross validation
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
# bestScores�ó�ֵԽСԽ��
# ��bestScores���������з�Ӧ��these best models��alternatives ֮�������ͳ����
# ������һ��random sample��������ͬ��confidence�ó����ô��
# compAnalysis�����ṩ����Ӧ����Ϣ
# �ú�������model��alternatives֮�����Wilcoxon tests
######################################################################################
compAnalysis(res.all, against = 'cv.rf.v3', datasets = c('a1', 'a2', 'a4', 'a6'))


###############################################
# Predictions for the Seven Algae
###############################################
# ����ģ�͵�ѡ�񣺻��ģ�͵�NMSE����ƫ����
# main goal�� ����140��test samples���7��predictions


##################################################################
# regression trees�� �����˴���unknown values�ķ���
##################################################################

####  obtain seven models
bestModelsNames <- sapply(bestScores(res.all),
                          function(x) x['nmse', 'system'])

learners <- c(rf = 'randomForest', rpart = 'rpartXse')
funcs <- learners[sapply(strsplit(bestModelsNames, '\\.'), function(x) x[2])]
parSetts <- lapply(bestModelsNames,
                   function(x) getVariant(x, res.all)@pars)


#####################################################
# strsplit�ָ��ַ���
#####################################################
?strsplit
strsplit(bestModelsNames, '\\.') 


bestModels <- list()
for(a in 1:7) {
  form <- as.formula(paste(names(clean.algae)[11+a], '~ .'))
  c(list(form, clean.algae[, c(1:11, 11+a)]), parSetts[[a]])
}




# ʹ��experimentalComparison�Ը�ģ�ͽ��жԱ�
# bestScores�����ڻ�öԱ�ʵ������õĽ��
??bestScores
# randomForest�е�randomForest()������
# compAnalysis�����ɼ�����һ��������ݣ����Զ��������ṩ��ͬ�Ľ����
# compAnalysi�����а�����һϵ�е�ģ����ɹ�ѡ�񷽰�֮���Wilcoxon����



rm(list = ls())
















