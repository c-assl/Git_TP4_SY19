#setwd("/Users/cecileasselin/Desktop/SY19/TD4/")
rm(list=ls())

############# CLASSIFICATION ####################

#classif <- read.table(file='./Data/TPN1_a22_clas_app.txt', header = TRUE)
classif <- read.table(file='TPN1_a22_clas_app.txt', header = TRUE)
head(classif) # données quantitatives de X1 à X45, données qualitatives X46 à X50, y est la donnée de classe à prédire (3 classes différentes)
summary(classif)
# besoin nettoyer les données ? je ne pense pas 

###### 0. Separation des données en test et train
n <- nrow(classif)
p <- ncol(classif)-1
nb.train <- round(2*n/3) 
nb.test <- n - nb.train

# We declare the class variable as a factor (because it is a (qualitative) category variable) 
classif$y<-as.factor(classif$y)
classif$y
# seed
set.seed(1729) # the Hardy–Ramanujan number

# Training/Testing data
train <- sample(1:n, nb.train) 
classif.train <- classif[train,] 
classif.test <- classif[-train,]


######## 1. ADL
# install.packages(MASS)
library(MASS)

lda.classif <- lda(y~., data=classif.train)
pred.classif.lda <- predict(lda.classif, newdata=classif.test)
summary(pred.classif.lda)

# calcul du taux d'erreur de ma prédiction : on est à 0.491018
err.lda <- mean(pred.classif.lda$class!=classif.test$y)
err.lda
# autre manière de calculer erreur empirique  
# matrix.conf.lda <- table(classif.test$y, pred.classif.lda$class) 
# matrix.conf.lda
# err.lda <- 1-sum(diag(matrix.conf.lda))/nb.test 
# err.lda


####### 2. NB 
library(naivebayes)
fit.naive <- naive_bayes(y~.,data=classif.train) 
pred.naive <- predict(fit.naive,newdata=classif.test)  ###Warning message:
########predict.naive_bayes(): more features in the newdata are provided as there are probability tables in the object. Calculation is performed based on features to be found in the tables. 
err.naive <- mean(pred.naive != classif.test$y) # on est à 0.3532934，meilleurs résultats qu'avec ADL

# ne pas oublier de vérifier l'hypothèse forte d’indépendance entre les prédicteurs faite ac NB (i.e. ???????? ??? ????????)
# quel test faire ?
##  qqPlot()要求用lm()拟合。

# Autre manière de calculer l'erreur
# conf.naive <- table(classif.test$y,pred.naive) 
# conf.naive
# err.naive <- 1-sum(diag(conf.naive))/nb.test 
# err.naive

####### 3. ADQ
# on n'a peut être pas assez de données pour faire une ADQ correcte (si besoin donner plus de données à l'ensemble d'apprentissage, et faire cross-validation)
fit.qda <- qda(y~.,data=classif.train)
pred.qda <- predict(fit.qda,newdata=classif.test) 
err.qda <- mean(classif.test$y != pred.qda$class) #0.3473054

###### 4. Comparaison ADL / NB / ADQ ###
library(pROC)
roc.lda <- roc(classif.test$y,as.vector(pred.classif.lda$posterior[,1])) 
plot(roc.lda,col="blue")

pred.nb.prob <- predict(fit.naive, newdata=classif.test, type="prob") # COMPRENDRE POURQUOI ON DOIT REFAIRE CETTE PREDICTION AVEC TYPE = PROB
roc.nb <- roc(classif.test$y, as.vector(pred.nb.prob[,1]))
#####Warning message:
######In roc.default(classif.test$y, as.vector(pred.classif.lda$posterior[,  :
###'response' has more than two levels. Consider setting 'levels' explicitly or using 'multiclass.roc' instead
                                                                    
plot(roc.nb, col='red') 
plot(roc.lda, add=TRUE)

# Problème : je n'arrive x à tracer ma ROC pour l'ADQ
roc.qda <- roc(classif.test$y,as.vector(pred.qda$posterior[,1])) 
plot(roc.qda, col='blue', add=TRUE)
legend("bottomright",legend=c("LDA","NB","QDA"),col=c("red","black","blue"),lty=1:2)

##### 5.KNN
# Loading package
library(e1071)
library(caTools)
library(class)

# Feature Scaling
train_scale <- scale(classif.train[, 1:45])
test_scale <- scale(classif.test[, 1:45])

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = classif.train$y,
                      k = 1)
classifier_knn

# Confusion Matrix
cm <- table(classif.test$y, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != classif.test$y)
print(paste('Accuracy =', 1-misClassError))
err.knn <- misClassError

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = classif.train$y,
                      k = 3)
misClassError <- mean(classifier_knn != classif.test$y)
print(paste('Accuracy =', 1-misClassError))
err.knn <- min(err.knn, misClassError)

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = classif.train$y,
                      k = 5)
misClassError <- mean(classifier_knn != classif.test$y)
print(paste('Accuracy =', 1-misClassError))
err.knn <- min(err.knn, misClassError)

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = classif.train$y,
                      k = 7)
misClassError <- mean(classifier_knn != classif.test$y)
print(paste('Accuracy =', 1-misClassError))
err.knn <- min(err.knn, misClassError)

# K = 15
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = classif.train$y,
                      k = 15)
misClassError <- mean(classifier_knn != classif.test$y)
print(paste('Accuracy =', 1-misClassError))
err.knn <- min(err.knn, misClassError)

# K = 19
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = classif.train$y,
                      k = 19)
misClassError <- mean(classifier_knn != classif.test$y)
print(paste('Accuracy =', 1-misClassError))
err.knn <- min(err.knn, misClassError)

for(k in 1:30){
  classifier_knn <- knn(train = train_scale,
                        test = test_scale,
                        cl = classif.train$y,
                        k = k)
  misClassError <- mean(classifier_knn != classif.test$y)
  print(paste(k, '- Accuracy =', 1-misClassError))
  err.knn <- min(err.knn, misClassError)
  #plot(k,1-misClassError)
}
print(paste('maxAccuracy =', 1-err.knn))


### Reste à faire : 
# il faudrait peut être normaliser nos données dès le départ (ds ts cas )pour l'instant je ne l'ai fait que pour KNN à la fin)
# faire des tests d'hypothèse pour NB, ADL & ADQ
# on pourrait aussi passer à la validation croisée K-fold
# essayer la regression logistique
# selection de modèle: subset selec / regularizat° / feature extract° --> ACP

#### tentative subset selection (attention je ne sais pas si c'est censé fonctionner avec de la classification)
# install.packages("leaps")
# library(leaps)   #function to search for the best model
# reg.fit<-regsubsets(y ~.,data=classif.train) # method='exhaustive' => trop lent, & jsp si je garde nvmax ou x
# plot(reg.fit,scale="r2")




### multinomial logistic regression
## https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
## https://zhuanlan.zhihu.com/p/266664085
install.packages("nnet")
library(nnet)

#数据准备
#classif.train et classif.test
#多元分类模型构建
##因子的级别被重新排序，以便由ref指定的级别首先显示，其余级别下移。
##ref : 向量、列表或数据帧、数组、矩阵或表。如果ref是一个向量（整数或字符），假设它包含要作为第一个的级别的名称或编号；非常规级别是保留。如果ref是一个列表（但不是一个数据帧），每个列表元素中的因子级别是组合的。如果列表被命名，则名称将用作新的因子级别，否则将从旧的。如果ref是一个数据帧或二维数组，矩阵或table，假设第一列具有唯一的x级别，第二列分别具有该级别的分组。

#这里的ref到底是啥
classif.train$y<-relevel(as.factor(classif.train$y),ref="1") #选择参考分类，必须有ref,ref' must be an existing level
multi.logistreg<-multinom(y~., data=classif.train)
## multinom:it does not require the data to be reshaped (as the mlogit package does) and to mirror the example code found in Hilbe’s Logistic Regression Models.
##多分类的逻辑回归利用的是library(nnet)包中的multinom()函数。该函数有两个特点：一是需要选择参考分类；二是不能计算系数的显著性（需要自己计算）。多元逻辑回归中，假设有3个分类，会以参考分类作为参考，构建两个分类模型。模型计算的是该条数据属于3个分类的概率，取概率最大的分类为最终分类。
summary(multi.logistreg)
####以上结果我们可以看到虽然我们的Species有三个类别：setosa，versicolor和virginica，但是以上结果只显示了后两个类别的系数（Coefficients）和标准误（Std.Errors），而setosa类别的系数和标准误均没有在结果中展示。
##其原因是，多项逻辑回归的原理是，在结果变量的类别中，会选择一个类别作为参考类别，其他各个类别的系数结果都是以参考类别作为参考（即定义为1），进一步计算及进行诠释。其实在二分类的逻辑回归中，我们也应用到了这一点，0/1变量中，我们默认的0是参考类别，1是感兴趣类别。但是由于二分类结果没有第三个类别，所以不需要额外提出所谓的参考和非参考类别。但是多项逻辑回归就有所不同，有多个类别在结果变量，需要考虑参考类别的选择。

##系数显著性检验
z<-summary(multi.logistreg)$coefficients/summary(multi.logistreg)$standard.errors
pvalue<- (1 - pnorm(abs(z), 0, 1)) * 2
pvalue

#相对危险度（相对危险风险比，odds，与OR等价）relative risk ratio
exp(coef(multi.logistreg))

#得到模型的拟合值
#  use predicted probabilities to help you understand the model. You can calculate predicted probabilities for each of our outcome levels using the fitted function. We can start by generating the predicted probabilities for the observations in our dataset and viewing the first few rows
head(pp <- fitted(multi.logistreg))

## 测试集结果预测
pred.multi.logistreg<-predict(multi.logistreg, classif.test)
pred.multi.logistreg
pred.multi.logistreg.prob<-predict(multi.logistreg, classif.test, "prob")
pred.multi.logistreg.prob


#预测正确百分比
err.logistreg <- mean(pred.multi.logistreg!=classif.test$y)
err.logistreg
# ou
matrix.multi.logistreg<-table(classif.test$y,pred.multi.logistreg)
matrix.multi.logistreg
err.logistreg<- 1-sum(diag(matrix.multi.logistreg))/nb.test
err.logistreg  #on est à 0.4850299

##以下这两种方式有什么区别？似乎都可以用
roc.logistreg <- roc(classif.test$y, as.vector(pred.multi.logistreg.prob[,1]))
roc.logistreg<-roc(classif.test$y,as.vector(pred.multi.logistreg.prob$posterior[,1]))
plot(roc.logistreg,col='purple',add=TRUE)

#####K-fold cross-validation
#install.packages(caret)
library(caret)
folds<-createFolds(y=classif$y,k=10)
folds
library(pROC)
max=0
num=0
auc_value<-as.numeric()
for(i in 1:10){  #10折
  fold.classif.test <- classif[folds[[i]],]# 剩下的数据作为训练集
  fold.classif.train <- classif[-folds[[i]],]#取folds[[i]]作为测试集
  fold.lm <- lm(as.numeric(y)~.,data=fold.classif.train)
  fold.pred <- predict(fold.lm,type='response',newdata=fold.classif.test)
  ##AUC值越大，准确率越高
  ##但AUC只能用于二分类模型评价
  ## https://zhuanlan.zhihu.com/p/33407505
  #auc_value<- append(auc_value,as.numeric(auc(as.numeric(fold.classif.test[,51]),fold.pred)))
  roc.curve<-roc(as.numeric(fold.classif.test$y),fold.pred)
  plot(roc.curve,add=TRUE)
  if (i==1 | i==10){
    plot(roc.curve,add=TRUE) 
  }
}
#idx<-which.max(auc_value)
#print(auc_value)


######ajouter les variables transformees
#####变量太多了

## 要算mse吗??????????????



###########plot_all +KNN
plot(roc.nb, col='black') 
plot(roc.lda, col='red',add=TRUE)
plot(roc.qda, col='green', add=TRUE)
plot(roc.logistreg,col='blue',add=TRUE)
legend("bottomright",legend=c("NB","LDA","QDA","Logistic Regression"),col=c("black","red","green","blue"),lty=1:2)




#######48行  2. NB 
library(naivebayes)
fit.naive <- naive_bayes(y~.,data=classif.train) 
nb.lm<-lm(y~.,data=classif.train) 
nb.lm
pred.naive <- predict(fit.naive,newdata=classif.test)  ###Warning message:
########predict.naive_bayes(): more features in the newdata are provided as there are probability tables in the object. Calculation is performed based on features to be found in the tables. 
err.naive <- mean(pred.naive != classif.test$y) # on est à 0.3532934，meilleurs résultats qu'avec ADL
## noramlite  tp2page5
##moyen1 Tracer qqnorm
##Error in qqnorm.default(resid(fit.naive)) : y is empty or has only NAs
qqnorm(resid(fit.naive))
qqline(resid(fit.naive))
resid(fit.naive) ###残差为0？？？？？？？？？？？？？？？为什么残差为0
##moyen2 Tracer histogramme
hist(resid(fit.naive), freq = FALSE)
eps <- seq(-2, 2, 0.01)
lines(eps, dnorm(eps, mean=0, sd=sd(resid(fit.naive))))

shapiro.test(resid(fit.naive))   #Error in shapiro.test(fit.naive) : 不是所有的is.numeric(x)都是TRUE
for(i in 1:nrow(classif)){
  classif[i,]<-as.numeric(classif[i,])
}
resid(classif) #null
resid(fit.naive) #null
shapiro.test(classif[,1])      ##### 判断每一列符合正态分布？？？？？



####下面的没用上

library(plyr)
library(reshape2)

#1、根据训练集创建朴素贝叶斯分类器

#1.1、生成类别的概率
##计算训练集合D中类别出现的概率，即P{c_i}
##输入：trainData 训练集，类型为数据框
##      strClassName 指明训练集中名称为    strClassName列为分类结果
##输出：数据框，P{c_i}的集合，类别名称|概率（列名为 prob）
class_prob <- function(trainData, strClassName){
  #训练集样本数
  #nrow返回行数
  length_train <- nrow(trainData)
  dTemp <- ddply(trainData, strClassName, "nrow")
  dTemp <- transform(dTemp, length_train=length_train) 
  dTemp <- ddply(dTemp, strClassName, mutate, prob=nrow/length_train)
  dTemp[,-c(2,3)]
}

##1.2、生成每个类别下，特征取不同值的概率
##计算训练集合D中,生成每个类别下，特征取不同值的概率，即P{fi|c_i}
##输入：trainData 训练集，类型为数据框
##      strClassName 指明训练集中名称为strClassName列为分类结果，其余的全部列认为是特征值
##输出：数据框，P{fi|c_i}的集合，类别名称|特征名称|特征取值|概率（列名为 prob）
feature_class_prob <- function(trainData, strClassName){
  # 横表转换为纵表
  data.melt <- melt(trainData,id=c(strClassName))
  # 统计频数
  aa <- ddply(data.melt, c(strClassName,"variable","value"), "nrow")
  # 计算概率
  bb <- ddply(aa, c(strClassName,"variable"), mutate, sum = sum(nrow), prob = nrow/sum)
  # 增加列名
  colnames(bb) <- c("class.name",
                    "feature.name",
                    "feature.value",
                    "feature.nrow",
                    "feature.sum",
                    "prob")
  # 返回结果
  bb[,c(1,2,3,6)]
}
## 以上创建完朴素贝叶斯分类器


## 2、使用生成的朴素贝叶斯分类器进行预测
##使用生成的朴素贝叶斯分类器进行预测P{fi|c_i}
##输入：oneObs 数据框，待预测的样本，格式为 特征名称|特征值
##      pc 数据框，训练集合D中类别出现的概率，即P{c_i}  类别名称|概率
##      pfc 数据框，每个类别下，特征取不同值的概率，即P{fi|c_i}
##                  类别名称|特征名称|特征值|概率
##输出：数据框，待预测样本的分类对每个类别的概率，类别名称|后验概率（列名为 prob）
pre_class <- function(oneObs, pc,pfc){
  colnames(oneObs) <- c("feature.name", "feature.value")
  colnames(pc) <- c("class.name","prob")
  colnames(pfc) <- c("class.name","feature.name","feature.value","prob")
  
  # 取出特征的取值的条件概率
  feature.all <- join(oneObs,pfc,by=c("feature.name","feature.value"),type="inner")
  # 取出特征取值的条件概率连乘
  feature.prob <- ddply(feature.all,.(class.name),summarize,prob_fea=prod(prob))  #prod为连乘函数
  
  #取出类别的概率
  class.all <- join(feature.prob,pc,by="class.name",type="inner")
  #输出结果
  ddply(class.all,.(class.name),mutate,pre_prob=prob_fea*prob)[,c(1,4)]
}


##3、数据测试1
##用上面苹果的数据作为例子进行测试
#训练集
train.apple <- data.frame(
  size=c("大","小","大","大","小","小"),
  weight=c("轻","重","轻","轻","重","轻"),
  color=c("红","红","红","绿","红","绿"),
  taste=c("good","good","bad","bad","bad","good")
)
#待预测样本
oneObs <- data.frame(
  feature.name =c("size", "weight", "color"),
  feature.value =c("大","重","红")
)

#预测分类
pc <- class_prob(train.apple, "taste")
pfc <- feature_class_prob(train.apple, "taste")
pre_class(oneObs, pc, pfc)
