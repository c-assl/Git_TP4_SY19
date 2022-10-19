#setwd("/Users/cecileasselin/Desktop/SY19/TD4/")
rm(list=ls())

############# CLASSIFICATION ####################

#classif <- read.table(file='./Data/TPN1_a22_clas_app.txt', header = TRUE)
classif <- read.table(file='TPN1_a22_clas_app.txt', header = TRUE)
head(classif) 
# données quantitatives de X1 à X45, données qualitatives X46 à X50, y est la donnée de classe à prédire (3 classes différentes)
summary(classif)


###### 0. Separation des données en test et train
n <- nrow(classif)
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

######## 1. LDA
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
sprintf("Taux d'erreur de LDA : %f",err.lda)

####### 2. QDA
# on n'a peut être pas assez de données pour faire une ADQ correcte (si besoin donner plus de données à l'ensemble d'apprentissage, et faire cross-validation)
fit.qda <- qda(y~.,data=classif.train)
pred.qda <- predict(fit.qda,newdata=classif.test) 
err.qda <- mean(classif.test$y != pred.qda$class) #0.3473054

####### 3. RDA
library(klaR)
fit.rda <- rda(y~.,data=classif.train,scale=FALSE)
pred.rda <- predict(fit.rda,newdata=classif.test) 
err.rda <- mean(classif.test$y != pred.rda$class) #0.3413174
err.rda

####### 4. NB 
library(naivebayes)
fit.naive <- naive_bayes(y~.,data=classif.train) 
pred.naive <- predict(fit.naive,newdata=classif.test)  ###Warning message:
#predict.naive_bayes(): more features in the newdata are provided as there are probability tables in the object. Calculation is performed based on features to be found in the tables. 
err.naive <- mean(pred.naive != classif.test$y) # on est à 0.3532934，meilleurs résultats qu'avec ADL
# Autre manière de calculer l'erreur
# conf.naive <- table(classif.test$y,pred.naive) 
# conf.naive
# err.naive <- 1-sum(diag(conf.naive))/nb.test 
# err.naive

# ne pas oublier de vérifier l'hypothèse forte d’indépendance entre les prédicteurs faite ac NB (i.e. ???????? ??? ????????)
# quel test faire ? chisq test ou pearson
library(vcd)
mytable<-xtabs(~X1+X2,data=classif)
chisq.test(mytable)

####### 5. classification/decision trees
#Tree growing
library(rpart)
library(rpart.plot)
tree <- rpart(y~., data = classif.train, method="class",subset=train, parms = list(split = 'gini'))
rpart.plot(tree, box.palette="RdBu", shadow.col="gray",fallen.leaves=FALSE)
plotcp(fit)

## Cross-validation error (figure)
## reste à faire

# Pruning
pruned_tree<-prune(tree,cp=0.026)
rpart.plot(pruned_tree, box.palette="RdBu", shadow.col="gray", fallen.leaves=FALSE)

# Test error rate estimation
pred.tree=predict(pruned_tree,newdata=classif.test,type='class') # yhat
matrix.pred.tree<-table(classif.test$y,pred.tree)
err.tree<-1-mean(classif.test$y==pred.tree) # OU err<-1-sum(diag(matrix.pred.tree))/nb.test
err.tree # on est à 0.5149701

###### 6. multinomial logistic regression
library(nnet)
classif.train$y<-relevel(as.factor(classif.train$y),ref="1") # ref must be an existing level in classif$y
multi.logistreg<-multinom(y~., data=classif.train)
summary(multi.logistreg)

# Coefficient Test de signification
z<-summary(multi.logistreg)$coefficients/summary(multi.logistreg)$standard.errors
pvalue<- (1 - pnorm(abs(z), 0, 1)) * 2
pvalue

#相对危险度（相对危险风险比，odds，与OR等价）relative risk ratio
exp(coef(multi.logistreg))

head(pp <- fitted(multi.logistreg))

pred.multi.logistreg<-predict(multi.logistreg, classif.test)
pred.multi.logistreg

err.logistreg <- mean(pred.multi.logistreg!=classif.test$y)
# ou
#matrix.multi.logistreg<-table(classif.test$y,pred.multi.logistreg)
#err.logistreg<- 1-sum(diag(matrix.multi.logistreg))/nb.test
err.logistreg #on est à 0.4850299


###### 7. Comparaison LDA / QDA / RDA / NB / TREE / LOGISTIC
sprintf("Taux d'erreur de LDA : %f",err.lda)
sprintf("Taux d'erreur de QDA : %f",err.qda)
sprintf("Taux d'erreur de RDA : %f",err.rda)
sprintf("Taux d'erreur de Naive Bayes : %f",err.naive)
sprintf("Taux d'erreur de classification/decision trees : %f",err.tree)
sprintf("Taux d'erreur de multinomial logistic regression : %f",err.logistreg)

library(pROC)
roc.lda <- roc(classif.test$y,as.vector(pred.classif.lda$posterior[,1])) 
roc.qda <- roc(classif.test$y,as.vector(pred.qda$posterior[,1])) # OU roc.qda <- roc(classif.test$y,as.vector(pred.qda[,1]))
roc.rda <- roc(classif.test$y,as.vector(pred.rda$posterior[,1]))

pred.tree.prob<-predict(tree, classif.test, "prob")
roc.tree <- roc(classif.test$y, as.vector(pred.tree.prob[,1])) 
pred.multi.logistreg.prob<-predict(multi.logistreg, classif.test, "prob")
roc.logistreg <- roc(classif.test$y, as.vector(pred.multi.logistreg.prob[,1])) # OU roc.logistreg<-roc(classif.test$y,as.vector(pred.multi.logistreg.prob$posterior[,1])) 
pred.nb.prob <- predict(fit.naive, newdata=classif.test, type="prob") # COMPRENDRE POURQUOI ON DOIT REFAIRE CETTE PREDICTION AVEC TYPE = PROB
roc.nb <- roc(classif.test$y, as.vector(pred.nb.prob[,1]))

# Area under the curve
roc.lda        # 0.848
roc.qda        # 0.6173
roc.rda        # 0.8407
roc.nb         # 0.916
roc.tree       # 0.783
roc.logistreg  # 0.81
# NB > LDA > RDA > LOGISTIC REGRESSION > TREE > QDA

plot(roc.nb, col='black') 
plot(roc.lda, col='red',add=TRUE)
plot(roc.qda, col='green', add=TRUE)
plot(roc.rda, col='yellow', add=TRUE)
plot(roc.tree, col='blue', add=TRUE)
plot(roc.logistreg,col='purple',add=TRUE)
legend("bottomright",legend=c("NB","LDA","QDA","RDA","TREE","Logistic Regression"),col=c("black","red","green","yellow","blue","purple"),lty=1:2)


##### k-fold cross-validation
# autre moyen - not common
#library(caret)
#krda<-train(y~.,data=classif,method="rda",trControl=trainControl(method="cv",number=10))
#krda$results
#krda$bestTune # gamma=0,lambda=0.5

K<-10
n<-nrow(classif)
#folds<-createFolds(y=classif$y,k=10)
folds<-sample(1:K,n,replace=TRUE)

cv.err<-matrix(0,ncol=1,nrow=K)

plot.new()
par(mfrow=c(2,3)) 
plot.cv.error<-function(data,x.title){
  #ic.error.bar<-function(x,lower,upper,length=0.1){
  #  arrows(x,upper,x,lower,angle=90,code=3,length=length,col='red')
  #}
  
  #calculer les erreurs moyennes et l'erreur type(standard error)
  means.errs<-apply(data,2,mean)#mean(cv.err[,1]) # colMeans(data)
  
  stderr<-function(x) sd(x)/sqrt(length(x))
  std.errs<-apply(data,2,stderr)
  plot(data,xlab=x.title,type="b",ylab="CV.error")
  #plot(data,type="b",ylim=range(means.err+1.6*std.errs,means.errs-1.6*std.errs),xlab=x.title,ylab="CV.error")
}

for(k in 1:K){
  fold.test<-classif[folds==k,]
  fold.train<-classif[folds!=k,]
  fold.classif<-lda(y~.,data=fold.train)
  fold.predict<-predict(fold.classif,newdata=fold.test)
  cv.err[k,1]<-mean(fold.predict$class!=fold.test$y)
}
plot.cv.error(cv.err,x.title = "LDA - 10 crossvalidation")

for(k in 1:K){
  fold.test<-classif[folds==k,]
  fold.train<-classif[folds!=k,]
  fold.classif<-qda(y~.,data=fold.train)
  fold.predict<-predict(fold.classif,newdata=fold.test)
  cv.err[k,1]<-mean(fold.predict$class!=fold.test$y)
}
plot.cv.error(cv.err,x.title = "QDA - 10 crossvalidation")

for(k in 1:K){
  fold.test<-classif[folds==k,]
  fold.train<-classif[folds!=k,]
  fold.classif<-rda(y~.,data=fold.train)
  fold.predict<-predict(fold.classif,newdata=fold.test)
  cv.err[k,1]<-mean(fold.predict$class!=fold.test$y)
}
plot.cv.error(cv.err,x.title = "RDA - 10 crossvalidation")

for(k in 1:K){
  fold.test<-classif[folds==k,]
  fold.train<-classif[folds!=k,]
  fold.classif<-multinom(y~.,data=fold.train)
  fold.predict<-predict(fold.classif,newdata=fold.test)
  cv.err[k,1]<-mean(fold.predict$class!=fold.test$y)
}
plot.cv.error(cv.err,x.title = "LOGISTIC REGRESSION - 10 crossvalidation")

for(k in 1:K){
  fold.test<-classif[folds==k,]
  fold.train<-classif[folds!=k,]
  fold.classif<-naive_bayes(y~.,data=fold.train)
  fold.predict<-predict(fold.classif,newdata=fold.test)
  cv.err[k,1]<-mean(fold.predict$class!=fold.test$y)
}
plot.cv.error(cv.err,x.title = "Naive Bayes - 10 crossvalidation")



##### 8.KNN
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



for(k in 1:30){
  classifier_knn <- knn(train = train_scale,
                        test = test_scale,
                        cl = classif.train$y,
                        k = k)
  misClassError <- mean(classifier_knn != classif.test$y)
  print(paste(k, '- Accuracy =', 1-misClassError))
  err.knn <- min(err.knn, misClassError)
  plot(k,1-misClassError)
}
print(paste('KNN - maxAccuracy =', 1-err.knn))
