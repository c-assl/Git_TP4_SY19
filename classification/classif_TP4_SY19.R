setwd("/Users/cecileasselin/Desktop/SY19/TD4/")
rm(list=ls())

############# CLASSIFICATION ####################

classif <- read.table(file='./Data/TPN1_a22_clas_app.txt', header = TRUE)
head(classif) # données quantitatives de X1 à X45, données qualitatives X46 à X50, y est la donnée de classe à prédire (3 classes différentes)
summary(classif)

classif.scaled<-scale(classif) # A FAIRE : comparer mes results ac scaled / non scaled

###### 0. Separation des données en test et train
n <- nrow(classif)
p <- ncol(classif)-1
nb.train <- round(2*n/3) 
nb.test <- n - nb.train

# We declare the class variable as a factor (because it is a (qualitative) category variable) 
classif$y<-as.factor(classif$y)

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

# calcul du taux d'erreur de ma prédiction : on est à 0.49
err.lda <- mean(pred.classif.lda$class!=classif.test$y)

# autre manière de calculer erreur empirique  
# matrix.conf.lda <- table(classif.test$y, pred.classif.lda$class) 
# matrix.conf.lda
# err.lda <- 1-sum(diag(matrix.conf.lda))/nb.test 
# err.lda


####### 2. NB 
library(naivebayes)
fit.naive <- naive_bayes(y~.,data=classif.train) 
pred.naive <- predict(fit.naive,newdata=classif.test)
err.naive <- mean(pred.naive != classif.test$y) # meilleurs résultats qu'avec ADL

# ne pas oublier de vérifier l'hypothèse forte d’indépendance entre les prédicteurs faite ac NB (i.e. 𝑥𝑖 ⟂ 𝑥𝑗)
# quel test faire ?
class(classif$X46)
# classif$X46<-as.factor(classif$X46)
chisq.test(classif[c(0:45)])
classif[c(0:45)]

classif.chi2 <- classif

for (i in 1:10) {
  classif.chi2$i = classif.chi2$i + 20
}

# Autre manière de calculer l'erreur
# conf.naive <- table(classif.test$y,pred.naive) 
# conf.naive
# err.naive <- 1-sum(diag(conf.naive))/nb.test 
# err.naive

####### 3. ADQ
# on n'a peut être pas assez de données pour faire une ADQ correcte (si besoin donner plus de données à l'ensemble d'apprentissage, et faire cross-validation)
fit.qda <- qda(y~.,data=classif.train)
pred.qda <- predict(fit.qda,newdata=classif.test) 
err.qda <- mean(classif.test$y != pred.qda$class)

###### 4. Comparaison ADL / NB / ADQ ###
library(pROC)
roc.lda <- roc(classif.test$y,as.vector(pred.classif.lda$posterior[,1])) 
plot(roc.lda)

pred.nb.prob <- predict(fit.naive, newdata=classif.test, type="prob") # COMPRENDRE POURQUOI ON DOIT REFAIRE CETTE PREDICTION AVEC TYPE = PROB
roc.nb <- roc(classif.test$y, as.vector(pred.nb.prob[,1]))
plot(roc.nb, col='red') 
plot(roc.lda, add=TRUE)

# Problème : je n'arrive x à tracer ma ROC pour l'ADQ
# roc.qda <- roc(classif.test$y,as.vector(pred.qda$posterior[,1])) 
# plot(roc.qda, col='blue', add=TRUE)


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


##### 6. Regression logistique
library(nnet)
fit<-multinom(y~., data=classif.train)
pred.classif<-predict(fit, newdata=classif.test)
perf<-table(classif.test$y, pred.classif)
err.reglog <- 1-sum(diag(perf))/nb.test




## Validation croisée
K<-10


### Reste à faire : 
# il faudrait peut être normaliser nos données dès le départ (ds ts cas) pr l'instant je ne l'ai fait que pour KNN à la fin)
# faire des tests d'hypothèse pour NB, ADL & ADQ
# faut il consid données X46 à X50 comme classes aussi ??? j'imagine q oui mais jsp si possible faire preds en les prenant en compte ds ce cas là
# on pourrait aussi passer à la validation croisée K-fold
# essayer la regression logistique 
# selection de modèle: subset selec / regularizat° / feature extract° 

#### tentative subset selection (attention je ne sais pas si c'est censé fonctionner avec de la classification)
# install.packages("leaps")
# library(leaps)   #function to search for the best model
# reg.fit<-regsubsets(y ~.,data=classif.train) # method='exhaustive' => trop lent, & jsp si je garde nvmax ou x
# plot(reg.fit,scale="r2")