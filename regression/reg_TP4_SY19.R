setwd("/Users/cecileasselin/Desktop/SY19/TD4/")
rm(list=ls())
library('FNN')

############# REGRESSION ####################

reg <- read.table(file='./Data/TPN1_a22_reg_app.txt', header = TRUE)
head(reg)
summary(reg) # toutes les variables de X1 à X100 valent entre 0 & 10, & ont une moyenne autour de 5 / y est compris entre -5 & 371, & a une moyenne à 165


# 0. Separation du jeu de données en train et test
# Generation of 2/3 observations
n <- nrow(reg)
p <- ncol(reg)-1
nb.train <- round(2*n/3) 
nb.test <- n - nb.train

# seed
# set.seed(1729) # the Hardy–Ramanujan number
set.seed(1)

# Training/Testing data
train <- sample(1:n, nb.train) 
reg.train <- reg[train,] 
reg.test <- reg[-train,]

##### 1. Regression Linéaire (reprise du code du TP2)
model.reg <- lm(y ~., data = reg.train) 
model.reg$coefficients
summary(model.reg) # Predicteurs significatifs a garder = X1 X6 X11 12 14 15 17 21 22 25 27 29 32 33 35 37 39 42 46 47 48 49 50 52 54 56 58 59 60 62 63 68 70 71 72 74 75 80 83 84 87 88 89 90 95 96 99
# on a bien une regression significative car p-value: < 2.2e-16

# je veux comparer mon erreur en utilisant tous les predicteurs / en utilisant que ceux qui sont significatifs
par(mfrow=c(1,3))

# 1.1 Regression linéaire en utilisant tous les predicteurs
plot(reg.train$y, fitted(model.reg)) # j'ai l'air d'avoir trop de fitted
abline(0, 1)

# 1.2 Regression linéaire en utilisant que les prédicteurs significatifs (. * ** et ***)
model.selectivereg <- lm(y ~ X1+X6+X11+X12+X14+X15+X17+X22+X25+X27+X32+X33+X35+X37+X39+X42+X46+X47+X48+X50+X52+X56+X58+X59+X60+X62+X63+X68+X70+X71+X72+X74+X75+X80+X83+X84+X87+X88+X89+X90+X95+X96+X99, data = reg.train) 
model.selectivereg$coefficients
summary(model.selectivereg)  
# on a toujours une regression significative car p-value: < 2.2e-16
plot(reg.train$y, fitted(model.selectivereg)) # j'ai l'air d'avoir trop de fitted
abline(0, 1)

## 1.3 Regression linéaire en ne gardant que les coefficients très significatifs (***)
model.veryselectivereg <- lm(y ~ X6+X11+X12+X15+X17+X21+X22+X25+X27+X29+X32+X33+X35+X37+X39+X42+X46+X47+X48+X49+X50+X52+X54+X56+X59+X60+X63+X68+X70+X72+X74+X80+X83+X84+X87+X88+X89+X90+X96, data = reg.train) 
model.veryselectivereg$coefficients
summary(model.veryselectivereg)  
# on a toujours une regression significative car p-value: < 2.2e-16
plot(reg.train$y, fitted(model.veryselectivereg)) # j'ai l'air d'avoir trop de fitted
abline(0, 1)

# Conclusion : plus on prend un modèle parcimonieux (avec peu de predicteurs), plus on a DF (Degree of Freedom) & Residual standard error élevés, mais Multiple et Adjusted R-squared diminuent 

# comparaison des residus : je pense que ça va être la même chose
# plot(reg.train$y, rstandard(model.reg))
# abline(h = 0)
# plot(reg.train$y, rstandard(model.selectivereg))
# abline(h = 0)
# plot(reg.train$y, rstandard(model.veryselectivereg))
# abline(h = 0)

# comparaison des resultats
pred.reg <- predict(model.reg, newdata=reg.test)
pred.selectivereg <- predict(model.selectivereg, newdata=reg.test)
pred.veryselectivereg <- predict(model.veryselectivereg, newdata=reg.test)

###### 2. regression KNN
x.app <- reg.train[, c(-101)] # on retire la colonne y 
y.app <- reg.train[, 101]
x.tst <- reg.test[, c(-101)]
kmin <- 6 # k minimisant l'erreur quadratique avec de données non normalisése
model.knn <- knn.reg(train=x.app, test = x.tst, y=y.app, k = 6)

##### Comparaison de l'erreur quadratique moyenne entre mes 3 modèles de Regression linéaire, et KNN 
cbind(mean((reg.test[, c('y')] - pred.reg)^2),
      mean((reg.test[, c('y')] - pred.selectivereg)^2),
      mean((reg.test[, c('y')] - pred.veryselectivereg)^2),
      mean((reg.test[, c('y')] - model.knn$pred)^2))

# la meilleure erreur est obtenue avec La Regression Linéaire qui prend tous les predicteurs : 178.212


#### 3. tentative de PCR (Principal Component Regression)
install.packages('pls')
library('pls')
pcr.fit<-pcr(y~., data=reg, scale="TRUE", validation="CV")
summary(pcr.fit)
par(mfrow=c(1,1))
validationplot(pcr.fit, val.type="MSEP", legendpos="topright")

##### Reste à faire / à tester :
# tester feature selection
# faire regularization avec Ridge / Lasso 

# peut être faire plus un ratio 90/10 pour mon train/test parce que j'ai pas tant de données que ça
