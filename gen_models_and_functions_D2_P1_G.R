rm(list = ls())
library(caret)
library(klaR)
library(gridExtra)
library(tidyverse)
library(leaps)
library(MASS)

set.seed(1729) # the Hardy CRamanujan number

# I. Apprentissage des modèles

######### 1.A. REGRESSION: Forward stepwise Regression
# chargement des données
reg.data <- read.table(file = "data/TPN1_a22_reg_app.txt", header = TRUE)
rows <- nrow(reg.data)
cols <- ncol(reg.data) - 1
train_size <- 2 / 3
nb_train <- round(train_size * rows)
nb_test <- rows - nb_train

# Separation des données en test et train
train <- sample(1:rows, nb_train)
reg.train <- reg.data[train, ]
reg.test <- reg.data[-train, ]

# apprentissage du modèle
MSE <- function(y_test, y_predict) {
    mean((y_test - y_predict)^2)
}

train.control <- trainControl(method = "cv", number = 5)
fwd.reg <- train(y ~ .,
    data = reg.train,
    method = "leapForward",
    tuneGrid = data.frame(nvmax = 1:100),
    trControl = train.control
)

# Best set of predictors
fwd.coefs <- coef(fwd.reg$finalModel, fwd.reg$bestTune$nvmax)
fwd.pred.names <- names(fwd.coefs)[1:fwd.reg$bestTune$nvmax + 1]

# Fit linear regression, with the above predictors
model.reg <- lm(paste("y", "~", paste(fwd.pred.names, collapse = " + ")),
    data = reg.train
)


######### 1.B. CLASSIFICATION : Apprentissage d'un modèle de Regularized Discriminant Analysis

# chargement des données
# setwd("/Users/cecileasselin/Desktop/SY19/TD4/")
classif <- read.table(file = "data/TPN1_a22_clas_app.txt")
classif$y<-as.factor(classif$y)

# Separation des données en test et train
n <- nrow(classif)
nb.train <- round(2 * n / 3)
nb.test <- n - nb.train
train <- sample(1:n, nb.train)
classif.train <- classif[train, ]
classif.test <- classif[-train, ]

# apprentissage du modèle : on cherche les meilleurs paramètres lambda et gamma pour la RDA
cv_5_grid <- trainControl(method = "cv", number = 5)
fit_rda_grid <- train(y ~ ., data = classif.train, method = "rda", trControl = cv_5_grid)
fit_rda_grid
plot(fit_rda_grid)
# The final values used for the model were gamma = 0 and lambda = 0.5

# création du modèle avec les paramètres appris
model.cls <- rda(y ~ ., data = classif.train, gamma = 0, lambda = 0.5)



# II. Création des fonctions de prédiction


# 2.A. REGRESSION
prediction_reg <- function(dataset) {
	library(gridExtra)
	library(tidyverse)
	library(leaps)
	library(MASS)
    pred.fwd <- predict(model.reg, newdata = dataset)
    pred.fwd
}
pred.fwd <- prediction_reg(reg.test)
MSE(reg.test$y, pred.fwd)


# 2.B. CLASSIFICATION
prediction_cls <- function(dataset) {
    # load lib for RDA
    library(klaR)

    pred.rda <- predict(model.cls, dataset)$class
    pred.rda
}

# Affichage du taux d'erreur qu'on aurait obtenu avec classif.test
pred.rda <- prediction_cls(classif.test)
mean(classif.test$y != pred.rda)



# 3. Sauvegarder sous forme de fichier .Rdata les fonctions `prediction_reg`, `prediction_cls`.
# Sauvegarder également les objets utilisés dans ces fonctions (`model.reg` et `model.cls` dans l'exemple) !

save(
    "model.reg",
    "model.cls",
    "prediction_reg",
    "prediction_cls",
    file = "env.Rdata"
)
