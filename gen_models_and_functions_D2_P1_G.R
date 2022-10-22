rm(list=ls())
library(caret)
library(klaR)

set.seed(1729) # the Hardy CRamanujan number

# 1. Apprentissage des modèles

######### 1.A. REGRESSION : a completer
model.reg <- {
  
}

######### 1.B. CLASSIFICATION : Apprentissage d'un modèle de Regularized Discriminant Analysis 

# chargement des données
setwd("/Users/cecileasselin/Desktop/SY19/TD4/")
classif <- read.table(file = "data/TPN1_a22_clas_app.txt") 
classif$y<-as.factor(classif$y)

# Separation des données en test et train
n <- nrow(classif)
nb.train <- round(2*n/3) 
nb.test <- n - nb.train
train <- sample(1:n, nb.train) 
classif.train <- classif[train,] 
classif.test <- classif[-train,]

# apprentissage du modèle : on cherche les meilleurs paramètres lambda et gamma pour la RDA
cv_5_grid = trainControl(method = "cv", number = 5)
fit_rda_grid = train(y ~ ., data=classif.train, method = "rda", trControl = cv_5_grid)
fit_rda_grid
plot(fit_rda_grid)
# The final values used for the model were gamma = 0 and lambda = 0.5

# création du modèle avec les paramètres appris
model.cls <- rda(y~., data=classif.train, gamma=0, lambda=0.5) 



# 2. Création des fonctions de prédiction


# 2.A. REGRESSION : à completer
prediction_reg <- function(dataset) {
  # Ne pas oublier de charger **à l'intérieur de la fonction** les # bibliothèques utilisées.
  library(...)
  # Attention à ce que retourne un modèle en prédiction. Par exemple, # la lda retourne une liste nommée. On sélectionne alors les classes.
  predict(clas, test_set)$class
}

# 2.B. CLASSIFICATION 
prediction_cls <- function(dataset) { 
  # load lib for RDA
  library(klaR)
  
  pred.rda<-predict(model.cls, dataset)$class
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

