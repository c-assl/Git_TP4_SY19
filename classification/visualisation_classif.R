setwd("/Users/cecileasselin/Desktop/SY19/TD4/")
rm(list=ls())

############# Visualisation ####################

classif <- read.table(file='./Data/TPN1_a22_clas_app.txt', header = TRUE)
head(classif) # données quantitatives de X1 à X45, données qualitatives X46 à X50, y est la donnée de classe à prédire (3 classes différentes)
summary(classif)


###### 1. Visualisation des dépendances

plot(y~X1, data=classif)
plot(y~X2, data=classif)
plot(y~X3, data=classif)
plot(y~X4, data=classif)
plot(y~X5, data=classif)
plot(y~X6, data=classif)
plot(y~X7, data=classif)
plot(y~X8, data=classif)
plot(y~X9, data=classif)
plot(y~X10, data=classif)
plot(y~X11, data=classif)
plot(y~X12, data=classif)
plot(y~X20, data=classif)
plot(y~X30, data=classif)
plot(y~X40, data=classif)
plot(y~X46, data=classif)
plot(y~X47, data=classif)
plot(y~X48, data=classif)
plot(y~X49, data=classif)
plot(y~X50, data=classif)

plot(classif[1:10]) # ne sert à rien, voir si je pourrais filtrer avec un vecteur de type c(1:10+51)
plot(classif[41:51])

cor(classif, method = "pearson") # but d'analyser numériquement et plus graphiquement les corrélations
# je ne sais pas trop interpréter mais j'ai l'impression qu'on n'a pas de trop gros coefficients de corrélation (au max 0.12)




# Correlation
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  text(0.5, 0.5, txt)
}
# Create the plots
pairs(classif[,c(1,2,3,4,5,6,7)], lower.panel = panel.cor)
pairs(classif[,c(10,11,12,13,14,15,16)], lower.panel = panel.cor)
pairs(classif[,c(46,47,48,49,50,51,45)], lower.panel = panel.cor)


######### 2. PCA => je n'y arrive pas a cause de lambda
classif.scaled<-scale(classif)

# PCA exploration
pca <- prcomp(classif.scaled, scale = TRUE, center = TRUE, retx = T)
pca.var <- pca$sdev^2

prop_var <- pca.var / sum(pca.var)
plot(prop_var,
    xlab = "principal component",
    ylab = "Proportion of Variance Explained",
    ylim = c(0, 1),
    type = "b",
)

##### autre tentative de transformation des données
# merge.classif <- classif %>% pivot_longer(X1:X45, names_to = "question", values_to = "response")
# print(longer_data)