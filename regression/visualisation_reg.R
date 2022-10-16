setwd("/Users/cecileasselin/Desktop/SY19/TD4/")
rm(list=ls())

############# Visualisation avant REGRESSION ####################

reg <- read.table(file='./Data/TPN1_a22_reg_app.txt', header = TRUE)
head(reg)
summary(reg) # toutes les variables de X1 à X100 valent entre 0 & 10, & ont une moyenne autour de 5 / y est compris entre -5 & 371, & a une moyenne à 165

# plot mes données
# Correlation
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  text(0.5, 0.5, txt)
}
# Create the plots
pairs(reg[,c(1,2,3,4,5,101)], lower.panel = panel.cor) # on a l'intuition que ça va toujours être pareil => je pense q besoin PCA
pairs(reg[,c(6,7,8,9,10,101)], lower.panel = panel.cor)
pairs(reg[,c(11,12,13,14,15,101)], lower.panel = panel.cor)
pairs(reg[,c(97,98,99,100,101)], lower.panel = panel.cor)

