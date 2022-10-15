# Git_TP4_SY19

## I. Dossier classification

### visualisation_classif.R
=> code pour visualiser les données avant de faire des traitements dessus :
- 1. J’essaie d’observer les dependences entre y et les autres variables (je n’ai rien trouvé d’interessant)
- 2. PCA (pour l’instant ça ne fonctionne pas mais si on réussit ce serait interessant)

### classif_TP4_SY19.R
1. Analyse Discriminante linéaire (LDA)
2. Classifier Bayesien Naïf (NB : Naive Bayes) 
3. Analyse Discriminante Quadratique (QDA)
4. Comparaison LDA / NB / QDA a l’aide des courbes COR
5. KNN

#### Les résultats que j’ai pour l’instant : 
QDA et NB semblent avoir les meilleures performances : erreur autour de 0.35
(mais attention QDA risque de faire de l’overfitting, et pour Naive Bayes il faut vérifier les hypothèses d’indépendance)

#### Reste à faire : 
- Régression Logistique
- ACP pour visualiser nos données
- Essayer de faire de la validation croisée
- selection de modèle: subset selec / regularizat° ?
- Tests d’hypothèse à vérifier (notamment pour Naive Bayes)


## II. Dossier regression

### visualisation_reg.R
=> code pour visualiser les données avant de faire des traitements dessus : j’observe les corrélations entre y et les autres variables

### reg_TP4_SY19.R
1. Régression Linéaire
2. Regression KNN
3. PCR (Principal Component Regression)

#### Les résultats que j’ai pour l’instant : 
la meilleure erreur est obtenue avec La Regression Linéaire qui prend tous les predicteurs : 178.212

#### Reste à faire : 
- Comparer avec l’erreur minimum que l’on peut obtenir avec la PCR
- Selection de modèle (feature selection, regularization avec Ridge et Lasso, …)

## III. Dossier Rendu
=> dossier dans lequel on peut mettre à jour le rapport, et le fichier RData quand on avance dessus
(Pour l’instant j’ai mis un Notebook RStudio dedans mais c’est juste un fichier de test, je n’ai rien écris et je n’ai pas commencé le rapport)
