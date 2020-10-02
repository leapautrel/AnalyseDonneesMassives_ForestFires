# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)
require(ggmap)
require (leaflet)
require(nnet)  

# Creation de fonctions ----


# Set Working Directory ----
# setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret") # Direction fichier Lea
setwd("C:/Users/mimi/Desktop/M2/Analyse de données massives/projet") # Direction fichier Junyi

# Importation ----
fires_all <- fread(
	"fires_all.csv",
	header = TRUE,
	sep = ";",
	na.strings = "",
	blank.lines.skip = TRUE,
	stringsAsFactors = TRUE
)

# Nettoyage du jeu de données ----
## Sélection des 6 colonnes utiles (cause, lieu,)
fires <- fires_all[, c(25, 20, 29, 31, 32)]
summary(fires)



# construction de la prediction----
### selection de 10000 individus pour essayer de construire la prediction 
fires_pred <- fires[1:10000,]
head(fires_pred)
head(summary(fires_pred))

# on appelle les variables explicatives, la variable qu'on cherche a expliquer, Y, est stat_cause_descr
x = fires_pred[, 2:5] 
y = fires_pred$stat_cause_descr

mod = multinom(stat_cause_descr~.,data = fires_pred)
coef(mod)


mod = multinom(stat_cause_descr~.,
               data=fires_pred,
               maxit=200) # ML fit of the logistic model #il converge ? 150 it?rations, d?viance apr?s cv 192.72
coef(mod) # les valeurs des coeef impact?s en allant jusque'? la cv

### Assessment of the fit  
### en utilisan la d?viance expliqu?e, 
### diff?rence entre d?viance du mod?le nul et la d?viance du mod?le avec toutes les variables

deviance(mod)          # Residual deviance # 

mod0 = multinom(stat_cause_descr~1,data=fires_pred) # ML fit of the null model
deviance(mod0)-deviance(mod)                # Explained deviance

### Observed versus fitted values
### indicateurs plus graphiques 

proba = fitted(mod)            # Estimated probabilities of each class
observed_class = fires_pred$stat_cause_descr
head(data.frame(round(proba,3),observed_class))

# Confusion matrix

fitted_class = predict(mod,type="class")  # Bayes rule pour chaque individu 
# je peux calculer la proba q'uil provienne du site 1 2 etc. 
#et il serait dans le site avec la plus grande proba
head(fitted_class)

confusion = table(observed_class,fitted_class)
confusion

rowSums(confusion)
confusion_percentage = 100*confusion/outer(rowSums(confusion),rep(1,5))
round(confusion_percentage,3)





