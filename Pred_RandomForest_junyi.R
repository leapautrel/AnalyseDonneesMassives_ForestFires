# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
library(data.table)	# adapté aux données massives
library(ggplot2) # graphiques
library(randomForest) # prediction
library(rpart) #prédiction CART

# Set Working Directory ----
#setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret") # Direction fichier  GitHub Lea
setwd("C:/Users/mimi/Desktop/M2/Analyse de données massives/projet") # Direction fichier Junyi

# Importation jeu de données propre (cf document 'importation_initiale') ----
# fires <- fread(
#   "fires.csv",
#   header = TRUE,
#   sep = ",",
#   na.strings = "",
#   blank.lines.skip = TRUE,
#   stringsAsFactors = TRUE
# )
# 
fires$fire_year <- as.factor(fires$fire_year) # annee 
summary(fires)

# Visualisation : les données sont-elles équilibrées ?
## aggregation
fires_bycause <-
	fires[, list(fire_count = .N), by = stat_cause_descr]
## plot
ggplot(data = fires_bycause,
			 aes(x = reorder(stat_cause_descr, -fire_count), y = fire_count/1000)) +
	geom_bar(stat = 'identity', fill = 'red') +
	labs(x = '', y = 'Nombre de feux (en milliers)', title = 'Causes des feux de 1992 à 2015') +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2 etapes
# 1 - verifier s'il est possible de predire la cause sur les donnees dont on sait la cause
# 2 - si 1) marche, on va predire la cause pour les donnees ou la cause est inconnue

#######################
# Etape 1 - Peut on faire une prediction correcte ?
#######################


# Pour verifier ca, on va selectionner les lignes ou la cause est connue
fires_knowncause <- fires[stat_cause_descr != "Missing/Undefined",]
fires_knowncause$stat_cause_descr <- factor(fires_knowncause$stat_cause_descr)

# méthode 1----
# On fait un JDD train et un JDD test
set.seed(123)

## index des lignes prises ou non (80% dans train, 20% dans test)
train_index <- sample(c(TRUE, FALSE), nrow(fires_knowncause), replace = TRUE, prob = c(0.8, 0.2))
test_index <- !train_index

## Creation des tableaux----
### Tables train
x_train <- as.data.table(fires_knowncause[train_index, c(2:4, 6)])
y_train <- fires_knowncause[train_index, 1]

### Tables test
x_test <- as.data.table(fires_knowncause[test_index, c(2:4, 6)])
y_test <- fires_knowncause[test_index, 1]



# methode 2 avec toutes les années [marche maos bofff]----
train <- as.data.table(fires_knowncause[train_index, c(1:4, 6)])
test <- as.data.table(fires_knowncause[test_index, c(1:4, 6)])

# Random Forest
mod_rf <- randomForest(stat_cause_descr~.,
											 data = fires_knowncause,
											 ntree = 3,
											 mtry = 2,
											 na.action=na.fail)
mod_rf

levels(fires_knowncause$stat_cause_descr)


# CART annee 2005 (88604 ind.)----

# fires_knowncause <- fires[stat_cause_descr != "Missing/Undefined",]
# fires_knowncause$stat_cause_descr <- factor(fires_knowncause$stat_cause_descr)

fires_2005 <- fires[ fire_year %in% c(1995,2000,2005,2010,2015),]
dim(fires_2005)
fires_2005[410850:410872,]
# fires_2005$fire_year <- as.numeric(fires_2005$fire_year)
fires_2005

set.seed(123)

## index des lignes prises ou non (80% dans train, 20% dans test)

train_index_2005 <- sample(c(TRUE, FALSE), 
                           nrow(fires_2005),
                           replace = TRUE,
                           prob = c(0.8, 0.2))
test_index_2005 <- !train_index


# train <- as.data.table(fires_knowncause[train_index, c(1:4, 6)])
# test <- as.data.table(fires_knowncause[test_index, c(1:4, 6)])
train_2005 <- as.data.table(fires_2005[train_index_2005, c(1:4, 6)])
test_2005 <- as.data.table(fires_2005[test_index_2005, c(1:4, 6)])




mod.CART <- rpart(stat_cause_descr ~ ., 
                  data = train)
pred.CART <- predict(mod.CART,
                     newdata=test,
                     type="class")

cM <- caret::confusionMatrix(factor(pred.CART,levels=levels(test$stat_cause_descr)),
                             reference=test$stat_cause_descr)
cM$overall["Accuracy"]
plot(mod.CART,uniform=TRUE)
text(mod.CART, cex = 0.75,use.n=FALSE)

# random forest avec que 5 dernières années 2010-2015 ----
# random forest avec que 5 années de 5 ans en 5 depuis 1995 ----

#######################
# Etape 2 - Prediction des causes pour les lignes Missing/Undefined
#######################