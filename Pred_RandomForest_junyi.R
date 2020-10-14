# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
library(data.table)	# adapté aux données massives
library(ggplot2) # graphiques
library(randomForest) # prediction
library(caret)
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
# fires_knowncause <- fires[stat_cause_descr != "Missing/Undefined",]
# fires_knowncause$stat_cause_descr <- factor(fires_knowncause$stat_cause_descr)
# 
# # méthode 1----
# # On fait un JDD train et un JDD test
# set.seed(123)
# 
# ## index des lignes prises ou non (70% dans train, 30% dans test)
# train_index <- sample(c(TRUE, FALSE), nrow(fires_knowncause), replace = TRUE, prob = c(0.8, 0.2))
# test_index <- !train_index
# 
# ## Creation des tableaux----
# ### Tables train
# x_train <- as.data.table(fires_knowncause[train_index, c(2:4, 6)])
# y_train <- fires_knowncause[train_index, 1]
# 
# ### Tables test
# x_test <- as.data.table(fires_knowncause[test_index, c(2:4, 6)])
# y_test <- fires_knowncause[test_index, 1]
# 
# 
# 
# # methode 2 avec toutes les années [marche maos bofff]----
# train <- as.data.table(fires_knowncause[train_index, c(1:4, 6)])
# test <- as.data.table(fires_knowncause[test_index, c(1:4, 6)])
# 
# # Random Forest
# mod_rf <- randomForest(stat_cause_descr~.,
# 											 data = fires_knowncause,
# 											 ntree = 3,
# 											 mtry = 2,
# 											 na.action=na.fail)
# mod_rf
# 
# levels(fires_knowncause$stat_cause_descr)


# CART annee 2005 (88604 ind.)----

# fires_knowncause <- fires[stat_cause_descr != "Missing/Undefined",]
# fires_knowncause$stat_cause_descr <- factor(fires_knowncause$stat_cause_descr)


fires_2005 <- fires[ stat_cause_descr != "Missing/Undefined" & fire_year %in% c(1995,2000,2005,2010,2015),]
dim(fires_2005)


fires_2005$fire_year <- factor(fires_2005$fire_year) # elle est utile cette ligne?? OUI
fires_2005$stat_cause_descr<- factor(fires_2005$stat_cause_descr)
levels(fires_2005$fire_year)
levels(fires_2005$stat_cause_descr)


set.seed(123)

## index des lignes prises ou non (80% dans train, 20% dans test)

train_index_2005 <- sample(c(TRUE, FALSE), 
                           nrow(fires_2005),
                           replace = TRUE,
                           prob = c(0.8, 0.2))
test_index_2005 <- !train_index_2005
train_2005 <- as.data.table(fires_2005[train_index_2005, c(1:4, 6)])
test_2005 <- as.data.table(fires_2005[test_index_2005, c(1:4, 6)])
print(object.size(train_2005), units = 'Mb')

# CART rpart verifier accuracy

mod.CART <- rpart(stat_cause_descr ~ ., 
                  data = train_2005)
pred.CART <- predict(mod.CART,
                     newdata=test_2005,
                     type="class")

cM <- caret::confusionMatrix(factor(pred.CART,levels=levels(test_2005$stat_cause_descr)),
                             reference=test_2005$stat_cause_descr)
cM$overall["Accuracy"]
plot(mod.CART,uniform=TRUE)
text(mod.CART, cex = 0.75,use.n=FALSE)


# random forest avec que 5 années de 5 ans en 5 depuis 1995 ----
# random forest avec que 5 années de 5 ans en 5 depuis 1995 ----

nbarbres <- rep(0, 10)
accuracy <- rep(0, 10)
j = 1
for (i in seq(from = 1,
              to = 50,
              length.out = 5)){
  # On fait le modèle randomForest à i arbres
  mod.RF <- randomForest(stat_cause_descr ~ .,
                         ntree = i,
                         data = train_2005)
  
  # On récupère la prédiction
  pred.RF <- predict(mod.RF,
                     newdata = test_2005,
                     type = "response")
  
  # On fait une matrice de confusion pour récupérer l'accuracy
  cM <-caret::confusionMatrix(factor(pred.RF, levels = levels(test_2005$stat_cause_descr)),
                              reference = test_2005$stat_cause_descr)
  accuracy_i <- cM$overall["Accuracy"]
  nbarbres[j] <- i
  accuracy[j] <- accuracy_i
  print(j)
  j = j+1
}

# boucle avec les trucs 
plot(nbarbres, accuracy)

# random forest avec que 5 années de 5 ans en 5 depuis 1995 ----

nbarbres <- rep(0, 10)
accuracy <- rep(0, 10)
j = 1
for (i in seq(from = 1,
              to = 50,
              length.out = 5)){
  # On fait le modèle randomForest à i arbres
  mod.RF <- randomForest(stat_cause_descr ~ .,
                         ntree = i,
                         data = train_2005)
  
  # On récupère la prédiction
  pred.RF <- predict(mod.RF,
                     newdata = test_2005,
                     type = "response")
  
  # On fait une matrice de confusion pour récupérer l'accuracy
  cM <-caret::confusionMatrix(factor(pred.RF, levels = levels(test_2005$stat_cause_descr)),
                              reference = test_2005$stat_cause_descr)
  accuracy_i <- cM$overall["Accuracy"]
  nbarbres[j] <- i
  accuracy[j] <- accuracy_i
  print(j)
  j = j+1
}

# boucle avec les trucs 
plot(nbarbres, accuracy)

# boucle avec les trucs 

x <- rep(0,14)
y <- rep(0,14)
for (i in seq(from = 1, to = 70, by = 5)){

  mod.RF <- randomForest(stat_cause_descr ~ ., 
                       ntree = i,
                       data = train_2005)

  pred.RF <- predict(mod.RF,
                   newdata= test_2005,
                   type = "response")

  cM <- caret::confusionMatrix(factor(pred.RF,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr)
  tree_i <- cM$overall["Accuracy"]
  x <- x + i
  y <- y + tree_i
}

## méthode bourrin----
time_70 <- system.time(mod.RF_70 <- randomForest(stat_cause_descr ~ .,
                                      ntree = 70,
                                      data = train_2005))

pred.RF_70 <- predict(mod.RF_70,
                      newdata= test_2005,
                      type="response")

cM <- caret::confusionMatrix(factor(pred.RF_70,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr)
tree_70 <- cM$overall["Accuracy"]
tree_70

time_60 <- system.time(mod.RF_60 <- randomForest(stat_cause_descr ~ .,
                          ntree = 60,
                          data = train_2005))

pred.RF_60 <- predict(mod.RF_60,
                   newdata= test_2005,
                   type="response")

cM <- caret::confusionMatrix(factor(pred.RF_60,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr)
tree_60 <- cM$overall["Accuracy"]
tree_60

time_50 <- system.time(mod.RF_50 <- randomForest(stat_cause_descr ~ .,
                                       ntree = 50,
                                       data = train_2005))

pred.RF_50 <- predict(mod.RF_50,
                   newdata= test_2005,
                   type="response")

cM <- caret::confusionMatrix(factor(pred.RF_50,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr)
tree_50 <- cM$overall["Accuracy"]
tree_50

time_40 <- system.time(mod.RF_40 <- randomForest(stat_cause_descr ~ .,
                          ntree = 40,
                          data = train_2005))

pred.RF_40 <- predict(mod.RF_40,
                   newdata= test_2005,
                   type="response")

cM <- caret::confusionMatrix(factor(pred.RF_40,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr)
tree_40 <- cM$overall["Accuracy"]
tree_40

time_30 <- system.time(mod.RF_30 <- randomForest(stat_cause_descr ~ .,
                          ntree = 30,
                          data = train_2005))

pred.RF_30 <- predict(mod.RF_30,
                   newdata= test_2005,
                   type="response")

cM <- caret::confusionMatrix(factor(pred.RF_30,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr)
tree_30 <- cM$overall["Accuracy"]
tree_30

time_20 <- system.time (mod.RF_20 <- randomForest(stat_cause_descr ~ .,
                          ntree = 20,
                          data = train_2005))

pred.RF_20 <- predict(mod.RF_20,
                   newdata= test_2005,
                   type="response")

cM <- caret::confusionMatrix(factor(pred.RF_20,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr)
tree_20 <- cM$overall["Accuracy"]
tree_20

time_10 <- system.time(mod.RF_10 <- randomForest(stat_cause_descr ~ .,
                          ntree = 10,
                          data = train_2005))

pred.RF_10 <- predict(mod.RF_10,
                   newdata= test_2005,
                   type="response")

cM <- caret::confusionMatrix(factor(pred.RF_10,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr)
tree_10 <- cM$overall["Accuracy"]

nbarbres <- c(10,20,30,40,50,60,70)
accuracy <- c(tree_10, tree_20, tree_30, tree_40, tree_50, tree_60, tree_70)
time <- c(time_10[3], time_20[3], time_30[3], time_40[3], time_50[3], time_60[3], time_70[3])

plot(nbarbres, accuracy, type = "l")
plot(nbarbres, time, "l")

# essai avec lapply 

lapply(x = train_2005, FUN = randomForest(stat_cause_descr ~ ., 
                                   ntree = i,
                                   data = train_2005))

lapply(x = test_2005, FUN = predict(mod.RF, newdata= test_2005,
                                    type = "response"))

lapply(x = test_2005, FUN = caret::confusionMatrix(factor(pred.RF,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr))


### un exemple :

# #create 3 models
# RF.models = lapply(1:3,function(mtry) {
#   randomForest(formula=Species~.,data=iris[-test,],mtry=mtry)
# })
# #append extra model
# RF.models[[length(RF.models)+1]] = randomForest(formula=Species~.,data=iris[-test,],mtry=4)
# summary(RF.models)
# 
# #predict all models
# sapply(RF.models,predict,newdata=iris[test,])
# 
# #predict one model
# predict(RF.models[[length(RF.models)]],newdata=iris[test,])


tree_i <- cM$overall["Accuracy"]
x <- x + i
y <- y + tree_i




# j'ai utilisé une méthode bourrin parce que j'ai pas réussi lapply 
# mais meme comme ca ca marche pas :'(

# random forest "optimise"----

system.time(mod.RF <- randomForest(stat_cause_descr ~ ., 
                       ntree = 80,
                       nodesize = 100,
                       data = train_2005))
print(mod.RF)

mod.RF <- randomForest(stat_cause_descr ~ ., 
                       data = train_2005,
                       importance = TRUE)

pred.RF <- predict(mod.RF,
                   newdata= test_2005,
                   type="response")

cM <- caret::confusionMatrix(factor(pred.RF,levels=levels(test_2005$stat_cause_descr)),reference=test_2005$stat_cause_descr)
cM$overall["Accuracy"]
mod.RF$importance[order(mod.RF$importance[, 1], 
                                   decreasing = TRUE), ]


#######################
# Etape 2 - Prediction des causes pour les lignes Missing/Undefined
#######################