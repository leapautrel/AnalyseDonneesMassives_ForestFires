# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)
<<<<<<< Updated upstream
require(ggmap)
require (leaflet)
require(nnet)  
=======
require(nnet)            # Multinomial logistic regression
require(leaps)           # For regsubsets
require(RcmdrMisc)       # For Stepwise
require(pls)             # For segments
require(groupdata2)      # For fold
require(boot)            # For cv.glm


>>>>>>> Stashed changes

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
# x = fires_pred[, 2:5] 
# y = fires_pred$stat_cause_descr

<<<<<<< Updated upstream
mod = multinom(stat_cause_descr~.,data = fires_pred)
coef(mod)


mod = multinom(stat_cause_descr~.,
               data=fires_pred,
               maxit=200) # ML fit of the logistic model #il converge ? 150 it?rations, d?viance apr?s cv 192.72
=======
mod = multinom(stat_cause_descr~.,
               data=fires_pred,
               maxit=200,
               trace=FALSE) # ML fit of the logistic model #il converge ? 150 it?rations, d?viance apr?s cv 192.72
>>>>>>> Stashed changes
coef(mod) # les valeurs des coeef impact?s en allant jusque'? la cv

### Assessment of the fit  
### en utilisan la d?viance expliqu?e, 
### diff?rence entre d?viance du mod?le nul et la d?viance du mod?le avec toutes les variables

deviance(mod)          # Residual deviance # 

<<<<<<< Updated upstream
mod0 = multinom(stat_cause_descr~1,data=fires_pred) # ML fit of the null model
=======
mod0 = multinom(stat_cause_descr~1,
                data=fires_pred) # ML fit of the null model
>>>>>>> Stashed changes
deviance(mod0)-deviance(mod)                # Explained deviance

### Observed versus fitted values
### indicateurs plus graphiques 

proba = fitted(mod)            # Estimated probabilities of each class
observed_class = fires_pred$stat_cause_descr
head(data.frame(round(proba,3),observed_class))

# Confusion matrix

fitted_class = predict(mod,type="class")  # Bayes rule pour chaque individu 
# je peux calculer la proba q'uil provienne du site 1 2 etc. 
<<<<<<< Updated upstream
#et il serait dans le site avec la plus grande proba
=======
# et il serait dans le site avec la plus grande proba
>>>>>>> Stashed changes
head(fitted_class)

confusion = table(observed_class,fitted_class)
confusion

rowSums(confusion)
colSums(confusion)
<<<<<<< Updated upstream
confusion_percentage = 100*confusion/outer(rowSums(confusion),rep(1,5))
round(confusion_percentage,3)


# faire la ligne avec une CV 


=======
# confusion_percentage = 100*confusion/outer(rowSums(confusion),rep(1,5))
# round(confusion_percentage,3)

### ADB2
# faire la ligne avec une CV 
## ML fit the most complete model for 'stast_cause_descr'
# ML fit of the logistic model



## Accuracy for the best sub-models

observed = fires_pred$stat_cause_descr

### Accuracy values for best submodels
acc = rep(0,5) # Initialize a vector of accuracy values
for (k in 1:5) {
  select = stepwise(mod,direction="forward/backward",criterion="AIC",steps=k,trace=0)
  predictions = predict(select,type="class")
  acc[k] = mean(predictions==observed)
}   

### 10-fold cross-validated accuracy values for best submodels

# cvacc = rep(0,5) # Initialize a vector of accuracy values
# 
# folds = fold(fires_pred,k=10,cat_col="stats_cause_descr")$".folds" # Create balanced segments
# folds 
# 
# cvpredictions = rep("1",nrow(fires_pred)) # Initialize a vector of predicted classes
# 
# for (k in 1:5) {
#   select = stepwise(mod,direction="forward/backward",criterion="AIC",steps=k,trace=0)
#   for (j in 1:10) {
#     train = fires_pred[folds!=j,]
#     test = fires_pred[folds==j,]
#     submod = multinom(formula(select),data=train,trace=FALSE,maxit=200) 
# #     cvpredictions[folds==j] = predict(submod,newdata=test,type="class")
# # #   }
# #   cvacc[k] = mean(cvpredictions==fires_pred$stat_cause_descr)
# # }   
# 
# ### Accuracy plot for stepwise feature selection
# plot(1:5,acc,type="b",pch=16,xlab="Number of variables in the submodel",
#      ylab="Accuracy",main="Quality of fit of best submodels",
#      cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2,col="darkgray",bty="l")
# 
# lines(1:5,
#       cvacc,type="b",col="blue",pch=15,lwd=2)
# 
# legend("topleft",
#        lwd=2,
#        pch=c(16,15),
#        legend=c("Internal validation", "Cross validation"),
#        bty="n",
#        cex=1.25,
#        col=c("darkgray","blue"))
# grid()

### 10-fold cross-validated accuracy for best submodel

folds = fold(fires_pred,k=10,cat_col="stat_cause_descr")$".folds" # Create balanced segments
folds 

cvpredictions = rep("1",nrow(fires_pred)) # Initialize a vector of predicted classes

# plus propre parce qu'on ajuste sur les données d'apprentissage 
#et on n'utilise pas les données test pour l'apprentissage

for (j in 1:10) {
  train = fires_pred[folds!=j,]
  test = fires_pred[folds==j,]
  mod = multinom(stat_cause_descr~.,data=train,trace=FALSE,maxit=200) 
  select = stepwise(mod,direction="forward/backward",criterion="AIC",trace=0)
  cvpredictions[folds==j] = predict(select,newdata=test,type="class")
  print(paste("Segment ",j,sep=""))
}

mean(cvpredictions==fires_pred$stat_cause_descr)

table(cvpredictions$stat_cause_descr,cvpredictions)

### regression penalisee


## Standard Normal Variate transformation of NIRS

## Displays the NIRS

## LASSO estimation of the regression model

y = fires_preds$stat_cause_descr

### Choice of the best penalty parameter

loglambda = seq(-10,-1,length=100)

fires_pred.cvlasso = cv.glmnet(snv_x,y,family="multinomial",type.measure="deviance",
                                lambda=exp(loglambda))

### CV'd residual deviance for each penalty parameter

plot(fires_pred.cvlasso)

### Assessment of the model obtained with optimal lambda

fires_pred.lasso = glmnet(snv_x,y,family="multinomial",lambda=exp(loglambda))

proba = predict(fires_pred.lasso,newx=snv_x,
                type="response")[,,which.min(fire_pred.cvlasso$cvm)]
head(proba)   # For each pred, estimated class probabilities

predictions = predict(fire_pred.lasso,newx=snv_x,
                      type="class")[,which.min(fires_pred.cvlasso$cvm)]
head(predictions)   # For each fire_pred, predicted class using Bayes rule

confusion = table(fire_pred$stat_cause_descr,predictions)

acc = mean(fires_pred$stat_cause_descr==predictions)
acc

### Accuracy of the Lasso regression model

dta = data.frame(snv_x,"stat_cause_descr"=y)
dim(dta)

segs = fold(dta,k=10,cat_col="stat_cause_descr")$".folds"

# 10-fold balanced partition of the sample

cvpredictions = rep("0",nrow=nrow(dta))
# Empty matrix to store the CV'd probabilities,
# # for each coffee, of being located in Loc. 6
# 
# for (k in 1:10) {
#   train = dta[segs!=k,]
#   test = dta[segs==k,]
#   dta.cvlasso = cv.glmnet(as.matrix(train[,-1051]),train[,1051],family="multinomial",
#                           type.measure="deviance",lambda=exp(loglambda))
#   dta.lasso = glmnet(as.matrix(train[,-1051]),train[,1051],family="multinomial",
#                      lambda=exp(loglambda))
#   cvpredictions[segs==k] = predict(dta.lasso,newx=as.matrix(test[,-1051]),
#                                    type="class")[,which.min(dta.cvlasso$cvm)]
#   print(paste("Segment ",k," over 10",sep=""))
# }

cv.acc = mean(fires_pred_nirs$stat_cause_descr==cvpredictions)
cv.acc # 0.95
>>>>>>> Stashed changes



