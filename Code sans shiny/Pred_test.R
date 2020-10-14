rm(list = ls())

library (data.table)
library(randomForest)
library(ggplot2)

# Importation des donnees ---- 
fires <- fread(
	"D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret/AnalyseDonneesMassives_ForestFires/App Shiny/data/fires.csv",
	header = TRUE,
	sep = ",",
	na.strings = "",
	blank.lines.skip = TRUE,
	stringsAsFactors = TRUE
)

# Sélection des feux de cause connue pour les années qui nous intéressent ----
fires_selec <-
	fires[stat_cause_descr != "Missing/Undefined" &
					fire_year %in% c(1995, 2000, 2005, 2010, 2015), ]
fires_selec$fire_year <- factor(fires_selec$fire_year)
fires_selec$stat_cause_descr <- factor(fires_selec$stat_cause_descr)

# Création des tableaux train et test ----
set.seed(123)
train_index <- sample(c(TRUE, FALSE),
                           nrow(fires_selec),
                           replace = TRUE,
                           prob = c(0.8, 0.2))
test_index <- !train_index
data_train <- as.data.table(fires_selec[train_index, c(1:4, 6)])
data_test <- as.data.table(fires_selec[test_index, c(1:4, 6)])
print(object.size(data_train), units = 'Mb')

# Fonction pour random forest selon ntree ----
rf_ntrees <- function(ntree) {
	mod_rf <- randomForest(stat_cause_descr ~ .,
												 ntree = ntree,
												 data = data_train)
	p_rf <- predict(mod_rf,
									newdata = data_test,
									type = "response")
	cM <- caret::confusionMatrix(factor(p_rf,
																			levels = levels(data_test$stat_cause_descr)),
															 reference = data_test$stat_cause_descr)
	accuracy <- cM$overall["Accuracy"]
	syst_time <- system.time(randomForest(stat_cause_descr ~ .,
																				ntree = ntree,
																				data = data_train))[3]
	list(ntree = ntree, accuracy = accuracy, syst_time = syst_time)
}

# Application fonction ----
vec_ntree <- c(1, 2, 5, 10, 25, 50, 70)
res <- sapply(vec_ntree, FUN = rf_ntrees)
res_opt_rf <- data.frame(t(res))
res_opt_rf$ntree <- as.numeric(res_opt_rf$ntree)
res_opt_rf$accuracy <- as.numeric(res_opt_rf$accuracy)
res_opt_rf$syst_time <- as.numeric(res_opt_rf$syst_time)
res_opt_rf

# Export pour réutiliser ces données dans l'app Shiny
write.csv(res_opt_rf,".\\res_opt_rf.csv", row.names = FALSE)

# Graphiques Optimisation de l'algorithme randomForest
ggplot(data = res_opt_rf,
			 aes(x = ntree, y = accuracy)) +
	geom_line(colour = "#B83A1B")  +x
	geom_point(colour = "#B83A1B") +
	labs(x = "Nombre d'arbres", 
			 y = "Accuracy") +
	theme_minimal()

ggplot(data = res_opt_rf,
			 aes(x = ntree, y = syst_time)) +
	geom_line(colour = "#B83A1B") +
	geom_point(colour = "#B83A1B") +
	labs(x = "Nombre d'arbres", 
			 y = "Temps de calcul (en secondes)") +
	theme_minimal()
