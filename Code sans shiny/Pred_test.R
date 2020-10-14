rm(list = ls())

library (data.table)				# Dataframe plus efficace pour donnees imposantes
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
vec_ntree <- c(1, 2, 5, 10, 25, 50, 75)
res <- sapply(vec_ntree, FUN = rf_ntrees)
res

# CA C'EST PLUS BON, a réadapter avec la fonction
data_plot <- data.frame(ntree = ntree,
												accuracy = accuracy, 
												syst_time = syst_time)

# Graphique
ggplot() +
	geom_line(mapping = aes(x = ntree, y = accuracy)) +
theme_minimal()
	
ggplot() +
	geom_line(mapping = aes(x = ntree, y = syst_time)) +
	theme_minimal()
