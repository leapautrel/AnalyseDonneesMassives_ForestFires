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

# On initalise les vecteurs
ntree <- c(1, 5, 10, 25, 50)
accuracy <- rep(0, 5)
syst_time <- rep(0, 5)

# 1 tree
mod_rf_1 <- randomForest(stat_cause_descr ~ .,
														 ntree = 1,
														 data = data_train)
p_rf_1 <- predict(mod_rf_1,
											newdata = data_test,
											type = "response")
cM_1 <- caret::confusionMatrix(factor(p_rf_1, 
																		levels = levels(data_test$stat_cause_descr)), 
														 reference = data_test$stat_cause_descr)
accuracy[1] <- cM_1$overall["Accuracy"]
syst_time[1] <- system.time(randomForest(stat_cause_descr ~ .,
																			 ntree = 1,
																			 data = data_train))[3]

# 5 trees
mod_rf_5 <- randomForest(stat_cause_descr ~ .,
												 ntree = 5,
												 data = data_train)
p_rf_5 <- predict(mod_rf_5,
									newdata = data_test,
									type = "response")
cM_5 <- caret::confusionMatrix(factor(p_rf_5, 
																		levels = levels(data_test$stat_cause_descr)), 
														 reference = data_test$stat_cause_descr)
accuracy[2] <- cM_5$overall["Accuracy"]
syst_time[2] <- system.time(randomForest(stat_cause_descr ~ .,
																				 ntree = 5,
																				 data = data_train))[3]
# 10 trees
mod_rf_10 <- randomForest(stat_cause_descr ~ .,
												 ntree = 10,
												 data = data_train)
p_rf_10 <- predict(mod_rf_10,
									newdata = data_test,
									type = "response")
cM_10 <- caret::confusionMatrix(factor(p_rf_10, 
																			levels = levels(data_test$stat_cause_descr)), 
															 reference = data_test$stat_cause_descr)
accuracy[3] <- cM_10$overall["Accuracy"]
syst_time[3] <- system.time(randomForest(stat_cause_descr ~ .,
																				 ntree = 10,
																				 data = data_train))[3]

# 25 trees 
mod_rf_25 <- randomForest(stat_cause_descr ~ .,
												 ntree = 25,
												 data = data_train)
p_rf_25 <- predict(mod_rf_25,
									newdata = data_test,
									type = "response")
cM_25 <- caret::confusionMatrix(factor(p_rf_25, 
																			levels = levels(data_test$stat_cause_descr)), 
															 reference = data_test$stat_cause_descr)
accuracy[4] <- cM_25$overall["Accuracy"]
syst_time[4] <- system.time(randomForest(stat_cause_descr ~ .,
																				 ntree = 25,
																				 data = data_train))[3]

# 50 trees
mod_rf_50 <- randomForest(stat_cause_descr ~ .,
													ntree = 50,
													data = data_train)
p_rf_50 <- predict(mod_rf_50,
									 newdata = data_test,
									 type = "response")
cM_50 <- caret::confusionMatrix(factor(p_rf_50, 
																			 levels = levels(data_test$stat_cause_descr)), 
																reference = data_test$stat_cause_descr)
accuracy[5] <- cM_50$overall["Accuracy"]
syst_time[5] <- system.time(randomForest(stat_cause_descr ~ .,
																				 ntree = 50,
																				 data = data_train))[3]

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
