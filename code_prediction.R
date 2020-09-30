# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)
require(ggmap)
require (leaflet)
require(nnet)  

# Creation de fonctions ----


# Set Working Directory ----
setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret") # Direction fichier Lea
# setwd("C:/Users/mimi/Desktop/M2/Analyse de données massives/projet") # Direction fichier Junyi

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
fires <- fires_all[, c(25, 20, 21, 29, 31, 32)]
summary(fires)



# construction de la prediction----
### selection de 10000 individus pour essayer de construire la prediction 
fires_pred <- fires[1:10000,]
head(fires_pred)
head(summary(fires_pred))

# on appelle les variables explicatives, la variable qu'on cherche a expliquer, Y, est stat_cause_descr
x = fires_pred[, 2:6] 
y = fires_pred$stat_cause_descr


### regarder le cafe pas le pig!!!!!!!!!!!!

mod = multinom(stat_cause_descr~.,data = fires_pred)
coef(mod)










# # Essai ggplot carte feux/USA ----
# ## Contours de la carte (localisaton)
# usa <- c(
# 	left = -127,
# 	bottom = 24,
# 	right = -64,
# 	top = 49.3
# )
# 
# ## Creation de la carte
# usa_map <- ggmap::get_map(
# 	location = usa,
# 	zoom = 4,
# 	messaging = FALSE)
# 
# ## Affichage de la carte
# ggmap::ggmap(usa_map)



# Essai leaflet carte ----
m <- leaflet::leaflet() %>%
	leaflet::addTiles() %>%
	leaflet::setView(lng = -95, 
									 lat = 37, 
									 zoom = 3) %>%
	addCircles(
		lng = fires[, "longitude"],
		lat = fires[, "latitude"],
		color = (fires[, "stat_cause_descr"]),
		fillOpacity = 1,
		opacity = 1
	)
m

# Site utile pour faire ça : https://rgeomatic.hypotheses.org/550