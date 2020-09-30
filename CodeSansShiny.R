# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)
require(ggmap)
require (leaflet)

# Creation de fonctions ----


# Set Working Directory ----
setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret/AnalyseDonneesMassives_ForestFires") # Direction fichier  GitHub Lea
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

## Modification des colonnes de types inadaptés
### Modification de l'année en format factor
		# ne marche pas			
		# fires <- fires[ ,lapply(.SD, as.factor), by=fire_year]

### A faire : modifier la date en format date

## Vérification du jeu de données
summary(fires)

# Exporter le jeu de données propre ----
	# Afin d'avoir une application shiny qui charge plus rapidement
	# On exporte le jeu de données avec uniquement les colonnes utiles
	# Pour l'application shiny, on n'aura donc pas besoin de charger toutes les colonnes inutiles
write.csv(fires,".\\fires.csv", row.names = TRUE)


# Carte leaflet ----
## Creation des vecteurs latitude/longitude

m <- leaflet::leaflet() %>%
	leaflet::addTiles() %>%
	leaflet::setView(lng = -95, 
									 lat = 37, 
									 zoom = 3) 
# %>%
# 	addCircles(
# 		lng = fires[, 6],
# 		lat = fires[, 5],
# 		color = (fires[, "stat_cause_descr"]),
# 		fillOpacity = 1,
# 		opacity = 1
# 	)
m

# Site utile pour faire ça : https://rgeomatic.hypotheses.org/550