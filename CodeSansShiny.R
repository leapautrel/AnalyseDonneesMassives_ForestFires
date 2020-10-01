# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)
require(ggmap)
require (leaflet)

# Creation de fonctions ----


# Set Working Directory ----
setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret") # Direction fichier  GitHub Lea
# setwd("C:/Users/mimi/Desktop/M2/Analyse de données massives/projet") # Direction fichier Junyi

# Importation jeu de données propre (cf suite si qqn n'a pas le jeu de données sélectionné) ----
fires <- fread(
	"fires.csv",
	header = TRUE,
	sep = ",",
	na.strings = "",
	blank.lines.skip = TRUE,
	stringsAsFactors = TRUE
)


# <!> A FAIRE TOURNER 1 FOIS SEULEMENT <!> Nettoyage jeu de données complet puis export du jeu de données propre ----
# Importation jeu de données complet
fires_all <- fread(
	"fires_all.csv",
	header = TRUE,
	sep = ";",
	na.strings = "",
	blank.lines.skip = TRUE,
	stringsAsFactors = TRUE
)

# Nettoyage du jeu de données
## Sélection des 6 colonnes utiles (cause, lieu,)
fires <- fires_all[, c(25, 20, 21, 29, 31, 32)]

## Modification des colonnes de types inadaptés
### Modification de l'année en format factor
		# ne marche pas			
		# fires <- fires[ ,lapply(.SD, as.factor), by=fire_year]

### A faire : modifier la date en format date

## Vérification du jeu de données
summary(fires)

# Exporter le jeu de données propre
	# Afin d'avoir une application shiny qui charge plus rapidement
	# On exporte le jeu de données avec uniquement les colonnes utiles
	# Pour l'application shiny, on n'aura donc pas besoin de charger toutes les colonnes inutiles
write.csv(fires,".\\fires.csv", row.names = TRUE)

# Carte leaflet ----

# creation carte
m <- leaflet::leaflet() %>%
	leaflet::addTiles()

# position carte
m <- leaflet::setView(
	map = m,
	lng = -95,
	lat = 37,
	zoom = 3
)
	
## Centrage de la carte (pas fou)
# m <- fitBounds(map = m, 
# 							 lng1 = min(fires[, longitude]),
# 							 lat1 = min(fires[, latitude]),
# 							 lng2 = max(fires[, longitude]), 
# 							 lat2 = max(fires[, latitude]))

# ajout des cercles
m <- addCircleMarkers(map = m, 
											lng = fires[1:1000, longitude], 
											lat = fires[1:1000, latitude],
											radius = fires[1:1000, fire_size]/18000, 
											weight = 0.2, 
											stroke = T, 
											opacity = 100,
											fill = T, 
											fillColor = "#920000", 
											fillOpacity = 30,
											color = "white")

m

# Site utile pour faire ça : https://rgeomatic.hypotheses.org/550