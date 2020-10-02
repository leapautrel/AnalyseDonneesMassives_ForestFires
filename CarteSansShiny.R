# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)	# adapté aux données massives
# require(ggmap)			# essai 1 de carte (changé pour leaflet)
require (leaflet) 		# Pour la carte 
require(RColorBrewer) # Pour la couleur dans la carte


# Creation de fonctions ----


# Set Working Directory ----
setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret") # Direction fichier  GitHub Lea
# setwd("C:/Users/mimi/Desktop/M2/Analyse de données massives/projet") # Direction fichier Junyi

# Importation jeu de données propre (cf document 'importation_initiale') ----
fires <- fread(
	"fires.csv",
	header = TRUE,
	sep = ",",
	na.strings = "",
	blank.lines.skip = TRUE,
	stringsAsFactors = TRUE
)


# Carte leaflet ----

# Creation d'une palette en fonction de la cause
pal <- leaflet::colorFactor(
	palette = brewer.pal(nlevels(fires[,stat_cause_descr]),
											 "Set3"),
	domain = fires[, stat_cause_descr]
)

## Carte
# creation carte
m <- leaflet::leaflet(fires[1:10000,]) %>%
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
m <- leaflet::addCircleMarkers(
	map = m,
	lng = fires[1:10000, longitude],
	lat = fires[1:10000, latitude],
	radius = fires[1:10000, fire_size] / 10000,
	weight = 0.2,
	stroke = T,
	opacity = 100,
	fill = T,
	color = ~pal(stat_cause_descr),
	fillOpacity = 0.75
)

# ajout de la legende
addLegend(map = m,
					"bottomright", 
					pal = pal, 
					values = ~stat_cause_descr,
					title = "Cause of the fire",
					opacity = 1
)


# Site utile pour faire ça : https://rgeomatic.hypotheses.org/550