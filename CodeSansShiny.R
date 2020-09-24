# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)
require(ggplot)
require (leaflet)

# Set Working Directory ----
setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet") # Direction fichier Lea
# setwd("C:/Users/mimi/Desktop/M2/Analyse de données massives/projet") # Direction fichier Junyi

# Importation ----
fire <- fread(
	"fires_all.csv",
	header = TRUE,
	sep = ";",
	na.strings = "",
	blank.lines.skip = T
)
fire[, Shape := NULL] # Supprime la ligne Shape

## Verification de la bonne importation
summary(fire)

# Essai ggplot carte feux/USA ----
## Contours de la carte (localisaton)
usa <- c(
	left = -127,
	bottom = 24,
	right = -64,
	top = 49.3
)

## Creation de la carte
usa_map <- ggmap::get_map(
	location = usa,
	zoom = 4,
	messaging = FALSE)

## Affichage de la carte
ggmap::ggmap(usa_map)


# Essai leaflet carte ----
m <- leaflet::leaflet()
m <- leaflet::addTiles(m)
m <- leaflet::setView(m, lng = -95, lat = 37, zoom = 3) 
m
