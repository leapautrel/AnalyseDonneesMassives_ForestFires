# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)
require (lubridate)

# Set Working Directory ----
# setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret") # Direction fichier  GitHub Lea
# setwd("C:/Users/mimi/Desktop/M2/Analyse de données massives/projet") # Direction fichier Junyi

# <!> A FAIRE TOURNER 1 FOIS SEULEMENT <!> 
# Nettoyage jeu de données complet puis export du jeu de données propre
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
### Modifier la date en format date
fires_all$fire_date <-
	as.Date(fires_all[, discovery_doy], 
					origin = paste(fires_all[, fire_year] - 1, "-12-31", sep = ""))

# fires_all$fire_month <- month(fires_all[, fire_date], TRUE) # (inutile en fait)
print(object.size(fires_all), units = 'Gb')
names(fires_all)

## Sélection des colonnes utiles (cause, lieu,)
fires <- fires_all[, c(25, 29, 31, 32, 35, 20, 40)] 


## Vérification du jeu de données
summary(fires)

# Exporter le jeu de données propre
# Afin d'avoir une application shiny qui charge plus rapidement
# On exporte le jeu de données avec uniquement les colonnes utiles
# Pour l'application shiny, on n'aura donc pas besoin de charger toutes les colonnes inutiles
write.csv(fires,".\\fires.csv", row.names = FALSE)
