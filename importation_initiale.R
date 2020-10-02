# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)

# Set Working Directory ----
setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret") # Direction fichier  GitHub Lea
# setwd("C:/Users/mimi/Desktop/M2/Analyse de données massives/projet") # Direction fichier Junyi

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
### A faire : modifier la date en format date
new_date <- as.Date(all[, 20], origin = paste(all[, 18] - 1, "-12-31", sep = ""))
## Sélection des 6 colonnes utiles (cause, lieu,)
fires <- fires_all[, c(25, 20, 21, 29, 31, 32)] # adapter

## Modification des colonnes de types inadaptés
### Modification de l'année en format factor
# ne marche pas			
# fires <- fires[ ,lapply(.SD, as.factor), by=fire_year]

## Vérification du jeu de données
summary(fires)

# Exporter le jeu de données propre
# Afin d'avoir une application shiny qui charge plus rapidement
# On exporte le jeu de données avec uniquement les colonnes utiles
# Pour l'application shiny, on n'aura donc pas besoin de charger toutes les colonnes inutiles
write.csv(fires,".\\fires.csv", row.names = FALSE)
