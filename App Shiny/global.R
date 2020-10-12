# Importation des packages ----
library(shiny)							# Shiny : pour l'application
library(shinydashboard)			# Jolie application shiny
library(ggplot2)						# Pour les graphiques
library (data.table)				# Dataframe plus efficace pour donnees imposantes
library (leaflet)						# Cartographie
library(geojsonio)					# Cartographie (etats USA)

# Importation des donnees ---- 
fires <- fread(
	"./data/fires.csv",
	header = TRUE,
	sep = ",",
	na.strings = "",
	blank.lines.skip = TRUE,
	stringsAsFactors = TRUE
)

# Importation des polygones des Etats des USA
states <-
	geojsonio::geojson_read(
		"./us-states.json",
		what = "sp"
	)


# Transformation des donnees ----
## On ajuste les types des colonnes mal importees
fires$fire_year <- as.factor(fires$fire_year) # l'annee en facteur
fires$month <- as.factor(data.table::month(fires[, fire_date]))

## On transforme les donnees pour la cartographie
states@data[,3] <- NULL
state.abb <- append(state.abb, c("DC", "PR"))
state.name <- append(state.name, c("District of Columbia", "Puerto Rico"))
fires$state <- state.name[match(fires[, state], state.abb)]

# Graphiques non interactifs ----
# Graphique de l'annee
plot_annee <- ggplot(as.data.frame(fires), aes(x = fire_year)) +
	geom_histogram(stat = "count", # Nombre de feux
								 aes(fill = stat_cause_descr), # Couleur selon cause
								 position = "stack") + # Histogramme empile
	labs(title = "", x = "", y = "") + # Titres
	scale_fill_discrete(name = "Cause of the fire") + # Nom de la legende
	theme(axis.text.x = element_text(angle = 70), # Penche les annees
				plot.title = element_text(hjust = 0.5) # Centre le titre
	)

# Graphique par mois
plot_mois <- ggplot(as.data.frame(fires), aes(x = month)) +
	geom_histogram(stat = "count", # Nombre de feux
								 aes(fill = stat_cause_descr), # Couleur selon cause
								 position = "stack") + # Histogramme empile
	labs(title = "", x = "", y = "") + # Titres
	scale_fill_discrete(name = "Cause of the fire") + # Nom de la legende 
	scale_x_discrete(labels=c("1" = "January   ",
														"2" = "February   ",
														"3" = "March   ",
														"4" = "April   ",
														"5" = "May   ",
														"6" = "June   ",
														"7" = "July   ",
														"8" = "August   ",
														"9" = "September   ",
														"10" = "October   ",
														"11" = "November   ",
														"12" = "December   ")) +
	theme(axis.text.x = element_text(angle = 70), # Penche les annees
				plot.title = element_text(hjust = 0.5) # Centre le titre
	)