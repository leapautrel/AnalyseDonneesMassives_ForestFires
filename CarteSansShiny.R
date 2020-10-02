# Vidage de l'environnement ----
rm(list = ls())

# Importation des packages ----
require (data.table)	# adapté aux données massives
# require(ggmap)			# essai 1 de carte (changé pour leaflet)
require (leaflet) 		# Pour la carte 
require(geojsonio)		# Pour les States


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

fires$fire_year <- as.factor(fires$fire_year)
summary(fires)


# On récupère le vecteur de 2 dates
vecdates <- c("2005-01-01","2006-12-31")

# On fait une sélection du tableau entre ces dates
fires_vecdate <- copy(fires[which(fires[, fire_date] >= vecdates[1] &
																		fires[, fire_date] <= vecdates[2])])
# on trie par taille de feu
fires_vecdate <- data.table::setorderv(x = fires_vecdate,
																			cols = "fire_size",
																			order = -1)
# On fait une sélection des 1000 plus gros feux pour la carte leaflet
fires_vecdate_1000 <- fires_vecdate[1:1000,]

# Carte leaflet ----
# Cette carte permet de visualiser les 1000 plus gros feux dans cette période


# nb de causes -> GLOBAL
ncauses <- nlevels(fires[, stat_cause_descr])

# Creation d'une palette en fonction de la cause -> GLOBAL
pal <- leaflet::colorFactor(
	palette = rainbow(ncauses),
	domain = fires[, stat_cause_descr]
)


## Carte
# creation carte 1000 plus gros feux
m <- leaflet::leaflet(fires_vecdate_1000) %>%
	leaflet::addTiles() %>%
	
	# Centrage de la carte
	fitBounds(
		lng1 = min(fires_vecdate_1000[, longitude]),
		lat1 = min(fires_vecdate_1000[, latitude]),
		lng2 = max(fires_vecdate_1000[, longitude]),
		lat2 = max(fires_vecdate_1000[, latitude])
	) %>%
	
	# ajout des cercles
	leaflet::addCircleMarkers(
		lng = fires_vecdate_1000[, longitude],
		lat = fires_vecdate_1000[, latitude],
		radius = fires_vecdate_1000[, fire_size] / 15000,
		weight = 0.2,
		stroke = T,
		opacity = 100,
		fill = T,
		color = ~ pal(stat_cause_descr),
		fillOpacity = 0.3
	) %>%
	
	# ajout de la legende
	addLegend(
		"bottomright",
		pal = pal,
		values = ~ stat_cause_descr,
		title = "Cause of the fire",
		opacity = 0.5
	)

# Affichage carte
m

# Carte tous les feux ----

# On importe un fichier "states"
states <- geojsonio::geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json",
																	what = "sp")

# On supprime l'info 'density' parce qu'on s'en moque
states@data[,3] <- NULL

# On rajoute une colonne 'fires_count' dans chaque state
### Ajout des codes pou' DC et Puerto Rico aux listes state.abb / state.name
state.abb <- append(state.abb, c("DC", "PR"))
state.name <- append(state.name, c("District of Columbia", "Puerto Rico"))

### Passage de abb à name ('NY' -> 'New York')
fires_vecdate$state <- state.name[match(fires_vecdate[, state], state.abb)]

### On fait un aggregate
fires_vecdate_bystate <- fires_vecdate[, list(fire_count = .N),
																			 by = list(name = state)]
fires_vecdate_bystate <- fires_vecdate_bystate[order (name)]

### On merge 'states' et 'fires_vecdate_bystate'
states@data <- merge.data.frame(
	x = states@data,
	y = fires_vecdate_bystate,
	by = "name")

# on initialise des paramètres de classes (nombre, couleur)
bins <- c(0, 20, 50, 100, 200, 500, 1000, 2000, 5000, Inf)
pal2 <- colorBin("YlOrRd", domain = states$fire_count, bins = bins)

# on rajoute les labels
labels <- sprintf(
	"<strong>%s</strong><br/>%g fires",
	states@data[,1], states@data[,3]
) %>% lapply(htmltools::HTML)


# on créé la carte
m2 <- leaflet(states) %>%
	addTiles() %>%
	addProviderTiles("MapBox",
									 options = providerTileOptions(
									 	id = "mapbox.light",
									 	accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
									 )) %>%
	addPolygons(
		fillColor = ~ pal2(fire_count),
		weight = 2,
		opacity = 1,
		color = "white",
		dashArray = "3",
		fillOpacity = 0.7,
		highlight = highlightOptions(
			weight = 5,
			color = "#666",
			dashArray = "",
			fillOpacity = 0.7,
			bringToFront = TRUE
		),
		label = labels,
		labelOptions = labelOptions(
			style = list("font-weight" = "normal", padding = "3px 8px"),
			textsize = "15px",
			direction = "auto"
		)
	) %>%
	fitBounds(
		lng1 = min(fires_vecdate[, longitude]),
		lat1 = min(fires_vecdate[, latitude]),
		lng2 = max(fires_vecdate[, longitude]),
		lat2 = max(fires_vecdate[, latitude])
	) %>% addLegend(
		pal = pal2,
		values = ~ fire_count,
		opacity = 0.7,
		title = NULL,
		position = "bottomright"
	)

m2

# ATTENTION IL Y A UN PROBLEME ICI
# Décalage des états sur la carte de Puerto Rico jusqu'à Wyoming
# par ex : lieu Puerto Rico -> Affichage Wyoming sur carte
# Lieu Wyoming -> Affichage Wiscontin
# etc