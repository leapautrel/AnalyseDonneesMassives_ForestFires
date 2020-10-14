shinyServer(function(input, output) {
	# 3.1 Accueil : jeu de données ----
	output$jeudedonnees <- renderDataTable({
		DT::datatable(fires,
									options = list(pageLength = 5,
																 searching = FALSE))
	})
	
	# UI 3.1 Accueil : Infos boxes ----
	## Feux par foudre en moyenne
	output$fireslighning <- renderInfoBox({
		infoBox(
			"Nombre moyen de feux par an causés par la foudre",
			round(mean(fires[, list(fire_count = .N), by = list(stat_cause_descr, fire_year)]$fire_count)),
			icon = icon("bolt"),
			color = "yellow"
		)
	})
	
	## State ayant connu le + de feux
	output$statemostfires <- renderInfoBox({
		infoBox(
			"Etat ayant connu le plus de feux entre 1992 et 2015",
			fires[which.max((fires[, list(state, fire_count = .N), by = list(state)])$fire_count), 5],
			icon = icon("flag-usa"),
			color = "lime"
		)
	})
	
	## Cause la plus frequente
	output$causefreq <- renderInfoBox({
		infoBox(
			"Cause la plus fréquente des feux",
			fires[which.max((fires[, list(stat_cause_descr, fire_count = .N), by = list(stat_cause_descr)])$fire_count), 1],
			icon = icon("dumpster-fire"),
			color = "red"
		)
	})
	
	
	# 3.2 Graphiques non interactifs ----
	output$plot_annee <-
		renderPlot({
			plot_annee
		})
	output$plot_mois <- renderPlot({
		plot_mois
	})
	output$plot_taillecause <- renderPlot({
		plot_taillecause
	})
	
	
	# 3.3 Carto ----
	# 3.3.1 Infos time frame ----
	output$infotimeframe1 <- renderInfoBox({
		input$go_cartes
		isolate({
			# GC1 - Recuperer le vecteur des dates ----
			r <- as.Date(input$idDateRange)
			
			# GC2 - Tableau i des feux dans ce daterange ----
			i <-
				fires[which(fires$fire_date >= r[1] &
											fires$fire_date <= r[2]),]
			i <- data.table::setorderv(x = i,
																 cols = "fire_size",
																 order = -1)
			
			infoBox(
				"Nombre de feux total",
				nrow(i),
				icon = icon("fire"),
				color = "yellow"
			)
		})
	})
	
	output$infotimeframe2 <- renderInfoBox({
		input$go_cartes
		isolate({
			# GC1 - Recuperer le vecteur des dates ----
			r <- as.Date(input$idDateRange)
			
			# GC2 - Tableau i des feux dans ce daterange ----
			i <-
				fires[which(fires$fire_date >= r[1] &
											fires$fire_date <= r[2]),]
			i <- data.table::setorderv(x = i,
																 cols = "fire_size",
																 order = -1)
			
			infoBox(
				"Surface totale brulée (en km²)",
				round(sum(i[, ]$fire_size / 247.105381)),
				icon = icon("tree"),
				color = "lime"
			)
		})
	})
	
	output$infotimeframe3 <- renderInfoBox({
		input$go_cartes
		isolate({
			# GC1 - Recuperer le vecteur des dates ----
			r <- as.Date(input$idDateRange)
			
			# GC2 - Tableau i des feux dans ce daterange ----
			i <-
				fires[which(fires$fire_date >= r[1] &
											fires$fire_date <= r[2]),]
			i <- data.table::setorderv(x = i,
																 cols = "fire_size",
																 order = -1)
			
			infoBox(
				"Surface moyenne brulée (en m²/jour)",
				round(mean(i[, list(fire_size), by = list(fire_date)]$fire_size / 4046.85642), 2),
				icon = icon("globe-americas"),
				color = "aqua"
			)
		})
	})
	
	# 3.3.2 Carte 1 : 1000 plus gros feux ----
	output$carte1 <- renderLeaflet({
		input$go_cartes
		isolate({
			# Recuperer le vecteur des dates
			r <- as.Date(input$idDateRange)
			
			# Selection des n largest fires
			i <-
				fires[which(fires$fire_date >= r[1] & fires$fire_date <= r[2]), ]
			i <- data.table::setorderv(x = i,
																 cols = "fire_size",
																 order = -1)
			nlargestfires <- input$nblargestfires
			i <- i[1:nlargestfires,]
			
			# Creation d'une palette de couleurs
			ncauses <- nlevels(i[, stat_cause_descr])
			pal <- leaflet::colorFactor(palette = palcol,
																	domain = i[, stat_cause_descr])
			
			# Creation de la carte
			m <- leaflet::leaflet(i) %>%
				leaflet::addTiles() %>%
				
				# Centrage de la carte
				fitBounds(
					lng1 = min(i[, longitude]),
					lat1 = min(i[, latitude]),
					lng2 = max(i[, longitude]),
					lat2 = max(i[, latitude])
				) %>%
				
				# ajout des cercles
				leaflet::addCircles(
					lng = i[, longitude],
					lat = i[, latitude],
					radius = i[, fire_size],
					weight = 0.2,
					stroke = T,
					opacity = 100,
					fill = T,
					color = ~ pal(stat_cause_descr),
					fillOpacity = 0.2
				) %>%
				
				# ajout de la legende
				addLegend(
					"bottomleft",
					pal = pal,
					values = ~ stat_cause_descr,
					title = "Cause of the fire",
					opacity = 0.7
				)
		})
	})
	
	# 3.3.3 Carte 2 : aggrege par etat ----
	output$carte2 <- renderLeaflet({
		input$go_cartes
		isolate({
			# Recuperer le vecteur des dates
			r <- as.Date(input$idDateRange)
			
			# Recuperer tous les feux de cette periode
			i <-
				fires[which(fires$fire_date >= r[1] &
											fires$fire_date <= r[2]), ]
			
			# Agregation par etat
			fires_vecdate_bystate <- i[, list(fire_count = .N),
																 by = list(name = state)]
			j <- fires[, list(fire_count = 0), by = list(name = state)]
			j <- merge(fires_vecdate_bystate,
								 j,
								 by = intersect("name", "name"),
								 all = TRUE)
			j$fire_count <- j$fire_count.x + j$fire_count.y
			j$fire_count[which(is.na(j$fire_count))] <- 0
			fires_vecdate_bystate <- j[,-c(2, 3)]
			fires_vecdate_bystate <- fires_vecdate_bystate[order (name)]
			states@data <- merge.data.frame(x = states@data,
																			y = fires_vecdate_bystate,
																			by = "name")
			states@data <-
				rbind(states@data[1:39,], states@data[41:52,], states@data[40,])
			
			# Bins de la legende
			bins <- round(seq(
				from = 0,
				to = max(fires_vecdate_bystate$fire_count) + 51,
				length.out = 7
			), -2) # arrondi à la centaine
			
			# Palette des couleurs
			pal2 <-
				colorBin("YlOrRd",
								 domain = states$fire_count,
								 bins = bins)
			
			# Legende interactive quand survol de la souris
			labels <- sprintf("<strong>%s</strong><br/>%g fires",
												states@data[, 1],
												states@data[, 3]) %>% lapply(htmltools::HTML)
			
			# Creation de la carte
			m2 <- leaflet(states) %>%
				addTiles() %>%
				
				# Tiles
				addProviderTiles("MapBox",
												 options = providerTileOptions(
												 	id = "mapbox.light",
												 	accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
												 )) %>%
				
				# Polygones des Etats
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
				
				# Centrage de la carte
				fitBounds(
					lng1 = min(fires[, longitude]),
					lat1 = min(fires[, latitude]),
					lng2 = max(fires[, longitude]),
					lat2 = max(fires[, latitude])
				) %>%
				
				# Legende
				addLegend(
					pal = pal2,
					values = ~ fire_count,
					opacity = 0.7,
					title = "Number of fires",
					position = "bottomleft"
				)
		})
	})
	
	# 3.4 Prevision ----
	# 3.4.1 Equilibre des donnees
	output$plot_firespred_bycause <-
		renderPlot({
			plot_firespred_bycause
		})
})
