## 1. header -------------------------------
header <- dashboardHeader(
  title = h1("US wildfires from 1992 to 2015"),
  titleWidth  = 350,
  dropdownMenu(
    type = 'messages',
    messageItem(
      from = "Source",
      message = "Les données viennent de kaggle",
      icon = icon("kaggle"),
      href = "https://www.kaggle.com/rtatman/188-million-us-wildfires"
    )
  )
)

## 2. sidebar -------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      
      ## 2.1 - Accueil -----------
      menuItem( "Présentation", tabName = 'accueil', icon = icon('fire')),
      
      ## 2.2 Graphiques non interactifs -----------
      menuItem( "Visualisation", tabName = 'graph', icon = icon('chart-bar')),
      
      ## 2.3 - Cartographie ----------------------
      menuItem( "Cartographie", tabName = 'carto', icon = icon('globe-americas')),
      
      ## 2.4 - Prediction ----------------------
      menuItem( "Prédiction", tabName = 'pred', icon = icon('search'))
      )
  )


## 3. body -------------------------------
body <- dashboardBody(tabItems(
	## 3.1 Accueil -------------------------------------------------------------
	tabItem(
		tabName = "accueil",
		h2("Présentation"),
		# 3.1.1 - Presentation ----
		fluidRow(
			shinydashboard::box(
				title = h3("1.88 Million US Wildfires"),
				status = "warning",
				width = 12,
				solidHeader = F,
				HTML(
					"<p>",
					"Ces données ont été récoltées pour être utilisées dans le cadre du programme national des Etats-Unis d'analyse des feux (Fire Program Analysis).
                	Le jeu de données initial contient diverses informations, dont entre autres :",
					"</p>",
					"<ul>",
					"<li> <p> Sur l'obtention des données de chaque feu (organisme...) ;</p>",
					"<li> <p> Sur la localisation du feu et sa forme ;</p>",
					"<li> <p> Sur la date de découverte du feu et sur la durée du feu ;</p>",
					"<li> <p> ...</p>",
					"</ul>",
					"<p>",
					"Elles proviennent de <a href='https://www.kaggle.com/rtatman/188-million-us-wildfires'>kaggle</a>.
            			Afin de simplifier le jeu de données, seules certaines informations sont gardées.",
					"</p>"
				)
			)
		),
		
		# 3.1.2 : Résumé du jeu de données ----
		
		fluidRow(
			h3("Tableau de données modifié"),
			dataTableOutput("jeudedonnees")
		),
		
		# 3.1.3 Info boxes ----
		fluidRow(
			br(),
			h3("Quelques informations"),
			infoBox(
				"Nombre de feux entre 1992 et 2015",
				nrow(fires),
				icon = icon("fire-extinguisher"),
				color = "aqua",
				width = 6
			),
			infoBoxOutput("fireslighning", width = 6)
		),
		fluidRow(
			infoBoxOutput("statemostfires", width = 6),
			infoBoxOutput("causefreq", width = 6)
		)
	),
	
	## 3.2 Graphiques non interactifs -------------------------------------------------------------
	tabItem(
		tabName = "graph",
		h2("Visualisation des données"),
		fluidRow(
			column(
				4.5,
				align = "center",
				h3("Number of fires by year"),
				p("By cause",
					style = "text-align: center; "),
				plotOutput('plot_annee')
			),
			
			p(br(), br(), br()),
			column(
				4.5,
				align = "center",
				h3("Number of fires by month"),
				p("By cause - Sum from 1992 to 2015",
					style = "text-align: center; "),
				plotOutput('plot_mois')
			),
			
			p(br(), br(), br()),
			column(
				4.5,
				align = "center",
				h3("Average wildfire size by cause"),
				p("Data from 1992 to 2015",
					style = "text-align: center; "),
				plotOutput('plot_taillecause')
			)
		)
	),
	
	## 3.3 Cartographie --------------------------------------------------------
	tabItem(
		tabName = "carto",
		h2("Cartographie"),
		# 3.3.1 Time frame
		fluidRow(column(
			width = 12,
			align = "center",
			box(
				# Selection de l'intervalle de temps
				dateRangeInput(
					inputId = "idDateRange",
					label = "Choose a time frame",
					start = "1992-01-01",
					end = "2015-12-31",
					format = "yyyy-mm-dd",
					language = "en",
					separator = " to "
				),
				br(),
				
				# Choix du nb de feux à afficher
				sliderInput(
					inputId = "nblargestfires",
					label = h3("Number of fires to display on the 1st map"),
					min = 2,
					max = 10000,
					value = 1000,
					step = 1
				),
				
				# Bouton update
				actionButton("go_cartes", "Update"),
				
				# Options box
				width = 12,
				status = "warning",
				solidHeader = FALSE,
				collapsible = TRUE
			)
		)),
		# 3.3.2 Quelques infos dans ce time frame
		fluidRow(
			br(),
			h3("Quelques infos dans cet intervalle de temps"),
			infoBoxOutput("infotimeframe1", width = 4),
			infoBoxOutput("infotimeframe2", width = 4),
			infoBoxOutput("infotimeframe3", width = 4),
			br(), br()
		),
		
		# 3.3.3 Affichage des cartes ----
		fluidRow(
			# Carte 1
			column(
				12,
				align = "center",
				h3("Location and cause of the largest fires in this time frame"),
				tabPanel("carte 1.", leafletOutput("carte1")),
				br(), br()
			),
			# Carte 2
			fluidRow(
				column(
					12,
					align = "center",
					h3("Number of fires in each state in this time frame"),
					tabPanel("carte 2.", leafletOutput("carte2"))
				)
			))
		),
		
		## 3.3 Prediction ----------------------------------------------------------
		tabItem(tabName = "pred",
						h2("Prédiction"))
	)
)


# 4. Rassemblement : dashboardPage----
# 4.1 Feuille de style ----
tags$head(tags$style(
	HTML(
		'h1 {font-family: "Times New Roman", Times, serif; font-size: 20px; letter-spacing: -1.2px; font-weight: 400; text-transform: uppercase; }
		h2 { font-family: Baskerville, "Baskerville Old Face" ; font-size: 40px; text-align: center; }
    h3 { font-family: Baskerville, "Baskerville Old Face", "Hoefler Text", Garamond, "Times New Roman", serif; font-size: 30px; text-align: center; }
    h4 { font-family: Baskerville, "Baskerville Old Face", "Hoefler Text", Garamond, "Times New Roman", serif; font-size: 23px; text-align: center; letter-spacing: -1.2px; font-weight: 400; text-transform: uppercase; }
    p { font-family: Baskerville, "Baskerville Old Face", "Hoefler Text", Garamond, "Times New Roman", serif; font-size: 20px; text-align: justify; } 
		div.info.legend.leaflet-control br {clear: both; text-align: left; align-self: left;}'
	)
),
# 4.2 dashboardPage ----
dashboardPage(
	# bootstrapPage(
	# 	tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both; text-align: left;}")),
		header,
		sidebar,
		body,
		skin = "black"
	)
)