# A finir : arbre de décision graphe
# + matrice de confusion

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
      menuItem( "Prédiction", tabName = 'pred', icon = icon('search'),
      					menuItem("Problématique et démarche", 
      									 tabName = 'pred1',
      									 icon = icon('play')),
      					menuItem("Random Forest", 
      									 tabName = 'pred2',
      									 icon = icon('play')),
      					menuItem("Conclusion", 
      									 tabName = 'pred3',
      									 icon = icon('play'))
      					)
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
			infoBoxOutput("causefreq", width = 6),
			infoBoxOutput("statemostfires", width = 6)
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
					label = h3("Choose a time frame"),
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
					label = HTML("<h3> Number of fires to display on the 1st map </h3>
											 <p>The first map will display only the n largest fires.</p>"),
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
			br(),
			br()
		),
		
		# 3.3.3 Affichage des cartes ----
		fluidRow(
			# Carte 1
			column(
				12,
				align = "center",
				h3("Location and cause of the largest fires in this time frame"),
				tabPanel("carte 1.", leafletOutput("carte1")),
				br(),
				br()
			),
			# Carte 2
			fluidRow(column(
				12,
				align = "center",
				h3("Number of fires in each state in this time frame"),
				tabPanel("carte 2.", leafletOutput("carte2"))
			))
		)
	),
	
	
	## 3.3 Prédiction ----
	## 3.3.1 Problématique et démarche ----
	tabItem(
		tabName = "pred1",
		h2("Prédiction"),
		fluidRow(
			shinydashboard::box(
				title = h3("Problématique"),
				status = "warning",
				width = 8,
				solidHeader = F,
				HTML(
					"<p>
					La cause d'un certain nombre de feux reste inconnue dans la base de données.
					Or connaître les causes des feux a de nombreuses utilités, notamment pour prévenir la formation de feux.
					<i> Par exemple, cela pourrait permettre de faire des campagnes de communication dans les zones où il y a beaucoup de feux dûs à la combustion de débris. </i>
					</p>"
				)
			),
			br(),
			infoBox(
				"Feux totaux",
				nrow(fires),
				icon = icon("fire"),
				color = "yellow",
				width = 4
			),
			infoBox(
				"Feux dont la cause est inconnue",
				nrow(fires[stat_cause_descr == "Missing/Undefined",]),
				icon = icon("poo-storm"),
				color = "yellow",
				width = 4
			)
		),
		fluidRow(
			column(
				width = 6,
				shinydashboard::box(
				status = "warning",
				width = 12,
				solidHeader = F,
				HTML(
					"<h3> Démarche </h3> <br>
					 <p> D'abord, sur un jeu de données avec <u>uniquement des causes connues</u>, vérifier s'il est possible de prédire la cause des feux en fonction de :</p>
						<ul>
							<li><p>La taille du feu (en acres)</p></li>
							<li><p>La localisation du feu (longitude + latitude)</p></li>
							<li><p>L'année du feu</p></li>
						</ul>
					<p> Sur le <u>jeu de données complet</u>, si une prédiction est possible, prédire la cause des feux où elle n'est pas connue avec la méthode validée à l'étape précédente. </p></li>
					"
				),
				HTML(
					"<h3> Sélection des données </h3>",
					"<p> Dans le cadre d'une prédiction, seuls les feux des années 1995, 2000, 2005, 2010, 2015 seront étudiés.
					Ce choix a été fait afin de : </p>
					<ul>
						<li> <p> Diminuer la quantité de données pour diminuer les temps de calcul des algorithmes</p></li>
						<li> <p> Tout en préservant des informations liées à la variabilité dans le temps </p></li>
					</ul>"
				)
			)),
			column(
				width = 6,
				align = "center",
				shinydashboard::box(
					width = 12,
					solidHeader = F,
					HTML(
						"
				<h1> Causes des feux (années 1995, 2000, 2005, 2010, 2015) </h1>
				<br>
				<p> Ce graphique permet de voir que les données ne sont <b>pas équilibrées</b> :
				il y a, par exemple, beaucoup plus de feux dus à la combustion de débris qu'à des feux d'artifices.
				Cela devrait être gardé en tête pour le choix des algorithmes de prédictions. </p>
						 "
					),
					plotOutput('plot_firespred_bycause')
				)
			)
		)
	),
	
	## 3.3.2 Random Forest :Efficacité de la méthode ----
	tabItem(tabName = "pred2",
					h2("Prédiction"),
					fluidRow(
						column(
							width = 12,
							shinydashboard::box(
								status = "warning",
								width = 12,
								solidHeader = F,
								HTML(
									"<h3>Régression logistique multinomiale</h3> <br>
								<p style='text-align:center'> Données trop nombreuses et volumineuses, impossible techniquement.<p>"
								)
							),
							shinydashboard::box(
								status = "warning",
								width = 12,
								solidHeader = F,
								HTML(
									"<h3>Arbres décisionnels</h3> <br>
								<p style='text-align:center'> Etape 1 : avec un arbre </p>"
								)
							),
							shinydashboard::box(
								status = "warning",
								width = 12,
								solidHeader = F,
								HTML(
									"<h3> Arbres décisionnels : Forêt aléatoire </h3> <br>
									 	<p style='text-align:center'> Méthode rapide, donc possible avec des données importantes. </p>"
								),
								HTML("<h1>Fonction R</h1>"),
								includeMarkdown("code_extract.Rmd"),
								HTML("<h1>Optimisation de l'algorithme randomForest</h1>"),
								plotOutput('res_opt_rf_acc'),
								plotOutput('res_opt_rf_tps')
							)
						)
					)
	),
	
	## 3.3.3 Prédiction ----
	tabItem(
		tabName = "pred3",
		h2("Prédiction"),
		fluidRow(
			column(
				width = 12,
				shinydashboard::box(
					status = "warning",
					width = 12,
					solidHeader = F,
					HTML(
						"<h3> Conclusion </h3> <br>
					 <p> 
					 L'accuracy est environ de 50% avec cette méthode, peu importe le nombre d'arbres choisi.
					 Prédire les causes inconnues avec cette méthode n'a donc pas de sens, il y aurait trop d'erreurs.
					 </p>
						"
					)
				)),
			column(
				width = 12,
				align = "center",
				shinydashboard::box(
					width = 12,
					solidHeader = F,
					HTML(
						"
							<h3>Suite possible</h3> <br>
							<p> Pour prédir la cause du feu avec une meilleure précision, plusieurs pistes sont envisageables :
								<ul>
									<li><p> Utiliser plus de variables (<i>mais cela prendrait beaucoup de temps !</i>) </p></li>
									<li><p> Ajuster les autres hyperparamètres de l'algorithme randomForest, comme par exemple la profondeur maximum de chaque arbre. Ici, seul le paramètre 'Nombre d'arbres' a été optimisé. </p></li>
									<li><p> Utiliser d'autres méthodes, comme par exemple un réseau de neurones</p></li>
									<li><p> Fusionner certaines variables réponses pour prédire uniquement des causes intéressantes et sur lesquels on pourrait agir (ex : fusionner 'Lightning' et 'Miscellaneous'), 
									puis voir si cela améliore la prédiction</p></li>
								</ul>
							</p>
						 "
					),
				)
			)
		)
	)
	
))


# 4. Rassemblement : dashboardPage----
# 4.1 Feuille de style ----
tags$head(tags$style(
	HTML(
		'h1 {font-family: "Times New Roman", Times, serif; font-size: 20px; letter-spacing: -1.2px; font-weight: 400; text-transform: uppercase; }
		h2 { font-family: Baskerville, "Baskerville Old Face" ; font-size: 40px; text-align: center; }
    h3 { font-family: Baskerville, "Baskerville Old Face", "Hoefler Text", Garamond, "Times New Roman", serif; font-size: 30px; text-align: center; }
    h4 { font-family: Baskerville, "Baskerville Old Face", "Hoefler Text", Garamond, "Times New Roman", serif; font-size: 23px; text-align: center; letter-spacing: -1.2px; font-weight: 400; text-transform: uppercase; }
    p { font-family: Baskerville, "Baskerville Old Face", "Hoefler Text", Garamond, "Times New Roman", serif; font-size: 20px; text-align: justify; } 
    p.codeR {font-family: "Courier New", Courier, monospace; font-size: 15px; letter-spacing: -1.2px; word-spacing: -1.2px;}
		div.info.legend.leaflet-control br {clear: both; text-align: left; align-self: left;}'
	)
),
# 4.2 dashboardPage ----
dashboardPage(
		header,
		sidebar,
		body,
		skin = "black"
	)
)