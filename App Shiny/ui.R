## 1. header -------------------------------
header <- dashboardHeader(
  title = "US wildfires from 1992 to 2015",
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
      
      ## 2.3 - Cartographie ----------------------
      menuItem( "Cartographie", tabName = 'carto', icon = icon('globe-americas')),
      
      ## 2.4 - Prediction ----------------------
      menuItem( "Prédiction", tabName = 'pred', icon = icon('search'))
      )
  )

## 3. body -------------------------------
body <- dashboardBody(
  tabItems(
    ## 3.1 Accueil -------------------------------------------------------------
    tabItem(tabName = "accueil",
            h1("Présentation"),
            fluidRow(
            	box(
            		title = h2("1.88 Million US Wildfires"),
            		status = "warning",
            		width = 12,
            		solidHeader = F,
            		textOutput("txt_presentation")
            	)
            ),
            # Ligne d'informations
            fluidRow(
              infoBox("Nombre de feux entre 1992 et 2015", nrow(fires), icon = icon("fire-extinguisher"), color = "aqua", width = 6),
              infoBoxOutput("fireslighning", width = 6)
              ),
            fluidRow(
              infoBoxOutput("statemostfires", width = 6),
              infoBoxOutput("causefreq", width = 6)
              ),
            
            # Graphiques non interactifs
            fluidRow(
            	p(br(), br(), br()),
              column(4.5, align="center", 
                     h2("Number of fires by year"),
              			 p("By cause"),
                     plotOutput('plot_annee')),
            	
            	p(br(), br(), br()),
              column(4.5, align="center", 
                     h2("Number of fires by month"),
              			 p("By cause - Sum from 1992 to 2015"),
                     plotOutput('plot_mois')),
            	
            	p(br(), br(), br()),
    					column(4.5, align="center", 
              			 h2("Average wildfire size by cause"),
    								 p("Data from 1992 to 2015"),
              			 plotOutput('plot_taillecause'))
            )
            ), 
    
    ## 3.2 Cartographie --------------------------------------------------------
    tabItem(
      tabName = "carto",
      h1("Cartographie"),
      fluidRow(
        column(width = 12, align = "center",
               box(
                 # Selection de l'intervalle de temps
                 dateRangeInput(
                   inputId = "idDateRange",
                   label = h2("Choose a time frame"),
                   start = "1992-01-01",
                   end = "2015-12-31",
                   format = "yyyy-mm-dd",
                   language = "fr",
                   separator = " to "
                 ),
                 
                 # Bouton update
                 actionButton("go_cartes", "Update time frame"),
                 
                 # Options box
                 width = 12,
                 status = "warning",
                 solidHeader = FALSE,
                 collapsible = TRUE
               )
               )
        ), 
        
      # Affichage des cartes
      fluidRow(
        # Carte 1
        column(12, align="center", 
               h2("Location and cause of the 10,000 largest fires in this time frame"),
               tabPanel("carte 1.", leafletOutput("carte1")))),

        # Carte 2
      fluidRow(
        column(12, align="center", 
               h2("Number of fires in each state in this time frame"),
               tabPanel("carte 2.", leafletOutput("carte2")))
        )
      ), 

    ## 3.3 Prediction ----------------------------------------------------------
    tabItem(tabName = "pred",
            h1("Prédiction")
    )
  )
)


# 4. Rassemblement:dashboardPage----
# 4.1 Feuille de style ----
tags$head(tags$style(
	HTML(
		'h1 { font-family: Baskerville, "Baskerville Old Face" ; font-size: 40px; text-align: center; }
    h2 { font-family: Baskerville, "Baskerville Old Face", "Hoefler Text", Garamond, "Times New Roman", serif; font-size: 30px; text-align: center; }
    p { font-family: Baskerville, "Baskerville Old Face", "Hoefler Text", Garamond, "Times New Roman", serif; font-size: 20px; text-align: justify; } 
		div.info.legend.leaflet-control br {clear: both; text-align: left; align-self: center;}'
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