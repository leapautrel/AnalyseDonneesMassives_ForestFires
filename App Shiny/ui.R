library(shiny)
library(leaflet)
shinyUI(fluidPage(
  
  # Application title
  headerPanel("Hello Shiny!"),
  
  tabsetPanel(
    tabPanel("Diagrammes en barres.",
             sidebarPanel(
               radioButtons(
                 inputId = "idRadio",
                 label = "Afficher le nombre de feux par:",
                 selected = 1,
                 choices = c("annee." = 1, "mois." = 2))),
             plotOutput("plot")), 
    tabPanel("cartes.",
             sidebarPanel(
               dateRangeInput(
                 inputId = "idDateRange",
                 label = "Please Select a date range",
                 start = "2015-01-01", 
                 end = "2015-08-12",
                 format = "yyyy-mm-dd",
                 language = "fr", 
                 separator = " a "),
               checkboxInput(
                 inputId = "idCheck",
                 label = "Afficher seulement les gros feux (taille >= 10000).", TRUE)
               ),
             tabsetPanel(
               tabPanel("carte 1.", leafletOutput("carte1")), 
               tabPanel("carte 2.", leafletOutput("carte2")))
             )
  )
))
