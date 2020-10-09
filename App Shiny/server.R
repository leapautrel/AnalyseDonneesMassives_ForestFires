require(shiny)
require(ggplot2)
require (data.table)
require (leaflet)
require(geojsonio)
setwd("D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret") # Direction fichier  GitHub Lea

fires_all <- fread(
  "fires.csv",
  header = TRUE,
  sep = ",",
  na.strings = "",
  blank.lines.skip = TRUE,
  stringsAsFactors = TRUE
)
annee <- fires_all$fire_year
annee <- as.data.frame(annee)
mois <- fires_all$fire_month
mois <- as.data.frame(mois)
fires_all$fire_date <- as.Date(fires_all$fire_date)
plot_annee <- ggplot(annee) + aes(x = annee) + geom_bar(fill = "orange")
plot_mois <- ggplot(mois) + aes(x = mois) + geom_bar(fill = "green")
fires <- fires_all
fires$fire_year <- as.factor(fires$fire_year)
states <-
  geojsonio::geojson_read(
    "D:/Google Drive/Agrocampus/M2/UE4-AnalyseDonneesMassiveR/Projet_Foret/AnalyseDonneesMassives_ForestFires/App Shiny/us-states.json",
    what = "sp"
  )
states@data[,3] <- NULL
state.abb <- append(state.abb, c("DC", "PR"))
state.name <- append(state.name, c("District of Columbia", "Puerto Rico"))
fires$state <- state.name[match(fires[, state], state.abb)]

shinyServer(function(input, output) {
  output$carte1 <- renderLeaflet({
    r <- as.Date(input$idDateRange)
    i <- fires[which(fires$fire_date >= r[1] & fires$fire_date <= r[2]),]
    if(input$idCheck){i <- i[which(i$fire_size >= 10000),]}
    ncauses <- nlevels(i[, stat_cause_descr])
    pal <- leaflet::colorFactor(
      palette = c("#FF0000FF", "#FF7600FF", "#FFEB00FF", "#0d590f", "#27FF00FF", "#0b210a", "#00FFC4FF", "#872a0b", "#7175f5", "#181ec9", "#9D00FFFF", "#e685d4", "#FF0076FF"),
      domain = i[, stat_cause_descr]
    )
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
    m
  })
  output$carte2 <- renderLeaflet({
    r <- as.Date(input$idDateRange)
    i <- fires[which(fires$fire_date >= r[1] & fires$fire_date <= r[2]),]
    if(input$idCheck){i <- i[which(i$fire_size >= 10000),]}
    fires_vecdate_bystate <- i[, list(fire_count = .N),
                                           by = list(name = state)]
    j <- fires[, list(fire_count = 0), by = list(name = state)]
    j <- merge(fires_vecdate_bystate, j, by = intersect("name", "name"), all = TRUE)
    j$fire_count <- j$fire_count.x + j$fire_count.y
    j$fire_count[which(is.na(j$fire_count))] <- 0
    fires_vecdate_bystate <- j[, - c(2, 3)]
    fires_vecdate_bystate <- fires_vecdate_bystate[order (name)]
    states@data <- merge.data.frame(
      x = states@data,
      y = fires_vecdate_bystate,
      by = "name")
    states@data <- rbind(states@data[1:39,], states@data[41:52,], states@data[40,])
    bins <- c(0, 20, 50, 100, 200, 500, 1000, 2000, 5000, Inf)
    pal2 <- colorBin("YlOrRd", domain = states$fire_count, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g fires",
      states@data[,1], states@data[,3]
    ) %>% lapply(htmltools::HTML)
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
        lng1 = min(fires[, longitude]),
        lat1 = min(fires[, latitude]),
        lng2 = max(fires[, longitude]),
        lat2 = max(fires[, latitude])
      ) %>% addLegend(
        pal = pal2,
        values = ~ fire_count,
        opacity = 0.7,
        title = NULL,
        position = "bottomright"
      )

    m2
  })
  output$plot <- renderPlot({
  if(input$idRadio == "1"){
    plot_annee} 
  else {
    plot_mois}
  }
  )
})
