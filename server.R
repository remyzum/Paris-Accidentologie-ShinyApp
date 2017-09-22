library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(magrittr)
library(rCharts)
library(shinycssloaders)


######   Read data #######
#setwd("/Users/admin/Desktop/Shiny/Accidentology_Paris/Accidentology-Paris")
data <- read_csv("data7.csv", col_types = cols(Precip_day = col_double()))

## Isole chaque modalité
name1 <- sort(unique(data$vehicule_1_cadmin))
name2 <- sort(unique(data$vehicle_2_cadmin))
name3 <- sort(unique(data$usager_1_grav))
name4 <- sort(unique(data$year_quarter))
dayofweek <- sort(unique(data$DayOfWeek))
name5 <- sort(unique(data$Freq))




shinyServer(function (input, output, session){
  
  
  
  #######~~~~~~~~~~~~~ REACTIVES ~~~~~~~~~~~~########
  
  
  date_seq <- reactive({seq(input$range[1], input$range[2], by = "day")}) 
  df0 <- reactive({    
    if (input$HoursOfDay == "Toutes Heures") {InterH <- data}
    else {InterH <- data[data$FourHours == input$HoursOfDay,]}
    return(InterH)}) 
  df1 <- reactive({filter(df0(), dayDate %in% date_seq())})
  
  dataset_blessure <- reactive({
    if (input$Blessure == "Toutes") {dfblessure <- df1()}
    else {dfblessure <- df1()[df1()$usager_1_grav == input$Blessure,]}
    return(dfblessure)}) 
  dataset_voiture1 <- reactive({
    if (input$Category1 == "Tous Véhicules") {dfCat1 <- dataset_blessure()}
    else {dfCat1 <- dataset_blessure()[dataset_blessure()$vehicule_1_cadmin == input$Category1,]}
    return(dfCat1)}) 
  dataset_voiture2 <- reactive({
    if (input$Category2 == "Tous Véhicules") {dfCat2 <- dataset_voiture1()}
    else {dfCat2 <- dataset_voiture1()[dataset_voiture1()$vehicle_2_cadmin == input$Category2,]}
    return(dfCat2)})
  dataset1 <- reactive({
    if (input$DayOfWeek == "Tous les jours") {dfWD <- dataset_voiture2()}
    else {dfWD <- dataset_voiture2()[dataset_voiture2()$DayOfWeek == input$DayOfWeek,]}
    return(dfWD)}) 
  data_reactive_hist <- reactive({
    if (input$DayOfWeek == "Tous les jours") {dfWW <- df1()}
    else {dfWW  <- df1()[df1()$DayOfWeek == input$DayOfWeek,]}
    return(dfWW)})
  
  dataset2 <- reactive({ df3 <- ddply(dataset1(), .(Lat,Long), summarise, count = length(carr))})
  
  
  
  #######~~~~~~~~~~~~~ OBSERVERS ~~~~~~~~~~~~########
  
  #### Observer reset des valeurs input quand on change de TabPanel 
  #### Et avec la vue standard 2 (firefox ne marche pas)
  observeEvent(input$mainTabs, {
    shinyjs::reset("SelectionVar")
    shinyjs::reset("Category2")
    shinyjs::reset("VisuRadio")
    #shinyjs::reset("BarRadio")
    
  })    
  
  ## Reset des blessure pour le mode vue clusters
  observeEvent(
    input$VisuRadio,{
    if (input$VisuRadio == 2)
    shinyjs::reset("Blessure")
  })   
  
  
  ## Crée un reactive suivant les inputs catégories et visuRadio
  ## Si vue cluster ou vue carte de chaleur alors tous véhicule inclu dans catégorie 2 
  InputListen <- reactive({input$VisuRadio})
  observeEvent({InputListen()},isolate({
    if(input$VisuRadio == 1 || input$VisuRadio == 4 )
      {updateSelectInput(session,"Category2",choices = c("Tous Véhicules",name2), selected = "Deux Roues > 125")}
      
  })
  
  )   
  
  ## Si vue markers alors pas possible de voir tous véhicules dans catégorie 2  
  observeEvent({InputListen()},isolate({
    if(input$VisuRadio == 2)#{return()}
    {updateSelectInput(session,"Category2",choices = name2, selected = "Deux Roues > 125")}
    
  })
  
  )   
  
  #######~~~~~~~~~~~~~ OUTPUT MAPS ~~~~~~~~~~~~########
  
  #~~~~~ Tab basemap avec dots et renderleaflet 
  
  output$map1 <- renderLeaflet({
      icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'white')
      countAccident_map1 <- paste0("Nombre d'accidents : ","<span style='color: #ff0000;'><strong>",count(dataset1()),"</strong></span>")
      leaflet() %>% setView(lng = 2.3488000, lat = 48.8534100, zoom = 14) %>%
      addProviderTiles('CartoDB.Positron') %>%
      addTiles() %>%
      addAwesomeMarkers(data = dataset1(), lng = ~ dataset1()$Long, lat = ~ dataset1()$Lat, 
                        popup = paste0("<span style='color: #7f0000'><strong>Description Accident</strong></span>",
                                       "<br><span style='color: salmon;'><strong>1er Véhicule impliqué: </strong></span>", 
                                       dataset1()$vehicule_1_cadmin, 
                                       "<br><span style='color: salmon;'><strong>2ème Véhicule impliqué: </strong></span>", 
                                       dataset1()$vehicle_2_cadmin,
                                       "<br><span style='color: salmon;'><strong>Gravité: </strong></span>", 
                                       dataset1()$usager_1_grav,
                                       "<br><span style='color: salmon;'><strong>Date: </strong></span>", 
                                       dataset1()$dayDate,
                                       "<br><span style='color: salmon;'><strong>Heure: </strong></span>", 
                                       dataset1()$Hour,"H",
                                       "<br><span style='color: salmon;'><strong>Adresse: </strong></span>",
                                       dataset1()$adresse),
                        icon = icon.glyphicon) %>%
      addControl(html = countAccident_map1, position = "bottomright")  %>%
      addMiniMap(position = 'topright',
                zoomLevelOffset = -4,
                 zoomAnimation = TRUE)
      
    
  })
  
  
  
  
  #~~~~~ Tab basemap avec dots colorisés en fonction de la gravité de l'accident et renderleaflet 
  output$map2 <- renderLeaflet({
    
    ColorIcons <- makeAwesomeIcon(icon= 'flag', 
                                  markerColor = ifelse(dataset1()$usager_1_grav == "Aucun Blessé", 'blue',
                                                       ifelse(dataset1()$usager_1_grav == "Blessé Léger", 'orange',
                                                              ifelse(dataset1()$usager_1_grav == "Blessé Hospitalisé", 'red', 'black'))), 
                                  iconColor = 'white')
    
    
    Map2_Icon_legend <- paste0(img(src = "image/blue_2.png", height="22", width="18" )," Aucun blessé",
                                 "<br>",img(src = "image/orange_2.png",height="22", width="17")," Blessé léger",
                                 "<br>",img(src = "image/red_2.png",height="22", width="16")," Blessé hospitalisé",
                                 "<br>",img(src = "image/black_2.png",height="22", width="17")," Décès")
                               
    countAccident_map1 <- paste0("Nombre d'accidents : ","<span style='color: #ff0000;'><strong>",count(dataset1()),"</strong></span>")
    
      leaflet() %>% setView(lng = 2.3488000, lat = 48.8534100, zoom = 14) %>%
      addProviderTiles('CartoDB.Positron') %>%
      addTiles() %>%
      addAwesomeMarkers(data = dataset1(), 
                        icon = ColorIcons,
                        lng = ~ dataset1()$Long, 
                        lat = ~ dataset1()$Lat,
                        popup = paste0("<span style='color: #7f0000'><strong>Description Accident</strong></span>",
                                  "<br><span style='color: salmon;'><strong>1er Véhicule impliqué: </strong></span>", 
                                  dataset1()$vehicule_1_cadmin, 
                                  "<br><span style='color: salmon;'><strong>2ème Véhicule impliqué: </strong></span>", 
                                  dataset1()$vehicle_2_cadmin,
                                  "<br><span style='color: salmon;'><strong>Gravité: </strong></span>", 
                                  dataset1()$usager_1_grav,
                                  "<br><span style='color: salmon;'><strong>Date: </strong></span>", 
                                  dataset1()$dayDate,
                                  "<br><span style='color: salmon;'><strong>Heure: </strong></span>", 
                                  dataset1()$Hour,"H",
                                  "<br><span style='color: salmon;'><strong>Adresse: </strong></span>",
                                  dataset1()$adresse)) %>%
      addControl(html = Map2_Icon_legend, position = "bottomleft") %>%
      addControl(html = countAccident_map1, position = "bottomright")  %>%  
      addMiniMap(position = 'topright',
                 zoomLevelOffset = -4,
                 zoomAnimation = TRUE)
  })
  

  
  #~~~~~ Tab basemap avec clusters 
  output$map3 <- renderLeaflet({
    
    icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'white')
    countAccident_map1 <- paste0("Nombre d'accidents : ","<span style='color: #ff0000;'><strong>",count(dataset1()),"</strong></span>")
    leaflet() %>% setView(lng = 2.3488000, lat = 48.8534100, zoom = 12) %>%
      addProviderTiles('CartoDB.Positron') %>%
      addTiles() %>%
      addAwesomeMarkers(data = dataset1(), 
                      lng = ~ dataset1()$Long, 
                      lat = ~ dataset1()$Lat, 
                      popup = paste0("<span style='color: #7f0000'><strong>Description Accident</strong></span>",
                                     "<br><span style='color: salmon;'><strong>1er Véhicule impliqué: </strong></span>", 
                                     dataset1()$vehicule_1_cadmin, 
                                     "<br><span style='color: salmon;'><strong>2ème Véhicule impliqué: </strong></span>", 
                                     dataset1()$vehicle_2_cadmin,
                                     "<br><span style='color: salmon;'><strong>Gravité: </strong></span>", 
                                     dataset1()$usager_1_grav,
                                     "<br><span style='color: salmon;'><strong>Date: </strong></span>", 
                                     dataset1()$dayDate,
                                     "<br><span style='color: salmon;'><strong>Heure: </strong></span>", 
                                     dataset1()$Hour,"H",
                                     "<br><span style='color: salmon;'><strong>Adresse: </strong></span>",
                                     dataset1()$adresse),
                      icon = icon.glyphicon, 
                      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE,
                                                            spiderfyOnMaxZoom = TRUE,
                                                            showCoverageOnHover = TRUE,
                                                            maxZoom = 9)) %>%
      addMiniMap(position = 'topright',
                 zoomLevelOffset = -4,
                 zoomAnimation = TRUE) %>%
      addControl(html = countAccident_map1, position = "bottomright")
    
  })
  
  #~~~~~ Tab basemap avec heatMap 
  
  ## liaison avec resize en fonction du browser dans UI
  observeEvent(input$dimension,{
    output$baseMap <- renderMap({
      #countAccident_map1 <- paste0("Nombre d'accidents : ","<span style='color: #ff0000;'><strong>",count(dataset1()),"</strong></span>")
       baseMap = Leaflet$new()
       baseMap$setView(c(48.85341,2.34880,13))
       baseMap$addParams(height = '100%', width = '100%', zoom = 12)
       baseMap
     })
  
  output$heatMap <- renderUI({
    
    
    ##~~~~~~ Creation de JSON avec 'paste0()'.
    j <- paste0("[",dataset2()[,"Lat"], ",", dataset2()[,"Long"], ",", dataset2()[,"count"], "]", collapse=",")
    j <- paste0("[",j,"]")
    
    
    ##~~~~~~ Fonctionne et layer ne se superposent pas
    tags$body(tags$script(HTML(sprintf("
                                        var addressPoints = %s
                                         if (typeof heat === typeof undefined) {
                                           heat = L.heatLayer(addressPoints, {radius: 12,blur: 15,maxZoom: 5,max: 6.0,
                                                                              gradient: {0.0: 'green',0.5: 'yellow',1.0: 'red' }}),
                                          heat.addTo(map)} 
                                          else {heat.setLatLngs(addressPoints)}", j )))) 
    
  })
  
  })
  
  
  
  #######~~~~~~~~~~~~~ OUTPUT Diagrammes ~~~~~~~~~~~~########
  
  #~~~~~ Tab Diagrammes
  
  ## BarPlots rangée du Haut analyse chronologique
  output$distPlot_test1 <- renderPlot({
    levels_dayofweek <-  c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche")
    levels_yearquarter <- c("2012 Q1","2012 Q2","2012 Q3","2012 Q4","2013 Q1","2013 Q2","2013 Q3","2013 Q4")
    levels_HoursDay <- c("00H-04H","04H-08H","08H-12H","12H-16H","16H-20H","20H-00H")
    DF_dataset_dayofweek_factor <- reactive({factor(dataset_voiture2()$DayOfWeek, levels = c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche"),ordered =TRUE)})
    DF_dataset_yearQuarter_factor <- reactive({factor(dataset_voiture2()$year_quarter, levels = levels_yearquarter,ordered =TRUE)})
    DF_dataset_HoursDay_factor <- reactive({factor(dataset_voiture2()$FourHours, levels = levels_HoursDay, ordered =TRUE)})
    
    
    par(mfrow=c(1,3))
    barplot1 <- barplot(table(DF_dataset_yearQuarter_factor()),col = "#00abff", border = "white", main = "Nombre d'Accidents par trimestre", ylim = NULL,axes = FALSE, axisnames = FALSE)
    text(barplot1, par("usr")[3], labels = levels_yearquarter, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=1.4)
    axis(2, las =2)
    
    barplot2 <- barplot(table(DF_dataset_dayofweek_factor()),col = "#00abff", border = "white", main = "Nombre d'Accidents par jour de la semaine", ylim = NULL,axes = FALSE, axisnames = FALSE)
    text(barplot2, par("usr")[3], labels = levels_dayofweek, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=1.4)
    axis(2, las =2)
    
    barplot3 <- barplot(table(DF_dataset_HoursDay_factor()),col = "#00abff", border = "white", main = "Nombre d'Accidents par tranche horaire", ylim = NULL,axes = FALSE, axisnames = FALSE)
    text(barplot3, par("usr")[3], labels = levels_HoursDay, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=1.4)
    axis(2, las =2)
  })
  
  ## BarPlots rangée du bas analyse chronologique
  output$distPlot5 <- renderPlot({
    levels_monthyear <- c("Janvier 2012","Février 2012","Mars 2012","Avril 2012","Mai 2012","Juin 2012","Juillet 2012","Août 2012","Septembre 2012","Octobre 2012","Novembre 2012", "Decembre 2012", "Janvier 2013","Février 2013","Mars 2013","Avril 2013","Mai 2013","Juin 2013","Juillet 2013","Août 2013","Septembre 2013","Octobre 2013","Novembre 2013", "Decembre 2013" )
    DF_dataset_monthyear_factor <- reactive({factor(dataset_voiture2()$MonthYear, levels = levels_monthyear,ordered =TRUE)})
    levels_year <- c("2012","2013")
    DF_dataset_year_factor <- reactive({factor(dataset_voiture2()$Year, levels = levels_year,ordered =TRUE)})
   
    layout(matrix(c(1,1,1,2), nrow = 1, ncol = 4, byrow = TRUE))
    
    ## BarPlot pour tous les mois
    barplot4 <- barplot(table(DF_dataset_monthyear_factor()),col = "#00abff", border = "white", main = "Nombre d'Accidents par mois", ylim = NULL, axes = FALSE, axisnames = FALSE)
    text(barplot4, par("usr")[3], labels = levels_monthyear, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
    axis(2, las =2)
    
    ## BarPlot pour tous 2012 et 2013
    barplot5 <- barplot(table(DF_dataset_year_factor()),col = "#00abff", border = "white", main = "Nombre d'Accidents par an", ylim = NULL, axes = FALSE, axisnames = FALSE)
    text(barplot5, par("usr")[3], labels = levels_year, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
    axis(2, las =2)
    
  })
  
  ## BarPlot analyse par catégorie
  output$distPlot6 <- renderPlot({
    levels_vehicule_1 <- c("Voiture","Deux Roues 50-125","Deux Roues > 125","Vélo","Poids Lourd","Autobus","Autocar","Tramway")
    levels_gravité <- c("Aucun Blessé","Blessé Léger","Blessé Hospitalisé","Décès")
    DF_dataset_vehicule1_factor <- reactive({factor(data_reactive_hist()$vehicule_1_cadmin, levels = levels_vehicule_1)})
    DF_dataset_gravite_factor <- reactive({factor(data_reactive_hist()$usager_1_grav, levels = levels_gravité)})
    barplot6 <- barplot(table(DF_dataset_gravite_factor(),DF_dataset_vehicule1_factor()),
                        col = c("#1d81cd","#cd901d", "#cd381d", "#000000"), 
                        border = "white", 
                        main = "Nombre d'Accidents par type de véhicule", 
                        ylim = NULL, 
                        axes = FALSE, 
                        axisnames = FALSE,
                        legend.text=TRUE,
                        args.legend=list(x = "topright", bty = "n", inset=c(0.05, 0))
                          )
                        
    text(barplot6, par("usr")[3], labels = levels_vehicule_1, srt = 30, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
    axis(2, las =2)
  })
  
  
  ## BarPlot analyse par gravité
  output$distPlot7 <- renderPlot({
    levels_vehicule_1 <- c("Voiture","Deux Roues 50-125","Deux Roues > 125","Vélo","Poids Lourd","Autobus","Autocar","Tramway")
    levels_gravité <- c("Aucun Blessé","Blessé Léger","Blessé Hospitalisé","Décès")
    DF_dataset_vehicule1_factor <- reactive({factor(data_reactive_hist()$vehicule_1_cadmin, levels = levels_vehicule_1)})
    DF_dataset_gravite_factor <- reactive({factor(data_reactive_hist()$usager_1_grav, levels = levels_gravité)})
    barplot7 <- barplot(table(DF_dataset_vehicule1_factor(),DF_dataset_gravite_factor()),
                        col = c("#1d81cd","#cd901d", "#cd381d","#beaed4", "#fdc086", "#ffff99", "#32CD32", "#800000" ), 
                        border = "white", 
                        main = "Nombre d'Accidents par type de gravité", 
                        ylim = NULL, 
                        axes = FALSE, 
                        axisnames = FALSE,
                        legend.text=TRUE,
                        args.legend=list(x = "topright", bty = "n", inset=c(0.05, 0))
    )
   
    text(barplot7, par("usr")[3], labels = levels_gravité, srt = 30, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
    axis(2, las =2)
  })    
  
  
})