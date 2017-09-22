library(shiny)
library(shinyjs)
library(shinycssloaders)
library(leaflet)
library(devtools)
library(rCharts)


shinyUI(
  fluidPage(
    
      ####   It uses Javascript to detect the browser window size (initial size and any resize), 
      ####   and use Shiny.onInputChange to send the data to the server code for processing. 
      ####   It uses shiny:connected event to get the initial window size, as Shiny.onInputChange 
      ####   is not ready for use until shiny is connected.
      tags$head(tags$script('
                        var dimension = [0, 0];
                          $(document).on("shiny:connected", function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                          });
                          $(window).resize(function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                          });
                          ')),
    
    title = "Accidentologie à Paris en 2012-2013",
  
    shinyjs::useShinyjs(),
  
    tags$head(includeCSS(file.path("www", "app.css"))),
  
    div(id = "header",
      
      div(id = "title",
          "Accidentologie à Paris"),
      
      div(id = "subsubtitle",
          "Analyse de données sur le dataset des accidents de la route à Paris" ,
          "en 2012 et 2013",
          HTML("&bull;"),
          "Le jeu de données est accessible sur",
           tags$a(href ="https://opendata.paris.fr/page/home/","Open Data Paris")
         )
  ),
  
  fluidRow(
    column(3, wellPanel(
      id = "leftPanel",
      
      div(
        id = "SelectionVar",
        
        conditionalPanel(
          condition = "input.BarRadio == 1 || input.mainTabs == 'MapTab' ",
        selectInput("Category1", "1er véhicule impliqué :", c("Tous Véhicules",name1), selected = "Tous Véhicules", selectize = FALSE)
        ),
        
        conditionalPanel(
          condition = "input.BarRadio == 1 ||  input.mainTabs == 'MapTab' ",
        selectInput("Category2", "2ème véhicule impliqué :", c("Tous Véhicules",name2), selected = "Deux Roues > 125", selectize = FALSE)
        ),
        
        
        conditionalPanel(
        condition = "input.BarRadio != 1 || input.mainTabs == 'MapTab' ",
        sliderInput("range", label = "Période de temps :", min = as.Date(min(data$dayDate)), max = as.Date(max(data$dayDate)),
                    value = c(as.Date("2012-01-01"),as.Date("2013-12-31")), round = FALSE,
                    animate = FALSE,
                    step = 30,
                    dragRange = FALSE,
                    timeFormat = "%F")
        ),
        
        conditionalPanel(
          condition = "input.BarRadio != 1 || input.mainTabs == 'MapTab' ",
        selectInput("DayOfWeek", " Jour de la semaine :", c("Tous les jours", "Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche"), selected = "Tous les jours", selectize = FALSE)
        ),
        
        conditionalPanel(
          condition = "input.BarRadio != 1 || input.mainTabs == 'MapTab' ",
        selectInput("HoursOfDay", "Tranche horaire :", c("Toutes Heures","00H-04H","04H-08H","08H-12H","12H-16H","16H-20H","20H-00H"), selected = "Toutes Heures", selectize = FALSE)
        ),
        
        conditionalPanel(
          condition = "(input.mainTabs == 'MapTab' && input.VisuRadio != 2) || (input.BarRadio == 1 && input.mainTabs == 'Stats')",
          selectInput("Blessure", "Gravité de  l'accident :", c("Toutes","Aucun Blessé","Blessé Léger","Blessé Hospitalisé","Décès"), selected = "Toutes", selectize = FALSE)
        )
        
      )
    )),
    
    column(9, wellPanel(
      tabsetPanel(
        id = "mainTabs", type = "tabs",
        
        ## TabPanel pour les Cartes
        tabPanel(
          title = "Carte des Accidents", id ="MapTab", value = "MapTab",
          br(),
              
              ## Vue Cluster
              conditionalPanel(
              condition = "input.VisuRadio == 1",
              withSpinner(
                      leafletOutput("map3", height = 450, width = "100%"),
                      type = 6, color = "#d3d3d3")),#, width="100%", height="100%")),#),
          
              ## Vue Markers
              conditionalPanel(
              condition = "input.VisuRadio == 2",
              withSpinner(
                      leafletOutput("map2", height = 450, width = "100%"),
                      type = 6, color = "#d3d3d3")),
          
              ## Carte de Chqleur
              conditionalPanel(
              condition = "input.VisuRadio == 4",
              tags$style('.leaflet {height: 450px; width :"auto";}'),   
                         htmlOutput("baseMap"),
                         tags$head(tags$script(src="http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js")),
              withSpinner(          
                         uiOutput("heatMap"),
                         type = 6, color = "#d3d3d3")),
              
                         
              ## Radio Buttons pour les cartes
              absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                         top = 35, left = 480, 
                         width = "200px", height = "40px",    
              radioButtons("VisuRadio", "", choices = list("Vue Cluster" = 1,
                                                           "Vue Markers" = 2,
                                                           "Carte de Chaleur" = 4),
                           selected = 1, inline = TRUE))),
              #actionButton("reset_button", "Reset view")),
        
        ## TabPanel pour les diagrammes 
        tabPanel(
          title = "Diagrammes", id ="Stats", value = "Stats",
          
          ## Analyse Chronologique
          conditionalPanel(
            condition = "input.BarRadio == 1",
            withSpinner(plotOutput("distPlot_test1", height = 250, width = "100%"),color = "#d3d3d3"),
            withSpinner(plotOutput("distPlot5", height = 250, width = "100%"),color = "#d3d3d3")),
          
          ## Analyse par Catégorie
          conditionalPanel(
            condition = "input.BarRadio == 2",
              plotOutput("distPlot6", height = 600, width = "100%")),
          
          ## Analyse Par Gravité
          conditionalPanel(
            condition = "input.BarRadio == 3",
            plotOutput("distPlot7", height = 600, width = "100%")),
              
          ## Radio Buttons pour le choix des diagrammes
          absolutePanel(id = "controls_2", class = "panel panel-default", fixed = FALSE,
                        top = 35, left = 480, 
                        width = "200px", height = "40px",    
                        radioButtons("BarRadio", "", choices = list("Analyse Chronologique" = 1,
                                                                    "Par Catégorie" = 2,
                                                                    "Par Gravité" = 3),
                                     selected = 1, inline = TRUE)))#)
              
    
  )
  
))
)
))