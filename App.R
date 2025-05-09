# -- Application Shiny : Onglets avec cartes, histogramme et légende dynamique

# installation des librairies nécessaires
library(shiny)
library(dplyr)
library(ggplot2)
library(osmdata)
library(sf)
library(tmap)
library(mapview)
library(leaflet)
library(DT)
library(shinyjs)
library(stringr)

# Charger les données
Grand_lyon <- st_read(dsn="data/EPCI_GLyon4326.geojson")
jardin <- st_read(dsn="data/jardin_collectif_text.geojson")
jardin_cercle <- st_read(dsn="data/jardin_collectif_centroide_text.geojson")
Mask_AURA <- st_read(dsn="data/MASK_AURA_4326.geojson")

# Liste des types & couleurs
layers_info <- list(
  "JARDIN PARTAGÉ" = "darkgreen",
  "FERME URBAINE" = "darkseagreen",
  "JARDIN DE RUE" = "green",
  "JARDIN PÉDAGOGIQUE" = "gold",
  "JARDIN À CLASSER" = "brown",
  "JARDIN D'INSERTION" = "greenyellow",
  "JARDIN FAMILIAL" = "goldenrod"
)

# UI
ui <- fluidPage(
  titlePanel("Application d'analyse des jardins partagés"),
  tabsetPanel(
    tabPanel("Introduction",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("JARDIN_FAMILIAL_intro", "Jardin familial", value = TRUE),
                 checkboxInput("JARDIN_A_CLASSER_intro", "Jardin à classer", value = FALSE),
                 checkboxGroupInput("sub_filter_classes_intro", "Jardins partagés (Sous-catégories) :",
                                    choices = list(
                                      "Ferme urbaine" = "FERME URBAINE",
                                      "d'insertion" = "JARDIN D'INSERTION",
                                      "de rue" = "JARDIN DE RUE",
                                      "partagé" = "JARDIN PARTAGÉ",
                                      "pédagogique" = "JARDIN PÉDAGOGIQUE"
                                    ),
                                    selected = c("FERME URBAINE", "JARDIN D'INSERTION", "JARDIN DE RUE", "JARDIN PARTAGÉ", "JARDIN PÉDAGOGIQUE"))
               ),
               mainPanel(
                 leafletOutput("map_intro", height = "70vh"),
                 dataTableOutput("table_intro")
               )
             )
    ),
    tabPanel("Analyse Scraping",
             sidebarLayout(
               sidebarPanel(
                 textInput("search_word", "Entrez un mot-clé à rechercher :", value = ""),
                 textOutput("nb_jardins_concernes"),
                 br(),
                 h5("Top 20 des jardins (par nombre d’occurrences)"),
                 plotOutput("plot_occurrences", height = "350px", click = "plot_click")
               ),
               mainPanel(
                 leafletOutput("map_scraping", height = "60vh"),
                 uiOutput("no_result_text"),
                 dataTableOutput("table_scraping")
               )
             )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  pal <- colorFactor(palette = unname(unlist(layers_info)), domain = names(layers_info))
  
  filter_data <- reactive({
    selected_classes <- c()
    if (input$JARDIN_FAMILIAL_intro) selected_classes <- c(selected_classes, "JARDIN FAMILIAL")
    if (input$JARDIN_A_CLASSER_intro) selected_classes <- c(selected_classes, "JARDIN À CLASSER")
    selected_classes <- c(selected_classes, input$sub_filter_classes_intro)
    jardin %>% filter(classe_mot %in% selected_classes)
  })
  
  output$map_intro <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.4)) %>%
      setView(lng = 4.85, lat = 45.75, zoom = 8)  # Zoom sur AURA
  })
  
  
  observe({
    proxy <- leafletProxy("map_intro")
    proxy %>% clearShapes()
    proxy %>% addPolygons(data = filter_data(),
                          fillColor = ~pal(classe_mot),
                          color = "black",
                          weight = 1,
                          opacity = 1,
                          fillOpacity = 0.5,
                          popup = ~paste("<strong>", name, "</strong>", "<br>", "Type : ", classe_mot))
  })
  
  observe({
    selected_classes <- c()
    if (input$JARDIN_FAMILIAL_intro) selected_classes <- c(selected_classes, "JARDIN FAMILIAL")
    if (input$JARDIN_A_CLASSER_intro) selected_classes <- c(selected_classes, "JARDIN À CLASSER")
    selected_classes <- c(selected_classes, input$sub_filter_classes_intro)
    
    leafletProxy("map_intro") %>%
      clearControls() %>%
      addLegend(position = "bottomright",
                pal = pal,
                values = selected_classes,
                title = "Types de jardins",
                opacity = 1)
  })
  
  output$table_intro <- renderDataTable({
    bounds <- input$map_intro_bounds
    data <- filter_data()
    if (is.null(bounds)) return(data[0, ])
    df <- data %>% filter(
      st_coordinates(st_centroid(geometry))[,1] >= bounds$west &
        st_coordinates(st_centroid(geometry))[,1] <= bounds$east &
        st_coordinates(st_centroid(geometry))[,2] >= bounds$south &
        st_coordinates(st_centroid(geometry))[,2] <= bounds$north)
    datatable(st_set_geometry(df, NULL), options = list(scrollX = TRUE), width = "100%")
  })
  
  get_circle_data <- reactive({
    if (input$search_word == "") return(jardin_cercle[0, ])
    jardin_cercle %>%
      mutate(occurrences = str_count(toupper(texte_nettoye), toupper(input$search_word))) %>%
      filter(occurrences > 0)
  })
  
  output$nb_jardins_concernes <- renderText({
    nb <- nrow(get_circle_data())
    if (input$search_word == "") return("")
    else if (nb == 0) return("Aucun jardin collectif concerné.")
    else if (nb == 1) return("Résultat : 1 jardin collectif est concerné.")
    else return(paste0("Résultat : ", nb, " jardins collectifs sont concernés."))
  })
  
  output$map_scraping <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.4)) %>%
      addCircles(data = jardin_cercle,
                 lat = ~st_coordinates(geometry)[,2],
                 lng = ~st_coordinates(geometry)[,1],
                 radius = 6,
                 color = "grey34",
                 fillColor = "grey34",
                 fillOpacity = 0.5,
                 popup = ~paste("<strong>", name, "</strong>", "<br>", "Type : ", classe_mot))
  })
  
  observe({
    filtered_data <- get_circle_data()
    proxy <- leafletProxy("map_scraping") %>% clearShapes()
    
    if (nrow(filtered_data) > 0) {
      output$no_result_text <- renderUI({ NULL })
      proxy %>% addCircles(data = filtered_data,
                           lat = ~st_coordinates(geometry)[,2],
                           lng = ~st_coordinates(geometry)[,1],
                           radius = ~occurrences * 10,
                           color = "grey34",
                           fillColor = "grey34",
                           fillOpacity = 0.5,
                           popup = ~paste("<strong>", name, "</strong>", "<br>", "Occurrences : ", occurrences))
    } else {
      output$no_result_text <- renderUI({
        div(style = "color:red; padding:10px;",
            if (input$search_word == "") "Veuillez entrer un mot-clé pour lancer une recherche."
            else paste0("Aucun résultat trouvé pour le mot « ", input$search_word, " »"))
      })
    }
  })
  
  output$plot_occurrences <- renderPlot({
    data <- get_circle_data()
    if (nrow(data) == 0) return(NULL)
    top20 <- data %>% arrange(desc(occurrences)) %>% slice_head(n = 20)
    ggplot(top20, aes(x = reorder(ID, occurrences), y = occurrences)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Occurrences du mot-clé par jardin",
           x = "ID du jardin",
           y = "Nombre d'occurrences") +
      theme_minimal(base_size = 13)
  })
  
  observeEvent(input$plot_click, {
    data <- get_circle_data()
    top20 <- data %>% arrange(desc(occurrences)) %>% slice_head(n = 20)
    index <- nrow(top20) - round(input$plot_click$y) + 1
    if (index >= 1 && index <= nrow(top20)) {
      coords <- st_coordinates(top20[index, ]$geometry)
      leafletProxy("map_scraping") %>%
        flyTo(lng = coords[1], lat = coords[2], zoom = 16)
    }
  })
  
  output$table_scraping <- renderDataTable({
    bounds <- input$map_scraping_bounds
    data <- get_circle_data()
    if (is.null(bounds)) return(data[0, ])
    df <- data %>% filter(
      st_coordinates(geometry)[,1] >= bounds$west &
        st_coordinates(geometry)[,1] <= bounds$east &
        st_coordinates(geometry)[,2] >= bounds$south &
        st_coordinates(geometry)[,2] <= bounds$north)
    datatable(st_set_geometry(df, NULL), options = list(scrollX = TRUE), width = "100%")
  })
}

shinyApp(ui = ui, server = server)