# -- Application Shiny : Onglets avec cartes, histogramme et légende dynamique
options("shiny.port" = 3841, "shiny.host" = "0.0.0.0", "golem.app.prod" = TRUE)

#____________________________ library __________________________________________ 
library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(DT)
library(shinyjs)
library(stringr)
library(DBI)
library(purrr)
library(shinycssloaders) # pour le spinner de chargmt
library(shinybusy)
#____________________________ sources __________________________________________
source("R/format_table.R")
source("R/connect_to_jacob.R")   # connexion Postgres

# --- Mot de passe pour afficher le texte (depuis .Renviron)
TEXT_PWD <- Sys.getenv("SCRAPING_TEXT_PASSWORD", unset = "")


# ------------------Wms
WMS_BASE   <- "https://geoserver-dev.evs.ens-lyon.fr/geoserver/wms"
WMS_LAYER  <- "jacob:jardin_pnt_infos"   # ← mets ici le nom exact publié
WMS_STYLE  <- ""                          # ou "jacob:mon_style" si tu veux forcer un SLD

# ---------------- Helpers ----------------
`%||%` <- function(a, b) if (is.null(a)) b else a
sql_escape <- function(x) gsub("'", "''", x)

make_envelope_sql <- function(bounds) {
  sprintf("ST_MakeEnvelope(%f, %f, %f, %f, 4326)",
          bounds$west, bounds$south, bounds$east, bounds$north)
}

empty_sf_4326 <- function() sf::st_sf(geometry = sf::st_sfc(), crs = 4326)

# Convertit un sf en data.frame avec colonnes lng/lat (filtre géométries vides)
sf_add_coords <- function(x) {
  if (!inherits(x, "sf") || nrow(x) == 0) return(x)
  x <- x[!sf::st_is_empty(x), ]
  if (nrow(x) == 0) return(x)
  coords <- sf::st_coordinates(sf::st_geometry(x))
  x$lng <- coords[,1]
  x$lat <- coords[,2]
  x
}

# --- Helpers accents & échappement regex ---
.make_accent_pattern <- function(keyword) {
  if (is.na(keyword) || !nzchar(keyword)) return(NULL)
  k <- keyword
  k <- stringr::str_replace_all(k, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")
  k <- stringr::str_to_lower(k)
  k <- stringr::str_replace_all(k, "a", "[aàáâäãåā]")
  k <- stringr::str_replace_all(k, "c", "[cç]")
  k <- stringr::str_replace_all(k, "e", "[eéèêëē]")
  k <- stringr::str_replace_all(k, "i", "[iíìîïī]")
  k <- stringr::str_replace_all(k, "o", "[oóòôöõō]")
  k <- stringr::str_replace_all(k, "u", "[uúùûüū]")
  k <- stringr::str_replace_all(k, "y", "[yýÿŷ]")
  k
}

# === Lexique
get_lemma_forms <- function(lemma, con) {
  if (is.null(lemma) || !nzchar(lemma)) return(character(0))
  sql <- paste0("
    SELECT DISTINCT word
    FROM ", paste0('"Jacob_data"', ".", "jardin_lexique_lemma"), "
    WHERE lower(lemma) = lower(?lem)
      AND word IS NOT NULL
      AND trim(word) <> ''
  ")
  q <- DBI::sqlInterpolate(con, sql, lem = lemma)
  out <- tryCatch(DBI::dbGetQuery(con, q), error = function(e) data.frame())
  if (!nrow(out)) return(character(0))
  tolower(trimws(out$word))
}

build_regex_from_forms <- function(forms_vec) {
  if (length(forms_vec) == 0) return(NULL)
  esc <- stringr::str_replace_all(forms_vec, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")
  esc <- unique(esc[nzchar(esc)])
  if (!length(esc)) return(NULL)
  paste0("(?<!\\p{L})(", paste(esc, collapse = "|"), ")(?!\\p{L})")
}

.highlight_keyword <- function(text, lemma, con = NULL, forms = NULL) {
  if (is.na(text) || !nzchar(text) || is.na(lemma) || !nzchar(lemma)) return(text)
  if (is.null(forms)) {
    if (is.null(con)) return(text)
    forms <- get_lemma_forms(lemma, con)
  }
  if (!length(forms)) {
    forms <- tolower(c(lemma, paste0(lemma, c("s","es","e","er","é","ée","és","ées"))))
  }
  pat <- build_regex_from_forms(forms)
  if (is.null(pat)) return(text)
  stringr::str_replace_all(
    text,
    stringr::regex(pat, ignore_case = TRUE),
    "<b style='color:#000'>\\0</b>"
  )
}

colorize_by_filename <- function(df_texts, lemma, con, forms = NULL) {
  if (nrow(df_texts) == 0) return(NA_character_)
  cols <- c("#1f77b4", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
  df_texts <- df_texts %>% arrange(filename %||% "")
  uniq_f <- unique(df_texts$filename %||% "")
  col_map <- setNames(cols[(seq_along(uniq_f)-1) %% length(cols) + 1], uniq_f)
  if (!"source_url" %in% names(df_texts)) df_texts$source_url <- NA_character_
  if (!"filename"   %in% names(df_texts)) df_texts$filename   <- NA_character_
  if (!"texte_nettoye" %in% names(df_texts)) df_texts$texte_nettoye <- ""
  
  if (is.null(forms)) forms <- get_lemma_forms(lemma, con)
  
  parts <- purrr::pmap_chr(df_texts, function(garden_id, filename, source_url, texte_nettoye, ...) {
    col <- col_map[[filename %||% ""]]
    t <- texte_nettoye %||% ""
    if (!nzchar(t)) return("")
    t <- .highlight_keyword(t, lemma, con = con, forms = forms)
    filename_safe <- if (!is.null(filename) && nzchar(filename)) htmltools::htmlEscape(filename) else ""
    badge_file <- if (nzchar(filename_safe)) sprintf("<span style='background:%s20;color:%s;padding:2px 6px;border-radius:8px;font-size:90%%;margin-right:6px;'>%s</span>", col, col, filename_safe) else ""
    src <- if (!is.null(source_url)) as.character(source_url) else ""
    src_safe <- if (nzchar(src)) htmltools::htmlEscape(src) else ""
    badge_src <- if (nzchar(src_safe)) sprintf("<span style='background:#00000010;color:#444;padding:2px 6px;border-radius:8px;font-size:90%%;margin-right:6px;'>Source : %s</span>", src_safe) else ""
    sprintf("<div style='margin-bottom:6px; color:%s;'>%s%s%s</div>", col, badge_file, badge_src, t)
  })
  paste(parts[parts != ""], collapse = "")
}

# --- Schéma & tables ---
DB_SCHEMA <- '"Jacob_data"'
T_POLY    <- paste0(DB_SCHEMA, '.jardin_poly')
T_PNT     <- paste0(DB_SCHEMA, '.jardin_pnt_simple')             # (plus utilisé pour la carte intro)
T_INFOS   <- paste0(DB_SCHEMA, '.jardin_infos')
T_SPEC    <- paste0(DB_SCHEMA, '.jardin_collectif_spec_n')  # clé = garden_id
T_TEXT    <- paste0(DB_SCHEMA, '.jardins_texte_url')
T_LEX     <- paste0(DB_SCHEMA, '.jardin_lexique_lemma')
GEOM_COL  <- "geom"

layers_info <- list(
  "JARDIN PARTAGÉ"      = "darkgreen",
  "FERME URBAINE"       = "darkseagreen",
  "JARDIN DE RUE"       = "green",
  "JARDIN PÉDAGOGIQUE"  = "gold",
  "JARDIN À CLASSER"    = "brown",
  "JARDIN D'INSERTION"  = "greenyellow",
  "JARDIN FAMILIAL"     = "goldenrod"
)
known_names <- names(layers_info)
known_cols  <- unname(unlist(layers_info))

# Libellés personnalisés pour la légende (par valeur de classe_mot)
legend_labels <- c(
  "JARDIN PARTAGÉ"      = "Jardin partagé",
  "JARDIN PÉDAGOGIQUE"  = "Jardin pédagogique",
  "JARDIN DE RUE"       = "Jardin de rue",
  "JARDIN D'INSERTION"  = "Jardin d'insertion",
  "FERME URBAINE"       = "Ferme urbaine",
  "JARDIN FAMILIAL"     = "Jardin familial",
  "JARDIN À CLASSER"    = "À classer"
)

ensure_cols <- function(df) {
  if (is.null(df)) return(df)
  if (!"occurrences" %in% names(df)) df$occurrences <- 0L
  if (!"spec" %in% names(df)) df$spec <- NA_real_
  df
}

                                                                            # ---------------- UI ----------------
ui <- navbarPage(
  title = div(
    img(src = "logo_jacob_clean.png", height = "50px", style = "max-height:100px;"),
    style = "display:flex; justify-content:center; align-items:center; width:100%;"
  ),
  
  header = shinyjs::useShinyjs(),  # <-- active shinyjs
  
  # Onglet 1 : Introduction = WMS (3–11) puis Polygones (>= 12)
  tabPanel("Introduction",
           fluidRow(
             column(width=3,
                    # état du zoom
                    htmlOutput("zoom_hint_intro"),
                    checkboxGroupInput("modgest", "Mode de gestion",
                                       choices = c(
                                         "Jardin à classer" = "JARDIN À CLASSER",
                                         "Jardin familial"  = "JARDIN FAMILIAL",
                                         "Jardin partagé"   = "JARDIN PARTAGÉ"
                                       ),
                                       selected=c("JARDIN FAMILIAL", "JARDIN PARTAGÉ","JARDIN À CLASSER")),
                    conditionalPanel(condition = "input.modgest.includes('JARDIN PARTAGÉ')",
                                     fluidRow(column(width=10,offset=1,
                                                     checkboxGroupInput("sub_filter_classes_intro", "sous-catégories",
                                                                        choices = list(
                                                                          "partagé"      = "JARDIN PARTAGÉ",
                                                                          "pédagogique"  = "JARDIN PÉDAGOGIQUE",
                                                                          "de rue"       = "JARDIN DE RUE",
                                                                          "d'insertion"  = "JARDIN D'INSERTION",
                                                                          "Ferme urbaine"= "FERME URBAINE"
                                                                        ),
                                                                        selected = c("FERME URBAINE","JARDIN D'INSERTION",
                                                                                     "JARDIN DE RUE","JARDIN PARTAGÉ",
                                                                                     "JARDIN PÉDAGOGIQUE"))
                                     ))),
                    h5("Affichage"),
                    checkboxInput("all_columns_1","Montrer toutes les variables", value=FALSE)
             ),
             column(width=9, leafletOutput("map_intro", height = "50vh"))
           ),
           dataTableOutput("table_intro")
  ),
  
  # Onglet 2 : Scraping
  tabPanel("Analyse Scraping",
           fluidRow(
             column(width=4,
                    div(
                      style = "display:flex; flex-direction:column; gap:2px; max-width:300px;",
                      textInput("search_word", "Entrez un mot-clé (lemma) et appuyez sur Rechercher :", value = ""),
                      actionButton("do_search", "Rechercher", icon = icon("search"),
                                   style="width:120px; font-size:90%;")
                    ),
                    textOutput("nb_jardins_concernes"),
                    br(),
                    plotOutput("plot_occurrences", height = "350px", click = "plot_click")
             ),
             column(width=8,
                    leafletOutput("map_scraping", height = "50vh"),
                    uiOutput("no_result_text")
             )
           ),
           dataTableOutput("table_scraping"),
           uiOutput("text_zone")
  )
)

                                                                  # ---------------- SERVER ----------------

############################################################################### ONGLET INTRO / ################################################################ 
server <- function(input, output, session) {
  
  # ---- Verrou d'accès aux textes
  pending_garden_id <- reactiveVal(NULL)    # mémorise l'ID cliqué en attente du mot de passe
  
  open_text_password_modal <- function() {
    showModal(modalDialog(
      title = "Accès restreint",
      tagList(
        p("Veuillez entrer le mot de passe pour afficher le texte :"),
        passwordInput("pwd_input", label = NULL, placeholder = "Mot de passe"),
        uiOutput("pwd_error_ui")  # ligne d’erreur si mauvais mot de passe
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_pwd", "Valider", class = "btn btn-primary")
      ),
      easyClose = FALSE
    ))
  }
  output$pwd_error_ui <- renderUI({ NULL })
  
  # ---- Charge le texte d'un jardin et l'affiche dans text_zone
  load_and_show_text <- function(gid) {
    if (is.null(gid) || gid == "") return(invisible(NULL))
    w <- trimws(input$search_word %||% "")
    
    show_modal_spinner(
      spin = "fading-circle",
      text = sprintf("Chargement du texte pour le jardin %s...", gid),
      color = "#8EBF8E"
    )
    
    con <- connect_to_jacob()
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    
    sql_text <- sprintf("
    SELECT filename, source_url, texte_nettoye
    FROM %s
    WHERE garden_id = '%s'
  ", T_TEXT, sql_escape(gid))
    
    txt <- tryCatch(DBI::dbGetQuery(con, sql_text), error = function(e) data.frame())
    remove_modal_spinner()
    
    if (nrow(txt) == 0) {
      showNotification("Aucun texte trouvé pour ce jardin.", type = "warning")
      r_selected_text(NULL)
      return(invisible(NULL))
    }
    
    forms_for_w <- get_lemma_forms(w, con)
    if ("highlight_texts" %in% names(input) && isTRUE(input$highlight_texts)) {
      txt$texte_nettoye <- .highlight_keyword(txt$texte_nettoye, w, con = con, forms = forms_for_w)
    }
    
    html_text <- colorize_by_filename(txt, lemma = w, con = con, forms = forms_for_w)
    r_selected_text(html_text)
    showNotification(sprintf("✅ Texte chargé pour le jardin %s", gid), type = "message")
    invisible(NULL)
  }
  
  observeEvent(input$confirm_pwd, {
    req(input$pwd_input)
    if (nzchar(TEXT_PWD) && identical(input$pwd_input, TEXT_PWD)) {
      # pas de text_unlocked(TRUE) -> on reste "locké" pour la prochaine fois
      gid <- isolate(pending_garden_id())
      pending_garden_id(NULL)
      removeModal()
      if (!is.null(gid)) load_and_show_text(gid)
    } else {
      output$pwd_error_ui <- renderUI(
        div(style="color:#c62828;margin-top:6px;", "Mot de passe incorrect.")
      )
    }
  })
  
  
  # ---- Classes sélectionnées ----
  r_get_selected_classes <- reactive({
    selected_classes <- input$modgest
    if ("Jardin partagé" %in% input$modgest) {
      selected_classes <- input$modgest[input$modgest!="Jardin partagé"]
      selected_classes <- c(selected_classes, input$sub_filter_classes_intro)
    }
    selected_classes
  })
  
  # ---- POLYGONES (inchangé) ----
  filter_data <- reactive({
    bounds  <- input$map_intro_bounds
    zoom    <- input$map_intro_zoom %||% 1
    classes <- r_get_selected_classes()
    if (is.null(bounds) || length(classes) == 0 || zoom < 12) return(empty_sf_4326())
    
    con <- connect_to_jacob()
    env <- make_envelope_sql(bounds)
    classes_sql <- paste0("ARRAY[", paste(sprintf("'%s'", sql_escape(classes)), collapse=","), "]")
    
    sql <- sprintf("
      SELECT
        p.id,
        i.name,
        i.source_layer,
        i.surface_m2,
        i.classe_brute,
        i.classe_mot,
        p.%s AS geom
      FROM %s p
      JOIN %s i ON i.id = p.id
      WHERE i.classe_mot = ANY(%s)
        AND ST_Intersects(p.%s, %s)
      LIMIT 50000;", GEOM_COL, T_POLY, T_INFOS, classes_sql, GEOM_COL, env)
    
    out <- sf::st_read(con, query = sql, quiet = TRUE)
    if (nrow(out) == 0) return(empty_sf_4326())
    if (is.na(sf::st_crs(out)) || sf::st_crs(out)$epsg != 4326) out <- sf::st_transform(out, 4326)
    out
  })
  
  # ---- Palette polygones ----
  pal_poly <- reactive({
    data_poly <- filter_data()
    dom <- character(0)
    if ("classe_mot" %in% names(data_poly)) dom <- c(dom, data_poly$classe_mot)
    dom <- sort(unique(dom))
    if (length(dom) == 0) return(colorFactor(palette = known_cols, domain = character(0)))
    cols <- vapply(dom, function(cl) {
      if (cl %in% known_names) layers_info[[cl]] else {
        known_cols[((which(dom == cl) - 1) %% length(known_cols)) + 1]
      }
    }, character(1))
    colorFactor(palette = unname(cols), domain = dom)
  })
  
  # ---- Carte INTRO : init avec WMS (group) ----
  output$map_intro <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.4)) %>%
      addWMSTiles(
        baseUrl = WMS_BASE,
        layers  = WMS_LAYER,   # nom sans 'jacob:'
        options = WMSTileOptions(
          version     = "1.1.1",
          format      = "image/png",
          transparent = TRUE,
          tiled       = TRUE,
          styles      = WMS_STYLE                  # "point" mais la j'ai déjà créé un style sur QGIS SLD
        ),
        group = "WMS points"
      ) %>%
      addLayersControl(
        overlayGroups = c("WMS points","Polygones"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Polygones") %>%
      setView(lng = 2.35, lat = 46.7, zoom = 5)
  })
  
  # >>> Toggle WMS (zoom 3–11) vs Polygones (zoom >= 12)
  observe({
    z <- input$map_intro_zoom %||% 1
    proxy_map <- leafletProxy("map_intro")
    
    # Affiche un petit état au-dessus des filtres
    output$zoom_hint_intro <- renderUI({
      if (z >= 12) 
        HTML("<div style='color:#02b808;margin-bottom:6px;'>🔓 Zoom ≥ 12 : filtres activés.</div>")
      else 
        HTML("<div style='color:#9e9e9e;margin-bottom:6px;'>🔒 Zoomez pour utiliser les filtres. (≥ 12) </div>")
    })
    
    # Active/désactive les filtres selon le zoom
    if (z >= 12) {
      shinyjs::enable("modgest")
      shinyjs::enable("sub_filter_classes_intro")
    } else {
      shinyjs::disable("modgest")
      shinyjs::disable("sub_filter_classes_intro")
      # Option : vider les sélections quand c’est verrouillé
      # updateCheckboxGroupInput(session, "modgest", selected = character(0))
      # updateCheckboxGroupInput(session, "sub_filter_classes_intro", selected = character(0))
    }
    
    if (z >= 12) {
      # -- afficher Polygones, masquer WMS
      data_poly <- filter_data()
      proxy_map %>% hideGroup("WMS points") %>% showGroup("Polygones") %>% clearGroup("Polygones")
      
      if (nrow(data_poly) > 0) {
        pal <- pal_poly()
        proxy_map %>% addPolygons(
          data = data_poly,
          fillColor = ~pal(classe_mot),
          color = "black", weight = 1, opacity = 1, fillOpacity = 0.5,
          popup = ~paste0("<strong>", id, "</strong>",
                          "<br>Type : ", classe_mot,
                          "<br>Surface : ", surface_m2,
                          "<br>Nom : ", name),
          group = "Polygones"
        )
      }
      
      # Légende pour polygones
      proxy_map %>% clearControls()
      data_any <- data_poly
      if (nrow(data_any) > 0 && "classe_mot" %in% names(data_any)) {
        pal <- pal_poly()
        classes_presentes <- sort(unique(data_any$classe_mot))
        # 1) couleurs à partir de la palette
        cols <- pal(classes_presentes)
        # 2) étiquettes personnalisées (fallback = valeur brute si non mappée)
        tab <- table(data_any$classe_mot)
        labels <- vapply(classes_presentes, function(cl) {
          base <- if (cl %in% names(legend_labels)) legend_labels[[cl]] else cl
          sprintf("%s (%d)", base, as.integer(tab[[cl]] %||% 0))
        }, character(1))
        
        proxy_map %>% addLegend(
          position = "bottomright",
          colors   = cols,
          labels   = labels,
          title    = "Types de jardins",
          opacity  = 1)
      }
      
    } else if (z >= 3 && z <= 11) {
      # -- afficher WMS, masquer Polygones (pas de légende ici)
      proxy_map %>% hideGroup("Polygones") %>% showGroup("WMS points") %>% clearControls()
      
      # Légende WMS via GetLegendGraphic (WMS public : pas besoin de proxy)
      legend_url <- paste0(
        WMS_BASE,
        "?service=WMS&request=GetLegendGraphic&format=image/png&layer=",
        URLencode(WMS_LAYER, reserved = TRUE)
      )
      proxy_map %>% addControl(
        html = sprintf(
          '<div style="background:white;padding:6px;border-radius:6px">
             <b>Types de jardins</b><br><img src="%s" style="max-width:180px">
           </div>', legend_url),
        position = "bottomright"
      )
      
    } else {
      proxy_map %>% hideGroup("WMS points") %>% hideGroup("Polygones") %>% clearControls()
    }
  })
  
  # ---- Table intro
  output$table_intro <- renderDataTable({
    data <- filter_data()
    if(!input$all_columns_1){
      data <- data %>% select(any_of(c("id","name","source_layer","surface_m2","classe_brute","classe_mot")))
    }
    if (nrow(data) == 0) return(data.frame())
    data %>% st_drop_geometry() %>% format_table()
  })
  
  ###################################################################################### ONGLET SCRAPING : ################################################################ 
  
  
  r_get_jacob_word <- eventReactive(input$do_search, {
    w <- trimws(input$search_word %||% "")
    if (w == "") return(empty_sf_4326())
    
    show_modal_spinner(
      spin = "fading-circle",
      text = sprintf("Recherche en cours pour « %s »...", w),
      color = "#8EBF8E"
    )
    
    con <- connect_to_jacob()
    
    # --- Agrégation SQL directe ---
    sql_metrics <- paste0("
      SELECT 
        s.garden_id,
        s.lemma,
        SUM(s.n) AS occurrences,
        MAX(s.spec) AS spec
      FROM ", T_SPEC, " s
      WHERE LOWER(TRIM(s.lemma)) = LOWER(TRIM(?word_exact))
      GROUP BY s.garden_id, s.lemma
    ")
    
    q1 <- DBI::sqlInterpolate(con, sql_metrics, word_exact = w)
    agg_data <- tryCatch(DBI::dbGetQuery(con, q1), error = function(e) NULL)
    
    if (is.null(agg_data) || nrow(agg_data) == 0) {
      remove_modal_spinner()
      return(empty_sf_4326())
    }
    
    # --- Géométries + infos ---
    gids_sql <- paste(sprintf("'%s'", sql_escape(as.character(agg_data$garden_id))), collapse = ",")
    sql_geom <- sprintf("
      SELECT 
        p.id,
        ST_X(p.geom) AS lng,
        ST_Y(p.geom) AS lat,
        i.name,
        i.source_layer,
        i.surface_m2,
        i.classe_mot,
        i.classe_brute
      FROM %s p
      LEFT JOIN %s i ON i.id = p.id
      WHERE p.id IN (%s)
    ", T_PNT, T_INFOS, gids_sql)
    
    geom_data <- tryCatch(DBI::dbGetQuery(con, sql_geom), error = function(e) NULL)
    out <- dplyr::left_join(geom_data, agg_data, by = c("id" = "garden_id")) %>%
      mutate(
        occurrences = as.integer(occurrences),
        spec = as.numeric(spec),
        highlight = case_when(
          !is.na(spec) & spec >  0.1 ~ "steelblue",
          !is.na(spec) & spec < -1.5 ~ "coral",
          TRUE ~ "lightgrey"
        ),
        texte = NA_character_
      )
    
    remove_modal_spinner()
    sf::st_as_sf(out, coords = c("lng", "lat"), crs = 4326)
  }, ignoreInit = TRUE)
  
  
  #  Quand on lance une nouvelle recherche, on vide le texte sélectionné dans text_zone
  observeEvent(input$do_search, {
    r_selected_text(NULL)   # remet la zone de texte à vide
  })
  
  
  # --- Spinner OFF quand tout est prêt ---
  observeEvent(r_get_jacob_word(), {
    shinyjs::delay(800, remove_modal_spinner())
  })
  
  
  # ---- Texte résumé résultats ----
  output$nb_jardins_concernes <- renderText({
    data <- r_get_jacob_word()
    if (is.null(data) || nrow(data) == 0) return("Aucun résultat pour ce terme.")
    data <- ensure_cols(data)
    nb <- nrow(dplyr::filter(data, occurrences > 0))
    if (nb == 0) "Aucun jardin collectif concerné."
    else if (nb == 1) "Résultat : 1 jardin collectif est concerné."
    else paste0("Résultat : ", nb, " jardins collectifs sont concernés.")
  })
  
  
  # ---- Carte ----
  output$map_scraping <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.4)) %>%
      setView(lng = 2.35, lat = 46.7, zoom = 5)
  })
  
  
  observe({
    filtered_data <- r_get_jacob_word()
    proxy_map <- leafletProxy("map_scraping") %>% clearShapes() %>% clearMarkers()
    
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      output$no_result_text <- renderUI({
        div(style = "color:red; padding:10px;",
            "Aucun résultat pour ce terme. La recherche fonctionne par lemme : essayez le singulier ou l’infinitif.")
      })
      return()
    }
    
    filtered_data <- ensure_cols(filtered_data)
    if (!all(c("lng","lat") %in% names(filtered_data))) filtered_data <- sf_add_coords(filtered_data)
    if (!(nrow(filtered_data) > 0 && all(c("lng","lat") %in% names(filtered_data)))) {
      output$no_result_text <- renderUI({
        div(style = "color:red; padding:10px;", "Résultats trouvés mais géométrie manquante.")
      })
      return()
    }
    
    output$no_result_text <- renderUI({ NULL })
    bb <- sf::st_bbox(filtered_data)
    proxy_map %>% fitBounds(lng1 = bb$xmin, lat1 = bb$ymin, lng2 = bb$xmax, lat2 = bb$ymax)
    
    proxy_map %>% addCircleMarkers(
      data = filtered_data,
      lng = ~lng, lat = ~lat,
      radius = ~(log1p(occurrences) * 4),
      stroke = TRUE, color = ~highlight, weight = 1, opacity = 0.9,
      fillColor = ~highlight, fillOpacity = 0.5,
      popup = ~paste0("<strong>", id, "</strong>",
                      "<br>Occurrences : ", occurrences,
                      "<br>Spécificité : ", round(spec,2),
                      "<br>Nom : ", name,
                      "<br>Type : ", classe_mot)
    )
  })
  
  
  # ---- Graphique top 20 ----
  output$plot_occurrences <- renderPlot({
    req(input$do_search)
    word_used <- isolate(input$search_word)
    data <- r_get_jacob_word()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    data <- ensure_cols(data)
    top20 <- data %>% arrange(desc(occurrences)) %>% slice_head(n = 20)
    ggplot(top20, aes(x = reorder(id, occurrences), y = occurrences)) +
      geom_col(fill = "lightblue") + coord_flip() +
      labs(title = paste0("Les 20 jardins où le mot « ", word_used, " » apparaît le plus souvent"),
           x = "ID du jardin", y = "Nombre d'occurrences") +
      theme_minimal(base_size = 13)
  })
  
  
  # --- Valeur réactive : texte du jardin sélectionné ---
  r_selected_text <- reactiveVal(NULL)
  
  # --- Table principale sans les textes au départ ---
  output$table_scraping <- renderDataTable({
    data <- r_get_jacob_word()
    if (is.null(data) || nrow(data) == 0) return(data.frame())
    df <- data %>%
      sf::st_drop_geometry() %>%
      dplyr::select(any_of(c(
        "id","name","lemma","occurrences","spec","source_layer","surface_m2","classe_mot"
      )))
    datatable(
      df,
      selection = "single",
      rownames = FALSE,   # ✅ ici, pas dans options
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # --- Quand on clique une ligne, on demande le mot de passe si nécessaire, puis on charge le texte ---
  # --- À chaque clic de ligne : demander systématiquement le mot de passe
  observeEvent(input$table_scraping_rows_selected, {
    sel  <- input$table_scraping_rows_selected
    data <- r_get_jacob_word()
    if (is.null(sel) || length(sel) == 0 || is.null(data) || nrow(data) == 0) return()
    
    gid <- data$id[sel[1]]
    if (is.null(gid) || gid == "") return()
    
    pending_garden_id(gid)
    open_text_password_modal()   # on ouvre la modale à chaque fois
  })
  
  
  
  # --- Zone d'affichage du texte sélectionné ---
  output$text_zone <- renderUI({
    txt <- r_selected_text()
    if (is.null(txt) || !nzchar(txt)) return(NULL)
    div(
      style = "background:white; padding:15px; border-radius:8px;
           margin-top:15px; height:60vh; overflow-y:scroll;
           box-shadow:0 2px 8px rgba(0,0,0,0.1); font-size:95%;",
      HTML(txt)
    )
  })
  
  # ---- Bouton pour charger les textes (inchangé) ----
  observeEvent(input$load_texts, {
    data_base <- r_get_jacob_word()
    if (is.null(data_base) || nrow(data_base) == 0) {
      showNotification("Aucun résultat à enrichir avec les textes.", type = "warning")
      return()
    }
    
    w <- trimws(input$search_word %||% "")
    con <- connect_to_jacob()
    
    gids <- unique(data_base$id)
    if (length(gids) == 0) {
      showNotification("Aucun jardin à traiter.", type = "message")
      return()
    }
    
    max_gids <- min(length(gids), 50)
    gids <- gids[seq_len(max_gids)]
    gids_sql <- paste(sprintf("'%s'", sql_escape(as.character(gids))), collapse = ",")
    
    show_modal_spinner(
      spin = "fading-circle",
      text = sprintf("Chargement des textes (%d jardins max)...", max_gids),
      color = "#8EBF8E"
    )
    
    sql_texts <- sprintf("
    SELECT 
      garden_id,
      STRING_AGG(
        COALESCE(filename, '') || ' — ' || 
        COALESCE(source_url, '') || ' : ' || 
        COALESCE(texte_nettoye, ''), 
        E'\\n---\\n'
      ) AS texte
    FROM %s
    WHERE garden_id IN (%s)
    GROUP BY garden_id
  ", T_TEXT, gids_sql)
    
    texts <- tryCatch(DBI::dbGetQuery(con, sql_texts), error = function(e) {
      showNotification(paste("Erreur SQL :", e$message), type = "error")
      data.frame()
    })
    
    remove_modal_spinner()
    
    if (nrow(texts) == 0) {
      showNotification("Aucun texte trouvé pour ces jardins.", type = "warning")
      return()
    }
    
    if (isTRUE(input$highlight_texts)) {
      forms_for_w <- get_lemma_forms(w, con)
      texts$texte <- vapply(texts$texte, function(t) {
        .highlight_keyword(t, w, con = con, forms = forms_for_w)
      }, character(1))
    }
    
    updated <- data_base %>% left_join(texts, by = c("id" = "garden_id"))
    r_data_with_texts(updated)
    
    showNotification(sprintf("✅ Textes ajoutés pour %d jardins.", nrow(texts)), type = "message")
  })
}

shinyApp(ui = ui, server = server)
