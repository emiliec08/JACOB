# -- Application Shiny : Onglets avec cartes, histogramme et légende dynamique
options("shiny.port" = 3841, "shiny.host" = "0.0.0.0", "golem.app.prod" = TRUE)

# ---------------- Libs ----------------
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

source("R/format_table.R")
source("R/connect_to_jacob.R")   # connexion Postgres

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

# === Lexique: table des formes par lemma ===
# Récupère toutes les formes (word) correspondant au lemma (insensible à la casse)
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

# Construit une regex safe à partir d'une liste de formes (accents et métas échappés)
build_regex_from_forms <- function(forms_vec) {
  if (length(forms_vec) == 0) return(NULL)
  esc <- stringr::str_replace_all(forms_vec, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")
  esc <- unique(esc[nzchar(esc)])
  if (!length(esc)) return(NULL)
  paste0("(?<!\\p{L})(", paste(esc, collapse = "|"), ")(?!\\p{L})")
}

# --- Surlignage basé sur le lexique (word <-> lemma) ---
# - text: texte HTML/plain
# - lemma: le lemma saisi (input)
# - con: connexion DB (obligatoire si forms=NULL)
# - forms: vecteur optionnel de formes; si fourni, on n'interroge pas la DB
.highlight_keyword <- function(text, lemma, con = NULL, forms = NULL) {
  if (is.na(text) || !nzchar(text) || is.na(lemma) || !nzchar(lemma)) return(text)
  
  # 1) Récupère les formes depuis la DB (si non fournies)
  if (is.null(forms)) {
    if (is.null(con)) return(text)
    forms <- get_lemma_forms(lemma, con)
  }
  
  # 2) Fallback minimal si le lexique ne renvoie rien
  if (!length(forms)) {
    # on tente quelques variantes régulières basiques autour du lemma
    forms <- tolower(c(lemma, paste0(lemma, c("s","es","e","er","é","ée","és","ées"))))
  }
  
  # 3) Construit la regex
  pat <- build_regex_from_forms(forms)
  if (is.null(pat)) return(text)
  
  # 4) Remplacement: gras noir
  stringr::str_replace_all(
    text,
    stringr::regex(pat, ignore_case = TRUE),
    "<b style='color:#000'>\\0</b>"
  )
}

# --- Coloration par filename + surlignage via lexique
colorize_by_filename <- function(df_texts, lemma, con, forms = NULL) {
  if (nrow(df_texts) == 0) return(NA_character_)
  cols <- c("#1f77b4", "#2ca02c", "#d62728", "#9467bd", "#8c564b")  # palette discrète
  
  # ordre stable par filename pour une couleur constante
  df_texts <- df_texts %>% arrange(filename %||% "")
  uniq_f <- unique(df_texts$filename %||% "")
  col_map <- setNames(cols[(seq_along(uniq_f)-1) %% length(cols) + 1], uniq_f)
  
  # On s'assure que les colonnes existent
  if (!"source_url" %in% names(df_texts)) df_texts$source_url <- NA_character_
  if (!"filename"   %in% names(df_texts)) df_texts$filename   <- NA_character_
  if (!"texte_nettoye" %in% names(df_texts)) df_texts$texte_nettoye <- ""
  
  # Récupère les formes UNE SEULE FOIS si pas fournies
  if (is.null(forms)) {
    forms <- get_lemma_forms(lemma, con)
  }
  
  parts <- pmap_chr(df_texts, function(garden_id, filename, source_url, texte_nettoye, ...) {
    col <- col_map[[filename %||% ""]]
    t <- texte_nettoye %||% ""
    if (!nzchar(t)) return("")
    
    # Surligner via lexique
    t <- .highlight_keyword(t, lemma, con = con, forms = forms)
    
    # Badges filename + source_url
    filename_safe <- if (!is.null(filename) && nzchar(filename)) htmltools::htmlEscape(filename) else ""
    badge_file <- if (nzchar(filename_safe)) {
      sprintf("<span style='background:%s20;color:%s;padding:2px 6px;border-radius:8px;font-size:90%%;margin-right:6px;'>%s</span>", col, col, filename_safe)
    } else ""
    
    src <- if (!is.null(source_url)) as.character(source_url) else ""
    src_safe <- if (nzchar(src)) htmltools::htmlEscape(src) else ""
    badge_src <- if (nzchar(src_safe)) {
      sprintf("<span style='background:#00000010;color:#444;padding:2px 6px;border-radius:8px;font-size:90%%;margin-right:6px;'>Source : %s</span>", src_safe)
    } else ""
    
    sprintf("<div style='margin-bottom:6px; color:%s;'>%s%s%s</div>", col, badge_file, badge_src, t)
  })
  
  paste(parts[parts != ""], collapse = "")
}

# --- Schéma & tables ---
DB_SCHEMA <- '"Jacob_data"'
T_POLY    <- paste0(DB_SCHEMA, '.jardin_poly')
T_PNT     <- paste0(DB_SCHEMA, '.jardin_pnt')
T_INFOS   <- paste0(DB_SCHEMA, '.jardin_infos')
T_SPEC    <- paste0(DB_SCHEMA, '.jardin_collectif_spec')      # clé = garden_id
T_TEXT    <- paste0(DB_SCHEMA, '.jardins_texte_url')          # clé = garden_id
T_LEX     <- paste0(DB_SCHEMA, '.jardin_lexique_lemma')       # <-- lexique (word, lemma)
GEOM_COL  <- "geom"

# Couleurs connues
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

# --- Helper pare-balles ---
ensure_cols <- function(df) {
  if (is.null(df)) return(df)
  if (!"occurrences" %in% names(df)) df$occurrences <- 0L
  if (!"spec" %in% names(df)) df$spec <- NA_real_
  df
}

# ---------------- UI ----------------
ui <- navbarPage(
  # Titre image (mettre le fichier dans /www)
  title = div(
    img(src = "logo_jacob_clean.png", height = "50px", style = "max-height:100px;"),
    style = "display:flex; justify-content:center; align-items:center; width:100%;"
  ),
  
  # Onglet 1 : Polygones
  tabPanel("Introduction",
           fluidRow(
             column(width=3,
                    checkboxGroupInput("modgest", "Mode de gestion",
                                       choices = c(
                                         "Jardin à classer" = "JARDIN À CLASSER",
                                         "Jardin familial"  = "JARDIN FAMILIAL",
                                         "Jardin partagé"   = "JARDIN PARTAGÉ"
                                       ),
                                       selected=c("JARDIN FAMILIAL", "JARDIN PARTAGÉ")),
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
                      textInput("search_word", "Entrez un mot-clé (lemma) :", value = ""),
                      actionButton("do_search", "Rechercher", icon = icon("search"),
                                   style="width:120px; font-size:90%;")
                    ),
                    textOutput("nb_jardins_concernes"),
                    br(),
                    h5("Top 20 des jardins contenant"),
                    plotOutput("plot_occurrences", height = "350px", click = "plot_click"),
                    checkboxInput("all_columns_2","Montrer toutes les variables", value=FALSE)
             ),
             column(width=8,
                    leafletOutput("map_scraping", height = "50vh"),
                    uiOutput("no_result_text")
             )),
           dataTableOutput("table_scraping")
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  # ---- Classes sélectionnées ----
  r_get_selected_classes <- reactive({
    selected_classes <- input$modgest
    if ("Jardin partagé" %in% input$modgest) {
      selected_classes <- input$modgest[input$modgest!="Jardin partagé"]
      selected_classes <- c(selected_classes, input$sub_filter_classes_intro)
    }
    selected_classes
  })
  
  # ---- POLYGONES ----
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
  
  # >>> Points (centroïdes) pour aperçu national entre zoom 3 et 11
  filter_overview_points <- reactive({
    bounds  <- input$map_intro_bounds
    zoom    <- input$map_intro_zoom %||% 1
    classes <- r_get_selected_classes()
    if (is.null(bounds) || length(classes) == 0 || zoom < 3 || zoom > 11) return(empty_sf_4326())
    
    con <- connect_to_jacob()
    env <- make_envelope_sql(bounds)
    classes_sql <- paste0("ARRAY[", paste(sprintf("'%s'", sql_escape(classes)), collapse=","), "]")
    
    sql <- sprintf("
      SELECT
        p.id,
        i.name,
        i.source_layer,
        i.classe_brute,
        i.classe_mot,
        ST_PointOnSurface(p.%s) AS geom
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
  # <<<
  
  # ---- Palette ----
  pal_poly <- reactive({
    data_poly <- filter_data()
    data_pnt  <- filter_overview_points()
    dom <- character(0)
    if ("classe_mot" %in% names(data_poly)) dom <- c(dom, data_poly$classe_mot)
    if ("classe_mot" %in% names(data_pnt))  dom <- c(dom,  data_pnt$classe_mot)
    dom <- sort(unique(dom))
    if (length(dom) == 0) return(colorFactor(palette = known_cols, domain = character(0)))
    cols <- vapply(dom, function(cl) {
      if (cl %in% known_names) layers_info[[cl]] else {
        known_cols[((which(dom == cl) - 1) %% length(known_cols)) + 1]
      }
    }, character(1))
    colorFactor(palette = unname(cols), domain = dom)
  })
  
  output$map_intro <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.4)) %>%
      setView(lng = 2.35, lat = 46.7, zoom = 5)
  })
  
  # >>> Affichage conditionnel : points (zoom 3–11) vs polygones (zoom >= 12)
  observe({
    zoom <- input$map_intro_zoom %||% 1
    proxy <- leafletProxy("map_intro") %>% clearShapes()
    pal <- pal_poly()
    
    if (zoom >= 12) {
      data_poly <- filter_data()
      if (nrow(data_poly) > 0) {
        proxy %>% addPolygons(
          data = data_poly,
          fillColor = ~pal(classe_mot),
          color = "black", weight = 1, opacity = 1, fillOpacity = 0.5,
          popup = ~paste0("<strong>", id, "</strong>",
                          "<br>Type : ", classe_mot,
                          "<br>Surface : ", surface_m2,
                          "<br>Nom : ", name)
        )
      }
    } else if (zoom >= 3 && zoom <= 11) {
      data_pnt <- filter_overview_points()
      if (nrow(data_pnt) > 0) {
        data_pnt <- sf_add_coords(data_pnt)
        if (nrow(data_pnt) > 0 && all(c("lng","lat") %in% names(data_pnt))) {
          radius_val <- ifelse(zoom >= 9, 500, 1500)
          proxy %>% addCircles(
            data = data_pnt,
            lng = ~lng, lat = ~lat,
            radius = radius_val,
            stroke = TRUE, color = "lightgrey", weight = 0.2, opacity = 0.5,
            fillColor = ~pal(classe_mot), fillOpacity = 1,
            options = pathOptions(interactive = FALSE, bubblingMouseEvents = FALSE)
          )
        }
      }
    }
  })
  # <<<
  
  # >>> Légende valable dans les deux cas
  observe({
    data_poly <- filter_data()
    data_pnt  <- filter_overview_points()
    data_any  <- if (nrow(data_poly) > 0) data_poly else data_pnt
    
    proxy <- leafletProxy("map_intro") %>% clearControls()
    if (nrow(data_any) > 0 && "classe_mot" %in% names(data_any)) {
      pal <- pal_poly()
      classes_presentes <- sort(unique(data_any$classe_mot))
      proxy %>% addLegend("bottomright", pal = pal,
                          values = classes_presentes, title = "Types de jardins", opacity = 1)
    }
  })
  # <<<
  
  output$table_intro <- renderDataTable({
    data <- filter_data()
    if(!input$all_columns_1){
      data <- data %>% select(any_of(c("id","name","source_layer","surface_m2","classe_brute","classe_mot")))
    }
    if (nrow(data) == 0) return(data.frame())
    data %>% st_drop_geometry() %>% format_table()
  })
  
  # ---- SCRAPING (clé correcte : s.garden_id = p.id) ----
  r_get_jacob_word <- eventReactive(input$do_search, {
    w <- trimws(input$search_word %||% "")
    if (w == "") return(empty_sf_4326())
    con <- connect_to_jacob()
    
    # 1) Requête METRICS (PAS de jointure sur les textes -> pas de duplication)
    sql_metrics <- paste0("
      SELECT
        p.id,
        s.garden_id,
        (ST_Dump(p.", GEOM_COL, ")).geom AS geom,
        s.lemma,
        s.n   AS occurrences,
        s.spec,
        i.name,
        i.source_layer,
        i.surface_m2,
        i.classe_mot,
        i.classe_brute
      FROM ", T_PNT, " p
      JOIN ", T_SPEC, " s   ON s.garden_id = p.id
      LEFT JOIN ", T_INFOS, " i ON i.id = p.id
      WHERE LOWER(TRIM(s.lemma)) = LOWER(TRIM(?word_exact))
    ")
    q1 <- DBI::sqlInterpolate(con, sql_metrics, word_exact = w)
    out <- tryCatch(sf::st_read(con, query = q1, quiet = TRUE), error = function(e) NULL)
    if (is.null(out) || nrow(out) == 0) return(empty_sf_4326())
    if (is.na(sf::st_crs(out)) || sf::st_crs(out)$epsg != 4326) out <- sf::st_transform(out, 4326)
    
    # Agrégation par garden_id, en restant en sf
    out <- out[!sf::st_is_empty(out), ]
    if (nrow(out) == 0) return(empty_sf_4326())
    
    out_agg <- out %>%
      dplyr::group_by(garden_id) %>%
      dplyr::summarise(
        id           = dplyr::first(id),
        lemma        = dplyr::first(lemma),
        occurrences  = sum(occurrences, na.rm = TRUE),
        spec         = { v <- spec; if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) },
        name         = {
          nm <- name[!is.na(name) & trimws(name) != ""]
          if (length(nm)) nm[1] else dplyr::first(name)
        },
        source_layer = dplyr::first(source_layer),
        surface_m2   = dplyr::first(surface_m2),
        classe_mot   = dplyr::first(classe_mot),
        classe_brute = dplyr::first(classe_brute),
        geom         = sf::st_union(geom),
        .groups = "drop"
      ) %>%
      sf::st_make_valid() %>%
      dplyr::mutate(geom = sf::st_centroid(geom))
    
    if (is.na(sf::st_crs(out_agg)) || sf::st_crs(out_agg)$epsg != 4326) {
      out_agg <- sf::st_transform(out_agg, 4326)
    }
    out_agg <- out_agg[!sf::st_is_empty(out_agg), ]
    if (nrow(out_agg) == 0) return(empty_sf_4326())
    coords <- sf::st_coordinates(sf::st_geometry(out_agg))
    out_agg$lng <- coords[,1]; out_agg$lat <- coords[,2]
    
    # 2) Requête TEXTES (séparée) puis coloration par filename
    gids <- unique(out_agg$garden_id)
    if (length(gids) > 0) {
      gids_sql <- paste(sprintf("'%s'", sql_escape(as.character(gids))), collapse = ",")
      sql_texts <- sprintf("
        SELECT garden_id, filename, source_url, texte_nettoye
        FROM %s
        WHERE garden_id IN (%s)
      ", T_TEXT, gids_sql)
      texts <- tryCatch(DBI::dbGetQuery(con, sql_texts), error = function(e) data.frame())
    } else {
      texts <- data.frame()
    }
    
    if (nrow(texts) > 0) {
      # Prépare les formes UNE FOIS pour le lemme recherché
      forms_for_w <- get_lemma_forms(w, con)
      
      # Assemble HTML coloré par garden_id
      texts_by_g <- texts %>%
        mutate(texte_nettoye = ifelse(is.na(texte_nettoye), "", texte_nettoye),
               filename = ifelse(is.na(filename), "", filename)) %>%
        group_by(garden_id) %>%
        summarise(
          texte = colorize_by_filename(cur_data(), lemma = w, con = con, forms = forms_for_w),
          .groups = "drop"
        )
      out_agg <- out_agg %>% left_join(texts_by_g, by = "garden_id")
    } else {
      out_agg$texte <- NA_character_
    }
    
    # Couleurs des cercles selon spec
    out_agg <- out_agg %>%
      mutate(
        occurrences = as.integer(occurrences),
        highlight = case_when(
          !is.na(spec) & spec >  2 ~ "steelblue",
          !is.na(spec) & spec < -2 ~ "coral",
          TRUE                     ~ "grey"
        )
      )
    
    ensure_cols(out_agg)
  }, ignoreInit = TRUE)
  
  output$nb_jardins_concernes <- renderText({
    data <- r_get_jacob_word()
    if (is.null(data) || nrow(data) == 0) return("Aucun résultat pour ce mot.")
    data <- ensure_cols(data)
    nb <- nrow(dplyr::filter(data, occurrences > 0))
    if (nb == 0) "Aucun jardin collectif concerné."
    else if (nb == 1) "Résultat : 1 jardin collectif est concerné."
    else paste0("Résultat : ", nb, " jardins collectifs sont concernés.")
  })
  
  output$map_scraping <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.4))%>%
      setView(lng = 2.35, lat = 46.7, zoom = 5)
  })
  
  # --- MàJ carte scraping : recentrage + rendu visible (px vs mètres) ---
  observe({
    filtered_data <- r_get_jacob_word()
    proxy <- leafletProxy("map_scraping") %>% clearShapes() %>% clearMarkers()
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      output$no_result_text <- renderUI({
        div(style="color:red; padding:10px;", "Aucun résultat pour ce mot.")
      })
      return()
    }
    filtered_data <- ensure_cols(filtered_data)
    
    if (!all(c("lng","lat") %in% names(filtered_data))) {
      filtered_data <- sf_add_coords(filtered_data)
    }
    if (!(nrow(filtered_data) > 0 && all(c("lng","lat") %in% names(filtered_data)))) {
      output$no_result_text <- renderUI({
        div(style="color:red; padding:10px;", "Résultats trouvés mais géométrie manquante.")
      })
      return()
    }
    output$no_result_text <- renderUI({ NULL })
    
    # Recentrage auto
    bb <- sf::st_bbox(filtered_data)
    if (nrow(filtered_data) == 1) {
      proxy %>% flyTo(lng = filtered_data$lng[1], lat = filtered_data$lat[1], zoom = 14)
    } else if (is.finite(bb$xmin) && is.finite(bb$ymin) && is.finite(bb$xmax) && is.finite(bb$ymax)) {
      proxy %>% fitBounds(lng1 = bb$xmin, lat1 = bb$ymin, lng2 = bb$xmax, lat2 = bb$ymax)
    }
    
    # Rendu : pixels quand on est loin, mètres quand on est proche
    current_zoom <- input$map_scraping_zoom %||% 5
    if (!is.null(current_zoom) && current_zoom <= 9) {
      proxy %>% addCircleMarkers(
        data = filtered_data,
        lng = ~lng, lat = ~lat,
        radius = ~pmax(occurrences, 1) ^ 0.5 * 6,
        stroke = TRUE, color = ~highlight, weight = 1, opacity = 0.9,
        fillColor = ~highlight, fillOpacity = 0.5,
        popup = ~paste0("<strong>", id, "</strong>",
                        "<br>Occurrences : ", occurrences,
                        "<br>Spécificité : ", round(spec,2),
                        "<br>Nom : ", name,
                        "<br>Surface : ", surface_m2,
                        "<br>Type : ", classe_mot)
      )
    } else {
      proxy %>% addCircles(
        data = filtered_data,
        lng = ~lng, lat = ~lat,
        radius = ~pmax(occurrences, 1) * 50,
        color = ~highlight, opacity = 0.8,
        fillColor = ~highlight, fillOpacity = 0.5,
        popup = ~paste0("<strong>", id, "</strong>",
                        "<br>Occurrences : ", occurrences,
                        "<br>Spécificité : ", round(spec,2),
                        "<br>Nom : ", name,
                        "<br>Surface : ", surface_m2,
                        "<br>Type : ", classe_mot)
      )
    }
  })
  
  # ---- Graphique ----
  output$plot_occurrences <- renderPlot({
    data <- r_get_jacob_word()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    data <- ensure_cols(data)
    top20 <- data %>% arrange(desc(occurrences)) %>% slice_head(n=20)
    ggplot(top20, aes(x = reorder(id, occurrences), y = occurrences)) +
      geom_col(fill = "lightblue") + coord_flip() +
      labs(title="Occurrences du mot-clé par jardin", x="ID du jardin", y="Nombre d'occurrences") +
      theme_minimal(base_size = 13)
  })
  
  # ---- Clic sur le graphe ----
  observeEvent(input$plot_click, {
    data <- r_get_jacob_word()
    if (is.null(data) || nrow(data) == 0) return()
    data <- ensure_cols(data)
    
    top20 <- data %>% arrange(desc(occurrences)) %>% slice_head(n = 20)
    if (nrow(top20) == 0) return()
    
    level_order <- levels(reorder(top20$id, top20$occurrences))
    if (is.null(level_order)) {
      ord <- order(top20$occurrences, decreasing = FALSE)
      level_order <- unique(top20$id[ord])
    }
    
    y_idx <- suppressWarnings(round(input$plot_click$y))
    if (is.na(y_idx) || y_idx < 1 || y_idx > length(level_order)) return()
    
    id_clicked <- level_order[y_idx]
    row_clicked <- which(top20$id == id_clicked)
    if (length(row_clicked) == 0) return()
    
    coords <- sf::st_coordinates(sf::st_geometry(top20[row_clicked[1], ]))
    if (is.null(coords) || nrow(coords) == 0) return()
    
    leafletProxy("map_scraping") %>%
      flyTo(lng = coords[1, 1], lat = coords[1, 2], zoom = 16)
  })
  
  # ---- Table scraping (HTML pour voir <b>...</b> dans 'texte') ----
  output$table_scraping <- renderDataTable({
    data <- r_get_jacob_word()
    if (is.null(data) || nrow(data) == 0) return(data.frame())
    data <- ensure_cols(data)
    data <- dplyr::filter(data, occurrences > 0)
    
    data <- data %>% dplyr::select(any_of(c(
      "id","name","lemma","occurrences","spec","source_layer","surface_m2","classe_mot","texte"
    )))
    
    if (nrow(data) == 0) return(data[0, ])
    df <- data %>% sf::st_drop_geometry()
    DT::datatable(df, escape = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)
