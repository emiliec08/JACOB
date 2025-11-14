0.# -- Application Shiny : Onglets avec cartes, histogramme et légende dynamique
options("shiny.port" = 3841, "shiny.host" = "0.0.0.0", "golem.app.prod" = TRUE)

#____________________________ library __________________________________________ 
library(shiny)
library(dplyr)
library(glue)
library(ggplot2)
library(sf)
library(leaflet)
library(DT)
library(shinyjs)
library(stringr)
library(DBI)
library(purrr)
library(scales)
library(plotly)
library(classInt)
library(tidyr)
library(shinycssloaders) # pour le spinner de chargmt
library(shinybusy)
#____________________________ sources __________________________________________
source("R/format_table.R")
source("R/connect_to_jacob.R")   # connexion Postgres

#____________________ gestion du mot de passe __________________________________

# --- Mot de passe pour afficher le texte (depuis .Renviron)
TEXT_PWD <- Sys.getenv("SCRAPING_TEXT_PASSWORD", unset = "")

#_______________________________ wms ___________________________________________ 
WMS_BASE   <- "https://geoserver-dev.evs.ens-lyon.fr/geoserver/wms"
WMS_LAYER  <- "jacob:jardin_pnt_infos"   # ← mets ici le nom exact publié
WMS_STYLE  <- ""                          # ou "jacob:mon_style"

#____________________________ Helpers __________________________________________ 

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

# Vient colorier les textes (pour les diffs sources)
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

#____________________________ Schéma & tables __________________________________
DB_SCHEMA <- '"Jacob_data"'
T_POLY    <- paste0(DB_SCHEMA, '.jardin_poly_4326')
T_PNT     <- paste0(DB_SCHEMA, '.jardin_pnt_simple')             # (plus utilisé pour la carte intro)
T_INFOS   <- paste0(DB_SCHEMA, '.jardin_infos')
T_SPEC    <- paste0(DB_SCHEMA, '.jardin_collectif_spec_n')  # clé = garden_id
T_TEXT    <- paste0(DB_SCHEMA, '.jardins_texte_url')
T_LEX     <- paste0(DB_SCHEMA, '.jardin_lexique_lemma')
T_COSIA   <- paste0(DB_SCHEMA, '.cosia_fr_par_jardin_wide')
T_COM   <- paste0(DB_SCHEMA, '.commune_infos_poly_jardins')
COMMUNE_KEY <- "CODGEO"  # colonne de jardin_infos qui contient le code commune (adapter si besoin)
T_REG  <- paste0(DB_SCHEMA,'.regions')
GEOM_COL  <- "geom"

#____________________________ PALETTES & LÉGENDES (Etiquette)  _________________________________

# Palette principale : jardins
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


# Libellés personnalisés pour la légende (par valeur de classe_mot) -> Légendes texte des types de jardins
legend_labels <- c(
  "JARDIN PARTAGÉ"      = "Jardin partagé",
  "JARDIN PÉDAGOGIQUE"  = "Jardin pédagogique",
  "JARDIN DE RUE"       = "Jardin de rue",
  "JARDIN D'INSERTION"  = "Jardin d'insertion",
  "FERME URBAINE"       = "Ferme urbaine",
  "JARDIN FAMILIAL"     = "Jardin familial",
  "JARDIN À CLASSER"    = "À classer"
)

# Palette CoSIA : classes d’occupation du sol IGN
cosia_palette <- list(
  "Bâtiment"           = "#bf7378",
  "Zone imperméable"   = "#a4a8b3",
  "Zone perméable"     = "#917454",
  "Piscine"            = "#7ecdf7",
  "Serre"              = "#bae3d4",
  "Sol nu"             = "#b5ae94",
  "Surface eau"        = "#43729c",
  "Neige"              = "#ebebfc",
  "Conifère"           = "#376b34",
  "Feuillu"            = "#5e8f39",
  "Broussaille"        = "#b6bf4e",
  "Pelouse"            = "#9bd175",
  "Culture"            = "#d9cd66",
  "Terre labourée"     = "#c79f56",
  "Vigne"              = "#a88290",
  "Autre"              = "#424242"
)
cosia_classes <- names(cosia_palette)
cosia_cols    <- unname(unlist(cosia_palette))


#  Fonction utilitaire (comme avant)
ensure_cols <- function(df) {
  if (is.null(df)) return(df)
  if (!"occurrences" %in% names(df)) df$occurrences <- 0L
  if (!"spec" %in% names(df)) df$spec <- NA_real_
  df
}


#___________________________________________________________________________________ UI _________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________

ui <- navbarPage(
  windowTitle = "JACOB – Observatoire",
  title = div(
    img(src = "logo_jacob_clean.png", height = "50px", style = "max-height:100px;"),
    style = "display:flex; justify-content:center; align-items:center; width:100%;"
  ),
  
  # garde ceci tel quel
  header = shinyjs::useShinyjs(),
  
  # ton autre tags$style “Griser…”
  tags$style(HTML("
    #modgest .option-disabled label { opacity: 0.1; }
    #modgest .option-disabled { pointer-events: none; }
  ")),
  
  # ⬇️ CSS global (responsive + z-index des modales)
  tags$head(
    tags$style(HTML("
      /* Responsive: panneau de recherche */
      @media (max-width: 768px) {
        #search_panel{
          top: 12px !important;
          right: 12px !important;
          max-width: clamp(220px, 60vw, 420px) !important;
        }
      }

      /* S'assure que les modales Shiny passent au-dessus des panneaux flottants */
      .modal { z-index: 4000 !important; }
      .modal-backdrop { z-index: 3990 !important; }
    "))
  ),

  
  
  
  # Onglet 1 : Introduction = WMS (3–11) puis Polygones (>= 12)
  tabPanel("Introduction",
           fluidRow(
             column(
               width = 3,
               div(   # j'ai enfermé 'mode de gestion' et sous-ctgorie' dans une div 
                 id = "intro-filters",
                 style = "margin-left:120px;",   # ← décale légèrement vers la droite
                 # état du zoom
                 htmlOutput("zoom_hint_intro"),
                 checkboxGroupInput("modgest", "Mode de gestion",
                                    choices = c(
                                      "Jardin à classer" = "JARDIN À CLASSER",
                                      "Jardin familial"  = "JARDIN FAMILIAL",
                                      "Jardin partagé"   = "JARDIN PARTAGÉ"
                                    ),
                                    selected = c("JARDIN FAMILIAL","JARDIN PARTAGÉ","JARDIN À CLASSER")
                 ),
                 conditionalPanel(
                   condition = "input.modgest.includes('JARDIN PARTAGÉ')",
                   fluidRow(column(
                     width = 10, offset = 1,
                     checkboxGroupInput("sub_filter_classes_intro", "sous-catégories",
                                        choices = list(
                                          "partagé"       = "JARDIN PARTAGÉ",
                                          "pédagogique"   = "JARDIN PÉDAGOGIQUE",
                                          "de rue"        = "JARDIN DE RUE",
                                          "d'insertion"   = "JARDIN D'INSERTION",
                                          "Ferme urbaine" = "FERME URBAINE"
                                        ),
                                        selected = c("FERME URBAINE","JARDIN D'INSERTION",
                                                     "JARDIN DE RUE","JARDIN PARTAGÉ",
                                                     "JARDIN PÉDAGOGIQUE")
                     )
                   ))
                 ),
                 h5("Affichage"),
                 checkboxInput("all_columns_1","Montrer toutes les variables", value = FALSE)
               )
             ),
             column(width=9, leafletOutput("map_intro", height = "65vh"),
                    #  Barre de recherche flottante
                    absolutePanel(
                      id = "search_panel",
                      top = 10, left = 200,               # ⇐ ancré à droite (plus de conflit avec la légende)
                      width = NULL,                       # on laisse le CSS décider
                      draggable = TRUE,
                      style = paste(
                        "z-index:2000;",                  # ⇐ au-dessus des contrôles Leaflet
                        "max-width:clamp(220px, 28vw, 360px);",  # ⇐ largeur responsive
                        "background:rgba(255,255,255,0.92);",
                        "padding:0.6rem 0.75rem;",        # ⇐ rem au lieu de px
                        "border-radius:10px;",
                        "box-shadow:0 2px 8px rgba(0,0,0,0.25);"
                      ),
                      textInput("search_jardin_id", label = NULL,
                                placeholder = "🔍 ID du jardin (ex : 12118)"),
                      actionButton(
                        "search_jardin_btn", "Aller au jardin", icon = icon("magnifying-glass"),
                        style = "width:100%; background-color:#e9f0eb; color:#555; border:none;"
                      )
                    )
             )
           ),
           dataTableOutput("table_intro")
  ),
  
  # Onglet 2 : Scraping
  tabPanel("Analyse Scraping",
           tabsetPanel(id = "scraping_tabs",
                       
                       # ---- Sous-onglet 1 : Points (ton UI actuel inchangé) ----
                       tabPanel("Cercles proportionnels",
                                fluidRow(
                                  column(width = 3,
                                         div(
                                           style = "display:flex; flex-direction:column; gap:2px; max-width:300px;",
                                           textInput("search_word", "Entrez un mot-clé (lemma) et appuyez sur Rechercher :", value = "", placeholder = "ex : rat"),
                                           actionButton("do_search", "Rechercher", icon = icon("search"),
                                                        style="width:120px; font-size:90%;")
                                         ),
                                         textOutput("nb_jardins_concernes"),
                                         br(),
                                         plotOutput("plot_occurrences", height = "350px", click = "plot_click")
                                  ),
                                  column(width = 9,
                                         leafletOutput("map_scraping", height = "65vh"),
                                         uiOutput("no_result_text")
                                  )
                                ),
                                dataTableOutput("table_scraping"),
                                uiOutput("text_zone")
                       ),
                       
                       # ---- Sous-onglet 2 : Régions (diffusion / intensité) ----
                       tabPanel("Régions (diffusion / intensité)",
                                sidebarLayout(
                                  sidebarPanel(
                                    textInput("kw_region", "Mot-clé", value = "moustique", placeholder = "ex. moustique"),
                                    radioButtons("metric_region", "Métrique",
                                                 choices = c("Diffusion (%)" = "diffusion_pct",
                                                             "Occurrences totales" = "total_occ_kw"),
                                                 selected = "diffusion_pct", inline = TRUE),
                                    
                                    # >>> nouveaux contrôles de discrétisation <<<
                                    selectInput(
                                      "region_bins", "Discrétisation",
                                      choices = c("Quantiles (auto)" = "quantile",
                                                  "Jenks (naturelles)" = "jenks",
                                                  "Égal-intervalle" = "equal",
                                                  "Continu (sans classes)" = "continuous"),
                                      selected = "quantile"
                                    ),
                                    numericInput("region_k", "Nombre de classes", value = 6, min = 3, max = 9, step = 1),
                                    
                                    actionButton("run_regions", "Analyser"),
                                    hr(),
                                    div(style="margin-top:8px;", uiOutput("phrase_region"))
                                  ),
                                  mainPanel(
                                    leafletOutput("map_region", height = "60vh")
                                  )
                                )
                       )
                       
           )
  ),
  
  
  
  # ONGLET n°3
  tabPanel("Analyse croisée",
           fluidRow(
             column(
               width = 3,
               h4("Analyse"),
               textInput("word_bytype", "Mot-clé (lemma) :", value = "", placeholder = "ex : moustique"),
               actionButton("run_bytype", "Analyser", icon = icon("chart-column"),
                            style = "width:140px;"),
               br(), br(),
               
               #  un seul sélecteur de mode global ----
               selectInput(
                 "analysis_mode", "Mode d’analyse",
                 choices = c(
                   "Par type de jardin"                   = "type",
                   "Régression logistique (présence)"                  = "corr",
                   "Par densité communale"                = "dens",
                   "Par type de sol"                      = "sol"
                 ),
                 selected = "type"
               )
               ,
               
               #  bloc 'Mode (par type)' 
               conditionalPanel(
                 condition = "input.analysis_mode == 'corr'",
                 
                 # Choix de X (surface, pauvreté, niveau de vie)
                 selectInput(
                   "corr_x", "Variable explicative (X) :",
                   choices = c(
                     "Surface (m²)"             = "surface",
                     "Taux de pauvreté (%)"     = "pauvrete",
                     "Niveau de vie médian (€)" = "niveauvie"
                   ),
                   selected = "surface"
                 ),
                 
                 # Options d’affichage
                 checkboxInput("corr_logx", "Échelle log sur X (log1p)", FALSE),
                 checkboxInput("corr_trim", "Exclure extrêmes (au-delà du 99e centile) sur X", TRUE)
               )
               ,
               
               # Bloc d’options spécifiques au mode densité
               conditionalPanel(
                 condition = "input.analysis_mode == 'dens'",
                 radioButtons(
                   "dens_metric", "Mode (par densité)",
                   choices  = c("Volume (somme d’occurrences)" = "sum",
                                "Part de jardins (≥1 occurrence)" = "rate"),
                   selected = "sum"
                 )
               ),
               br(),
               downloadButton("dl_bytype_csv", "Télécharger le tableau (CSV)")
             ),
             
             column(
               width = 9,
               plotOutput("plot_bytype_main", height = "440px"),
               uiOutput("bytype_note"),
               
               conditionalPanel(
                 condition = "input.analysis_mode == 'type' && input.show_median == true",
                 hr(),
                 plotOutput("plot_bytype_median", height = "360px")
               ),
               
               conditionalPanel(
                 condition = "input.analysis_mode == 'corr'",
                 hr(),
                 uiOutput("corr_box")
               ),
               
               hr(),
               dataTableOutput("table_bytype")
             )
           )
  )
)

#___________________________________________________________________________________ SERVER _________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________

server <- function(input, output, session) {
  
  ################################################# GESTION DU MDP DANS L'ONGLET 2 ###########################################################################
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
  
  ################################################## GESTION DU MDP DANS L'ONGLET 2 ###########################################################################
  ################################################## ONGLET 1 : INTRODUCTION ###########################################################################
  
  
  
  #  Classes sélectionnées 
  r_get_selected_classes <- reactive({
    sel <- input$modgest %||% character(0)
    # Si la grande catégorie "JARDIN PARTAGÉ" est cochée,
    # on remplace cette valeur par les sous-catégories choisies dans sub_filter_classes_intro.
    if ("JARDIN PARTAGÉ" %in% sel) {
      sel <- setdiff(sel, "JARDIN PARTAGÉ")
      sel <- c(sel, input$sub_filter_classes_intro %||% character(0))
    }
    # Sécurité : enlever vides/NA et dédoublonner
    sel <- unique(sel[!is.na(sel) & nzchar(sel)])
    sel
  })
  
  observe({
    subs <- input$sub_filter_classes_intro %||% character(0)
    has_subs <- length(subs) > 0
    
    if (!has_subs) {
      # Grise + rend inactif 'JARDIN PARTAGÉ' (sans le décocher)
      shinyjs::runjs("
      $('#modgest input[value=\"JARDIN PARTAGÉ\"]').closest('.checkbox').addClass('option-disabled');
    ")
    } else {
      # Rétablit l’aspect normal si au moins une sous-catégorie est cochée
      shinyjs::runjs("
      $('#modgest input[value=\"JARDIN PARTAGÉ\"]').closest('.checkbox').removeClass('option-disabled');
    ")
    }
  })
  
  # POLYGONES 
  filter_data <- reactive({
    bounds  <- input$map_intro_bounds
    zoom    <- input$map_intro_zoom %||% 1
    classes <- r_get_selected_classes()
    if (is.null(bounds) || length(classes) == 0 || zoom < 12) return(empty_sf_4326())
    
    show_all <- isTRUE(input$all_columns_1)  # ← tient compte du bouton
    con <- connect_to_jacob()
    env <- make_envelope_sql(bounds)
    classes_sql <- paste0("ARRAY[", paste(sprintf("'%s'", sql_escape(classes)), collapse=","), "]")
    
    # 👉 si coché : toutes les colonnes de jardin_infos (i.*)
    # sinon : sous-ensemble “léger”
    if (show_all) {
      select_infos <- "i.*"
      select_id    <- ""               # i.id déjà présent dans i.*
    } else {
      select_infos <- paste(
        "i.name, i.source_layer, i.surface_m2, i.classe_brute, i.classe_mot"
      )
      select_id    <- "i.id,"          # on expose id clairement
    }
    
    sql <- sprintf("
    SELECT
      %s
      %s,
      p.%s AS geom
    FROM %s p
    JOIN %s i ON i.id = p.id
    WHERE i.classe_mot = ANY(%s)
      AND ST_Intersects(p.%s, %s)
    LIMIT 50000;",
                   select_id, select_infos, GEOM_COL, T_POLY, T_INFOS, classes_sql, GEOM_COL, env
    )
    
    out <- sf::st_read(con, query = sql, quiet = TRUE)
    if (nrow(out) == 0) return(empty_sf_4326())
    if (is.na(sf::st_crs(out)) || sf::st_crs(out)$epsg != 4326) out <- sf::st_transform(out, 4326)
    out
  })
  
  
  
  
  #  Chargement CoSIA (% par jardin) 
  get_cosia_composition <- function(jardin_id) {
    con <- connect_to_jacob()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    sql <- sprintf("SELECT * FROM %s WHERE id = '%s';", T_COSIA, as.integer(jardin_id))
    df <- tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
    
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df_long <- df %>%
      tidyr::pivot_longer(
        cols = -id,
        names_to = "classe",
        values_to = "pourcentage"
      ) %>%
      dplyr::filter(pourcentage > 0) %>%
      arrange(desc(pourcentage))
    
    df_long
  }
  
  
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
      # 🌿 Fond clair (par défaut)
      addProviderTiles(
        "CartoDB.Positron",
        options = providerTileOptions(opacity = 0.4, zIndex = 100),
        group = "Fond clair"
      ) %>%
      
      # 🛰️ Fond satellite
      addProviderTiles(
        "Esri.WorldImagery",
        options = providerTileOptions(zIndex = 100),
        group = "Satellite (Esri)"
      ) %>%
      
      # le fond CoSIA IGN
      addTiles(
        urlTemplate = paste0(
          "https://data.geopf.fr/wmts?",
          "SERVICE=WMTS&VERSION=1.0.0&REQUEST=GetTile",
          "&LAYER=IGNF_COSIA_2021-2023",
          "&STYLE=normal&TILEMATRIXSET=PM",
          "&FORMAT=image/png&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
          "&apikey=essentiels"
        ),
        options = tileOptions(opacity = 0.85, minZoom = 6, maxZoom = 19, zIndex = 200),
        group = "CoSIA 2021–2023",
        attribution = "CoSIA © IGN"
      ) %>%
      
      # Points WMS GeoServer
      addWMSTiles(
        baseUrl = WMS_BASE,
        layers  = WMS_LAYER,   # nom sans 'jacob:'
        options = WMSTileOptions(
          version     = "1.1.1",
          format      = "image/png",
          transparent = TRUE,
          tiled       = TRUE,
          styles      = WMS_STYLE,# "point" mais la j'ai déjà créé un style sur QGIS SLD
          zIndex      = 400 
        ),
        group = "WMS points"
      ) %>%
      
      addLayersControl(
        baseGroups = c("Fond clair", "Satellite (Esri)"),
        overlayGroups = c("CoSIA 2021–2023","Polygones"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
      # ⚙️ État initial
      showGroup("CoSIA 2021–2023") %>%
      showGroup("Polygones") %>%   # pour qu'il soit coché dans le contrôle dès le départ
      setView(lng = 2.35, lat = 46.7, zoom = 5)
    
  })
  
  ##### DES REACTIVES ET AUTRES
  # État des couches cochées
  is_poly_on <- reactive({ "Polygones" %in% (input$map_intro_groups %||% character(0)) })
  is_cosia_on <- reactive({ "CoSIA 2021–2023" %in% (input$map_intro_groups %||% character(0)) })
  
  # --- états pour gérer la transition & le choix utilisateur
  last_regime <- reactiveVal("low")      # "low" (<3), "mid" (3..11), "high" (>=12)
  poly_override <- reactiveVal(FALSE)    # TRUE si l'utilisateur a (dé)coché à z>=12
  
  # on attend 150 ms sans nouveau changement avant de déclencher → un seul redraw quand le zoom se “pose”.
  zoom_d    <- debounce(reactive(input$map_intro_zoom),   150)  # 150 ms
  bounds_d  <- throttle(reactive(input$map_intro_bounds), 200)  # optionnel
  ##### DES REACTIVES ET AUTRES
  
  
  # ---- Recherche d’un jardin par ID ----
  observeEvent(input$search_jardin_btn, {
    req(input$search_jardin_id)
    
    # 🔹 Nettoyage de l’entrée
    jardin_id <- trimws(as.character(input$search_jardin_id))
    
    # 🔹 Récupère la géométrie du jardin dans la base
    con <- connect_to_jacob()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    sql <- sprintf('SELECT id, ST_X(ST_Centroid(geom)) AS lon, ST_Y(ST_Centroid(geom)) AS lat
                  FROM "Jacob_data".jardin_poly_4326
                  WHERE id = %s;', DBI::dbQuoteLiteral(con, jardin_id))
    
    coords <- tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
    if (is.null(coords) || nrow(coords) == 0) {
      showNotification("Aucun jardin trouvé avec cet ID.", type = "warning")
      return()
    }
    
    # 🔹 Centre la carte sur le jardin
    leafletProxy("map_intro") %>%
      flyTo(lng = coords$lon[1], lat = coords$lat[1], zoom = 17) %>%
      addPopups(
        lng = coords$lon[1],
        lat = coords$lat[1],
        popup = sprintf("<b>Jardin %s</b>", jardin_id),
        options = popupOptions(closeButton = TRUE)
      )
  })
  
  
  observeEvent(input$map_intro_groups, {
    z <- input$map_intro_zoom %||% 1
    # Si l'utilisateur touche aux groupes pendant qu'on est en "high", on gèle l'automatique
    if (z >= 12) {
      poly_override(TRUE)
    }
  }, ignoreInit = TRUE)
  
  
  
  #  Toggle WMS (zoom 3–11) vs Polygones (zoom >= 12)
 observe({
  z <- zoom_d() %||% 1
    proxy_map <- leafletProxy("map_intro")
    
    # régime courant
    regime <- if (z >= 12) "high" else if (z >= 3) "mid" else "low"
    
    # si on change de régime, on réinitialise l'override quand on QUITTE "high"
    if (regime != last_regime()) {
      if (last_regime() == "high" && regime != "high") {
        poly_override(FALSE)  # reset : la prochaine montée pourra ré-auto-cocher
      }
      last_regime(regime)
    }
    
    # helpers existants
    add_cosia_legend <- function(proxy) {
      proxy %>% addLegend(
        position = "bottomleft",
        colors   = cosia_cols,
        labels   = cosia_classes,
        title    = "CoSIA (2021–2023)",
        opacity  = 1
      )
    }
    
    # UI : message zoom + (dé)activation filtres
    output$zoom_hint_intro <- renderUI({
      if (z >= 12)
        HTML("<div style='color:#02b808;margin-bottom:6px;'>🔓 Zoom ≥ 12 : filtres activés.</div>")
      else
        HTML("<div style='color:#9e9e9e;margin-bottom:6px;'>🔒 Zoomez pour utiliser les filtres. (≥ 12) </div>")
    })
    if (z >= 12) { shinyjs::enable("modgest"); shinyjs::enable("sub_filter_classes_intro") }
    else         { shinyjs::disable("modgest"); shinyjs::disable("sub_filter_classes_intro") }
    
    proxy_map %>% clearControls()
    
    # --- régimes mid et low : WMS visible, Polygones cachés ---
    if (regime == "mid") {
      proxy_map %>% showGroup("WMS points") %>% hideGroup("Polygones")
      # légende WMS
      legend_url <- paste0(
        WMS_BASE,
        "?service=WMS&request=GetLegendGraphic&format=image/png&layer=",
        URLencode(WMS_LAYER, reserved = TRUE)
      )
      proxy_map %>% addControl(
        html = sprintf('<div style="background:white;padding:6px;border-radius:6px">
                        <b>Types de jardins</b><br><img src="%s" style="max-width:180px">
                      </div>', legend_url),
        position = "bottomright"
      )
      if (is_cosia_on()) add_cosia_legend(proxy_map)
      return(invisible())
    }
    
    if (regime == "low") {
      proxy_map %>% hideGroup("WMS points") %>% hideGroup("Polygones")
      if (is_cosia_on()) add_cosia_legend(proxy_map)
      return(invisible())
    }
    
    # --- régime high (z >= 12) : transition + respect du choix utilisateur ---
    # 1) si l'utilisateur n'a PAS encore agi (override=FALSE) et que Polygones est décoché,
    #    on coche automatiquement pour éviter le trou noir lorsque le WMS se masque.
    if (!("Polygones" %in% (input$map_intro_groups %||% character(0))) && !poly_override()) {
      proxy_map %>% showGroup("Polygones")
    }
    
    # 2) on masque le WMS par défaut à ce zoom (mais cf. option ci-dessous)
    proxy_map %>% hideGroup("WMS points")
    
    # 3) dessiner (ou non) les polygones selon l'état actuel de la case
    if ("Polygones" %in% (input$map_intro_groups %||% character(0))) {
      proxy_map %>% clearGroup("Polygones")
      
      data_poly <- filter_data()
      if (nrow(data_poly) > 0) {
        pal <- pal_poly()
        proxy_map %>% addPolygons(
          data = data_poly,
          fillColor = ~pal(classe_mot),
          color = "black", weight = 1, opacity = 1, fillOpacity = 0.5,
          label = lapply(seq_len(nrow(data_poly)), function(i) {
            row <- data_poly[i, ]
            nom_txt   <- if (is.null(row$name) || is.na(row$name) || row$name == "") sprintf("Jardin %s", row$id) else htmltools::htmlEscape(as.character(row$name))
            classe_txt<- if (!is.null(row$classe_mot) && !is.na(row$classe_mot)) as.character(row$classe_mot) else "Non défini"
            surf_txt  <- if (!is.null(row$surface_m2) && !is.na(row$surface_m2)) sprintf("%.1f m²", as.numeric(row$surface_m2)) else "non renseignée"
            htmltools::HTML(sprintf("<div style='font-size:13px; line-height:1.3;'><b>%s</b><br><span style='color:#333;'>Type : %s</span><br><span style='color:#333;'>Surface : %s</span></div>", nom_txt, classe_txt, surf_txt))
          }),
          labelOptions = labelOptions(
            direction = "auto", sticky = TRUE, textsize = "13px",
            style = list("color" = "#111", "background" = "rgba(255,255,255,0.9)",
                         "border-radius" = "6px", "padding" = "5px 7px",
                         "box-shadow" = "0 1px 3px rgba(0,0,0,0.25)")
          ),
          highlightOptions = highlightOptions(
            color = "#d4af37", weight = 3, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8
          ),
          group   = "Polygones",
          layerId = ~id,
          options = pathOptions(zIndex = 600)
        )
        
        # légende polygones
        classes_presentes <- sort(unique(data_poly$classe_mot))
        cols <- pal(classes_presentes)
        tab  <- table(data_poly$classe_mot)
        labels <- vapply(classes_presentes, function(cl) {
          base <- if (cl %in% names(legend_labels)) legend_labels[[cl]] else cl
          sprintf("%s (%d)", base, as.integer(tab[[cl]] %||% 0))
        }, character(1))
        proxy_map %>% addLegend(
          position = "bottomright",
          colors   = cols,
          labels   = labels,
          title    = "Types de jardins",
          opacity  = 1
        )
      } else {
        # rien à dessiner -> on n'affiche pas de légende polygones
      }
    
    } else {
      # À z >= 12, l'utilisateur a décoché "Polygones" -> on ne montre rien (hors couches que tu gardes).
      # On cache les points WMS et on vide le groupe polygones. MAIS (voir la suite)
      proxy_map %>% hideGroup("WMS points") %>% clearGroup("Polygones")  #proxy_map %>% showGroup("WMS points") --> permet de ré-affiche le WMS si c'est mieux 
    }
    
    
    if (is_cosia_on()) add_cosia_legend(proxy_map)
  })
  
  
  # -- Double clic sur un polygone : afficher la composition CoSIA diagramme secteur
  observeEvent(input$map_intro_shape_click, {
    click <- input$map_intro_shape_click
    if (is.null(click$id)) return()
    jardin_id <- as.character(click$id)
    df <- get_cosia_composition(jardin_id)
    if (is.null(df)) {
      showNotification(sprintf("Pas de données CoSIA pour le jardin %s.", jardin_id), type = "warning")
      return()
    }
    # === Harmonisation des noms entre ta table et ta palette ===
    normalize_label <- function(x) {
      x <- iconv(x, to = "ASCII//TRANSLIT")        # enlève les accents
      x <- gsub("_", " ", x)                       # enlève les underscores
      x <- trimws(x)
      x
    }
    # Palette sans accents pour comparaison
    cosia_map_simplified <- setNames(
      cosia_cols,
      normalize_label(names(cosia_palette))
    )
    # Simplifie les noms de classe dans ton df pour les faire correspondre
    df$classe_simpl <- normalize_label(df$classe)
    # Attribution des couleurs en se basant sur les noms simplifiés
    df$color <- cosia_map_simplified[df$classe_simpl]
    df$color[is.na(df$color)] <- "#cccccc"  # couleur par défaut si pas trouvée
    # Graphique circulaire Plotly
    p <- plotly::plot_ly(
      data = df,
      labels = ~classe,
      values = ~pourcentage,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(colors = df$color, line = list(color = "#FFFFFF", width = 1))
    ) %>%
      plotly::layout(
        title = sprintf("Composition CoSIA – Jardin %s", jardin_id),
        showlegend = TRUE
      )
    showModal(modalDialog(
      title = sprintf("Composition du jardin à l'ID n°%s", jardin_id),
      plotly::renderPlotly(p),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  
  # -- Table intro (vue table attributaire)
  output$table_intro <- renderDataTable({
    data <- filter_data()
    if (nrow(data) == 0) return(data.frame())
    
    df <- data %>% sf::st_drop_geometry()
    
    if (!isTRUE(input$all_columns_1)) {
      # Cas habituel : peu de colonnes -> on garde ton format_table
      df <- dplyr::select(df, any_of(c(
        "id","name","source_layer","surface_m2","classe_brute","classe_mot"
      )))
      return(format_table(df))
    } else {
      # Cas "tout afficher" : on active juste le scroll horizontal, rien d'autre
      return(DT::datatable(
        df,
        escape    = FALSE,
        selection = "single",
        rownames  = FALSE,
        class     = "stripe hover compact",
        options   = list(
          scrollX     = TRUE,   # ⬅️ évite que ça déborde
          autoWidth   = TRUE,
          pageLength  = 10,
          deferRender = TRUE
        ),
        width = "100%"
      ))
    }
  })
  
  
  ###################################################################################### ONGLET N °2 SCRAPING : ################################################################ 
  
  ################################################## ONGLET 1 INTRODUCTION ###########################################################################
  
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
    
    # 🔤 Titre wrap (ajuste la largeur si besoin : 42–55)
    title_txt <- stringr::str_wrap(
      paste0("Les 20 jardins où le mot « ", word_used, " » apparaît le plus souvent"),
      width = 48
    )
    
    ggplot(top20, aes(x = reorder(id, occurrences), y = occurrences)) +
      geom_col(fill = "lightblue") +
      coord_flip(clip = "off") +  # ← évite le rognage
      labs(title = title_txt, x = "ID du jardin", y = "Nombre d'occurrences") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0, lineheight = 1.1, margin = margin(b = 8)),
        plot.margin = margin(t = 14, r = 18, b = 8, l = 8)
      )
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
  
                                                              #################### SOUS ONGLET DE L'ONGLET N°2 ####################
  
  # ---------- Carte choroplèthe par région (sans ST_Intersects) ----------
  observeEvent(input$run_regions, {
    req(input$kw_region, input$metric_region)
    
    kw_norm <- tolower(trimws(input$kw_region))
    kw_sql  <- gsub("'", "''", kw_norm)   # échappe les apostrophes pour SQL
    met     <- input$metric_region        # "diffusion_pct" | "total_occ_kw"
    
    con <- connect_to_jacob()
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    
    # On utilise jardin_infos.INSEE_REG pour rattacher chaque jardin à une région,
    # et la table regions (2154) qui contient geom + NOM + INSEE_REG + jardin_r (pré-calcul du nb de jardins).
    sql_regions <- sprintf("
    WITH occ AS (
      SELECT s.garden_id::text AS garden_id,
             SUM(s.n)::int     AS occ_kw
      FROM %s s
      WHERE lower(trim(s.lemma)) = '%s'
      GROUP BY s.garden_id
    ),
    per_garden AS (
      SELECT i.\"INSEE_REG\"       AS insee_reg,
             i.id::text            AS garden_id,
             COALESCE(o.occ_kw, 0) AS occ_kw
      FROM %s i
      LEFT JOIN occ o ON o.garden_id = i.id::text
      WHERE i.\"INSEE_REG\" IS NOT NULL
    ),
    agg AS (
      SELECT insee_reg,
             COUNT(*)                                           AS n_gardens_calc,
             COUNT(*) FILTER (WHERE occ_kw > 0)                 AS n_gardens_with_kw,
             COALESCE(SUM(occ_kw),0)                            AS total_occ_kw
      FROM per_garden
      GROUP BY insee_reg
    )
    SELECT
      r.\"INSEE_REG\"                                   AS insee_reg,
      r.\"NOM\"                                         AS nom_reg,
      ST_Transform(r.geom, 4326)                        AS geom,
      COALESCE(r.jardin_r, a.n_gardens_calc, 0)::int    AS n_gardens,     -- priorité au pré-calcul
      COALESCE(a.n_gardens_with_kw, 0)::int             AS n_gardens_with_kw,
      COALESCE(a.total_occ_kw, 0)::int                  AS total_occ_kw,
      CASE
        WHEN COALESCE(r.jardin_r, a.n_gardens_calc, 0) > 0
        THEN ROUND(100.0 * COALESCE(a.n_gardens_with_kw,0) / NULLIF(COALESCE(r.jardin_r, a.n_gardens_calc, 0),0)::numeric, 1)
        ELSE NULL
      END                                               AS diffusion_pct
    FROM %s r
    LEFT JOIN agg a ON a.insee_reg = r.\"INSEE_REG\";
  ", T_SPEC, kw_sql, T_INFOS, T_REG)
    
    # Lire directement en sf (car on sélectionne un champ geom)
    sf_reg <- sf::st_read(con, query = sql_regions, quiet = TRUE)
    validate(need(nrow(sf_reg) > 0, "Aucune région trouvée."))
    
    
    # ---- métrique ----
    sf_reg$metric_value <- if (met == "diffusion_pct") sf_reg$diffusion_pct else sf_reg$total_occ_kw
    vals    <- sf_reg$metric_value
    vals_ok <- vals[is.finite(vals) & !is.na(vals)]
    
    # ---- fonctions d'aide pour les classes ----
    make_breaks <- function(x, method = "quantile", k = 6) {
      x <- x[is.finite(x)]
      if (!length(x)) return(c(0, 1))
      if (length(unique(x)) < 3) return(unique(sort(c(0, x, max(x, na.rm = TRUE)))))
      if (method == "quantile") {
        brks <- classInt::classIntervals(x, n = k, style = "quantile")$brks
      } else if (method == "jenks") {
        brks <- classInt::classIntervals(x, n = k, style = "jenks")$brks
      } else if (method == "equal") {
        brks <- classInt::classIntervals(x, n = k, style = "equal")$brks
      } else {
        brks <- pretty(range(x, na.rm = TRUE), n = k)
      }
      brks <- unique(brks)
      if (length(brks) < 2) brks <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
      brks
    }
    
    # ---- palette + légende dynamiques ----
    bin_method <- input$region_bins %||% "quantile"
    k          <- if (!is.null(input$region_k)) input$region_k else 6
    
    if (met == "diffusion_pct") {
      # Diffusion : par défaut quantiles
      if (bin_method == "continuous") {
        pal <- leaflet::colorNumeric("YlOrRd", domain = vals_ok, na.color = "#e0e0e0")
        color_fun <- function(v) pal(v)
        add_leg <- function(map) leaflet::addLegend(
          map, pal = pal, values = vals, title = "Diffusion (%)",
          opacity = 0.9, labFormat = leaflet::labelFormat(suffix = " %", digits = 1)
        )
      } else {
        brks <- make_breaks(vals_ok, method = bin_method, k = k)
        pal  <- leaflet::colorBin("YlOrRd", domain = vals, bins = brks, na.color = "#e0e0e0", right = FALSE)
        color_fun <- function(v) pal(v)
        add_leg <- function(map) leaflet::addLegend(map, pal = pal, values = vals, title = "Diffusion (%)", opacity = 0.9)
      }
    } else {
      # Occurrences : auto-log si distribution très déséquilibrée
      med_pos <- suppressWarnings(stats::median(vals_ok[vals_ok > 0], na.rm = TRUE))
      skew    <- (max(vals_ok, na.rm = TRUE) + 1) / (ifelse(is.finite(med_pos) && med_pos > 0, med_pos, 1) + 1)
      use_log <- is.finite(skew) && skew > 100
      
      if (bin_method == "continuous") {
        if (use_log) {
          pal <- leaflet::colorNumeric("YlOrRd", domain = log1p(vals_ok), na.color = "#e0e0e0")
          color_fun <- function(v) pal(log1p(v))
          add_leg <- function(map) leaflet::addLegend(
            map, pal = pal, values = log1p(vals),
            title = "Occurrences (échelle log)", opacity = 0.9
          )
        } else {
          pal <- leaflet::colorNumeric("YlOrRd", domain = vals_ok, na.color = "#e0e0e0")
          color_fun <- function(v) pal(v)
          add_leg <- function(map) leaflet::addLegend(map, pal = pal, values = vals, title = "Occurrences totales", opacity = 0.9)
        }
      } else {
        x <- if (use_log) log1p(vals_ok) else vals_ok
        brks <- make_breaks(x, method = bin_method, k = k)
        pal  <- leaflet::colorBin("YlOrRd", domain = if (use_log) log1p(vals) else vals,
                                  bins = brks, na.color = "#e0e0e0", right = FALSE)
        color_fun <- function(v) if (use_log) pal(log1p(v)) else pal(v)
        leg_title <- if (use_log) "Occurrences (échelle log)" else "Occurrences totales"
        add_leg <- function(map) leaflet::addLegend(
          map, pal = pal, values = if (use_log) log1p(vals) else vals, title = leg_title, opacity = 0.9
        )
      }
    }
    
    # ---- rendu leaflet ----
    output$map_region <- leaflet::renderLeaflet({
      leaflet::leaflet(sf_reg) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          weight = 0.9, color = "#666",
          fillOpacity = 0.82,
          fillColor = ~color_fun(metric_value),
          label = ~lapply({
            fmt <- if (met == "diffusion_pct")
              function(x) paste0(formatC(x, digits = 1, format = "f"), " %")
            else
              function(x) formatC(x, big.mark = " ", format = "d")
            paste0(
              "<b>", htmltools::htmlEscape(nom_reg), "</b><br/>",
              if (met == "diffusion_pct") "Diffusion : <b>" else "Occurrences : <b>",
              fmt(metric_value), "</b><br/>",
              "Jardins : ", formatC(n_gardens, big.mark = " "),
              " &nbsp;|&nbsp; Avec mot : ", formatC(n_gardens_with_kw, big.mark = " ")
            )
          }, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        ) |>
        add_leg() |>
        leaflet::setView(lng = 2.5, lat = 46.5, zoom = 5)
    })
    
    # ---- phrase synthèse ----
    try({
      ord <- order(sf_reg$metric_value, decreasing = TRUE, na.last = NA)
      if (length(ord) >= 1) {
        best <- sf_reg[ord[1], ]
        txt_met <- if (met == "diffusion_pct") "la diffusion" else "les occurrences"
        output$phrase_region <- renderUI(htmltools::HTML(
          paste0("➡️ Le mot « <b>", htmltools::htmlEscape(input$kw_region),
                 "</b> » est davantage présent dans la région <b>",
                 htmltools::htmlEscape(best$nom_reg), "</b> (selon ", txt_met, ").")
        ))
      } else {
        output$phrase_region <- renderUI(htmltools::HTML("ℹ️ Aucune région avec une valeur calculable pour ce mot."))
      }
    }, silent = TRUE)
  })
  
  
  #   ###################################################################################### ONGLET 3 : Analyse croisée ################################################################ 
  # Pré-remplir le mot-clé depuis l’onglet 2 si dispo
  observeEvent(input$search_word, {
    if (nzchar(input$search_word) && !nzchar(input$word_bytype)) {
      updateTextInput(session, "word_bytype", value = input$search_word)
    }
  }, ignoreInit = TRUE)
  
  # ---- Requête principale + agrégations ----
  r_bytype <- eventReactive(input$run_bytype, {
    w <- trimws(input$word_bytype %||% "")
    if (!nzchar(w)) return(NULL)
    
    con <- connect_to_jacob()
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    
    # Occurrences par jardin pour le lemme
    sql_occ <- sprintf("
    WITH occ AS (
      SELECT (s.garden_id)::text AS id, SUM(s.n)::bigint AS occ
      FROM %s s
      WHERE LOWER(TRIM(s.lemma)) = LOWER(TRIM(?word))
      GROUP BY (s.garden_id)::text
    ),
    infos AS (
      SELECT 
        (i.id)::text            AS id,
        i.classe_mot,
        i.surface_m2,
        (i.\"INSEE_COM\")::text AS insee_com,
        (c.\"CODGEO\")::text    AS codgeo,
        c.\"LIBDENS7\"          AS libdens7,
        c.\"DENS7\"             AS dens7,
        c.\"TP6021\"     AS taux_pauvrete,
        c.\"Q221\" AS niveau_vie_median,
        CASE
          WHEN i.classe_mot IN ('JARDIN PÉDAGOGIQUE','JARDIN DE RUE',
                                'JARDIN D''INSERTION','FERME URBAINE',
                                'JARDIN PARTAGÉ')
            THEN 'JARDIN PARTAGÉ'
          WHEN i.classe_mot = 'JARDIN FAMILIAL'
            THEN 'JARDIN FAMILIAL'
          WHEN i.classe_mot = 'JARDIN À CLASSER'
            THEN 'JARDIN À CLASSER'
          ELSE COALESCE(i.classe_mot, 'À classer')
        END AS grand_type,
        CASE
          WHEN i.classe_mot IN ('JARDIN PÉDAGOGIQUE','JARDIN DE RUE',
                                'JARDIN D''INSERTION','FERME URBAINE')
            THEN i.classe_mot
          WHEN i.classe_mot = 'JARDIN PARTAGÉ'
            THEN 'JARDIN PARTAGÉ (générique)'
          WHEN i.classe_mot IS NULL
            THEN 'À classer'
          ELSE i.classe_mot
        END AS sous_type
      FROM %s i
      LEFT JOIN %s c
        ON (c.\"CODGEO\")::text = (i.\"INSEE_COM\")::text
    )
    SELECT
      inf.id,
      inf.grand_type,
      inf.sous_type,
      inf.surface_m2,
      inf.libdens7,
      inf.dens7,
      inf.taux_pauvrete,
      inf.niveau_vie_median,
      COALESCE(o.occ, 0) AS occ
    FROM infos inf
    LEFT JOIN occ o ON o.id = inf.id
  ", T_SPEC, T_INFOS, T_COM)
    
    df <- DBI::dbGetQuery(con, DBI::sqlInterpolate(con, sql_occ, word = w))
    if (is.null(df) || !nrow(df)) return(NULL)
    
    df <- df %>%
      dplyr::mutate(
        occ               = as.numeric(occ),
        surface_m2        = suppressWarnings(as.numeric(surface_m2)),
        taux_pauvrete     = suppressWarnings(as.numeric(taux_pauvrete)),
        niveau_vie_median = suppressWarnings(as.numeric(niveau_vie_median))
      )
    
    ## ---- Agrégations par type ----
    agg_grand <- df %>%
      dplyr::group_by(grand_type) %>%
      dplyr::summarise(
        occ_total          = sum(occ, na.rm = TRUE),
        nb_jardins_total   = dplyr::n(),
        nb_jardins_mention = sum(occ > 0, na.rm = TRUE),
        taux_jardins       = ifelse(nb_jardins_total > 0,
                                    nb_jardins_mention / nb_jardins_total, NA_real_),
        occ_median         = stats::median(occ, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(label_aff = grand_type)
    
    agg_sous <- df %>%
      dplyr::group_by(sous_type) %>%
      dplyr::summarise(
        occ_total          = sum(occ, na.rm = TRUE),
        nb_jardins_total   = dplyr::n(),
        nb_jardins_mention = sum(occ > 0, na.rm = TRUE),
        taux_jardins       = ifelse(nb_jardins_total > 0,
                                    nb_jardins_mention / nb_jardins_total, NA_real_),
        occ_median         = stats::median(occ, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(label_aff = sous_type)
    
    ## ---- Agrégations par densité communale ----
    agg_dens <- df %>%
      dplyr::filter(!is.na(libdens7) & libdens7 != "") %>%
      dplyr::group_by(libdens7) %>%
      dplyr::summarise(
        occ_total          = sum(occ, na.rm = TRUE),
        nb_jardins_total   = dplyr::n(),
        nb_jardins_mention = sum(occ > 0, na.rm = TRUE),
        taux_jardins       = ifelse(nb_jardins_total > 0,
                                    nb_jardins_mention / nb_jardins_total, NA_real_),
        occ_median         = stats::median(occ, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(label_aff = libdens7)
    
    ## ---- Agrégations par type de sol (CoSIA) pour les jardins "positifs" ----
    ids_pos <- df %>% dplyr::filter(occ > 0) %>% dplyr::pull(id)
    agg_sol <- NULL
    if (length(ids_pos)) {
      sql_cosia <- sprintf("SELECT * FROM %s WHERE id IN (%s);",
                           T_COSIA, paste(DBI::dbQuoteString(con, ids_pos), collapse = ","))
      cosia_w <- tryCatch(DBI::dbGetQuery(con, sql_cosia), error = function(e) NULL)
      
      if (!is.null(cosia_w) && nrow(cosia_w) > 0) {
        num_cols <- cosia_w %>% dplyr::select(-id) %>% dplyr::select(where(is.numeric)) %>% names()
        if (length(num_cols) > 0) {
          df_sol <- tidyr::pivot_longer(
            cosia_w, cols = dplyr::all_of(num_cols), names_to = "sol_class", values_to = "value"
          ) %>% dplyr::filter(!is.na(value) & value >= 0) %>%
            dplyr::mutate(sol_class = gsub("_", " ", sol_class, fixed = TRUE))
          total_j <- length(unique(df_sol$id))
          agg_sol <- df_sol %>%
            dplyr::group_by(sol_class) %>%
            dplyr::summarise(
              pct_total          = sum(value, na.rm = TRUE),
              nb_jardins_mention = sum(value > 0, na.rm = TRUE),
              .groups = "drop"
            )
          total_all <- sum(agg_sol$pct_total, na.rm = TRUE)
          if (total_all > 0 && total_j > 0) {
            agg_sol <- agg_sol %>%
              dplyr::mutate(
                pct_share    = pct_total / total_all,
                part_jardins = nb_jardins_mention / total_j,
                label_aff    = sol_class
              ) %>%
              dplyr::arrange(dplyr::desc(pct_share))
          } else agg_sol <- NULL
        }
      }
    }
    
    list(
      grand  = agg_grand,
      sous   = agg_sous,
      dens   = agg_dens,
      sols   = agg_sol,
      points = df
    )
  }, ignoreInit = TRUE)
  
  
  .make_logistic_data <- function(df, x_col, trim99 = TRUE) {
    # garde X et occ valides
    df <- df %>%
      dplyr::filter(is.finite(occ), !is.na(.data[[x_col]]), is.finite(.data[[x_col]]))
    
    # trimming 99e centile sur X uniquement (Y est binaire)
    if (trim99 && nrow(df) > 10) {
      qx <- stats::quantile(df[[x_col]], probs = 0.99, na.rm = TRUE)
      df <- dplyr::filter(df, .data[[x_col]] <= qx)
    }
    
    # variable binaire de présence
    df$pres <- as.integer(df$occ >= 1)
    df
  }
  
  .fit_logistic <- function(pts, x_col) {
    if (nrow(pts) < 10 || length(unique(pts$pres)) < 2) return(NULL)
    # formule : pres ~ X
    stats::glm(stats::as.formula(paste0("pres ~ `", x_col, "`")), data = pts, family = stats::binomial())
  }
  
  
  
  # ---- Plot principal (régression logistique) ----
  output$plot_bytype_main <- renderPlot({
    dat  <- r_bytype(); if (is.null(dat)) return(NULL)
    mode <- input$analysis_mode %||% "type"   # "type" | "dens" | "corr" | "sol"
    w    <- trimws(input$word_bytype %||% "mot")
    
    ## === 1) régression logistique : X = occ, Y = surface/pauvreté/niveau de vie ===
    if (mode == "corr") {
      pts <- dat$points  # on prend TOUS les jardins (présence/absence)
      
      # X (variable) et labels
      x_col <- switch(input$corr_x,
                      "surface"   = "surface_m2",
                      "pauvrete"  = "taux_pauvrete",
                      "niveauvie" = "niveau_vie_median",
                      "surface_m2")
      x_lab <- switch(input$corr_x,
                      "surface"   = "Surface du jardin (m²)",
                      "pauvrete"  = "Taux de pauvreté (%)",
                      "niveauvie" = "Niveau de vie médian (€)",
                      "Surface du jardin (m²)")
      w <- trimws(input$word_bytype %||% "mot")
      
      # Préparer données (binariser Y, éventuel trimming X)
      pts <- .make_logistic_data(pts, x_col, trim99 = isTRUE(input$corr_trim))
      
      # Fit logistique
      mod <- .fit_logistic(pts, x_col)
      if (is.null(mod)) {
        return(
          ggplot() + theme_void() +
            ggtitle("Régression logistique impossible : échantillon insuffisant ou Y constant (0/1).")
        )
      }
      
      # grille de prédiction pour la courbe en S
      xmin <- min(pts[[x_col]], na.rm = TRUE)
      xmax <- max(pts[[x_col]], na.rm = TRUE)
      newdata <- data.frame(tmp_x = seq(xmin, xmax, length.out = 200))
      names(newdata) <- x_col
      newdata$pred <- stats::predict(mod, newdata = newdata, type = "response")
      
      # Plot : points (0/1) + sigmoïde
      p <- ggplot(pts, aes(x = .data[[x_col]], y = pres)) +
        geom_jitter(height = 0.06, alpha = 0.35, size = 1.5) +
        geom_line(data = newdata, aes(y = pred), linewidth = 1.2) +
        labs(
          title = sprintf("Régression logistique — présence du mot « %s »", w),
          x = x_lab,
          y = "Présence du mot"
        ) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        theme_minimal(base_size = 13)
      
      if (isTRUE(input$corr_logx)) p <- p + scale_x_continuous(trans = "log1p")
      
      
      return(p)
    }
    
    
    
    
    ## === 2) Par type / Par densité ===
    if (mode %in% c("type", "dens")) {
      d <- if (mode == "type") {
        if (identical(input$agg_level, "grand")) dat$grand else dat$sous
      } else {
        dat$dens
      }
      if (is.null(d) || !nrow(d)) return(NULL)
      
      metric <- if (mode == "type") {
        if ((input$analysis_metric %||% "sum") == "rate") "taux_jardins" else "occ_total"
      } else { # dens
        if ((input$dens_metric %||% "sum") == "rate") "taux_jardins" else "occ_total"
      }
      
      title <- if (mode == "dens") {
        if (metric == "taux_jardins")
          sprintf("Part des jardins mentionnant « %s » (par densité de commune)", w)
        else
          sprintf("Occurrences totales de « %s » (par densité de commune)", w)
      } else {
        if (metric == "taux_jardins")
          sprintf("Part des jardins mentionnant « %s » (par type de jardin)", w)
        else
          sprintf("Occurrences totales de « %s » (par type de jardin)", w)
      }
      
      d <- d %>% dplyr::arrange(dplyr::desc(.data[[metric]]))
      
      return(
        ggplot(d, aes(x = reorder(label_aff, .data[[metric]]), y = .data[[metric]])) +
          geom_col() +
          coord_flip() +
          labs(
            title = title,
            x = if (mode == "dens") "Densité de commune" else "Type de jardin",
            y = if (metric == "taux_jardins") "Proportion de jardins" else "Occurrences (somme)"
          ) +
          theme_minimal(base_size = 13)
      )
    }
    
    ## === 3) Par type de sol (CoSIA) ===
    if (mode == "sol") {
      d <- dat$sols
      if (is.null(d) || !nrow(d)) return(NULL)
      d <- d %>% dplyr::arrange(dplyr::desc(pct_share))
      return(
        ggplot(d, aes(x = reorder(label_aff, pct_share), y = pct_share)) +
          geom_col() +
          coord_flip() +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          labs(
            title = sprintf("Poids relatif des types de sol (jardins mentionnant « %s »)", w),
            x = "Type de sol (CoSIA)",
            y = "Part relative des sols (en %)"
          ) +
          theme_minimal(base_size = 13)
      )
    }
  })
  
  # ---- Box de régression logistique ----
  output$corr_box <- renderUI({
    req(input$analysis_mode == "corr")
    dat <- r_bytype(); req(dat)
    
    pts <- dat$points
    
    # X choisi
    x_col <- switch(input$corr_x,
                    "surface"   = "surface_m2",
                    "pauvrete"  = "taux_pauvrete",
                    "niveauvie" = "niveau_vie_median",
                    "surface_m2")
    x_lab_h <- switch(input$corr_x,
                      "surface"   = "la surface du jardin",
                      "pauvrete"  = "le taux de pauvreté",
                      "niveauvie" = "le niveau de vie médian",
                      "la variable sélectionnée")
    mot <- htmltools::htmlEscape(trimws(input$word_bytype %||% ""))
    
    # données prêtes pour la logistique
    pts <- .make_logistic_data(pts, x_col, trim99 = isTRUE(input$corr_trim))
    mod <- .fit_logistic(pts, x_col)
    if (is.null(mod)) return(NULL)
    
    sm <- summary(mod)
    # Coeff du prédicteur (2e ligne)
    beta1 <- sm$coefficients[2, "Estimate"]
    p_wald <- sm$coefficients[2, "Pr(>|z|)"]
    OR <- exp(beta1)
    
    # Test global (rapport de vraisemblance)
    an <- anova(mod, test = "Chisq")
    p_lr <- tryCatch({
      p <- tail(an[["Pr(>Chi)"]], 1)
      if (length(p) == 0 || is.na(p)) NA_real_ else p
    }, error = function(e) NA_real_)
    
    # formatages
    fmt_p <- function(p) if (is.na(p)) "NA" else if (p < 0.001) "< 0,001" else
      formatC(p, format = "f", digits = 3, decimal.mark = ",")
    p_txt   <- fmt_p(p_wald)
    p_lr_txt<- fmt_p(p_lr)
    
    sens <- if (is.na(beta1)) "—" else if (beta1 > 0) "positive" else if (beta1 < 0) "négative" else "nulle"
    n <- nrow(pts)
    prop_pos <- mean(pts$pres, na.rm = TRUE)
    prop_txt <- paste0(round(prop_pos * 100, 1), " % de jardins mentionnent « ", mot, " ».")
    
    HTML(sprintf(
      "<div style='background:#f7f7f7;border-radius:8px;padding:10px;margin-top:6px;'>
       <div><b>Méthode :</b> Régression logistique (binomiale)</div>
       <div><b>n</b> = %d &nbsp; | &nbsp; %s</div>
       <div><b>Effet de %s :</b> OR = %.3f (β = %.3f) — p (Wald) = %s</div>
       <div><b>Test global (LR) :</b> p = %s</div>
       <div style='margin-top:6px;color:#444'>
         Interprétation : relation <b>%s</b> entre %s et la probabilité de mention du mot « <b>%s</b> ».
       </div>
     </div>",
      n, prop_txt, x_lab_h, OR, beta1, p_txt, p_lr_txt, sens, x_lab_h, mot
    ))
  })
  
  # ---- Note explicative sous le graphe ----
  output$bytype_note <- renderUI({
    dat <- r_bytype(); if (is.null(dat)) return(NULL)
    mode <- input$analysis_mode %||% "type"
    
    txt <- switch(
      mode,
      "corr" = "Chaque point = un jardin (0/1 : présence du mot). La courbe montre la probabilité estimée par une régression logistique.",
      "type" = if ((input$analysis_metric %||% "sum") == "rate")
        "Proportion de jardins d’un type où le mot apparaît au moins une fois (≥1)."
      else
        "Somme des occurrences du mot agrégée par type de jardin.",
      "dens" = if ((input$dens_metric %||% "sum") == "rate")
        "Proportion de jardins (par densité communale) où le mot apparaît (≥1)."
      else
        "Somme des occurrences agrégée par densité communale.",
      "sol"  = "Lecture des parts relatives de types de sol sur les jardins « positifs » (occ > 0)."
    )
    
    HTML(sprintf("<div style='margin-top:6px;color:#555'>%s</div>", txt))
  })
  
  
  # ---- Médianes (pour type & dens) ----
  output$plot_bytype_median <- renderPlot({
    req(input$analysis_mode %in% c("type","dens"), input$show_median)
    dat <- r_bytype(); if (is.null(dat)) return(NULL)
    
    d <- if (input$analysis_mode == "type") {
      if (identical(input$agg_level, "grand")) dat$grand else dat$sous
    } else {
      dat$dens
    }
    if (is.null(d) || !nrow(d)) return(NULL)
    
    d <- d %>% dplyr::arrange(dplyr::desc(occ_median))
    
    ggplot(d, aes(x = reorder(label_aff, occ_median), y = occ_median)) +
      geom_col() +
      coord_flip() +
      labs(
        title = if (input$analysis_mode == "dens")
          "Occurrences médianes par jardin (par densité de commune)"
        else
          "Occurrences médianes par jardin (par type)",
        x = if (input$analysis_mode == "dens") "Densité de commune" else "Type de jardin",
        y = "Médiane d'occurrences"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # ---- Tableau + téléchargement ----
  output$table_bytype <- renderDataTable({
    dat  <- r_bytype(); if (is.null(dat)) return(data.frame())
    mode <- input$analysis_mode %||% "type"
    
    if (mode == "corr") {
      pts <- dat$points %>%
        dplyr::select(id, grand_type, sous_type, libdens7,
                      surface_m2, taux_pauvrete, niveau_vie_median, occ)
      DT::datatable(pts, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
      
    } else if (mode == "sol") {
      d <- dat$sols
      if (is.null(d) || !nrow(d)) return(data.frame())
      out <- d %>%
        dplyr::transmute(
          `Type de sol (CoSIA)`        = label_aff,
          `Somme des %`                = round(pct_total, 2),
          `Part relative des sols (%)` = round(pct_share * 100, 1),
          `Jardins avec ce sol`        = nb_jardins_mention,
          `Part des jardins (%)`       = round(part_jardins * 100, 1)
        )
      DT::datatable(out, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
      
    } else {
      d <- if (mode == "type") {
        if (identical(input$agg_level, "grand")) dat$grand else dat$sous
      } else {
        dat$dens
      }
      out <- d %>%
        dplyr::transmute(
          Libellé                       = label_aff,
          `Occurrences (somme)`         = occ_total,
          `Jardins avec ≥1 occurrence`  = nb_jardins_mention,
          `Jardins (total)`             = nb_jardins_total,
          `Proportion`                  = round(taux_jardins * 100, 1),
          `Médiane / jardin`            = occ_median
        )
      DT::datatable(out, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    }
  })
  
  output$dl_bytype_csv <- downloadHandler(
    filename = function() {
      base <- paste0(
        "jacob_bytype_",
        gsub("\\s+", "_", tolower(trimws(input$word_bytype %||% "mot")))
      )
      mode <- input$analysis_mode %||% "type"
      if (mode == "corr") {
        paste0(base, "_corr_points.csv")
      } else if (mode == "dens") {
        paste0(base, "_dens_", (input$analysis_metric %||% "sum"), ".csv")
      } else if (mode == "sol") {
        paste0(base, "_sol_parts.csv")
      } else {
        paste0(base, "_type_", (input$analysis_metric %||% "sum"), "_", input$agg_level, ".csv")
      }
    },
    content = function(file) {
      dat <- r_bytype(); if (is.null(dat)) { write.csv(data.frame(), file); return() }
      mode <- input$analysis_mode %||% "type"
      if (mode == "corr") {
        pts <- dat$points %>%
          dplyr::select(id, grand_type, sous_type, libdens7,
                        surface_m2, taux_pauvrete, niveau_vie_median, occ)
        write.csv(pts, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else if (mode == "dens") {
        write.csv(dat$dens, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else if (mode == "sol") {
        write.csv(dat$sols, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        d <- if (identical(input$agg_level, "grand")) dat$grand else dat$sous
        write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
}

#________________________________________________________________________________________________________________________________________________________________
#___________________________________________________________________________________ CONNECTION _________________________________________________________________________

shinyApp(ui = ui, server = server)
