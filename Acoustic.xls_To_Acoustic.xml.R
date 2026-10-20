# app.R - Version avec couleur de barre de progression personnalisée (orange)

# ---------------------------------------------------------------
# 0) Configuration initiale
# ---------------------------------------------------------------
options(shiny.maxRequestSize = 500 * 1024^2)
options(shiny.reactlog = TRUE)
# ---------------------------------------------------------------
# 1) Chargement des librairies
# ---------------------------------------------------------------
library(shiny)
library(shinyjs)
library(bslib)
library(future)
library(promises)
library(furrr)
library(readxl)
library(xml2)
library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)
library(fontawesome)
plan(multisession, workers = availableCores() - 1)
# ---------------------------------------------------------------
# 2) Interface Utilisateur Modernisée
# ---------------------------------------------------------------
ui <- fluidPage(
  title = "Acoustic_XML_Editor",
  theme = bslib::bs_theme(
    version = 5,
    bg = "#f8f9fa",
    fg = "#212529",
    primary = "#0094CD",
    secondary = "#345675",
    success = "#20c997",
    base_font = font_google("Roboto"),
    heading_font = font_google("Roboto Condensed")
  ),
  # -------- STYLE PERSONNALISÉ DE LA BARRE DE PROGRESSION --------
  tags$head(
    tags$style(HTML("
      #header-progress-bar {
        background-color: orange !important;
      }
      #header-progress-bar.bg-success {
        background-color: #ff6600 !important;
      }
    "))
  ),
  useShinyjs(),
  # Header fixe avec barre de progression
  tags$header(
    class = "sticky-top bg-primary text-white p-3 shadow-sm",
    div(
      class = "container-fluid",
      div(
        class = "d-flex justify-content-between align-items-center",
        h1(
          class = "mb-0 fs-4 fw-bold",
          span(fa("file-code", fill = "white"), " Acoustic_XML_Editor")
        ),
        div(id = "progress-header", style = "width: 40%;")
      )
    )
  ),
  # Contenu principal
  div(
    class = "container-fluid mt-5 pt-4",
    fluidRow(
      # Sidebar - Partie configuration (inchangée)
      column(
        width = 4,
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-header bg-primary text-white",
            h4(class = "mb-0", "Configuration")
          ),
          div(
            class = "card-body",
            fileInput(
              inputId = "excel_file",
              label = tagList(fa("file-excel"), "Import Acoustic_LPA"),
              accept = c(".xlsx"),
              buttonLabel = "Browse…",
              placeholder = "Sélectionnez un fichier .xlsx",
              width = "100%"
            ),
            hr(),
            div(
              class = "d-grid gap-3",
              actionButton(
                inputId = "reconstruct_btn",
                label = tagList(fa("cogs"), "Build XML.file"),
                class = "btn-success btn-lg"
              ),
              downloadButton(
                outputId = "download_xml",
                label = tagList(fa("download"), "Export Acoustic.XML"),
                class = "btn-primary btn-lg"
              )
            )
          )
        )
      ),
      # Main Panel
      column(
        width = 8,
        # Journal d'activité (inchangé)
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-header bg-primary text-white",
            h4(class = "mb-0", "Journal d'activité")
          ),
          div(
            class = "card-body",
            verbatimTextOutput("status_text") %>%
              tagAppendAttributes(
                class = "bg-light p-3 rounded pre-scrollable",
                style = "min-height: 200px; max-height: 400px;"
              )
          )
        ),
        # --------- Aperçu des données déplacé ici ---------
        div(
          class = "card shadow-sm",
          div(
            class = "card-header bg-primary text-white",
            h4(class = "mb-0", "Aperçu des données")
          ),
          div(
            class = "card-body p-0",
            navset_pill(
              nav_panel(
                title = tagList(fa("info-circle"), "Métadonnées"),
                div(class = "table-responsive", tableOutput("preview_root_metadata"))
              ),
              nav_panel(
                title = tagList(fa("ruler-horizontal"), "Distances"),
                div(class = "table-responsive", tableOutput("preview_distances"))
              ),
              nav_panel(
                title = tagList(fa("wave-square"), "Fréquences"),
                div(class = "table-responsive", tableOutput("preview_frequencies"))
              ),
              nav_panel(
                title = tagList(fa("layer-group"), "Canaux"),
                div(class = "table-responsive", tableOutput("preview_ch_types"))
              ),
              nav_panel(
                title = tagList(fa("chart-bar"), "SA par ACOCAT"),
                div(class = "table-responsive", tableOutput("preview_sa_by_acocat"))
              ),
              nav_panel(
                title = tagList(fa("table"), "SA"),
                div(class = "table-responsive", tableOutput("preview_sa"))
              )
            )
          )
        )
        # ------ Fin du déplacement de l'aperçu ------
      )
    )
  ),
  # Footer (intentionnellement vide)
  tags$footer(class = "mt-5 py-3 bg-light text-center"),
  # JavaScript personnalisé
  tags$script(HTML("
// Initialisation de la barre de progression
$(document).ready(function() {
$('#progress-header').html(`
<div class=\"progress\" style=\"height: 25px;\">
<div id=\"header-progress-bar\"
class=\"progress-bar progress-bar-striped progress-bar-animated\"
role=\"progressbar\"
style=\"width: 0%\">
<span id=\"progress-text\">Prêt</span>
</div>
</div>
`);
});
// Gestionnaire de messages pour la progression dans la barre en haut
Shiny.addCustomMessageHandler('update_progress', function(data) {
const bar = $('#header-progress-bar');
const text = $('#progress-text');
bar.css('width', data.value + '%');
bar.attr('aria-valuenow', data.value);
text.text(data.detail || data.value + '%');
if (data.value >= 100) {
bar.removeClass('progress-bar-animated progress-bar-striped');
bar.addClass('bg-success');
text.text('Terminé !');
}
});
"))
)
# ---------------------------------------------------------------
# 3) Serveur Optimisé (sans modal)
# ---------------------------------------------------------------
server <- function(input, output, session) {
  # Variables réactives
  xml_path <- reactiveVal(NULL)
  perf_data <- reactiveVal(NULL)
  excel_data <- reactiveVal(NULL)
  preview_data_list <- reactiveVal(NULL) # stocke les 5 premières lignes de chaque feuille
  # ---------------------------------------------------------------
  # 3.1. Aperçu des données Excel
  # ---------------------------------------------------------------
  observeEvent(input$excel_file, {
    req(input$excel_file)
    # Affiche immédiatement un message dans le journal
    output$status_text <- renderText(" Chargement du fichier Excel…")
    session$sendCustomMessage("update_progress", list(value = 0, detail = "Chargement Excel"))
    tryCatch({
      required_sheets <- c(
        "root_metadata", "distances", "frequencies",
        "ch_types", "sa_by_acocat", "sa"
      )
      # Lecture des noms de feuilles
      sheet_names <- excel_sheets(input$excel_file$datapath)
      if (!all(required_sheets %in% sheet_names)) {
        stop(
          "Le fichier Excel doit contenir toutes les feuilles suivantes : ",
          paste(required_sheets, collapse = ", ")
        )
      }
      # Lecture des aperçus (5 premières lignes) pour chaque feuille
      previews <- map(
        setNames(required_sheets, required_sheets),
        ~ read_excel(input$excel_file$datapath, sheet = .x, n_max = 5)
      )
      preview_data_list(previews)
      # Stockage des données complètes pour traitement ultérieur
      full_data <- map(
        setNames(required_sheets, required_sheets),
        ~ read_excel(input$excel_file$datapath, sheet = .x)
      )
      excel_data(full_data)
      output$status_text <- renderText({
        paste0(
          " Fichier Excel chargé avec succès.\n",
          "Feuilles détectées : ", paste(sheet_names, collapse = ", "), "\n",
          "Aperçu disponible.\n",
          "Prêt pour la reconstruction."
        )
      })
      session$sendCustomMessage("update_progress", list(value = 20, detail = "Excel OK"))
    }, error = function(e) {
      preview_data_list(NULL)
      output$status_text <- renderText({
        paste0(" Erreur pendant l'import Excel :\n", e$message)
      })
      session$sendCustomMessage("update_progress", list(value = 0, detail = "Erreur Excel"))
    })
  })
  # ---------------------------------------------------------------
  # 3.1.b Rendu statique des aperçus
  # ---------------------------------------------------------------
  output$preview_root_metadata <- renderTable({
    df <- preview_data_list()[["root_metadata"]]
    req(df)
    df
  }, striped = TRUE)
  output$preview_distances <- renderTable({
    df <- preview_data_list()[["distances"]]
    req(df)
    df
  }, striped = TRUE)
  output$preview_frequencies <- renderTable({
    df <- preview_data_list()[["frequencies"]]
    req(df)
    df
  }, striped = TRUE)
  output$preview_ch_types <- renderTable({
    df <- preview_data_list()[["ch_types"]]
    req(df)
    df
  }, striped = TRUE)
  output$preview_sa_by_acocat <- renderTable({
    df <- preview_data_list()[["sa_by_acocat"]]
    req(df)
    df
  }, striped = TRUE)
  output$preview_sa <- renderTable({
    df <- preview_data_list()[["sa"]]
    req(df)
    df
  }, striped = TRUE)
  # ---------------------------------------------------------------
  # 3.2. Reconstruction du XML (sans modal, juste progression en haut)
  # ---------------------------------------------------------------
  observeEvent(input$reconstruct_btn, {
    req(excel_data())
    # **Ajout unique : capture de start_time**
    start_time <- Sys.time()
    # Début de la reconstruction
    output$status_text <- renderText(" Début de la reconstruction XML…")
    session$sendCustomMessage("update_progress", list(value = 20, detail = "Initialisation"))
    # Création du cache local
    cache_env <- new.env()
    # On enchaîne directement, sans modal
    tryCatch({
      # ─── Phase 1 : Préparation des données (20%) ───
      timing <- list()
      timing$debut_preparation <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 30, detail = "Préparation"))
      # Fonction de préparation optimisée (transtype en caractères et remplace NA)
      prepare_data <- function(df) {
        df %>%
          mutate(across(everything(), as.character)) %>%
          mutate(across(where(~ any(is.na(.x))), ~ ifelse(is.na(.x), "", .x)))
      }
      # Préparation des données Excel
      data_prep <- map(excel_data(), prepare_data)
      # Classement et ordonnancement
      distances_ord <- data_prep$distances %>% arrange(distance_id)
      frequencies_ord <- data_prep$frequencies %>% arrange(distance_id, freq)
      ch_types_ord <- data_prep$ch_types %>% arrange(distance_id, freq, type)
      sa_by_acocat_ord <- data_prep$sa_by_acocat %>% arrange(distance_id, freq, type, acocat)
      sa_ord <- data_prep$sa %>% arrange(distance_id, freq, type, acocat, ch)
      # Mise en cache de données fréquemment répétées
      cache_env$sa_by_acocat_ord <- sa_by_acocat_ord
      cache_env$sa_ord <- sa_ord
      timing$fin_preparation <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 40, detail = "Données prêtes"))
      # ─── Phase 2 : Construction du XML (60%) ───
      timing$debut_xml <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 50, detail = "Construction XML"))
      # Création du document XML racine
      doc <- xml_new_root("echosounder_dataset")
      # Ajout des métadonnées racines
      root_meta <- data_prep$root_metadata[1, ]
      root_values <- list(
        report_time = root_meta$report_time,
        lsss_version = root_meta$lsss_version,
        nation = root_meta$nation,
        platform = root_meta$platform,
        cruise = root_meta$cruise
      )
      walk(names(root_values), ~ xml_add_child(doc, .x, root_values[[.x]]))
      # Fonction interne pour ajouter un noeud <distance> complet
      add_distance_node <- function(did) {
        dist_data <- distances_ord %>% filter(distance_id == did)
        ligneD <- dist_data[1, ]
        node_dist <- xml_add_child(doc, "distance")
        xml_set_attr(node_dist, "log_start", ligneD$distance_id)
        xml_set_attr(node_dist, "start_time", ligneD$start_time)
        # Sous-éléments fixes de <distance>
        dist_children <- list(
          stop_time = ligneD$stop_time,
          integrator_dist = ligneD$integrator_dist,
          pel_ch_thickness = ligneD$pel_ch_thickness,
          bot_ch_thickness = ligneD$bot_ch_thickness,
          include_estimate = ligneD$include_estimate,
          lat_start = ligneD$lat_start,
          lat_stop = ligneD$lat_stop,
          lon_start = ligneD$lon_start,
          lon_stop = ligneD$lon_stop
        )
        walk(names(dist_children), ~ xml_add_child(node_dist, .x, dist_children[[.x]]))
        # Pour chaque fréquence associée à cette distance
        freq_data <- frequencies_ord %>% filter(distance_id == did)
        walk(unique(freq_data$freq), function(frq) {
          freq_group <- freq_data %>% filter(freq == frq)
          ligneF <- freq_group[1, ]
          node_freq <- xml_add_child(node_dist, "frequency")
          xml_set_attr(node_freq, "freq", ligneF$freq)
          xml_set_attr(node_freq, "transceiver", ligneF$transceiver)
          # Sous-éléments fixes de <frequency>
          freq_children <- list(
            threshold = ligneF$threshold,
            num_pel_ch = ligneF$num_pel_ch,
            num_bot_ch = ligneF$num_bot_ch,
            min_bot_depth = ligneF$min_bot_depth,
            max_bot_depth = ligneF$max_bot_depth,
            upper_interpret_depth = ligneF$upper_interpret_depth,
            lower_interpret_depth = ligneF$lower_interpret_depth,
            upper_integrator_depth= ligneF$upper_integrator_depth,
            lower_integrator_depth= ligneF$lower_integrator_depth,
            quality = ligneF$quality,
            bubble_corr = ligneF$bubble_corr
          )
          walk(names(freq_children), ~ xml_add_child(node_freq, .x, freq_children[[.x]]))
          # Pour chaque type de canal (ch_type) sous cette fréquence
          cht_data <- ch_types_ord %>% filter(distance_id == did, freq == frq)
          walk(unique(cht_data$type), function(typ) {
            node_cht <- xml_add_child(node_freq, "ch_type")
            xml_set_attr(node_cht, "type", typ)
            # Sous-éléments <sa_by_acocat>
            sacat_data <- cache_env$sa_by_acocat_ord %>%
              filter(distance_id == did, freq == frq, type == typ)
            walk(unique(sacat_data$acocat), function(ac) {
              node_sacat <- xml_add_child(node_cht, "sa_by_acocat")
              xml_set_attr(node_sacat, "acocat", ac)
              # Enfin, ajouter chaque <sa> pour ce acocat
              sa_data <- cache_env$sa_ord %>%
                filter(distance_id == did, freq == frq, type == typ, acocat == ac)
              walk(seq_len(nrow(sa_data)), function(i) {
                row_i <- sa_data[i, ]
                node_sa <- xml_add_child(node_sacat, "sa", row_i$value)
                xml_set_attr(node_sa, "ch", row_i$ch)
              })
            })
          })
        })
      }
      # Exécution de add_distance_node sur chaque ID de distance
      distance_ids <- unique(distances_ord$distance_id)
      total_dist <- length(distance_ids)
      walk2(distance_ids, seq_along(distance_ids), function(did, i) {
        add_distance_node(did)
        # Mise à jour périodique de la barre
        progress <- 50 + 40 * (i / total_dist)
        session$sendCustomMessage(
          "update_progress",
          list(
            value = round(progress),
            detail = paste0(i, "/", total_dist, " dist")
          )
        )
      })
      timing$fin_xml <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 90, detail = "Construction OK"))
      # ─── Phase 3 : Écriture du fichier (20%) ───
      timing$debut_ecriture <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 95, detail = "Écriture XML"))
      nom_fichier <- paste0("Aoustic_LPA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xml")
      tmpfile <- file.path(tempdir(), nom_fichier)
      write_xml(doc, file = tmpfile, encoding = "UTF-8")
      xml_path(tmpfile)
      timing$fin_ecriture <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 100, detail = "Terminé !"))
      # Calcul des performances
      perf <- list(
        lecture = 0,
        preparation = as.numeric(difftime(timing$fin_preparation, timing$debut_preparation, units = "secs")),
        xml = as.numeric(difftime(timing$fin_xml, timing$debut_xml, units = "secs")),
        ecriture = as.numeric(difftime(timing$fin_ecriture, timing$debut_ecriture, units = "secs")),
        total = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )
      perf_data(perf)
      # Message de fin
      output$status_text <- renderText({
        paste0(
          " Reconstruction terminée avec succès !\n",
          "Temps total : ", round(perf$total, 2), " secondes\n",
          "- Préparation des données : ", round(perf$preparation, 2), " s\n",
          "- Construction XML : ", round(perf$xml, 2), " s\n",
          "- Écriture du fichier : ", round(perf$ecriture, 2), " s\n\n",
          "Fichier généré : ", xml_path()
        )
      })
    }, error = function(e) {
      output$status_text <- renderText({
        paste0(" Erreur lors de la reconstruction :\n", e$message)
      })
      session$sendCustomMessage("update_progress", list(value = 0, detail = "Erreur"))
    })
  })
  # ---------------------------------------------------------------
  # 3.3. Téléchargement du XML
  # ---------------------------------------------------------------
  output$download_xml <- downloadHandler(
    filename = function() {
      if (!is.null(xml_path())) {
        basename(xml_path())
      } else {
        paste0("Acoustic_LPA", Sys.Date(), ".xml")
      }
    },
    content = function(file) {
      req(xml_path())
      file.copy(xml_path(), file, overwrite = TRUE)
    },
    contentType = "application/xml"
  )
}
# ---------------------------------------------------------------
# 4) Lancement de l'application
# ---------------------------------------------------------------
shinyApp(ui = ui, server = server)
