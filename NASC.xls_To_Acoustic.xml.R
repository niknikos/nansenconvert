# app.R
# 0) Configuration initiale
# ---------------------------------------------------------------
options(shiny.maxRequestSize = 500 * 1024^2)  # Limite à 500 MB
options(shiny.reactlog = TRUE)                # Pour le débogage
# ---------------------------------------------------------------
# 0) Chargement des librairies
# ---------------------------------------------------------------
library(shiny)
library(bslib)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(DT)

# ---------------------------------------------------------------
# 0.b) Contrôle qualité NASC + construction Acoustic_LPA
# ---------------------------------------------------------------
nasc_rows_bad_coords <- function(df) {
  cols <- c("Lat_S", "Lon_S", "Lat_E", "Lon_E")
  n <- nrow(df)
  if (n == 0) return(logical(0))
  if (!all(cols %in% names(df))) return(rep(FALSE, n))
  mat <- sapply(cols, function(cn) {
    suppressWarnings(as.numeric(df[[cn]])) %in% c(999, -999)
  })
  if (is.null(dim(mat))) mat <- matrix(mat, nrow = n, ncol = length(cols))
  rowSums(mat, na.rm = TRUE) > 0
}

nasc_rows_sa_zero <- function(df) {
  n <- nrow(df)
  if (n == 0) return(logical(0))
  if (!"Sa" %in% names(df)) return(rep(FALSE, n))
  v <- suppressWarnings(as.numeric(df$Sa))
  !is.na(v) & v == 0
}

nasc_rows_qc_any <- function(df) {
  nasc_rows_bad_coords(df) | nasc_rows_sa_zero(df)
}

build_lpa_from_nasc_staged <- function(df_nasc, md) {
  df_nasc <- df_nasc %>%
    mutate(
      Date_S = case_when(
        grepl("^[0-9]{8}$", as.character(Date_S)) ~ as.Date(as.character(Date_S), format = "%Y%m%d"),
        TRUE ~ as.Date(Date_S)
      ),
      Date_E = case_when(
        grepl("^[0-9]{8}$", as.character(Date_E)) ~ as.Date(as.character(Date_E), format = "%Y%m%d"),
        TRUE ~ as.Date(Date_E)
      ),
      Time_S = as.POSIXct(Time_S, format = "%H:%M:%S", tz = "UTC"),
      Time_E = as.POSIXct(Time_E, format = "%H:%M:%S", tz = "UTC")
    ) %>%
    arrange(Date_S, Time_S)

  if ("Interval" %in% names(df_nasc) && nrow(df_nasc) > 0 && !all(is.na(df_nasc$Interval))) {
    distance_id_vec <- numeric(nrow(df_nasc))
    distance_id_counter <- 1
    distance_id_vec[1] <- distance_id_counter
    for (i in 2:nrow(df_nasc)) {
      if (df_nasc$Interval[i] == df_nasc$Interval[i - 1]) {
        distance_id_vec[i] <- distance_id_counter
      } else {
        distance_id_counter <- distance_id_counter + 1
        distance_id_vec[i] <- distance_id_counter
      }
    }
    df_nasc$distance_id <- distance_id_vec
  } else {
    df_nasc$distance_id <- NA
  }

  df_nasc <- df_nasc %>%
    group_by(Date_S) %>%
    mutate(
      temps1 = as.POSIXct(paste(Date_S, "06:00:00"), tz = "UTC") + (row_number() - 1) * 35,
      temps2 = temps1 + 35,
      start_time = format(temps1, "%Y-%m-%d %H:%M:%S"),
      stop_time  = format(temps2, "%Y-%m-%d %H:%M:%S")
    ) %>%
    ungroup()

  df_nasc <- df_nasc %>%
    mutate(
      integrator_dist     = 0.1,
      pel_ch_thickness    = 10,
      bot_ch_thickness    = NA,
      include_estimate    = 1
    )

  df_nasc <- df_nasc %>%
    rename(
      lat_start              = Lat_S,
      lon_start              = Lon_S,
      lat_stop               = Lat_E,
      lon_stop               = Lon_E,
      max_bot_depth          = Exclude_below_line_range_max,
      min_bot_depth          = Exclude_below_line_depth_min,
      upper_integrator_depth = Exclude_above_line_depth_mean,
      value                  = Sa
    )

  df_nasc <- df_nasc %>%
    mutate(
      lower_interpret_depth   = Exclude_below_line_range_mean,
      lower_integrator_depth  = 500,
      upper_interpret_depth   = 10,
      quality                 = 1,
      bubble_corr             = 1,
      type                    = "P",
      freq                    = 38000,
      transceiver             = 1,
      threshold               = -70,
      num_pel_ch              = 2,
      num_bot_ch              = 0,
      ch                      = Layer
    )

  df_nasc <- df_nasc %>%
    mutate(
      acocat = case_when(
        Species_name == "Sardina pilchardus"         ~ 1,
        Species_name == "Engraulis encrasicolus"    ~ 2,
        Species_name == "Scomber colias"            ~ 3,
        Species_name == "Trachurus trachurus"       ~ 4,
        Species_name == "Trachurus trecae"          ~ 5,
        Species_name == "Trachurus picturatus"      ~ 6,
        Species_name == "Trachurus mediterraneus"   ~ 7,
        Species_name == "Sardinella aurita"         ~ 8,
        Species_name == "Sardinella maderensis"     ~ 9,
        Species_name == "Macroramphosus scolopax"   ~ 10,
        Species_name == "Macroramphosus gracilis"   ~ 11,
        Species_name == "Scomber scombrus"          ~ 12,
        Species_name == "Caranx rhonchus"           ~ 13,
        Species_name == "Plancton"                  ~ 14,
        Species_name == "Mesopelagique"             ~ 15,
        Species_name == "Autres"                    ~ 16,
        TRUE                                         ~ 16
      )
    )

  df_root_metadata <- data.frame(
    report_time  = md$report_time,
    lsss_version = md$lsss_version,
    nation       = md$nation,
    platform     = md$platform,
    cruise       = md$cruise,
    stringsAsFactors = FALSE
  )

  df_distances <- df_nasc %>%
    select(
      distance_id, start_time, stop_time,
      integrator_dist, pel_ch_thickness, bot_ch_thickness, include_estimate,
      lat_start, lat_stop, lon_start, lon_stop
    ) %>%
    distinct(distance_id, .keep_all = TRUE)

  df_frequencies <- df_nasc %>%
    select(
      distance_id, freq, transceiver, threshold,
      num_pel_ch, num_bot_ch,
      min_bot_depth, max_bot_depth,
      upper_interpret_depth, lower_interpret_depth,
      upper_integrator_depth, lower_integrator_depth,
      quality, bubble_corr
    ) %>%
    distinct()

  df_ch_types <- df_nasc %>%
    select(distance_id, freq, type) %>%
    distinct()

  df_sa_by_acocat <- df_nasc %>%
    select(distance_id, freq, type, acocat) %>%
    distinct()

  df_sa <- df_nasc %>%
    select(distance_id, freq, type, acocat, ch, value) %>%
    filter(value > 0)

  list(
    nasc_raw       = df_nasc,
    nasc_processed = df_nasc,
    lpa_sheets     = list(
      root_metadata = df_root_metadata,
      distances     = df_distances,
      frequencies   = df_frequencies,
      ch_types      = df_ch_types,
      sa_by_acocat  = df_sa_by_acocat,
      sa            = df_sa
    )
  )
}

# ---------------------------------------------------------------
# 1) Définition du thème et de l'UI
# ---------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    fg = "#333333",
    bg = "#f8f9fa"
  ),
  
  titlePanel("Convertisseur NASC → Acoustic_LPA"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # 1. Téléversement du fichier Source NASC (CSV ou XLSX)
      fileInput(
        inputId = "file_nasc",
        label = "Import Acoustic_LPA file (CSV ou XLSX)",
        accept = c(".csv", ".xlsx"),
        buttonLabel = "Browse…"
      ),
      tags$hr(),
      
      # 2. Saisie manuelle des métadonnées (root_metadata)
      h4("Métadonnées à renseigner"),
      textInput("md_report_time",   "report_time",      value = ""),
      textInput("md_lsss_version",  "lsss_version",     value = ""),
      textInput("md_nation",        "nation",           value = ""),
      textInput("md_platform",      "platform",         value = ""),
      textInput("md_cruise",        "cruise",           value = ""),
      tags$hr(),
      
      # 3. Boutons d'action
      actionButton("btn_process",       "🔄 Traiter le fichier",              class = "btn-primary"),
      tags$br(), tags$br(),
      downloadButton("dl_acoustic_lpa", "💾 Télécharger Acoustic_LPA.xlsx",   class = "btn-success")
    ),
    
    mainPanel(
      width = 9,
      uiOutput("ui_row_qc"),
      # Aperçu automatique des 6 feuilles Acoustic_LPA (affiché après traitement)
      uiOutput("ui_preview_lpa")
    )
  )
)

# ---------------------------------------------------------------
# 2) Définition du serveur
# ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # Réactifs pour stocker les données traitées
  rv <- reactiveValues(
    nasc_raw        = NULL,
    nasc_processed  = NULL,
    lpa_sheets      = NULL,
    nasc_staged     = NULL,
    n_bad_coord     = 0L,
    n_sa_zero       = 0L,
    qc_last_msg     = NULL
  )

  nasc_md <- function(input) {
    list(
      report_time  = input$md_report_time,
      lsss_version = input$md_lsss_version,
      nation       = input$md_nation,
      platform     = input$md_platform,
      cruise       = input$md_cruise
    )
  }

  apply_nasc_staged <- function(df_staged) {
    res <- build_lpa_from_nasc_staged(df_staged, nasc_md(input))
    rv$nasc_raw       <- res$nasc_raw
    rv$nasc_processed <- res$nasc_processed
    rv$lpa_sheets     <- res$lpa_sheets
  }
  
  # Flags pour afficher les aperçus quand on clique sur les boutons
  show_nasc_preview <- reactiveVal(FALSE)
  show_lpa_preview  <- reactiveVal(FALSE)
  
  # -------------------------------------------------------------
  # 2.1) Traitement des données NASC quand on clique sur "btn_process"
  # -------------------------------------------------------------
  observeEvent(input$btn_process, {
    req(input$file_nasc)
    
    # 1) Lecture du fichier NASC (CSV ou XLSX)
    ext <- tools::file_ext(input$file_nasc$datapath)
    if (ext == "csv") {
      df_nasc <- read.csv(
        input$file_nasc$datapath,
        stringsAsFactors = FALSE,
        na.strings = c("", "NA")
      )
    } else if (ext %in% c("xlsx", "xls")) {
      df_nasc <- read_excel(input$file_nasc$datapath)
      df_nasc <- as.data.frame(df_nasc, stringsAsFactors = FALSE)
    } else {
      showModal(modalDialog(
        title = "Erreur",
        "Format de fichier non supporté. Veuillez importer un CSV ou un XLSX.",
        easyClose = TRUE, footer = NULL
      ))
      return(NULL)
    }
    
    # ——— 2) Nettoyer systématiquement les noms de colonnes ———
    clean_names <- colnames(df_nasc) %>%
      trimws() %>%              # retirer les espaces en début/fin
      gsub("\\s+", " ", .)      # fusionner multiples espaces internes en un seul
    colnames(df_nasc) <- clean_names
    
    # —— 2.a) Species_percent : détection (casse / espaces → underscore) ——
    sp_key <- tolower(gsub("\\s+", "_", colnames(df_nasc)))
    sp_idx <- which(sp_key == "species_percent")
    if (length(sp_idx) == 1) {
      colnames(df_nasc)[sp_idx] <- "Species_percent"
    }
    
    # ——— 3) Définir la liste des colonnes requises ———
    required_cols <- c(
      "Date_S", "Time_S", "Date_E", "Time_E",
      "PRC_NASC",
      "Lat_S", "Lon_S", "Lat_E", "Lon_E",
      "Exclude_below_line_range_max",
      "Exclude_below_line_depth_min",
      "Exclude_above_line_depth_mean",
      "Species_percent", "Species_name","Exclude_below_line_range_mean","Layer","Interval"
    )
    
    # ——— 4) Vérifier leur présence ———
    missing_cols <- setdiff(required_cols, colnames(df_nasc))
    if (length(missing_cols) > 0) {
      showModal(modalDialog(
        title = "Colonnes manquantes",
        paste0(
          "Les colonnes suivantes sont absentes dans le fichier NASC :\n",
          paste(missing_cols, collapse = ", ")
        ),
        easyClose = TRUE, footer = NULL
      ))
      return(NULL)
    }
    
    # 4.b) Sa calculée : (PRC_NASC / Species_percent) * 100 (non fournie dans le NASC)
    prc_sa  <- suppressWarnings(as.numeric(df_nasc$PRC_NASC))
    sp_frac <- suppressWarnings(as.numeric(df_nasc$Species_percent))
    df_nasc$Sa <- ifelse(
      !is.na(sp_frac) & sp_frac != 0,
      (prc_sa / sp_frac) * 100,
      NA_real_
    )
    
    # 5) NASC après calcul Sa : base pour contrôle qualité et transformations
    rv$nasc_staged <- df_nasc
    rv$qc_last_msg <- NULL
    rv$n_bad_coord <- sum(nasc_rows_bad_coords(rv$nasc_staged))
    rv$n_sa_zero   <- sum(nasc_rows_sa_zero(rv$nasc_staged))
    apply_nasc_staged(rv$nasc_staged)

    # Aperçu Acoustic_LPA affiché automatiquement après traitement
    show_nasc_preview(FALSE)
    show_lpa_preview(TRUE)
    
    # Alerter l'utilisateur que le traitement est terminé
    showModal(modalDialog(
      title = "✅ Traitement terminé",
      "Le fichier NASC a été transformé et les feuilles Acoustic_LPA sont prêtes.",
      easyClose = TRUE, footer = NULL
    ))
  })

  output$ui_row_qc <- renderUI({
    if (is.null(rv$lpa_sheets)) return(NULL)
    wellPanel(
      h4("Contrôle qualité des lignes"),
      p(
        strong("Coordonnées 999 / -999 "),
        "(au moins une parmi Lat_S, Lon_S, Lat_E, Lon_E) : ",
        rv$n_bad_coord, " ligne(s)."
      ),
      actionButton("btn_drop_bad_coords", "Supprimer ces lignes (coords)"),
      p(strong("Sa = 0 : "), rv$n_sa_zero, " ligne(s)."),
      actionButton("btn_drop_sa_zero", "Supprimer ces lignes (Sa = 0)"),
      tags$hr(),
      p(
        strong("Critères combinés "),
        "(coords invalides et/ou Sa = 0) : ",
        if (!is.null(rv$nasc_staged)) sum(nasc_rows_qc_any(rv$nasc_staged)) else 0L,
        " ligne(s) distincte(s)."
      ),
      actionButton(
        "btn_drop_all_qc",
        "Supprimer tout (coords invalides et/ou Sa = 0)"
      ),
      tags$hr(),
      if (!is.null(rv$qc_last_msg) && nzchar(rv$qc_last_msg)) {
        qc_cls <- if (grepl("^<strong>Aucune suppression", rv$qc_last_msg)) {
          "alert alert-warning mb-0 py-2 small"
        } else {
          "alert alert-success mb-0 py-2 small"
        }
        div(class = qc_cls, HTML(rv$qc_last_msg))
      } else NULL
    )
  })

  observeEvent(input$btn_drop_bad_coords, {
    req(rv$nasc_staged)
    df <- rv$nasc_staged
    bad <- nasc_rows_bad_coords(df)
    if (!any(bad)) {
      rv$qc_last_msg <-
        "<strong>Aucune suppression</strong> — aucune ligne avec coordonnées 999 / -999 dans le NASC actuel."
      showNotification(
        "Aucune ligne à supprimer (coords). Les compteurs ci-dessus sont à jour.",
        type = "warning",
        duration = 8,
        closeButton = TRUE
      )
      return(invisible(NULL))
    }
    n_rem <- sum(bad)
    df <- df[!bad, , drop = FALSE]
    if (nrow(df) < 1L) {
      showModal(modalDialog(
        title = "Erreur",
        "Aucune ligne restante après suppression.",
        easyClose = TRUE, footer = NULL
      ))
      return(invisible(NULL))
    }
    rv$nasc_staged <- df
    rv$n_bad_coord <- sum(nasc_rows_bad_coords(df))
    rv$n_sa_zero   <- sum(nasc_rows_sa_zero(df))
    apply_nasc_staged(df)
    show_lpa_preview(TRUE)
    msg <- sprintf(
      "<strong>✅ %d ligne(s) supprimée(s)</strong> (coords invalides). NASC : <strong>%d</strong> ligne(s) restante(s). Aperçu Acoustic_LPA mis à jour.",
      n_rem,
      nrow(df)
    )
    rv$qc_last_msg <- msg
    showNotification(
      paste0(n_rem, " ligne(s) supprimée(s) (coords). NASC : ", nrow(df), " ligne(s)."),
      type = "message",
      duration = 12,
      closeButton = TRUE
    )
  })

  observeEvent(input$btn_drop_all_qc, {
    req(rv$nasc_staged)
    df <- rv$nasc_staged
    bad <- nasc_rows_qc_any(df)
    if (!any(bad)) {
      rv$qc_last_msg <-
        "<strong>Aucune suppression</strong> — aucune ligne à éliminer selon les critères combinés (coords ou Sa = 0)."
      showNotification(
        "Aucune ligne à supprimer (critères combinés).",
        type = "warning",
        duration = 8,
        closeButton = TRUE
      )
      return(invisible(NULL))
    }
    n_rem <- sum(bad)
    df <- df[!bad, , drop = FALSE]
    if (nrow(df) < 1L) {
      showModal(modalDialog(
        title = "Erreur",
        "Aucune ligne restante après suppression.",
        easyClose = TRUE, footer = NULL
      ))
      return(invisible(NULL))
    }
    rv$nasc_staged <- df
    rv$n_bad_coord <- sum(nasc_rows_bad_coords(df))
    rv$n_sa_zero   <- sum(nasc_rows_sa_zero(df))
    apply_nasc_staged(df)
    show_lpa_preview(TRUE)
    msg <- sprintf(
      "<strong>✅ %d ligne(s) supprimée(s)</strong> (critères combinés). NASC : <strong>%d</strong> ligne(s) restante(s). Aperçu Acoustic_LPA mis à jour.",
      n_rem,
      nrow(df)
    )
    rv$qc_last_msg <- msg
    showNotification(
      paste0(n_rem, " ligne(s) supprimée(s) (combiné). NASC : ", nrow(df), " ligne(s)."),
      type = "message",
      duration = 12,
      closeButton = TRUE
    )
  })

  observeEvent(input$btn_drop_sa_zero, {
    req(rv$nasc_staged)
    df <- rv$nasc_staged
    bad <- nasc_rows_sa_zero(df)
    if (!any(bad)) {
      rv$qc_last_msg <-
        "<strong>Aucune suppression</strong> — aucune ligne avec Sa = 0 dans le NASC actuel."
      showNotification(
        "Aucune ligne à supprimer (Sa = 0). Les compteurs ci-dessus sont à jour.",
        type = "warning",
        duration = 8,
        closeButton = TRUE
      )
      return(invisible(NULL))
    }
    n_rem <- sum(bad)
    df <- df[!bad, , drop = FALSE]
    if (nrow(df) < 1L) {
      showModal(modalDialog(
        title = "Erreur",
        "Aucune ligne restante après suppression.",
        easyClose = TRUE, footer = NULL
      ))
      return(invisible(NULL))
    }
    rv$nasc_staged <- df
    rv$n_bad_coord <- sum(nasc_rows_bad_coords(df))
    rv$n_sa_zero   <- sum(nasc_rows_sa_zero(df))
    apply_nasc_staged(df)
    show_lpa_preview(TRUE)
    msg <- sprintf(
      "<strong>✅ %d ligne(s) supprimée(s)</strong> (Sa = 0). NASC : <strong>%d</strong> ligne(s) restante(s). Aperçu Acoustic_LPA mis à jour.",
      n_rem,
      nrow(df)
    )
    rv$qc_last_msg <- msg
    showNotification(
      paste0(n_rem, " ligne(s) supprimée(s) (Sa = 0). NASC : ", nrow(df), " ligne(s)."),
      type = "message",
      duration = 12,
      closeButton = TRUE
    )
  })

  # -------------------------------------------------------------
  # 2.2) Aperçu du NASC transformé (lorsqu'on clique sur "btn_preview_nasc")
  # -------------------------------------------------------------
  observeEvent(input$btn_preview_nasc, {
    req(rv$nasc_processed)
    show_nasc_preview(TRUE)
  })
  
  output$ui_preview_nasc <- renderUI({
    if (!isTRUE(show_nasc_preview())) return(NULL)
    tagList(
      h4("Aperçu du NASC transformé"),
      div(style = "max-height:500px; overflow:auto;",
          DTOutput("tbl_nasc_processed")
      ),
      tags$hr()
    )
  })
  
  output$tbl_nasc_processed <- renderDT({
    datatable(
      rv$nasc_processed,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames  = FALSE
    )
  })
  
  # -------------------------------------------------------------
  # 2.3) Aperçu des 6 feuilles Acoustic_LPA (lorsqu'on clique sur "btn_preview_lpa")
  # -------------------------------------------------------------
  observeEvent(input$btn_preview_lpa, {
    req(rv$lpa_sheets)
    show_lpa_preview(TRUE)
  })
  
  output$ui_preview_lpa <- renderUI({
    if (!isTRUE(show_lpa_preview())) return(NULL)
    
    # Générer dynamiquement un tabsetPanel avec une tab par feuille
    tabs <- lapply(names(rv$lpa_sheets), function(sheet_name) {
      tabPanel(
        title = sheet_name,
        div(style = "max-height:400px; overflow:auto;",
            DTOutput(paste0("tbl_", sheet_name))
        )
      )
    })
    
    tagList(
      h4("Aperçu des feuilles Acoustic_LPA"),
      do.call(tabsetPanel, tabs),
      tags$hr()
    )
  })
  
  # Pour chaque feuille, créer un DTOutput correspondant
  observe({
    req(rv$lpa_sheets)
    sheets <- names(rv$lpa_sheets)
    for (sheet in sheets) {
      local({
        nm <- sheet
        output_id <- paste0("tbl_", nm)
        
        output[[output_id]] <- renderDT({
          df_sheet <- rv$lpa_sheets[[nm]]
          req(!is.null(df_sheet))
          datatable(
            df_sheet,
            options = list(pageLength = 8, scrollX = TRUE),
            rownames = FALSE
          )
        })
      })
    }
  })
  
  # -------------------------------------------------------------
  # 2.4) Téléchargement d'Acoustic_LPA.xlsx (6 feuilles)
  # -------------------------------------------------------------
  output$dl_acoustic_lpa <- downloadHandler(
    filename = function() {
      paste0("Acoustic_LPA.xlsx")
    },
    content = function(file) {
      req(rv$lpa_sheets)
      write_xlsx(rv$lpa_sheets, path = file)
    }
  )
}

# ---------------------------------------------------------------
# 3) Lancement de l'application
# ---------------------------------------------------------------
shinyApp(ui = ui, server = server)
