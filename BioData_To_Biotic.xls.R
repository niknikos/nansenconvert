# app.R — Application Shiny unifiée pour Fishstations_LPA, Catchsamples_LPA, Individuals_LPA, Biotic_LPA
# Toutes les logiques de scripts sont isolées dans des modules sans modification

# --- 0. Gestion des packages (auto-installation si besoin) ---
library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(writexl)
library(DT)
library(rhandsontable)
library(stringi)
library(shinyWidgets)
library(shinycssloaders)
library(shinyFiles)
library(shinyalert)
library(stringr)
library(tibble)
library(openxlsx)
library(bslib)
library(tools)
library(later)

# --- 0.bis Détection automatique du dossier de l'application ---
APP_DIR <- local({
  cand <- character(0)
  # 1) Fichier sourcé via source()/runApp()
  try(cand <- c(cand, dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE))), silent = TRUE)
  # 2) Rscript
  try({
    args <- commandArgs(trailingOnly = FALSE)
    m <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m)) cand <- c(cand, dirname(normalizePath(m, winslash = "/", mustWork = TRUE)))
  }, silent = TRUE)
  # 3) RStudio (éditeur actif)
  try({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      ctx <- rstudioapi::getSourceEditorContext()
      if (!is.null(ctx) && nzchar(ctx$path)) {
        cand <- c(cand, dirname(normalizePath(ctx$path, winslash = "/", mustWork = TRUE)))
      }
    }
  }, silent = TRUE)
  # 4) Dernier recours : répertoire de travail courant
  cand <- c(cand, normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  cand[nzchar(cand)][1]
})

# Helpers WoRMS / espèces (inliné ; ancien fichier species_worms.R)
worms_guess_data_path <- function(app_dir) {
  names_try <- c("WoRMS.xlsx", "WorMS.xlsx", "worms.xlsx", "WoRMS.csv", "WorMS.csv", "worms.csv")
  bases <- unique(c(
    file.path(app_dir, "data"),
    file.path(getwd(), "data"),
    "data"
  ))
  candidates <- as.vector(outer(bases, names_try, file.path))
  candidates[file.exists(candidates)][1]
}

#' Charge le référentiel WoRMS; retourne NULL si introuvable / erreur
worms_read_reference <- function(app_dir) {
  path <- worms_guess_data_path(app_dir)
  if (is.na(path) || !nzchar(path)) {
    return(NULL)
  }
  ext <- tolower(tools::file_ext(path))
  w <- tryCatch(
    if (ext == "csv") {
      readr::read_csv(path, na = c("", "NA"), show_col_types = FALSE)
    } else {
      readxl::read_excel(path)
    },
    error = function(e) NULL
  )
  if (is.null(w) || !is.data.frame(w) || !ncol(w)) {
    return(NULL)
  }
  nms <- names(w) %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    gsub("[^a-z0-9_]+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
  names(w) <- nms
  if (!"scientificname" %in% names(w)) {
    alts <- c("scientific_name", "valid_name", "name")
    h <- alts[alts %in% names(w)][1]
    if (is.na(h)) {
      return(NULL)
    }
    names(w)[names(w) == h] <- "scientificname"
  }
  w$scientificname <- trimws(as.character(w$scientificname))
  w <- w[nzchar(w$scientificname) & !is.na(w$scientificname), , drop = FALSE]
  w
}

norm_col_slug <- function(x) {
  v <- as.character(x)
  stringi::stri_trans_general(v, "Latin-ASCII") %>%
    tolower() %>%
    gsub("[^a-z0-9]+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
}

#' Variantes reconnues pour la colonne espèce (slug normalisé)
is_species_slug <- function(slug) {
  patts <- c(
    "^espece$", "^especes$", "^espec$", "^esp$", "^espe$",
    "^name_scientifique$", "^nom_scientifique$", "^nom_scient$",
    "^scientificname$", "^scientific_name$", "^scientific$", "^species$", "^espece_scient$",
    "_espece$", "^espece_"
  )
  vapply(slug, function(s) {
    if (!nzchar(s) || is.na(s)) {
      return(FALSE)
    }
    for (p in patts) {
      if (grepl(p, s, ignore.case = TRUE)) {
        return(TRUE)
      }
    }
    FALSE
  }, logical(1))
}

find_species_column_name <- function(df) {
  if (is.null(df) || !ncol(df)) {
    return(NA_character_)
  }
  sl <- norm_col_slug(names(df))
  names(sl) <- names(df)
  hit <- names(sl)[is_species_slug(sl) & !is.na(sl)]
  if (!length(hit)) {
    return(NA_character_)
  }
  hit[1L]
}

#' Format binomial: genre (Maj), épithètes en minuscule
norm_species_binomial <- function(x) {
  x <- trimws(as.character(x))
  x[nchar(x) == 0 | is.na(x)] <- NA_character_
  if (!length(x)) {
    return(character(0))
  }
  vapply(
    strsplit(x, "\\s+"),
    function(p) {
      p <- p[nzchar(p)]
      if (!length(p)) {
        return(NA_character_)
      }
      p <- p[!is.na(p) & p != "" & p != "NA"]
      if (!length(p)) {
        return(NA_character_)
      }
      p[1L] <- paste0(
        toupper(substr(p[1L], 1, 1)),
        tolower(substr(p[1L], 2, nchar(p[1L])))
      )
      if (length(p) > 1L) {
        p[-1L] <- tolower(p[-1L])
      }
      paste(p, collapse = " ")
    },
    character(1L)
  )
}

worms_key <- function(s) {
  s <- as.character(s)
  stringi::stri_trans_general(s, "Latin-ASCII") %>%
    tolower() %>%
    trimws() %>%
    gsub("\\s+", " ", .)
}

worms_index_canonical <- function(wdf) {
  if (is.null(wdf) || !nrow(wdf) || !"scientificname" %in% names(wdf)) {
    return(tibble::tibble(key = character(0), scientificname = character(0)))
  }
  tibble::tibble(
    scientificname = wdf$scientificname,
    key = worms_key(wdf$scientificname)
  ) %>%
    dplyr::filter(nzchar(key)) %>%
    dplyr::distinct(key, .keep_all = TRUE)
}

match_vector_to_worms <- function(
    values,
    widx,
    scientificname_full = NULL) {
  values <- as.character(values)
  n <- length(values)
  if (n == 0L) {
    return(
      list(
        canonical = character(0),
        matched = logical(0)
      )
    )
  }
  if (is.null(widx) || !nrow(widx)) {
    return(
      list(
        canonical = rep(NA_character_, n),
        matched = rep(FALSE, n)
      )
    )
  }
  wk_key <- as.character(widx$key)
  wk_sn <- as.character(widx$scientificname)
  k <- worms_key(values)
  hit <- wk_sn[match(k, wk_key)]
  mch <- !is.na(hit)
  sfull <- if (!is.null(scientificname_full) && length(scientificname_full)) {
    u <- as.character(scientificname_full)
    u[!is.na(u) & nzchar(u, keepNA = TRUE)]
  } else {
    character(0)
  }
  if (length(sfull)) {
    sfull_key <- vapply(
      sfull,
      function(s) {
        if (is.na(s) | !nzchar(s)) {
          return(NA_character_)
        }
        worms_key(s)
      },
      character(1L)
    )
  }
  for (i in which(!mch)) {
    v <- values[i]
    if (is.na(v) | !nzchar(v, keepNA = TRUE)) {
      next
    }
    if (length(sfull) && v %in% sfull) {
      mch[i] <- TRUE
      hit[i] <- v
      next
    }
    if (length(sfull)) {
      is_ok <- vapply(
        seq_along(sfull),
        function(j) {
          sfull[j] == v || (nzchar(sfull_key[j], keepNA = TRUE) & k[i] == sfull_key[j])
        },
        logical(1L)
      )
      wj <- which(is_ok)
      if (length(wj)) {
        mch[i] <- TRUE
        hit[i] <- sfull[wj[1L]]
      }
    }
  }
  list(
    canonical = hit,
    matched = mch
  )
}

#' Vecteur nommé pour selectizeInput : 1re entrée = conserver, puis scientificname
worms_select_choices <- function(scientificname_vec) {
  s <- unique(na.omit(as.character(scientificname_vec)))
  s <- s[nzchar(s, keepNA = TRUE) & !is.na(s)]
  if (length(s)) {
    s <- tryCatch(
      stringi::stri_sort(s, locale = "fr", na.last = FALSE),
      error = function(e) sort(s, method = "radix")
    )
  }
  if (!length(s)) {
    return(
      c("(impossible : liste scientificname vide dans WoRMS)" = "")
    )
  }
  v <- c("", s)
  nms <- c("(conserver l'import)", s)
  stats::setNames(v, nms)
}

## --- Fiche catch (Fiche-SP) : détection de colonnes ---

#' Correspondance colonnes pour Individuals (slug ASCII, dont espèce avec variantes)
indiv_map_by_slug <- function(file_cols, required_vec) {
  out <- setNames(rep(NA_character_, length(required_vec)), required_vec)
  if (!length(file_cols)) {
    return(as.list(out))
  }
  fc_sl <- norm_col_slug(file_cols)
  names(fc_sl) <- file_cols
  for (req in required_vec) {
    hit <- names(fc_sl)[!is.na(fc_sl) & fc_sl == req]
    if (length(hit)) {
      out[[req]] <- hit[1L]
    }
  }
  esc <- out[["espece"]]
  if (length(esc) != 1L || is.na(esc) || !nzchar(esc, keepNA = TRUE)) {
    h2 <- names(fc_sl)[!is.na(fc_sl) & is_species_slug(fc_sl)]
    if (length(h2)) {
      out[["espece"]] <- h2[1L]
    }
  }
  as.list(out)
}

catch_match_station <- function(slugs) {
  for (s in c("serialnumber", "station", "st", "n_station", "num_station", "n_station_")) {
    w <- which(slugs == s)
    if (length(w)) {
      return(names(slugs)[w[1L]])
    }
  }
  for (i in seq_along(slugs)) {
    si <- as.character(unname(unlist(slugs[i])))
    if (grepl("station|serial|n_station|st$", si)) {
      return(names(slugs)[i])
    }
  }
  NA_character_
}

catch_match_echp <- function(slugs) {
  s <- "ech_p"
  w <- which(slugs == s | slugs == "echp")
  if (length(w)) {
    return(names(slugs)[w[1L]])
  }
  w <- which(grepl("^ech", slugs))
  if (length(w)) {
    return(names(slugs)[w[1L]])
  }
  NA_character_
}

catch_match_totp <- function(slugs) {
  w <- which(slugs == "tot_p" | slugs == "totp")
  if (length(w)) {
    return(names(slugs)[w[1L]])
  }
  NA_character_
}

catch_match_totn <- function(slugs) {
  w <- which(slugs == "tot_n" | slugs == "totn" | slugs == "n_tot" | slugs == "n")
  if (length(w)) {
    return(names(slugs)[w[1L]])
  }
  w <- which(grepl("totn|^n$|_n$", slugs))
  if (length(w)) {
    return(names(slugs)[w[1L]])
  }
  NA_character_
}

#' Construit un data.frame unifié pour le pipeline catch ; harmonise "espece" en interne
catch_build_standard_frame <- function(df) {
  nms <- names(df)
  sl <- norm_col_slug(nms)
  names(sl) <- nms
  sp <- find_species_column_name(df)
  st <- catch_match_station(sl)
  ecp <- catch_match_echp(sl)
  ttp <- catch_match_totp(sl)
  ttn <- catch_match_totn(sl)
  err <- list()
  if (is.na(sp) || !nzchar(sp)) {
    err$species <- "Colonne espèce non détectée (attendu : espece, espèce, esp, scientificname, etc.)"
  }
  if (is.na(st) || !nzchar(st)) {
    err$station <- "Colonne station / serial non détectée"
  }
  if (is.na(ecp) || !nzchar(ecp)) {
    err$ech <- "Colonne Ech_P (poids de l’échantillon) non détectée"
  }
  if (is.na(ttn) || !nzchar(ttn)) {
    err$totn <- "Colonne Tot_N (effectif) non détectée"
  }
  if (is.na(ttp) || !nzchar(ttp)) {
    ttp <- NA_character_
  }
  if (length(err)) {
    return(list(
      error = err,
      df = NULL
    ))
  }
  out <- tibble::tibble(
    espece = .subset2(df, sp),
    serialnumber = .subset2(df, st),
    ech_p = suppressWarnings(as.numeric(.subset2(df, ecp))),
    tot_n = suppressWarnings(as.numeric(.subset2(df, ttn))),
    tot_p = if (!is.na(ttp) && nchar(ttp)) {
      suppressWarnings(as.numeric(.subset2(df, ttp)))
    } else {
      rep(NA_real_, nrow(df))
    }
  )
  list(
    error = NULL,
    df = out
  )
}

apply_species_map_chr <- function(values, sp_map) {
  out <- as.character(values)
  if (length(sp_map) && is.character(sp_map)) {
    for (old in names(sp_map)) {
      to <- unname(sp_map[old])
      if (length(old) && !is.na(old) && nzchar(as.character(old))) {
        out[out == as.character(old)] <- as.character(to)
      }
    }
  }
  out
}

#' Aligne le texte saisi/choisi sur l’orthographe exacte du réf. WoRMS (comme app_combine)
formater_choix_worms_canonique <- function(nom, scientificname_vec) {
  nom_c <- trimws(as.character(nom))
  if (!nzchar(nom_c) || is.null(scientificname_vec) || !length(scientificname_vec)) {
    return(as.character(nom))
  }
  sn <- as.character(scientificname_vec)
  sn <- sn[!is.na(sn) & nzchar(sn)]
  idx <- match(tolower(nom_c), tolower(trimws(sn)))
  if (is.na(idx)) {
    return(nom_c)
  }
  sn[[idx[1L]]]
}

#' Applique les remplacements espèce (même principe que app_combine : cible les lignes par ancien libellé)
apply_worms_replacements_catch <- function(
    espece,
    sp_map,
    formater_si_worms = FALSE,
    wdf_valide = NULL) {
  out <- as.character(espece)
  if (!length(sp_map) || !is.character(sp_map) || !length(names(sp_map))) {
    return(out)
  }
  noms_w <- if (!is.null(wdf_valide) && is.data.frame(wdf_valide) && "scientificname" %in% names(wdf_valide)) {
    wdf_valide$scientificname
  } else {
    NULL
  }
  for (old in names(sp_map)) {
    to <- as.character(sp_map[[old]])
    sp_old <- as.character(old)
    if (is.na(sp_old) || is.na(to) || !nzchar(sp_old) || !nzchar(to)) {
      next
    }
    if (isTRUE(formater_si_worms) && !is.null(noms_w)) {
      to <- formater_choix_worms_canonique(to, noms_w)
    }
    w <- which(out == sp_old)
    if (!length(w)) {
      w <- which(trimws(out) == trimws(sp_old))
    }
    if (length(w)) {
      out[w] <- to
    }
  }
  out
}

#' Applique normalisation binomiale + carte utilisateur (WoRMS) sur la colonne espece
indiv_apply_espece_workflow <- function(df, sp_map) {
  if (is.null(df) || !nrow(df) || !"espece" %in% names(df)) {
    return(df)
  }
  m <- if (is.null(sp_map) || !length(sp_map)) {
    character(0)
  } else {
    sp_map
  }
  e0 <- norm_species_binomial(df$espece)
  df$espece_pre_map <- e0
  df$espece <- apply_species_map_chr(e0, m)
  df
}

#' Compte de lignes : validées, corrigées, hors référentiel
#'
#' - \code{raw_in} : texte d’import (fichier) avant normalisation
#' - \code{final_in} : libellé retenu (après norm + mappage WoRMS)
#' - \code{pre_map} : optionnel — chaîne juste après normalisation binomiale, avant
#'   mappage manuel (modale). S’il diffère de \code{final_in}, c’est un remplacement
#'   explicite (compté en « corrigé »).
species_line_summary <- function(
    raw_in,
    final_in,
    widx,
    pre_map = NULL,
    scientificname_full = NULL) {
  raw_in <- as.character(raw_in)
  final_in <- as.character(final_in)
  n <- length(raw_in)
  if (n == 0L) {
    return(
      list(
        n_validated = 0L,
        n_corrected = 0L,
        n_unresolved = 0L,
        n_unresolved_species = 0L
      )
    )
  }
  if (length(final_in) != n) {
    stop("species_line_summary : longueur final_in != raw_in", call. = FALSE)
  }
  if (!is.null(pre_map) && length(pre_map) != n) {
    stop("species_line_summary : longueur pre_map incorrecte", call. = FALSE)
  }
  pre_map <- if (!is.null(pre_map) && length(pre_map) == n) {
    as.character(pre_map)
  } else {
    as.character(final_in)
  }
  mat <- if (!is.null(widx) && nrow(widx)) {
    match_vector_to_worms(final_in, widx, scientificname_full = scientificname_full)
  } else {
    list(matched = rep(FALSE, n))
  }
  in_ref <- isTRUE(!is.null(mat$matched)) && length(mat$matched) == n
  in_ref <- if (in_ref) {
    mat$matched & !is.na(final_in) & nzchar(final_in, keepNA = TRUE)
  } else {
    rep(FALSE, n)
  }
  raw_k <- vapply(
    raw_in,
    function(a) {
      if (is.na(a) | !nzchar(a)) {
        return(NA_character_)
      }
      worms_key(a)
    },
    character(1L)
  )
  fin_k <- vapply(
    final_in,
    function(a) {
      if (is.na(a) | !nzchar(a)) {
        return(NA_character_)
      }
      worms_key(a)
    },
    character(1L)
  )
  same_key_fichier_final <- (raw_k == fin_k) | (is.na(raw_k) & is.na(fin_k))
  same_pre_final <-
    (as.character(pre_map) == as.character(final_in)) |
    (is.na(pre_map) & is.na(final_in))
  pur_ok <- in_ref & same_key_fichier_final & same_pre_final
  pur_adj <- in_ref & !pur_ok
  n_val <- as.integer(sum(pur_ok, na.rm = TRUE))
  n_ch <- as.integer(sum(pur_adj, na.rm = TRUE))
  bad_mask <- !in_ref & !is.na(final_in) & nzchar(final_in, keepNA = TRUE)
  n_bad <- as.integer(sum(bad_mask, na.rm = TRUE))
  u_fail <- final_in[bad_mask]
  n_bad_sp <- if (length(u_fail)) {
    as.integer(length(unique(u_fail[!is.na(u_fail) & nzchar(u_fail, keepNA = TRUE)])))
  } else {
    0L
  }
  list(
    n_validated = n_val,
    n_corrected = n_ch,
    n_unresolved = n_bad,
    n_unresolved_species = n_bad_sp
  )
}

# --- 1. Thème principal ---
theme_modern <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#357AB7", secondary = "#6c757d",
  "body-bg" = "#f2f2f2", "sidebar-bg" = "#ffffff", "panel-bg" = "#ffffff",
  "font-size-base" = "1.05rem"
)

# --- 2. MODULE 1 : Fishstations_LPA (code strictement inchangé, encapsulé) ---
mod_fishstations_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(
      tagList(icon("location-dot", class="me-2"), "Fishstations_LPA")
    ),
    fluidRow(
      column(4,
        div(class = "card-container",
            div(tags$h5(icon("list-ol", class = "sidebar-icon"), strong("Étapes du workflow :")), style = "margin-bottom: 16px;"),
            actionButton(ns("createMetadata"), label = tagList(icon("file-code"), "1. Créer Metadata-XML"), class = "btn btn-primary", style = "width: 100%;"),
            br(), br(),
            fileInput(ns("file"), tagList(icon("file-excel"), "2. Import Stations file"), accept = c(".xlsx", ".xls", ".csv"), buttonLabel = "Browse…", placeholder = "Aucun fichier sélectionné", width = "100%"),
            br(),
            actionButton(ns("injectMetadata"), label = tagList(icon("syringe"), "3. Injecter Metadata dans Stations file"), class = "btn btn-secondary", style = "width: 100%;"),
            br(), br(),
            downloadButton(ns("downloadStations"), label = tagList(icon("download"), "4. Télécharger Fishstations_LPA"), class = "btn btn-secondary", style = "width: 100%;"),
            br(), br(),
            downloadButton(ns("downloadMetadataXML"), label = tagList(icon("download"), "5. Télécharger Missions_LPA"), class = "btn btn-primary", style = "width: 100%;")
        )
      ),
      column(8,
        div(class = "card-container",
            uiOutput(ns("fileError")),
            div(tags$h4(icon("table"), "Aperçu des feuilles Fishstations_LPA"),
                style = "margin-top: 8px; margin-bottom: 12px;"),
            tabsetPanel(
              id = ns("fs_preview_tabs"),
              tabPanel(
                "Données complètes",
                div(style = "max-height:500px; overflow:auto;",
                    DTOutput(ns("preview")) %>% tagAppendAttributes(style = "border-radius:6px;")
                )
              ),
              tabPanel(
                "Fishstations",
                div(style = "max-height:500px; overflow:auto;",
                    DTOutput(ns("stationsPreview")) %>% tagAppendAttributes(style = "border-radius:6px;")
                )
              )
            ),
            conditionalPanel(sprintf("input['%s'] > 0", ns("createMetadata")),
                             br(),
                             div(tags$h4(icon("table"), "Metadata XML (à remplir manuellement)"), style = "margin-bottom: 12px;"),
                             rHandsontableOutput(ns("metadataTable")) %>% tagAppendAttributes(style = "border-radius:6px;box-shadow:0 1px 4px rgba(0,0,0,0.1);")
            )
        )
      )
    )
  )
}
mod_fishstations_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    posCols <- c(
      "latitude_filage","longitude_filage",
      "latitude_virage","longitude_virage"
    )
    rawData <- reactiveVal(NULL)
    metadataDF <- reactiveVal(NULL)
    metadataCols <- c(
      "missiontype","startyear","platform","missionnumber",
      "missiontypename","callsignal","platformname","cruise",
      "missionstartdate","missionstopdate","purpose"
    )
    observeEvent(input$createMetadata, {
      metadataDF(
        data.frame(matrix("", nrow = 1, ncol = length(metadataCols), dimnames = list(NULL, metadataCols)), stringsAsFactors = FALSE)
      )
    })
    output$metadataTable <- renderRHandsontable({ req(metadataDF()); rhandsontable(metadataDF(), stretchH = "all") })
    observeEvent(input$metadataTable, { metadataDF(hot_to_r(input$metadataTable)) })
    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name) %>% tolower()
      tryCatch({
        if (ext %in% c("xlsx", "xls")) { df <- read_excel(input$file$datapath, guess_max = 10000)
        } else if (ext == "csv") { df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
        } else { stop("Format non supporté : ", ext) }
        names(df) <- names(df) %>%
          stringi::stri_trans_general("Latin-ASCII") %>%
          tolower() %>%
          gsub("[^a-z0-9]+", "_", .) %>%
          gsub("_+", "_", .) %>%
          gsub("^_|_$", "", .)
        for (col in c("missionnumber", "catchplatform", "stationtype")) { if (!col %in% names(df)) df[[col]] <- "" }
        rawData(df); output$fileError <- renderUI(NULL)
      }, error = function(e) {
        rawData(NULL)
        output$fileError <- renderUI(div(class = "error-text", paste("Erreur lors de la lecture du fichier :", e$message)))
      })
    })
    observeEvent(input$injectMetadata, {
      req(metadataDF(), rawData())
      m <- metadataDF()[1, ]
      df <- rawData()
      df <- df %>%
        mutate(
          missionnumber = m$missionnumber,
          catchplatform = m$platform,
          stationtype   = m$missiontype
        )
      rawData(df)
      showNotification("Metadata injecté dans la Fiche-ST", type = "message")
    })
    previewData <- reactive({
      df <- rawData() %>% req()
      existing_posCols <- posCols[posCols %in% names(df)]
      df_disp <- df
      for (col in existing_posCols) {
        if (col %in% names(df_disp)) {
          df_disp[[col]] <- ifelse(
            is.na(df_disp[[col]]) | df_disp[[col]] == "",
            sprintf("<div style='background-color:yellow;'>%s</div>", df_disp[[col]]),
            df_disp[[col]]
          )
        }
      }
      df_disp
    })
    output$preview <- renderDT({
      datatable(
        previewData(),
        escape = FALSE,
        editable = list(target = 'cell'),
        options = list(pageLength = 5, dom = 'tp', ordering = FALSE)
      )
    })
    observeEvent(input$preview_cell_edit, {
      info <- input$preview_cell_edit
      df <- rawData()
      val <- gsub("<.*?>", "", info$value)
      df[info$row, info$col] <- type.convert(val, as.is = TRUE)
      rawData(df)
      proxy <- dataTableProxy('preview')
      replaceData(proxy, previewData(), resetPaging = FALSE)
    })
    cleanedData <- reactive({
      df <- rawData() %>% req()
      df <- df %>% mutate(across(where(is.character), ~gsub("'", "", .)))
      existing_posCols <- posCols[posCols %in% names(df)]
      missing_posCols <- setdiff(posCols, names(df))
      if (length(existing_posCols) > 0) {
        df <- df %>% mutate(across(all_of(existing_posCols), as.numeric))
      }
      if (length(missing_posCols) > 0) {
        showNotification(
          paste0("Colonnes manquantes dans votre fichier et ignorées : ", paste(missing_posCols, collapse = ", ")),
          type = "warning", duration = 10
        )
      }
      df <- df %>%
        mutate(
          stationstartdate = {
            tmp <- suppressWarnings(parse_date_time(
              as.character(date),
              orders = c("Ymd HMS", "Ymd", "dmy HMS", "dmy", "mdy HMS", "mdy")
            ))
            ifelse(is.na(tmp), NA_character_, paste0(format(tmp, "%Y-%m-%d"), "Z"))
          },
          stationstopdate = stationstartdate
        ) %>%
        rename(
          bottomdepthstart = prof_fond_filage,
          bottomdepthstop  = prof_fond_virage
        ) %>%
        mutate(
          bottomdepthstart = as.numeric(bottomdepthstart),
          bottomdepthstop  = as.numeric(bottomdepthstop),
          prof_filet_filage = suppressWarnings(as.numeric(prof_filet_filage)),
          prof_filet_virage = suppressWarnings(as.numeric(prof_filet_virage)),
          fishingdepthmax = case_when(
            !is.na(prof_filet_filage) & !is.na(prof_filet_virage) & prof_filet_filage == prof_filet_virage ~ prof_filet_filage,
            !is.na(prof_filet_filage) & !is.na(prof_filet_virage) ~ pmax(prof_filet_filage, prof_filet_virage, na.rm = TRUE),
            TRUE ~ NA_real_
          ),
          fishingdepthmin = case_when(
            !is.na(prof_filet_filage) & !is.na(prof_filet_virage) & prof_filet_filage == prof_filet_virage ~ prof_filet_filage,
            !is.na(prof_filet_filage) & !is.na(prof_filet_virage) ~ pmin(prof_filet_filage, prof_filet_virage, na.rm = TRUE),
            TRUE ~ NA_real_
          )
        ) %>%
        mutate(
          serialnumber        = station,
          nation              = 58,
          haulvalidity        = 1,
          gear                = 3500,
          gearcount           = 1,
          logstart            = 100,
          logstop             = logstart + 15,
          distance            = 2.5,
          area                = 58,
          samplequality       = NA_character_,
          gearcondition       = NA_character_,
          trawldoorspread     = 15,
          verticaltrawlopening = 50
        ) %>%
        rename(
          stationstarttime = heure_filage,
          stationstoptime  = heure_virage,
          station          = station,
          soaktime         = duree_chalutage,
          stationcomment   = note,
          wirelength       = longueur_funes,
          vesselspeed      = vitesse_chalutage,
          direction        = cap_chalutage,
          latitudestart    = latitude_filage,
          longitudestart   = longitude_filage,
          latitudeend      = latitude_virage,
          longitudeend     = longitude_virage
        ) %>%
        mutate(
          latitudestart  = if_else(is.na(latitudestart)  & !is.na(latitudeend),  latitudeend,  latitudestart),
          latitudeend    = if_else(is.na(latitudeend)    & !is.na(latitudestart), latitudestart, latitudeend),
          longitudestart = if_else(is.na(longitudestart) & !is.na(longitudeend), longitudeend, longitudestart),
          longitudeend   = if_else(is.na(longitudeend)   & !is.na(longitudestart), longitudestart, longitudeend)
        ) %>%
        mutate(
          tmp_start = suppressWarnings(parse_date_time(as.character(stationstarttime), orders = c("HMS", "HM"))),
          tmp_stop  = suppressWarnings(parse_date_time(as.character(stationstoptime),  orders = c("HMS", "HM"))),
          tmp_start = if_else(is.na(tmp_start) & !is.na(tmp_stop), tmp_stop - minutes(30), tmp_start),
          tmp_stop  = if_else(is.na(tmp_stop)  & !is.na(tmp_start), tmp_start + minutes(30), tmp_stop),
          stationstarttime = if_else(is.na(tmp_start), NA_character_, paste0(format(tmp_start, "%H:%M:%OS3"), "Z")),
          stationstoptime  = if_else(is.na(tmp_stop),  NA_character_, paste0(format(tmp_stop,  "%H:%M:%OS3"), "Z"))
        ) %>%
        select(-tmp_start, -tmp_stop) %>%
        mutate(
          soaktime = as.numeric(soaktime) / 100
        )
      df
    })
    stationsData <- reactive({
      cleanedData() %>% select(any_of(c(
        "stationstartdate", "stationstopdate", "stationstarttime", "stationstoptime",
        "station", "serialnumber", "soaktime", "stationcomment",
        "wirelength", "vesselspeed", "direction", "bottomdepthstart",
        "bottomdepthstop", "fishingdepthmax", "fishingdepthmin", "trawldoorspread",
        "verticaltrawlopening", "latitudestart", "longitudestart",
        "latitudeend", "longitudeend", "nation", "haulvalidity", "gear", "gearcount",
        "logstart", "logstop", "distance", "area", "samplequality", "gearcondition",
        "missionnumber", "catchplatform", "stationtype"
      )))
    })
    output$stationsPreview <- renderDT({
      datatable(
        stationsData(),
        options = list(pageLength = 5, dom = 'tp'),
        rownames = FALSE
      )
    })
    output$downloadStations <- downloadHandler(
      filename = function() "Fishstations_LPA.xlsx",
      content = function(file) write_xlsx(stationsData(), file)
    )
    output$downloadMetadataXML <- downloadHandler(
      filename = function() "Missions_LPA.xlsx",
      content = function(file) write_xlsx(metadataDF(), file)
    )
  })
}

# --- 3. MODULE 2 : Catchsamples_LPA ---
mod_catchsamples_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(tagList(icon("ship", class = "me-2"), "Catchsamples_LPA")),
    sidebarLayout(
      sidebarPanel(width = 4,
                   div(class = "card mb-4",
                       div(class = "card-header h5 bg-primary text-white", icon("file-import", class = "me-2"), "1. Import des fichiers"),
                       div(class = "card-body",
                           fileInput(ns("fileSP"), "Import Catches file (CSV, XLS ou XLSX)", accept = c(".csv", ".xlsx", ".xls"), buttonLabel = "Browse…")
                       )
                   ),
                   div(class = "card",
                       div(class = "card-header h5 bg-success text-white", icon("file-export", class = "me-2"), "2. Exporter les résultats"),
                       div(class = "card-body",
                           downloadButton(ns("downloadData"), "📄 Télécharger la Fiche-SP Éditée", class = "btn-primary mb-2", style = "width: 100%;"),
                           downloadButton(ns("downloadCatchsamples"), "📦 Télécharger Catchsamples_LPA", class = "btn-success", style = "width: 100%;")
                       )
                   )
      ),
      mainPanel(width = 8,
                uiOutput(ns("catch_sp_badge")),
                div(class = "card",
                    div(class = "card-header h5 bg-info text-white", icon("table", class = "me-2"), "Aperçu du fichier catchsamples _généré"),
                    div(class = "card-body", DTOutput(ns("previewCatch")) %>% shinycssloaders::withSpinner(type = 6, color = "#3498db"))
                )
      )
    )
  )
}
mod_catchsamples_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv_c <- shiny::reactiveValues(
      df = NULL, espece_file = NULL, sp_remap = character(0),
      solved_session = 0L, err_import = NULL
    )
    wormsData <- shiny::reactive({
      worms_read_reference(APP_DIR)
    })
    widx_c <- shiny::reactive({
      w <- wormsData()
      if (is.null(w) || !nrow(w)) {
        return(NULL)
      }
      worms_index_canonical(w)
    })
    # Étape 1: import → normalisation + modal WoRMS si besoin
    observeEvent(input$fileSP, {
      req(input$fileSP)
      rv_c$err_import <- NULL
      rv_c$df <- NULL
      rv_c$espece_file <- NULL
      ext <- tolower(tools::file_ext(input$fileSP$name))
      dfin <- tryCatch(
        {
          if (ext == "csv") {
            read_csv(input$fileSP$datapath, na = c("", "NA"), show_col_types = FALSE)
          } else {
            read_excel(input$fileSP$datapath)
          }
        },
        error = function(e) {
          NULL
        }
      )
      if (is.null(dfin) || !nrow(dfin)) {
        rv_c$err_import <- "Impossible de lire le fichier (vide ou format invalide)."
        return()
      }
      st <- catch_build_standard_frame(dfin)
      if (!is.null(st$error)) {
        msg <- paste(unlist(st$error), collapse = " ")
        rv_c$err_import <- msg
        showNotification(
          paste("Import catchsamples :", msg), type = "error", duration = 10
        )
        return()
      }
      d0 <- st$df
      d0$espece_fichier <- as.character(d0$espece)
      d0$espece <- norm_species_binomial(d0$espece)
      d0$espece_pre_map <- d0$espece
      wdf <- wormsData()
      widx <- widx_c()
      if (is.null(wdf) || !nrow(wdf) || is.null(widx) || !nrow(widx)) {
        showNotification(
          "Fichier WoRMS introuvable ou illisible : validation d’espèces limitée. Placez un fichier WoRMS (CSV/xlsx) dans le dossier data/.",
          type = "warning",
          duration = 8
        )
        rv_c$df <- d0
        rv_c$espece_file <- d0$espece_fichier
        rv_c$sp_remap <- character(0)
        return()
      }
      showModal(
        modalDialog(
          title = "Validation WoRMS",
          div(
            class = "text-center p-4",
            div(
              class = "spinner-border text-primary",
              role = "status",
              tags$span(class = "visually-hidden", "Chargement")
            ),
            p(class = "mt-3 mb-0 small", "Vérification des noms d'espèces (WoRMS) en cours…")
          ),
          easyClose = FALSE, footer = NULL, size = "m"
        ),
        session = session
      )
      d0_l <- d0
      wdf_l <- wdf
      widx_l <- widx
      later::later(
        function() {
          u <- unique(stats::na.omit(as.character(d0_l$espece)))
          m <- match_vector_to_worms(u, widx_l, scientificname_full = wdf_l$scientificname)
          um <- u[!m$matched & nzchar(u)]
          if (length(um) == 0) {
            removeModal(session = session)
            rv_c$df <- d0_l
            rv_c$espece_file <- d0_l$espece_fichier
            rv_c$sp_remap <- character(0)
            return()
          }
          ch_choices <- tryCatch(
            worms_select_choices(wdf_l$scientificname),
            error = function(e) NULL
          )
          if (is.null(ch_choices) || !length(ch_choices)) {
            removeModal(session = session)
            rv_c$df <- d0_l
            rv_c$espece_file <- d0_l$espece_fichier
            rv_c$sp_remap <- character(0)
            showNotification(
              "Préparation de la liste WoRMS impossible — vérifiez le référentiel dans data/.",
              type = "error", duration = 8
            )
            return()
          }
          showModal(
            modalDialog(
              title = "Espèces non reconnues (WoRMS) — Catchsamples_LPA",
              p(
                "Certaines espèces ne figurent pas dans le référentiel. Utilisez la zone ci-dessous : saisissez le début d’un ", tags$em("scientificname"), " WoRMS pour filtrer la liste (grosse table).",
                " La 1re option « conserver l’import » ne joint pas l’enregistrement taxonomique.",
                class = "small text-muted"
              ),
              tagList(
                lapply(
                  seq_along(um),
                  function(i) {
                    uu <- um[i]
                    selectizeInput(
                      inputId = ns(paste0("catch_worm_", i)),
                      label = tagList(strong(htmltools::htmlEscape(uu)), " — WoRMS (scientificname) :"),
                      choices = ch_choices,
                      width = "100%",
                      options = list(
                        placeholder = "Taper pour filtrer (ex. Sardin, Liza)…",
                        maxOptions = 50000L,
                        maxItems = 1L
                      )
                    )
                  }
                )
              ),
              footer = tagList(
                actionButton(ns("catch_worms_apply"), "Appliquer les remplacements", class = "btn btn-primary"),
                actionButton(
                  ns("catch_worms_skip"), "Continuer sans correction (tout)", class = "btn btn-outline-secondary"
                )
              ),
              size = "l",
              easyClose = FALSE
            ),
            session = session
          )
          session$userData$catch_um <- um
          session$userData$catch_d0 <- d0_l
        },
        delay = 0.08
      )
    })
    observeEvent(input$catch_worms_apply, {
      d0 <- session$userData$catch_d0
      um <- session$userData$catch_um
      if (is.null(d0) || is.null(um)) {
        removeModal()
        return()
      }
      widx <- widx_c()
      mp <- character(0)
      for (i in seq_along(um)) {
        id_i <- paste0("catch_worm_", i)
        sel <- input[[id_i]]
        if (length(sel) && nzchar(sel[1L])) {
          mp[as.character(um[i])] <- as.character(sel[1L])
        }
      }
      d0$espece <- apply_worms_replacements_catch(
        d0$espece, mp, formater_si_worms = TRUE,
        wdf_valide = tryCatch(wormsData(), error = function(e) NULL)
      )
      rv_c$df <- d0
      rv_c$espece_file <- d0$espece_fichier
      rv_c$sp_remap <- mp
      removeModal()
      if (is.null(widx) || !nrow(widx)) {
        return()
      }
      wdf0 <- tryCatch(wormsData(), error = function(e) NULL)
      snf <- if (!is.null(wdf0) && "scientificname" %in% names(wdf0)) {
        wdf0$scientificname
      } else {
        NULL
      }
      m2 <- match_vector_to_worms(d0$espece, widx, scientificname_full = snf)
      n_ <- sum(m2$matched, na.rm = TRUE)
      showNotification(
        paste("WoRMS :", n_, "/", nrow(d0), "ligne(s) avec un nom d’espèce reconnu."),
        type = "message",
        duration = 3
      )
    })
    observeEvent(input$catch_worms_skip, {
      d0 <- session$userData$catch_d0
      if (!is.null(d0)) {
        rv_c$df <- d0
        rv_c$espece_file <- d0$espece_fichier
        rv_c$sp_remap <- character(0)
      }
      removeModal()
    })
    ficheStagedForTransform <- shiny::reactive({
      if (!is.null(rv_c$err_import)) {
        return(NULL)
      }
      if (is.null(rv_c$df) || !nrow(as.data.frame(rv_c$df))) {
        return(NULL)
      }
      req(rv_c$df)
    })
    transformedData <- shiny::reactive({
      d <- ficheStagedForTransform()
      req(d)
      wraw <- wormsData()
      wselect <- tibble::as_tibble(d) %>%
        rename(
          scientificname = espece,
          catchweight    = ech_p,
          catchcount     = tot_n
        ) %>%
        dplyr::mutate(
          catchweight = dplyr::if_else(is.na(catchweight), tot_p, catchweight)
        ) %>%
        dplyr::arrange(serialnumber) %>%
        dplyr::group_by(serialnumber) %>%
        dplyr::mutate(catchsampleid = dplyr::row_number()) %>%
        dplyr::ungroup()
      if (is.null(wraw) || !nrow(wraw)) {
        wj <- tibble::tibble(
          scientificname = wselect$scientificname,
          aphia = NA, class = NA, family = NA, catchcategory = NA
        )
        out <- wselect
      } else {
        wj <- wraw
        sn <- stringi::stri_trans_general(names(wj), "Latin-ASCII") %>% tolower() %>%
          gsub("[^a-z0-9_]+", "_", .) %>% gsub("_+", "_", .) %>% gsub("^_|_$", "", .)
        names(wj) <- sn
        needj <- c("scientificname", "aphia", "class", "family", "catchcategory")
        for (c in setdiff(needj, names(wj))) wj[[c]] <- NA
        wj <- wj[, intersect(union(needj, names(wj)), names(wj)), drop = FALSE]
        wj <- wj[, needj, drop = FALSE]
        out <- wselect %>% dplyr::left_join(wj, by = "scientificname")
      }
      out <- out %>%
        dplyr::select(-dplyr::any_of(c("espece_fichier", "espece_pre_map")))
      out %>%
        dplyr::mutate(
          commonname = as.character(scientificname),
          conservation = 1L,
          catchproducttype = 1L,
          missionnumber = NA,
          catchpartnumber = 1L,
          sampletype = NA,
          sampleproducttype = NA,
          lengthmeasurement = NA,
          lengthsampleweight = NA,
          lengthsamplecount = NA,
          specimensamplecount = NA
        )
    })
    catchSamplesData <- shiny::reactive({
      tdf <- tryCatch(transformedData(), error = function(e) NULL)
      if (is.null(tdf) || is.null(nrow(tdf)) || !nrow(tdf)) {
        return(NULL)
      }
      tdf %>%
        mutate(
          agingstructure = NA,
          catchcomment   = NA,
          raisingfactor  = as.numeric(tot_p) / as.numeric(catchweight)
        ) %>%
        select(
          scientificname, commonname, serialnumber, catchsampleid,
          missionnumber, catchcategory, catchpartnumber, aphia,
          sampletype, conservation, catchproducttype, sampleproducttype,
          lengthmeasurement, lengthsampleweight, lengthsamplecount,
          specimensamplecount, catchweight, catchcount,
          agingstructure, catchcomment, raisingfactor
        )
    })
    output$catch_sp_badge <- renderUI({
      err <- rv_c$err_import
      if (length(err) && nzchar(err)) {
        return(
          div(
            class = "alert alert-warning",
            style = "margin-bottom:0.5rem; padding:0.5rem 0.75rem; font-size:0.9rem;",
            icon("circle-exclamation"), " ", err
          )
        )
      }
      d <- rv_c$df
      if (is.null(d) || !nrow(d)) {
        return(
          p(
            class = "text-muted small mb-2",
            "Aucun fichier Fiche‑SP chargé, ou en attente de validation."
          )
        )
      }
      widx_b <- tryCatch(widx_c(), error = function(e) NULL)
      if (is.null(widx_b) || !nrow(widx_b)) {
        return(
          div(
            class = "d-flex flex-wrap align-items-center gap-3 small mb-2 p-2 rounded border text-muted",
            "Référentiel WoRMS non chargé : indicateur de statut d’espèces indisponible (placez le fichier dans data/ )."
          )
        )
      }
      pre_pm <- if ("espece_pre_map" %in% names(d)) d$espece_pre_map else NULL
      wd_b <- tryCatch(wormsData(), error = function(e) NULL)
      sn_full <- if (!is.null(wd_b) && "scientificname" %in% names(wd_b)) {
        wd_b$scientificname
      } else {
        NULL
      }
      sumx <- species_line_summary(
        d$espece_fichier, d$espece, widx_b,
        pre_map = pre_pm, scientificname_full = sn_full
      )
      nsp <- if (!is.null(sumx$n_unresolved_species)) {
        sumx$n_unresolved_species
      } else {
        0L
      }
      unres_lbl <- if (sumx$n_unresolved <= 0L) {
        "0"
      } else if (nsp < sumx$n_unresolved) {
        paste0(nsp, " espèce(s) · ", sumx$n_unresolved, " ligne(s)")
      } else {
        as.character(sumx$n_unresolved)
      }
      div(
        class = "d-flex flex-wrap align-items-center gap-3 small mb-2 p-2 rounded border",
        style = "background: #fff;",
        tags$span(HTML("&#9989;"), " Validées (réf. WoRMS, sans reformatage) : ", sumx$n_validated),
        tags$span(HTML("&#9888;"), " Corrigées / reformatées (modale) : ", sumx$n_corrected),
        tags$span(HTML("&#10060;"), " Non reconnues (réf. WoRMS) : ", unres_lbl)
      )
    })
    output$previewCatch <- renderDT(
      {
        csd <- tryCatch(catchSamplesData(), error = function(e) NULL)
        if (is.null(csd) || (is.data.frame(csd) && !nrow(csd))) {
          if (!is.null(rv_c$err_import)) {
            return(data.frame(Info = "Correction des erreurs d’import ci-dessus."))
          }
          return(
            data.frame(Info = "Importez une Fiche‑SP, puis appliquiez le référentiel WoRMS si proposé.")
          )
        }
        csd
      },
      options = list(pageLength = 10, scrollX = TRUE)
    )
    output$downloadData <- downloadHandler(
      filename = function() { paste0("Fiche-SP_transforme_", Sys.Date(), ".xlsx") },
      content = function(file) {
        dat <- tryCatch(transformedData(), error = function(e) NULL)
        if (is.null(dat) || (is.data.frame(dat) && !nrow(dat))) {
          stop("Aucune donnée à exporter.")
        }
        writexl::write_xlsx(dat, path = file)
      }
    )
    output$downloadCatchsamples <- downloadHandler(
      filename = function() { "Catchsamples_LPA.xlsx" },
      content = function(file) {
        csd <- tryCatch(catchSamplesData(), error = function(e) NULL)
        if (is.null(csd) || !nrow(csd)) {
          stop("Aucune donnée prête. Importez d’abord un fichier Fiche‑SP valide.")
        }
        writexl::write_xlsx(csd, path = file)
      }
    )
  })
}

# --- 4. MODULE 3 : Individuals_LPA (contenu remplacé par app-individuals) ---
INDIV_FQ_REQUIRED <- c("station", "nombre", "poids_mens", "espece", "taille_cm")
INDIV_FQ_LABELS <- c(
  station    = "station",
  nombre     = "nombre",
  Poids_Mens = "poids_mens",
  espece     = "espece",
  taille_cm  = "taille_cm"
)
INDIV_BIO_REQUIRED <- c("espece", "station", "nombre", "poids_g", "sexe", "sms")
INDIV_BIO_LABELS <- c(
  espece  = "espece",
  station = "station",
  nombre  = "nombre",
  poids_g = "poids_g",
  sexe    = "sexe",
  sms     = "sms"
)
INDIV_EXPORT_COL_ORDER <- c(
  "scientificname", "length", "sex", "serialnumber", "specimenid",
  "maturationstage", "individualweight", "Poids_Mens", "catchsampleid",
  "individualcomment", "diameter", "missionnumber", "specialstage",
  "individualproducttype", "lengthresolution"
)

indiv_norm_name <- function(x) tolower(trimws(as.character(x)))

indiv_auto_map_columns <- function(file_cols, required_norm) {
  fc_norm <- indiv_norm_name(file_cols)
  names(fc_norm) <- file_cols
  out <- setNames(rep(NA_character_, length(required_norm)), required_norm)
  for (req in required_norm) {
    hit <- names(fc_norm)[fc_norm == req]
    if (length(hit) >= 1) out[[req]] <- hit[1]
  }
  out
}

indiv_required_fields_missing_map <- function(map_list, required_vec) {
  if (is.null(map_list)) return(required_vec)
  need <- vapply(required_vec, function(r) {
    val <- map_list[[r]]
    is.null(val) || length(val) == 0 || is.na(val) || !nzchar(as.character(val))
  }, FUN.VALUE = logical(1))
  required_vec[need]
}

indiv_standardize_df <- function(df, required_norm, mapping_list) {
  m <- unlist(mapping_list)
  m <- m[!is.na(m) & nzchar(as.character(m))]
  for (req in required_norm) {
    if (!req %in% names(m)) next
    src <- m[[req]]
    if (is.na(src) || !nzchar(as.character(src))) next
    hit <- which(names(df) == src)
    if (length(hit)) names(df)[hit[1]] <- req
  }
  missing <- setdiff(required_norm, names(df))
  list(df = df, missing = missing)
}

indiv_map_sexe_export <- function(x) {
  v <- toupper(trimws(as.character(x)))
  out <- rep(NA_real_, length(v))
  out[v == "M"] <- 1
  out[v == "F"] <- 2
  out[v == "I"] <- NA_real_
  num_ok <- suppressWarnings(as.numeric(v))
  na_mask <- is.na(out) & !is.na(num_ok)
  out[na_mask] <- num_ok[na_mask]
  out
}

mod_individuals_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel(
      title = div(
        class = "d-flex align-items-center gap-2",
        icon("fish"),
        span("Individuals LPA — Import, jointure et export", class = "fw-semibold")
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        class = "indiv-lpa-sidebar",
        h5("Fichiers Excel"),
        uiOutput(ns("indiv_worms_badge")),
        fileInput(ns("fq_file"),  "Import Length frequency file (.xlsx / .xls)", accept = c(".xlsx", ".xls"), buttonLabel = "Browse…"),
        fileInput(ns("bio_file"), "Import Biology file (.xlsx / .xls)",           accept = c(".xlsx", ".xls"), buttonLabel = "Browse…"),
        tags$style(HTML(
          ".indiv-lpa-sidebar { overflow-y: auto !important; max-height: calc(100vh - 120px); }
           .indiv-lpa-sidebar .form-group { margin-bottom: .5rem; }
           .indiv-actions { display:flex; flex-direction:column; gap:.4rem; margin-top:.25rem; margin-bottom:.5rem; }
           .indiv-actions .btn { margin:0 !important; }"
        )),
        div(class = "indiv-actions",
          actionButton(ns("btn_join"), "Joint Biology with Length", class = "btn-primary w-100", icon = icon("link")),
          downloadButton(ns("dl_individuals"), "Export Individuals_LPA.xlsx", class = "btn-success w-100")
        ),
        hr(style = "margin:.5rem 0;"),
        helpText(
          "Fusion : même station, même espece, et colonne nombre (Fq) avec colonne nombre (Biologie).",
          "Valeurs numériques ; si plusieurs lignes ont la même valeur k, 1re avec 1re, 2e avec 2e, etc."
        ),
        helpText("Export : transformations appliquées sur les lignes jointes (voir aperçu ci-dessous).")
      ),
      div(
        h4(icon("table"), "Aperçu des feuilles Individuals_LPA"),
        tabsetPanel(
          id = ns("indiv_preview_tabs"),
          tabPanel(
            "Length frequency",
            uiOutput(ns("fq_map_ui")),
            verbatimTextOutput(ns("fq_status")),
            div(class = "table-responsive",
                style = "max-height:500px; overflow:auto;",
                tableOutput(ns("preview_fq")))
          ),
          tabPanel(
            "Biology",
            uiOutput(ns("bio_map_ui")),
            verbatimTextOutput(ns("bio_status")),
            div(class = "table-responsive",
                style = "max-height:500px; overflow:auto;",
                tableOutput(ns("preview_bio")))
          ),
          tabPanel(
            "Length Biology merged",
            verbatimTextOutput(ns("join_status")),
            div(class = "table-responsive",
                style = "max-height:500px; overflow:auto;",
                tableOutput(ns("preview_joined")))
          ),
          tabPanel(
            "Export Individuals_LPA.xlsx",
            p("Aperçu des colonnes exportées (10 premières lignes) :", class = "text-muted small"),
            div(class = "table-responsive",
                style = "max-height:500px; overflow:auto;",
                tableOutput(ns("preview_export"))),
            hr(),
            h6("Ordre des colonnes dans le fichier"),
            tags$pre(class = "bg-light p-2 rounded small", paste(INDIV_EXPORT_COL_ORDER, collapse = ", "))
          )
        )
      )
    )
  )
}

mod_individuals_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(
      fq_raw = NULL, bio_raw = NULL,
      fq_path = NULL, bio_path = NULL,
      fq_map = NULL, bio_map = NULL,
      fq_std = NULL, bio_std = NULL,
      joined = NULL, join_error = NULL,
      fq_err = NULL, bio_err = NULL,
      indiv_sp_remap = NULL,
      indiv_worms_solved_fq_fp = NULL,
      indiv_worms_solved_bio_fp = NULL,
      indiv_worms_awaiting = FALSE
    )

    observeEvent(input$fq_file, {
      req(input$fq_file)
      rv$indiv_worms_solved_fq_fp <- NULL
      rv$indiv_worms_solved_bio_fp <- NULL
      rv$indiv_sp_remap <- NULL
      rv$indiv_worms_awaiting <- FALSE
      rv$fq_err <- NULL
      tryCatch({
        p <- input$fq_file$datapath
        rv$fq_raw <- readxl::read_excel(p)
        rv$fq_path <- p
        am <- indiv_map_by_slug(names(rv$fq_raw), INDIV_FQ_REQUIRED)
        rv$fq_map <- as.list(am)
        names(rv$fq_map) <- INDIV_FQ_REQUIRED
      }, error = function(e) {
        rv$fq_raw <- NULL
        rv$fq_err <- conditionMessage(e)
      })
    })

    observeEvent(input$bio_file, {
      req(input$bio_file)
      rv$indiv_worms_solved_bio_fp <- NULL
      rv$indiv_worms_awaiting <- FALSE
      rv$bio_err <- NULL
      tryCatch({
        p <- input$bio_file$datapath
        rv$bio_raw <- readxl::read_excel(p)
        rv$bio_path <- p
        am <- indiv_map_by_slug(names(rv$bio_raw), INDIV_BIO_REQUIRED)
        rv$bio_map <- as.list(am)
        names(rv$bio_map) <- INDIV_BIO_REQUIRED
      }, error = function(e) {
        rv$bio_raw <- NULL
        rv$bio_err <- conditionMessage(e)
      })
    })

    output$fq_map_ui <- renderUI({
      req(rv$fq_raw, rv$fq_map)
      if (!is.null(rv$fq_err)) {
        return(div(class = "text-danger small", paste("Erreur :", rv$fq_err)))
      }
      need <- indiv_required_fields_missing_map(rv$fq_map, INDIV_FQ_REQUIRED)
      if (length(need) == 0) {
        return(div(class = "text-success small mb-2", icon("check-circle"),
                   " Toutes les colonnes Fq_taille sont reconnues automatiquement."))
      }
      cn <- unique(names(rv$fq_raw))
      cols <- c("(aucune)" = "", stats::setNames(cn, cn))
      tagList(
        h6("Correspondance manuelle — Fq_taille (colonnes non détectées)"),
        lapply(need, function(req_) {
          lbl <- names(INDIV_FQ_LABELS)[INDIV_FQ_LABELS == req_]
          if (!length(lbl)) lbl <- req_
          sel <- rv$fq_map[[req_]]
          if (is.null(sel) || is.na(sel)) sel <- ""
          selectInput(
            inputId = ns(paste0("fq_map_", req_)),
            label = lbl[1],
            choices = cols,
            selected = if (sel %in% cols) sel else ""
          )
        })
      )
    })

    observe({
      req(rv$fq_raw)
      for (req_ in INDIV_FQ_REQUIRED) {
        id_ <- paste0("fq_map_", req_)
        if (!is.null(input[[id_]])) {
          v <- input[[id_]]
          rv$fq_map[[req_]] <- if (nzchar(v)) v else NA_character_
        }
      }
    })

    output$bio_map_ui <- renderUI({
      req(rv$bio_raw, rv$bio_map)
      if (!is.null(rv$bio_err)) {
        return(div(class = "text-danger small", paste("Erreur :", rv$bio_err)))
      }
      need <- indiv_required_fields_missing_map(rv$bio_map, INDIV_BIO_REQUIRED)
      if (length(need) == 0) {
        return(div(class = "text-success small mb-2", icon("check-circle"),
                   " Toutes les colonnes Biologie sont reconnues automatiquement."))
      }
      cn <- unique(names(rv$bio_raw))
      cols <- c("(aucune)" = "", stats::setNames(cn, cn))
      tagList(
        h6("Correspondance manuelle — Biologie (colonnes non détectées)"),
        lapply(need, function(req_) {
          lbl <- names(INDIV_BIO_LABELS)[INDIV_BIO_LABELS == req_]
          if (!length(lbl)) lbl <- req_
          sel <- rv$bio_map[[req_]]
          if (is.null(sel) || is.na(sel)) sel <- ""
          selectInput(
            inputId = ns(paste0("bio_map_", req_)),
            label = lbl[1],
            choices = cols,
            selected = if (sel %in% cols) sel else ""
          )
        })
      )
    })

    observe({
      req(rv$bio_raw)
      for (req_ in INDIV_BIO_REQUIRED) {
        id_ <- paste0("bio_map_", req_)
        if (!is.null(input[[id_]])) {
          v <- input[[id_]]
          rv$bio_map[[req_]] <- if (nzchar(v)) v else NA_character_
        }
      }
    })

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    fq_std_reactive <- reactive({
      req(rv$fq_raw, rv$fq_map)
      m <- rv$fq_map
      for (k in names(m)) {
        if (is.null(m[[k]]) || is.na(m[[k]])) m[[k]] <- NA_character_
      }
      indiv_standardize_df(rv$fq_raw, INDIV_FQ_REQUIRED, m)
    })

    bio_std_reactive <- reactive({
      req(rv$bio_raw, rv$bio_map)
      m <- rv$bio_map
      for (k in names(m)) {
        if (is.null(m[[k]]) || is.na(m[[k]])) m[[k]] <- NA_character_
      }
      indiv_standardize_df(rv$bio_raw, INDIV_BIO_REQUIRED, m)
    })

    # Empreintes séparées : validation WoRMS indépendante pour chaque fichier (avant jointure).
    indiv_worms_fp_fq <- shiny::reactive({
      if (is.null(rv$fq_raw) || is.null(rv$fq_map)) {
        return(invisible(NULL))
      }
      if (length(indiv_required_fields_missing_map(rv$fq_map, INDIV_FQ_REQUIRED)) > 0) {
        return(invisible(NULL))
      }
      s1 <- tryCatch(fq_std_reactive(), error = function(e) NULL)
      if (is.null(s1) || length(s1$missing)) {
        return(invisible(NULL))
      }
      paste0("FQ|", rv$fq_path %||% "", "|", paste(unlist(rv$fq_map), collapse = "§"))
    })
    indiv_worms_fp_bio <- shiny::reactive({
      if (is.null(rv$bio_raw) || is.null(rv$bio_map)) {
        return(invisible(NULL))
      }
      if (length(indiv_required_fields_missing_map(rv$bio_map, INDIV_BIO_REQUIRED)) > 0) {
        return(invisible(NULL))
      }
      s2 <- tryCatch(bio_std_reactive(), error = function(e) NULL)
      if (is.null(s2) || length(s2$missing)) {
        return(invisible(NULL))
      }
      paste0("BIO|", rv$bio_path %||% "", "|", paste(unlist(rv$bio_map), collapse = "§"))
    })
    indiv_fq_worms_df <- shiny::reactive({
      s <- tryCatch(fq_std_reactive(), error = function(e) NULL)
      if (is.null(s) || length(s$missing)) {
        return(NULL)
      }
      m <- if (is.null(rv$indiv_sp_remap)) {
        character(0)
      } else {
        rv$indiv_sp_remap
      }
      indiv_apply_espece_workflow(s$df, m)
    })
    indiv_bio_worms_df <- shiny::reactive({
      s <- tryCatch(bio_std_reactive(), error = function(e) NULL)
      if (is.null(s) || length(s$missing)) {
        return(NULL)
      }
      m <- if (is.null(rv$indiv_sp_remap)) {
        character(0)
      } else {
        rv$indiv_sp_remap
      }
      indiv_apply_espece_workflow(s$df, m)
    })
    # WoRMS : fichier Length frequency (Fq_taille) seul, avant jointure
    observe({
      fp <- indiv_worms_fp_fq()
      if (is.null(fp) || !nzchar(fp)) {
        return()
      }
      if (!is.null(rv$indiv_worms_solved_fq_fp) && identical(fp, rv$indiv_worms_solved_fq_fp)) {
        return()
      }
      if (isTRUE(rv$indiv_worms_awaiting)) {
        return()
      }
      s1 <- tryCatch(fq_std_reactive(), error = function(e) NULL)
      if (is.null(s1) || is.null(s1$df) || length(s1$missing)) {
        return()
      }
      wdf <- tryCatch(worms_read_reference(APP_DIR), error = function(e) NULL)
      widx <- if (!is.null(wdf) && nrow(wdf)) {
        tryCatch(worms_index_canonical(wdf), error = function(e) NULL)
      } else {
        NULL
      }
      if (is.null(widx) || !nrow(widx)) {
        showNotification(
          "Fichier WoRMS indisponible (dossier data/) — validation Fq ignorée pour Individuals.",
          type = "warning", duration = 6
        )
        rv$indiv_worms_solved_fq_fp <- fp
        return()
      }
      rv$indiv_worms_awaiting <- TRUE
      showModal(
        modalDialog(
          title = "Validation WoRMS",
          div(
            class = "text-center p-4",
            div(
              class = "spinner-border text-primary",
              role = "status",
              tags$span(class = "visually-hidden", "Chargement")
            ),
            p(class = "mt-3 mb-0 small", "Vérification des noms d'espèces (WoRMS) en cours…")
          ),
          easyClose = FALSE, footer = NULL, size = "m"
        ),
        session = session
      )
      s1_l <- s1
      fp_l <- fp
      wdf_l <- wdf
      widx_l <- widx
      later::later(
        function() {
          if (is.null(s1_l) || is.null(s1_l$df)) {
            removeModal(session = session)
            rv$indiv_worms_awaiting <- FALSE
            rv$indiv_worms_solved_fq_fp <- fp_l
            return()
          }
          e1 <- norm_species_binomial(s1_l$df$espece)
          u <- unique(as.character(stats::na.omit(e1)))
          u <- u[nzchar(u, keepNA = TRUE) & !is.na(u)]
          m_w <- match_vector_to_worms(u, widx_l, scientificname_full = wdf_l$scientificname)
          um <- u[!m_w$matched & nzchar(u, keepNA = TRUE)]
          if (length(um) == 0) {
            removeModal(session = session)
            rv$indiv_worms_solved_fq_fp <- fp_l
            rv$indiv_worms_awaiting <- FALSE
            return()
          }
          ch_choices_i <- tryCatch(
            worms_select_choices(wdf_l$scientificname),
            error = function(e) NULL
          )
          if (is.null(ch_choices_i) || !length(ch_choices_i)) {
            removeModal(session = session)
            rv$indiv_worms_awaiting <- FALSE
            rv$indiv_worms_solved_fq_fp <- fp_l
            showNotification(
              "Préparation de la liste WoRMS impossible — vérifiez le référentiel dans data/.",
              type = "error", duration = 8
            )
            return()
          }
          w_txt <- p("Fichier Length frequency (Fq_taille) mappé. Saisissez le début d’un ", tags$em("scientificname"), " WoRMS pour filtrer. La 1re option conserve le libellé importé.", class = "small text-muted")
          showModal(
            modalDialog(
              title = "Espèces non reconnues (WoRMS) — Fq_taille / Individuals",
              w_txt,
              tagList(
                lapply(
                  seq_along(um),
                  function(i) {
                    uu <- um[i]
                    selectizeInput(
                      inputId = ns(paste0("indiv_worm_", i)),
                      label = tagList(strong(htmltools::htmlEscape(uu)), " — WoRMS (scientificname) :"),
                      choices = ch_choices_i,
                      width = "100%",
                      options = list(
                        placeholder = "Taper pour filtrer (scientificname)…",
                        maxOptions = 50000L,
                        maxItems = 1L
                      )
                    )
                  }
                )
              ),
              footer = tagList(
                actionButton(ns("indiv_worms_apply"), "Appliquer", class = "btn btn-primary"),
                actionButton(ns("indiv_worms_skip"), "Continuer sans correction", class = "btn btn-outline-secondary")
              ),
              size = "l", easyClose = FALSE
            ),
            session = session
          )
          session$userData$indiv_worms_scope <- "fq"
          session$userData$indiv_um <- um
          session$userData$indiv_w_fp <- fp_l
        },
        delay = 0.08
      )
    })
    # WoRMS : fichier Biologie seul, indépendamment du Fq (avant jointure)
    observe({
      fp <- indiv_worms_fp_bio()
      if (is.null(fp) || !nzchar(fp)) {
        return()
      }
      if (!is.null(rv$indiv_worms_solved_bio_fp) && identical(fp, rv$indiv_worms_solved_bio_fp)) {
        return()
      }
      if (isTRUE(rv$indiv_worms_awaiting)) {
        return()
      }
      s2 <- tryCatch(bio_std_reactive(), error = function(e) NULL)
      if (is.null(s2) || is.null(s2$df) || length(s2$missing)) {
        return()
      }
      wdf <- tryCatch(worms_read_reference(APP_DIR), error = function(e) NULL)
      widx <- if (!is.null(wdf) && nrow(wdf)) {
        tryCatch(worms_index_canonical(wdf), error = function(e) NULL)
      } else {
        NULL
      }
      if (is.null(widx) || !nrow(widx)) {
        showNotification(
          "Fichier WoRMS indisponible (dossier data/) — validation Biologie ignorée pour Individuals.",
          type = "warning", duration = 6
        )
        rv$indiv_worms_solved_bio_fp <- fp
        return()
      }
      rv$indiv_worms_awaiting <- TRUE
      showModal(
        modalDialog(
          title = "Validation WoRMS",
          div(
            class = "text-center p-4",
            div(
              class = "spinner-border text-primary",
              role = "status",
              tags$span(class = "visually-hidden", "Chargement")
            ),
            p(class = "mt-3 mb-0 small", "Vérification des noms d'espèces (WoRMS) en cours…")
          ),
          easyClose = FALSE, footer = NULL, size = "m"
        ),
        session = session
      )
      s2_l <- s2
      fp_l <- fp
      wdf_l <- wdf
      widx_l <- widx
      later::later(
        function() {
          if (is.null(s2_l) || is.null(s2_l$df)) {
            removeModal(session = session)
            rv$indiv_worms_awaiting <- FALSE
            rv$indiv_worms_solved_bio_fp <- fp_l
            return()
          }
          e2 <- norm_species_binomial(s2_l$df$espece)
          u <- unique(as.character(stats::na.omit(e2)))
          u <- u[nzchar(u, keepNA = TRUE) & !is.na(u)]
          m_w <- match_vector_to_worms(u, widx_l, scientificname_full = wdf_l$scientificname)
          um <- u[!m_w$matched & nzchar(u, keepNA = TRUE)]
          if (length(um) == 0) {
            removeModal(session = session)
            rv$indiv_worms_solved_bio_fp <- fp_l
            rv$indiv_worms_awaiting <- FALSE
            return()
          }
          ch_choices_i <- tryCatch(
            worms_select_choices(wdf_l$scientificname),
            error = function(e) NULL
          )
          if (is.null(ch_choices_i) || !length(ch_choices_i)) {
            removeModal(session = session)
            rv$indiv_worms_awaiting <- FALSE
            rv$indiv_worms_solved_bio_fp <- fp_l
            showNotification(
              "Préparation de la liste WoRMS impossible — vérifiez le référentiel dans data/.",
              type = "error", duration = 8
            )
            return()
          }
          w_txt <- p("Fichier Biologie mappé. Saisissez le début d’un ", tags$em("scientificname"), " WoRMS pour filtrer. La 1re option conserve le libellé importé.", class = "small text-muted")
          showModal(
            modalDialog(
              title = "Espèces non reconnues (WoRMS) — Biologie / Individuals",
              w_txt,
              tagList(
                lapply(
                  seq_along(um),
                  function(i) {
                    uu <- um[i]
                    selectizeInput(
                      inputId = ns(paste0("indiv_worm_", i)),
                      label = tagList(strong(htmltools::htmlEscape(uu)), " — WoRMS (scientificname) :"),
                      choices = ch_choices_i,
                      width = "100%",
                      options = list(
                        placeholder = "Taper pour filtrer (scientificname)…",
                        maxOptions = 50000L,
                        maxItems = 1L
                      )
                    )
                  }
                )
              ),
              footer = tagList(
                actionButton(ns("indiv_worms_apply"), "Appliquer", class = "btn btn-primary"),
                actionButton(ns("indiv_worms_skip"), "Continuer sans correction", class = "btn btn-outline-secondary")
              ),
              size = "l", easyClose = FALSE
            ),
            session = session
          )
          session$userData$indiv_worms_scope <- "bio"
          session$userData$indiv_um <- um
          session$userData$indiv_w_fp <- fp_l
        },
        delay = 0.08
      )
    })
    observeEvent(input$indiv_worms_apply, {
      um <- session$userData$indiv_um
      fp <- session$userData$indiv_w_fp
      sc <- session$userData$indiv_worms_scope
      if (is.null(um) || is.null(session$userData$indiv_w_fp)) {
        if (isTRUE(rv$indiv_worms_awaiting)) {
          removeModal()
        }
        rv$indiv_worms_awaiting <- FALSE
        return()
      }
      mp <- character(0)
      for (i in seq_along(um)) {
        id_i <- paste0("indiv_worm_", i)
        sel <- input[[id_i]]
        if (length(sel) && nzchar(sel[1L])) {
          mp[as.character(um[i])] <- as.character(sel[1L])
        }
      }
      if (length(mp)) {
        oldm <- if (is.null(rv$indiv_sp_remap)) {
          character(0)
        } else {
          rv$indiv_sp_remap
        }
        merged <- c(oldm, mp)
        wdup <- !duplicated(names(merged), fromLast = TRUE)
        rv$indiv_sp_remap <- merged[wdup]
      }
      if (identical(sc, "bio")) {
        rv$indiv_worms_solved_bio_fp <- fp
      } else {
        rv$indiv_worms_solved_fq_fp <- fp
      }
      rv$indiv_worms_awaiting <- FALSE
      removeModal()
    }, ignoreInit = TRUE)
    observeEvent(input$indiv_worms_skip, {
      fp <- session$userData$indiv_w_fp
      sc <- session$userData$indiv_worms_scope
      if (!is.null(fp)) {
        if (identical(sc, "bio")) {
          rv$indiv_worms_solved_bio_fp <- fp
        } else {
          rv$indiv_worms_solved_fq_fp <- fp
        }
      }
      rv$indiv_worms_awaiting <- FALSE
      removeModal()
    }, ignoreInit = TRUE)

    output$indiv_worms_badge <- renderUI({
      s1 <- tryCatch(fq_std_reactive(), error = function(e) NULL)
      s2 <- tryCatch(bio_std_reactive(), error = function(e) NULL)
      if (is.null(s1) || is.null(s2) || length(s1$missing) || length(s2$missing)) {
        return(NULL)
      }
      f1 <- tryCatch(indiv_fq_worms_df(), error = function(e) NULL)
      f2 <- tryCatch(indiv_bio_worms_df(), error = function(e) NULL)
      if (is.null(f1) || is.null(f2)) {
        return(NULL)
      }
      widx <- tryCatch(
        worms_index_canonical(worms_read_reference(APP_DIR)),
        error = function(e) NULL
      )
      if (is.null(widx) || !nrow(widx)) {
        return(
          div(
            class = "small text-muted mb-2",
            "WoRMS : référentiel non chargé (dossier data/)."
          )
        )
      }
      raws <- c(as.character(s1$df$espece), as.character(s2$df$espece))
      fins <- c(as.character(f1$espece), as.character(f2$espece))
      pre_m <- c(
        if ("espece_pre_map" %in% names(f1)) {
          as.character(f1$espece_pre_map)
        } else {
          as.character(f1$espece)
        },
        if ("espece_pre_map" %in% names(f2)) {
          as.character(f2$espece_pre_map)
        } else {
          as.character(f2$espece)
        }
      )
      wdf_i <- tryCatch(worms_read_reference(APP_DIR), error = function(e) NULL)
      sn_full_i <- if (!is.null(wdf_i) && "scientificname" %in% names(wdf_i)) {
        wdf_i$scientificname
      } else {
        NULL
      }
      sumx <- species_line_summary(
        raws, fins, widx, pre_map = pre_m, scientificname_full = sn_full_i
      )
      nsp_i <- if (!is.null(sumx$n_unresolved_species)) sumx$n_unresolved_species else 0L
      unres_i <- if (sumx$n_unresolved <= 0L) {
        "0"
      } else if (nsp_i < sumx$n_unresolved) {
        paste0(nsp_i, " espèce(s) · ", sumx$n_unresolved, " ligne(s)")
      } else {
        as.character(sumx$n_unresolved)
      }
      div(
        class = "small mb-2 p-2 rounded border",
        style = "background:#f8f9fa;",
        tags$span(HTML("&#9989;"), " Validées : ", sumx$n_validated), " · ",
        tags$span(HTML("&#9888;"), " Corrigées : ", sumx$n_corrected), " · ",
        tags$span(HTML("&#10060;"), " Non reconnues : ", unres_i)
      )
    })

    output$fq_status <- renderText({
      if (!is.null(rv$fq_err)) return(paste("Erreur lecture Fq :", rv$fq_err))
      req(fq_std_reactive())
      s <- fq_std_reactive()
      if (length(s$missing)) {
        paste("Colonnes obligatoires manquantes :", paste(s$missing, collapse = ", "),
              "\nVeuillez compléter le mapping affiché ci-dessus.")
      } else {
        "Fq_taille : prêt (colonnes normalisées)."
      }
    })

    output$bio_status <- renderText({
      if (!is.null(rv$bio_err)) return(paste("Erreur lecture Biologie :", rv$bio_err))
      req(bio_std_reactive())
      s <- bio_std_reactive()
      if (length(s$missing)) {
        paste("Colonnes obligatoires manquantes :", paste(s$missing, collapse = ", "),
              "\nVeuillez compléter le mapping affiché ci-dessus.")
      } else {
        "Biologie : prêt (colonnes normalisées)."
      }
    })

    output$preview_fq <- renderTable({
      df2 <- indiv_fq_worms_df()
      if (!is.null(df2)) {
        return(head(df2, 15))
      }
      s <- tryCatch(fq_std_reactive(), error = function(e) NULL)
      if (is.null(s) || length(s$missing)) {
        return(NULL)
      }
      head(s$df, 15)
    }, striped = TRUE)

    output$preview_bio <- renderTable({
      df2 <- indiv_bio_worms_df()
      if (!is.null(df2)) {
        return(head(df2, 15))
      }
      s <- tryCatch(bio_std_reactive(), error = function(e) NULL)
      if (is.null(s) || length(s$missing)) {
        return(NULL)
      }
      head(s$df, 15)
    }, striped = TRUE)

    observeEvent(input$btn_join, {
      rv$join_error <- NULL
      rv$joined <- NULL
      fq  <- tryCatch(fq_std_reactive(),  error = function(e) NULL)
      bio <- tryCatch(bio_std_reactive(), error = function(e) NULL)
      if (is.null(fq) || is.null(bio)) {
        rv$join_error <- "Chargez les deux fichiers et complétez le mapping."; return()
      }
      if (length(fq$missing) || length(bio$missing)) {
        rv$join_error <- "Mapping incomplet : vérifiez les colonnes obligatoires pour les deux tableaux."; return()
      }
      d1 <- tryCatch(indiv_fq_worms_df(),  error = function(e) NULL)
      d2 <- tryCatch(indiv_bio_worms_df(), error = function(e) NULL)
      if (is.null(d1) || is.null(d2) || !nrow(d1) || !nrow(d2)) {
        rv$join_error <- "Préparez Fq et Biologie (normalisation d’espèces impossible). Vérifiez les colonnes requises."
        return()
      }
      n_fq <- nrow(d1)
      join_attempt <- tryCatch({
        d1 <- d1 %>%
          mutate(
            station = as.character(station),
            espece = as.character(espece),
            .k_nombre = suppressWarnings(as.numeric(nombre))
          ) %>%
          group_by(station, espece, .k_nombre) %>%
          mutate(.rn_pair = dplyr::row_number()) %>%
          ungroup()
        d2 <- d2 %>%
          mutate(
            station = as.character(station),
            espece = as.character(espece),
            .k_nombre = suppressWarnings(as.numeric(nombre))
          ) %>%
          dplyr::filter(!is.na(station), !is.na(espece), !is.na(nombre), !is.na(.k_nombre)) %>%
          group_by(station, espece, .k_nombre) %>%
          mutate(.rn_pair = dplyr::row_number()) %>%
          ungroup()
        left_join(
          d1, d2,
          by = c("station", "espece", ".k_nombre", ".rn_pair"),
          suffix = c("_fq", "_bio")
        ) %>%
          dplyr::select(-dplyr::any_of(c(".k_nombre", ".rn_pair")))
      }, error = function(e) {
        rv$join_error <<- paste("Erreur fusion :", conditionMessage(e))
        NULL
      })
      if (is.null(join_attempt)) {
        shinyalert::shinyalert(
          title = "Erreur de jointure",
          text  = rv$join_error %||% "Impossible d'insérer la Biologie dans Fq_taille.",
          type  = "error"
        )
        return()
      }
      if (nrow(join_attempt) != n_fq) {
        rv$join_error <- paste0(
          "Le nombre de lignes (", nrow(join_attempt), ") ne correspond pas au fichier Fq (",
          n_fq, "). Vérifiez les doublons (station, espece, nombre) dans Biologie."
        )
        rv$joined <- join_attempt
        shinyalert::shinyalert(
          title = "Jointure partielle",
          text  = rv$join_error,
          type  = "warning"
        )
        return()
      }
      rv$joined <- join_attempt
      shinyalert::shinyalert(
        title = "Jointure réussie",
        text  = paste0("Biologie insérée dans Fq_taille avec succès (", n_fq, " lignes)."),
        type  = "success",
        timer = 2500,
        showConfirmButton = FALSE
      )
      showNotification(
        tagList(icon("check-circle"), " Biologie insérée dans Fq_taille (", n_fq, " lignes)."),
        type = "message", duration = 4
      )
    })

    output$join_status <- renderText({
      if (!is.null(rv$join_error) && nzchar(rv$join_error %||% "")) return(rv$join_error)
      if (is.null(rv$joined)) return("Cliquez sur « Insérer Biologie dans Fq_taille » après import et mapping.")
      paste("Lignes (identiques au fichier Fq_taille) :", nrow(rv$joined))
    })

    output$preview_joined <- renderTable({
      req(rv$joined)
      head(rv$joined, 15)
    }, striped = TRUE)

    export_df <- reactive({
      req(rv$joined)
      j <- rv$joined
      pick <- function(cands) {
        for (c in cands) if (c %in% names(j)) return(j[[c]])
        rep(NA, nrow(j))
      }
      taille         <- pick(c("taille_cm", "taille_cm_fq"))
      poids_g        <- pick(c("poids_g", "poids_g_bio"))
      espece         <- pick(c("espece", "espece_fq", "espece_bio"))
      station        <- pick(c("station", "station_fq", "station_bio"))
      nombre         <- pick(c("nombre_fq", "nombre"))
      poids_mens_col <- pick(c("poids_mens", "poids_mens_fq"))
      sms            <- pick(c("sms", "sms_bio"))
      sexe_col       <- pick(c("sexe", "sexe_bio"))

      out <- tibble(
        scientificname        = as.character(espece),
        length                = round(suppressWarnings(as.numeric(taille)) * 2) / 2 / 100,
        sex                   = indiv_map_sexe_export(sexe_col),
        serialnumber          = as.character(station),
        specimenid            = suppressWarnings(as.integer(nombre)),
        maturationstage       = as.character(sms),
        individualweight      = suppressWarnings(as.numeric(poids_g)) / 1000,
        Poids_Mens            = suppressWarnings(as.numeric(poids_mens_col)),
        catchsampleid         = NA_character_,
        individualcomment     = NA_character_,
        diameter              = NA_real_,
        missionnumber         = NA_character_,
        specialstage          = NA_character_,
        individualproducttype = 1L,
        lengthresolution      = 2L
      )
      out <- out[, INDIV_EXPORT_COL_ORDER]
      out
    })

    output$preview_export <- renderTable({
      req(rv$joined)
      validate(need(nrow(rv$joined) > 0, "Jointure vide : rien à exporter."))
      head(export_df(), 10)
    }, striped = TRUE)

    output$dl_individuals <- downloadHandler(
      filename = function() "Individuals_LPA.xlsx",
      content  = function(file) {
        if (is.null(rv$joined) || nrow(rv$joined) == 0) {
          stop("Aucune donnée jointe à exporter. Vérifiez la jointure.")
        }
        writexl::write_xlsx(list(Individuals = export_df()), path = file)
      }
    )
  })
}


# --- 5. MODULE 4 : Biotic_LPA ---
mod_biotic_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(tagList(icon("database", class = "me-2"), "Biotic_LPA")),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("in_missions"), "Missions_LPA.xlsx",     accept = ".xlsx", buttonLabel = "Browse…"),
        fileInput(ns("in_fish"),     "Fishstations_LPA.xlsx",  accept = ".xlsx", buttonLabel = "Browse…"),
        fileInput(ns("in_catch"),    "Catchesamples_LPA.xlsx", accept = ".xlsx", buttonLabel = "Browse…"),
        fileInput(ns("in_indiv"),    "Individuals_LPA.xlsx",   accept = ".xlsx", buttonLabel = "Browse…"),
        actionButton(ns("process"), "Injecter les données", class = "btn-primary w-100 mb-2"),
        downloadButton(ns("download_biotic"), "Télécharger Biotic_LPA", class = "btn-success w-100"),
        width = 3
      ),
      mainPanel(
        h4(icon("table"), "Aperçu des fichiers Biotic_LPA (5 premières lignes à l'import)"),
        tabsetPanel(
          id = ns("biotic_preview_tabs"),
          tabPanel(
            "Missions",
            div(style = "max-height:500px; overflow:auto;",
                DTOutput(ns("preview_missions")))
          ),
          tabPanel(
            "Fishstations",
            div(style = "max-height:500px; overflow:auto;",
                DTOutput(ns("preview_fish")))
          ),
          tabPanel(
            "Catchesamples",
            div(style = "max-height:500px; overflow:auto;",
                DTOutput(ns("preview_catch")))
          ),
          tabPanel(
            "Individuals",
            div(style = "max-height:500px; overflow:auto;",
                DTOutput(ns("preview_indiv")))
          )
        ),
        br(),
        h4(icon("database"), "Aperçu des données injectées"),
        tabsetPanel(
          id = ns("biotic_full_tabs"),
          tabPanel("Missions",       div(style = "max-height:500px; overflow:auto;", DTOutput(ns("tbl_missions")))),
          tabPanel("Fishstations",   div(style = "max-height:500px; overflow:auto;", DTOutput(ns("tbl_fish")))),
          tabPanel("Catchesamples",  div(style = "max-height:500px; overflow:auto;", DTOutput(ns("tbl_catch")))),
          tabPanel("Individuals",    div(style = "max-height:500px; overflow:auto;", DTOutput(ns("tbl_indiv"))))
        ),
        width = 9
      )
    )
  )
}
mod_biotic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$in_missions, {
      df <- read_excel(input$in_missions$datapath)
      output$preview_missions <- renderDT({ datatable(head(df,5), options=list(dom='t',paging=FALSE)) })
    })
    observeEvent(input$in_fish, {
      df <- read_excel(input$in_fish$datapath)
      output$preview_fish <- renderDT({ datatable(head(df,5), options=list(dom='t',paging=FALSE)) })
    })
    observeEvent(input$in_catch, {
      df <- read_excel(input$in_catch$datapath)
      output$preview_catch <- renderDT({ datatable(head(df,5), options=list(dom='t',paging=FALSE)) })
    })
    observeEvent(input$in_indiv, {
      df <- read_excel(input$in_indiv$datapath)
      output$preview_indiv <- renderDT({ datatable(head(df,5), options=list(dom='t',paging=FALSE)) })
    })
    data_in <- eventReactive(input$process, {
      req(input$in_missions, input$in_fish, input$in_catch, input$in_indiv)
      withProgress(message="Lecture des fichiers...", value=0, {
        incProgress(0.25)
        missions     <- read_excel(input$in_missions$datapath)
        fishstations <- read_excel(input$in_fish$datapath)
        incProgress(0.25)
        catchesamples<- read_excel(input$in_catch$datapath)
        individuals  <- read_excel(input$in_indiv$datapath)
        incProgress(0.25)
        fmt_numeric <- function(df) {
          df[] <- lapply(df, function(col) {
            if(is.character(col) && all(is.na(col)|!is.na(suppressWarnings(as.numeric(col))))) as.numeric(col) else col
          }); df
        }
        missions     <- fmt_numeric(missions)
        fishstations <- fmt_numeric(fishstations)
        catchesamples<- fmt_numeric(catchesamples)
        individuals  <- fmt_numeric(individuals)
        if ("specimenid" %in% colnames(individuals)) {
          individuals$specimenid <- suppressWarnings(as.integer(individuals$specimenid))
        }
        if ("startyear" %in% colnames(missions)) {
          missions$startyear <- suppressWarnings(as.integer(missions$startyear))
        }
        incProgress(0.1)
        if("missionnumber" %in% colnames(missions)) {
          if(!"missionnumber" %in% colnames(individuals)) {
            individuals <- individuals %>% mutate(missionnumber = missions$missionnumber)
          } else {
            individuals$missionnumber <- missions$missionnumber
          }
        }
        incProgress(0.1)
        if("missionnumber" %in% colnames(missions)) {
          catchesamples$missionnumber <- missions$missionnumber
        }
        incProgress(0.1)
        .fmt_sp <- function(x) {
          v <- gsub("\\s+", " ", trimws(as.character(x)))
          vapply(v, function(s) {
            if (is.na(s) || !nzchar(s)) return(s)
            parts <- tolower(strsplit(s, " ", fixed = TRUE)[[1]])
            parts[1] <- paste0(toupper(substr(parts[1], 1, 1)),
                               substr(parts[1], 2, nchar(parts[1])))
            paste(parts, collapse = " ")
          }, character(1), USE.NAMES = FALSE)
        }
        if ("scientificname" %in% colnames(individuals))
          individuals$scientificname  <- .fmt_sp(individuals$scientificname)
        if ("scientificname" %in% colnames(catchesamples))
          catchesamples$scientificname <- .fmt_sp(catchesamples$scientificname)
        if(all(c("scientificname","serialnumber") %in% colnames(individuals))) {
          .norm_key <- function(sp, sn) {
            sp <- tolower(trimws(as.character(sp)))
            sn <- trimws(as.character(sn))
            paste(sp, sn, sep = "||")
          }
          k_ind <- .norm_key(individuals$scientificname, individuals$serialnumber)
          k_cat <- .norm_key(catchesamples$scientificname, catchesamples$serialnumber)
          matched <- catchesamples$catchsampleid[match(k_ind, k_cat)]
          if ("catchsampleid" %in% colnames(individuals)) {
            individuals$catchsampleid <- dplyr::coalesce(matched, individuals$catchsampleid)
          } else {
            individuals$catchsampleid <- matched
          }
          n_miss <- sum(is.na(individuals$catchsampleid))
          if (n_miss > 0) {
            showNotification(
              paste0("Attention : ", n_miss, " ligne(s) d'Individuals sans correspondance (scientificname + serialnumber) dans Catchsamples."),
              type = "warning", duration = 8
            )
          }
        }
        incProgress(0.1)
        summary_indiv <- individuals %>%
          filter(!is.na(catchsampleid)&!is.na(serialnumber)) %>%
          group_by(serialnumber, catchsampleid) %>%
          summarise(
            poids = first(Poids_Mens),
            max_specimen = max(specimenid, na.rm=TRUE),
            .groups="drop"
          )
        for(col in c("sampleproducttype","lengthmeasurement","lengthsampleweight","lengthsamplecount","specimensamplecount","sampletype")){
          if(!col %in% colnames(catchesamples)) catchesamples[[col]] <- NA
        }
        catchesamples <- catchesamples %>%
          left_join(summary_indiv, by=c("serialnumber","catchsampleid")) %>%
          mutate(
            sampleproducttype   = if_else(!is.na(poids),1L,sampleproducttype),
            lengthmeasurement   = if_else(!is.na(poids),"E",lengthmeasurement),
            lengthsampleweight  = if_else(!is.na(poids),poids,lengthsampleweight),
            lengthsamplecount   = if_else(!is.na(poids),max_specimen,lengthsamplecount),
            specimensamplecount = if_else(!is.na(poids),max_specimen,specimensamplecount),
            sampletype          = if_else(!is.na(poids),20L,sampletype)
          ) %>% select(-poids,-max_specimen)
        incProgress(0.25)
        if(all(c("scientificname","Poids_Mens") %in% colnames(individuals))) {
          individuals <- individuals %>% select(-scientificname, -Poids_Mens)
        }
        list(missions=missions, fishstations=fishstations, catchesamples=catchesamples, individuals=individuals)
      })
    })
    output$tbl_missions <- renderDT({ req(data_in()); datatable(data_in()$missions) })
    output$tbl_fish     <- renderDT({ req(data_in()); datatable(data_in()$fishstations) })
    output$tbl_catch    <- renderDT({ req(data_in()); datatable(data_in()$catchesamples) })
    output$tbl_indiv    <- renderDT({ req(data_in()); datatable(data_in()$individuals) })
    output$download_biotic <- downloadHandler(
      filename = function(){"Biotic_LPA.xlsx"},
      content = function(file){
        dfs <- data_in()
        # Alignement EXACT sur Bioltic_valide_model.xlsx : toutes les colonnes
        # sont écrites en texte (inlineStr) pour éviter les erreurs de types
        # dans Stox (ex: specimenid / startyear integer vs character).
        .to_chr <- function(x) {
          if (is.numeric(x)) {
            # Évite la notation scientifique et les ".0" parasites sur entiers.
            s <- format(x, scientific = FALSE, trim = TRUE, nsmall = 0, justify = "none")
            s <- sub("\\.0+$", "", s)
          } else {
            s <- as.character(x)
          }
          s <- trimws(s)
          s[is.na(x) | !nzchar(s) | s == "NA"] <- NA_character_
          s
        }
        .all_to_char <- function(df) {
          if (is.null(df) || ncol(df) == 0) return(df)
          for (c in colnames(df)) df[[c]] <- .to_chr(df[[c]])
          df
        }
        dfs$missions      <- .all_to_char(dfs$missions)
        dfs$fishstations  <- .all_to_char(dfs$fishstations)
        dfs$catchesamples <- .all_to_char(dfs$catchesamples)
        dfs$individuals   <- .all_to_char(dfs$individuals)
        write_xlsx(x=list(
          missions=dfs$missions, fishstations=dfs$fishstations,
          catchesamples=dfs$catchesamples, individuals=dfs$individuals
        ), path=file)
      }
    )
  })
}

# --- 6. UI principale & routing par onglets avec icônes ---
ui <- tagList(
  tags$head(
    tags$style(
      htmltools::HTML("/* Selectize dans les modales : menu au-dessus de la modale (bslib 5) */\n.modal .selectize-dropdown { z-index: 2005 !important; max-height: 50vh; overflow: auto; }")
    )
  ),
  navbarPage(
  title = div(icon("database"), "LPA_Data_To_Biotic-Format"),
  theme = theme_modern,
  tabPanel(tagList(icon("location-dot"),   "Fishstations_LPA"), mod_fishstations_ui("fish")),
  tabPanel(tagList(icon("ship"),           "Catchsamples_LPA"), mod_catchsamples_ui("catch")),
  tabPanel(tagList(icon("fish"),           "Individuals_LPA"),  mod_individuals_ui("indiv")),
  tabPanel(tagList(icon("database"),       "Biotic_LPA"),       mod_biotic_ui("biotic"))
  )
)

server <- function(input, output, session) {
  mod_fishstations_server("fish")
  mod_catchsamples_server("catch")
  mod_individuals_server("indiv")
  mod_biotic_server("biotic")
}

shinyApp(ui, server)
