# app.R — Combined Shiny application for Fishstations_LPA, Catchsamples_LPA, Individuals_LPA, Biotic_LPA
# Each part of the workflow is a separate Shiny module

# --- 0. Packages ---
library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(writexl)
library(DT)
library(rhandsontable)
library(stringi)
library(shinycssloaders)
library(shinyalert)
library(tibble)
library(bslib)
library(tools)
library(later)

# --- 0.bis Locate the application folder ---
APP_DIR <- local({
  cand <- character(0)
  # 1) File sourced with source()/runApp()
  try(cand <- c(cand, dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE))), silent = TRUE)
  # 2) Rscript
  try({
    args <- commandArgs(trailingOnly = FALSE)
    m <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m)) cand <- c(cand, dirname(normalizePath(m, winslash = "/", mustWork = TRUE)))
  }, silent = TRUE)
  # 3) RStudio (active editor)
  try({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      ctx <- rstudioapi::getSourceEditorContext()
      if (!is.null(ctx) && nzchar(ctx$path)) {
        cand <- c(cand, dirname(normalizePath(ctx$path, winslash = "/", mustWork = TRUE)))
      }
    }
  }, silent = TRUE)
  # 4) Fallback: current working directory
  cand <- c(cand, normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  cand[nzchar(cand)][1]
})

# WoRMS / species helpers (inlined; formerly species_worms.R)
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

#' Loads the WoRMS reference list; returns NULL if it is missing or unreadable
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

#' Accepted variants for the species column (normalised slug)
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

#' Binomial format: genus capitalised, epithets in lower case
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

#' Named vector for selectizeInput: first entry = keep the imported name, then scientificname
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
      c("(not possible: the WoRMS scientificname list is empty)" = "")
    )
  }
  v <- c("", s)
  nms <- c("(keep the imported name)", s)
  stats::setNames(v, nms)
}

## --- Catches sheet: column detection ---

#' Column matching for Individuals (ASCII slug, including species-name variants)
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

#' Builds a standard data.frame for the catch pipeline; the species column becomes "espece"
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
    err$species <- "Species column not found (expected: espece, espèce, esp, scientificname, species, etc.)."
  }
  if (is.na(st) || !nzchar(st)) {
    err$station <- "Station / serial number column not found."
  }
  if (is.na(ecp) || !nzchar(ecp)) {
    err$ech <- "Ech_P column (sample weight) not found."
  }
  if (is.na(ttn) || !nzchar(ttn)) {
    err$totn <- "Tot_N column (number caught) not found."
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

#' Aligns a typed or chosen name with the exact WoRMS spelling
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

#' Applies species replacements (rows are matched by their old name)
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

#' Applies binomial normalisation + the user's WoRMS mapping to the espece column
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

#' Row counts: validated, corrected, not in the reference list
#'
#' - \code{raw_in}: name as imported (file), before normalisation
#' - \code{final_in}: name kept (after normalisation + WoRMS mapping)
#' - \code{pre_map}: optional; the name right after binomial normalisation, before
#'   the manual mapping (dialog). If it differs from \code{final_in}, it is an
#'   explicit replacement (counted as "corrected").
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
    stop("species_line_summary: length of final_in != raw_in", call. = FALSE)
  }
  if (!is.null(pre_map) && length(pre_map) != n) {
    stop("species_line_summary: wrong length of pre_map", call. = FALSE)
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

#' Great-circle distance in nautical miles between two positions (decimal
#' degrees); NA when a position is missing or out of range.
station_dist_nmi <- function(lat1, lon1, lat2, lon2) {
  lat1 <- suppressWarnings(as.numeric(lat1)); lon1 <- suppressWarnings(as.numeric(lon1))
  lat2 <- suppressWarnings(as.numeric(lat2)); lon2 <- suppressWarnings(as.numeric(lon2))
  bad <- function(la, lo) is.na(la) | is.na(lo) | abs(la) > 90 | abs(lo) > 180
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
    cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  d <- 2 * 3440.065 * asin(pmin(1, sqrt(a)))
  d[bad(lat1, lon1) | bad(lat2, lon2)] <- NA_real_
  d
}

#' Station values that are not in the stations file (editable in the
#' Fishstations_LPA tab). Codes follow the NMD reference lists.
STATION_DEFAULTS <- list(
  nation               = "",
  area                 = "",
  gear                 = "3500",
  gearcount            = 1,
  haulvalidity         = "1",
  trawldoorspread      = 15,
  verticaltrawlopening = 50
)

# --- 1. Main theme ---
theme_modern <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#357AB7", secondary = "#6c757d",
  "body-bg" = "#f2f2f2", "sidebar-bg" = "#ffffff", "panel-bg" = "#ffffff",
  "font-size-base" = "1.05rem"
)

# --- 2. MODULE 1: Fishstations_LPA ---
mod_fishstations_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(
      tagList(icon("location-dot", class="me-2"), "Fishstations_LPA")
    ),
    fluidRow(
      column(4,
        div(class = "card-container",
            div(tags$h5(icon("list-ol", class = "sidebar-icon"), strong("Workflow steps:")), style = "margin-bottom: 16px;"),
            actionButton(ns("createMetadata"), label = tagList(icon("file-code"), "1. Create mission metadata"), class = "btn btn-primary", style = "width: 100%;"),
            br(), br(),
            fileInput(ns("file"), tagList(icon("file-excel"), "2. Import Stations file"), accept = c(".xlsx", ".xls", ".csv"), buttonLabel = "Browse…", placeholder = "No file selected", width = "100%"),
            br(),
            actionButton(ns("injectMetadata"), label = tagList(icon("syringe"), "3. Add metadata to the stations file"), class = "btn btn-secondary", style = "width: 100%;"),
            br(), br(),
            downloadButton(ns("downloadStations"), label = tagList(icon("download"), "4. Download Fishstations_LPA"), class = "btn btn-secondary", style = "width: 100%;"),
            br(), br(),
            downloadButton(ns("downloadMetadataXML"), label = tagList(icon("download"), "5. Download Missions_LPA"), class = "btn btn-primary", style = "width: 100%;"),
            hr(),
            tags$h5(icon("sliders"), strong("Station defaults")),
            helpText("Applied to every station. Leave a field empty to omit it from the XML."),
            textInput(ns("st_nation"), "nation (NMD nation code)", value = STATION_DEFAULTS$nation),
            textInput(ns("st_area"), "area (NMD area code)", value = STATION_DEFAULTS$area),
            textInput(ns("st_gear"), "gear (NMD gear code)", value = STATION_DEFAULTS$gear),
            numericInput(ns("st_gearcount"), "gearcount", value = STATION_DEFAULTS$gearcount, min = 1),
            textInput(ns("st_haulvalidity"), "haulvalidity (code)", value = STATION_DEFAULTS$haulvalidity),
            numericInput(ns("st_trawldoorspread"), "trawldoorspread (m)", value = STATION_DEFAULTS$trawldoorspread),
            numericInput(ns("st_verticaltrawlopening"), "verticaltrawlopening (m)", value = STATION_DEFAULTS$verticaltrawlopening)
        )
      ),
      column(8,
        div(class = "card-container",
            uiOutput(ns("fileError")),
            div(tags$h4(icon("table"), "Fishstations_LPA sheets preview"),
                style = "margin-top: 8px; margin-bottom: 12px;"),
            tabsetPanel(
              id = ns("fs_preview_tabs"),
              tabPanel(
                "All data",
                div(style = "max-height:500px; overflow:auto;",
                    div(style = "border-radius:6px;", DTOutput(ns("preview")))
                )
              ),
              tabPanel(
                "Fishstations",
                div(style = "max-height:500px; overflow:auto;",
                    div(style = "border-radius:6px;", DTOutput(ns("stationsPreview")))
                )
              )
            ),
            conditionalPanel(sprintf("input['%s'] > 0", ns("createMetadata")),
                             br(),
                             div(tags$h4(icon("table"), "Mission metadata (fill in by hand)"), style = "margin-bottom: 12px;"),
                             div(style = "border-radius:6px;box-shadow:0 1px 4px rgba(0,0,0,0.1);", rHandsontableOutput(ns("metadataTable")))
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
    st_default_chr <- function(x) if (is.null(x) || !nzchar(trimws(x))) NA_character_ else trimws(x)
    st_default_num <- function(x) if (is.null(x) || is.na(x)) NA_real_ else x
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
        } else { stop("Unsupported format: ", ext) }
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
        output$fileError <- renderUI(div(class = "error-text", paste("Error while reading the file:", e$message)))
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
      showNotification("Metadata added to the stations sheet", type = "message")
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
          paste0("Columns missing from your file (ignored): ", paste(missing_posCols, collapse = ", ")),
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
          serialnumber         = station,
          nation               = st_default_chr(input$st_nation),
          haulvalidity         = st_default_chr(input$st_haulvalidity),
          gear                 = st_default_chr(input$st_gear),
          gearcount            = st_default_num(input$st_gearcount),
          area                 = st_default_chr(input$st_area),
          samplequality        = NA_character_,
          gearcondition        = NA_character_,
          trawldoorspread      = st_default_num(input$st_trawldoorspread),
          verticaltrawlopening = st_default_num(input$st_verticaltrawlopening)
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
        # Towed distance (nmi) from the start and end positions, computed before
        # a missing position is filled in from the other end
        mutate(
          distance = round(station_dist_nmi(latitudestart, longitudestart, latitudeend, longitudeend), 3)
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
          # duree_chalutage is in minutes; NMDBiotic soaktime is in decimal hours
          soaktime = round(suppressWarnings(as.numeric(soaktime)) / 60, 3)
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

# --- 3. MODULE 2: Catchsamples_LPA ---
mod_catchsamples_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(tagList(icon("ship", class = "me-2"), "Catchsamples_LPA")),
    sidebarLayout(
      sidebarPanel(width = 4,
                   div(class = "card mb-4",
                       div(class = "card-header h5 bg-primary text-white", icon("file-import", class = "me-2"), "1. Import files"),
                       div(class = "card-body",
                           fileInput(ns("fileSP"), "Import Catches file (CSV, XLS or XLSX)", accept = c(".csv", ".xlsx", ".xls"), buttonLabel = "Browse…")
                       )
                   ),
                   div(class = "card",
                       div(class = "card-header h5 bg-success text-white", icon("file-export", class = "me-2"), "2. Export results"),
                       div(class = "card-body",
                           downloadButton(ns("downloadData"), "📄 Download the edited catches sheet", class = "btn-primary mb-2", style = "width: 100%;"),
                           downloadButton(ns("downloadCatchsamples"), "📦 Download Catchsamples_LPA", class = "btn-success", style = "width: 100%;")
                       )
                   )
      ),
      mainPanel(width = 8,
                uiOutput(ns("catch_sp_badge")),
                div(class = "card",
                    div(class = "card-header h5 bg-info text-white", icon("table", class = "me-2"), "Generated catchsamples preview"),
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
    # Step 1: import -> normalisation + WoRMS dialog if needed
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
        rv_c$err_import <- "Could not read the file (empty or invalid format)."
        return()
      }
      st <- catch_build_standard_frame(dfin)
      if (!is.null(st$error)) {
        msg <- paste(unlist(st$error), collapse = " ")
        rv_c$err_import <- msg
        showNotification(
          paste("Catchsamples import:", msg), type = "error", duration = 10
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
          "WoRMS file missing or unreadable: species validation is limited. Put a WoRMS file (CSV/xlsx) in the data/ folder.",
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
          title = "WoRMS validation",
          div(
            class = "text-center p-4",
            div(
              class = "spinner-border text-primary",
              role = "status",
              tags$span(class = "visually-hidden", "Loading")
            ),
            p(class = "mt-3 mb-0 small", "Checking species names against WoRMS…")
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
              "Could not prepare the WoRMS list: check the reference file in data/.",
              type = "error", duration = 8
            )
            return()
          }
          showModal(
            modalDialog(
              title = "Species not found in WoRMS — Catchsamples_LPA",
              p(
                "Some species are not in the reference list. Type the start of a WoRMS ", tags$em("scientificname"), " below to filter the (long) list.",
                " The first option, \"keep the imported name\", does not attach the taxonomic record.",
                class = "small text-muted"
              ),
              tagList(
                lapply(
                  seq_along(um),
                  function(i) {
                    uu <- um[i]
                    selectizeInput(
                      inputId = ns(paste0("catch_worm_", i)),
                      label = tagList(strong(htmltools::htmlEscape(uu)), " — WoRMS (scientificname):"),
                      choices = ch_choices,
                      width = "100%",
                      options = list(
                        placeholder = "Type to filter (e.g. Sardin, Liza)…",
                        maxOptions = 50000L,
                        maxItems = 1L
                      )
                    )
                  }
                )
              ),
              footer = tagList(
                actionButton(ns("catch_worms_apply"), "Apply replacements", class = "btn btn-primary"),
                actionButton(
                  ns("catch_worms_skip"), "Continue without corrections (all)", class = "btn btn-outline-secondary"
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
        paste("WoRMS:", n_, "/", nrow(d0), "row(s) with a recognised species name."),
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
          catchcount     = tot_n
        ) %>%
        dplyr::mutate(
          # NMDBiotic catchweight is the weight of the whole catch the sample
          # represents (Tot_P, matching catchcount = Tot_N); Ech_P is only the
          # weighed subsample.
          catchweight = dplyr::if_else(is.na(tot_p), ech_p, tot_p)
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
          raisingfactor  = as.numeric(tot_p) / as.numeric(ech_p)
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
            "No catches sheet loaded yet, or waiting for validation."
          )
        )
      }
      widx_b <- tryCatch(widx_c(), error = function(e) NULL)
      if (is.null(widx_b) || !nrow(widx_b)) {
        return(
          div(
            class = "d-flex flex-wrap align-items-center gap-3 small mb-2 p-2 rounded border text-muted",
            "WoRMS reference not loaded: species status is unavailable (put the file in data/)."
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
        paste0(nsp, " species · ", sumx$n_unresolved, " row(s)")
      } else {
        as.character(sumx$n_unresolved)
      }
      div(
        class = "d-flex flex-wrap align-items-center gap-3 small mb-2 p-2 rounded border",
        style = "background: #fff;",
        tags$span(HTML("&#9989;"), " Validated (in WoRMS, unchanged): ", sumx$n_validated),
        tags$span(HTML("&#9888;"), " Corrected / reformatted (dialog): ", sumx$n_corrected),
        tags$span(HTML("&#10060;"), " Not found in WoRMS: ", unres_lbl)
      )
    })
    output$previewCatch <- renderDT(
      {
        csd <- tryCatch(catchSamplesData(), error = function(e) NULL)
        if (is.null(csd) || (is.data.frame(csd) && !nrow(csd))) {
          if (!is.null(rv_c$err_import)) {
            return(data.frame(Info = "Fix the import errors shown above."))
          }
          return(
            data.frame(Info = "Import a catches sheet, then apply the WoRMS corrections if offered.")
          )
        }
        csd
      },
      options = list(pageLength = 10, scrollX = TRUE)
    )
    output$downloadData <- downloadHandler(
      filename = function() { paste0("Catches_sheet_edited_", Sys.Date(), ".xlsx") },
      content = function(file) {
        dat <- tryCatch(transformedData(), error = function(e) NULL)
        if (is.null(dat) || (is.data.frame(dat) && !nrow(dat))) {
          stop("No data to export.")
        }
        writexl::write_xlsx(dat, path = file)
      }
    )
    output$downloadCatchsamples <- downloadHandler(
      filename = function() { "Catchsamples_LPA.xlsx" },
      content = function(file) {
        csd <- tryCatch(catchSamplesData(), error = function(e) NULL)
        if (is.null(csd) || !nrow(csd)) {
          stop("No data ready. Import a valid catches sheet first.")
        }
        writexl::write_xlsx(csd, path = file)
      }
    )
  })
}

# --- 4. MODULE 3: Individuals_LPA ---
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
        span("Individuals LPA — import, join and export", class = "fw-semibold")
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        class = "indiv-lpa-sidebar",
        h5("Excel files"),
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
          actionButton(ns("btn_join"), "Join Biology with Length", class = "btn-primary w-100", icon = icon("link")),
          downloadButton(ns("dl_individuals"), "Export Individuals_LPA.xlsx", class = "btn-success w-100")
        ),
        hr(style = "margin:.5rem 0;"),
        helpText(
          "Join: same station, same species (espece), and the nombre column of the length-frequency file with the nombre column of the biology file.",
          "Numeric values; when several rows share the same value k, the 1st is paired with the 1st, the 2nd with the 2nd, and so on."
        ),
        helpText("Export: the transformations are applied to the joined rows (see the preview).")
      ),
      div(
        h4(icon("table"), "Individuals_LPA sheets preview"),
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
            p("Preview of the exported columns (first 10 rows):", class = "text-muted small"),
            div(class = "table-responsive",
                style = "max-height:500px; overflow:auto;",
                tableOutput(ns("preview_export"))),
            hr(),
            h6("Column order in the file"),
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
        return(div(class = "text-danger small", paste("Error:", rv$fq_err)))
      }
      need <- indiv_required_fields_missing_map(rv$fq_map, INDIV_FQ_REQUIRED)
      if (length(need) == 0) {
        return(div(class = "text-success small mb-2", icon("check-circle"),
                   " All length-frequency columns were recognised automatically."))
      }
      cn <- unique(names(rv$fq_raw))
      cols <- c("(none)" = "", stats::setNames(cn, cn))
      tagList(
        h6("Manual matching — length-frequency (columns not detected)"),
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
        return(div(class = "text-danger small", paste("Error:", rv$bio_err)))
      }
      need <- indiv_required_fields_missing_map(rv$bio_map, INDIV_BIO_REQUIRED)
      if (length(need) == 0) {
        return(div(class = "text-success small mb-2", icon("check-circle"),
                   " All biology columns were recognised automatically."))
      }
      cn <- unique(names(rv$bio_raw))
      cols <- c("(none)" = "", stats::setNames(cn, cn))
      tagList(
        h6("Manual matching — biology (columns not detected)"),
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

    # Separate fingerprints: each file is checked against WoRMS on its own (before the join).
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
    # WoRMS: length-frequency file on its own, before the join
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
          "WoRMS file unavailable (data/ folder): length-frequency species check skipped for Individuals.",
          type = "warning", duration = 6
        )
        rv$indiv_worms_solved_fq_fp <- fp
        return()
      }
      rv$indiv_worms_awaiting <- TRUE
      showModal(
        modalDialog(
          title = "WoRMS validation",
          div(
            class = "text-center p-4",
            div(
              class = "spinner-border text-primary",
              role = "status",
              tags$span(class = "visually-hidden", "Loading")
            ),
            p(class = "mt-3 mb-0 small", "Checking species names against WoRMS…")
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
              "Could not prepare the WoRMS list: check the reference file in data/.",
              type = "error", duration = 8
            )
            return()
          }
          w_txt <- p("Length-frequency file mapped. Type the start of a WoRMS ", tags$em("scientificname"), " to filter. The first option keeps the imported name.", class = "small text-muted")
          showModal(
            modalDialog(
              title = "Species not found in WoRMS — length-frequency / Individuals",
              w_txt,
              tagList(
                lapply(
                  seq_along(um),
                  function(i) {
                    uu <- um[i]
                    selectizeInput(
                      inputId = ns(paste0("indiv_worm_", i)),
                      label = tagList(strong(htmltools::htmlEscape(uu)), " — WoRMS (scientificname):"),
                      choices = ch_choices_i,
                      width = "100%",
                      options = list(
                        placeholder = "Type to filter (scientificname)…",
                        maxOptions = 50000L,
                        maxItems = 1L
                      )
                    )
                  }
                )
              ),
              footer = tagList(
                actionButton(ns("indiv_worms_apply"), "Apply", class = "btn btn-primary"),
                actionButton(ns("indiv_worms_skip"), "Continue without corrections", class = "btn btn-outline-secondary")
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
    # WoRMS: biology file on its own, separately from the length-frequency file (before the join)
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
          "WoRMS file unavailable (data/ folder): biology species check skipped for Individuals.",
          type = "warning", duration = 6
        )
        rv$indiv_worms_solved_bio_fp <- fp
        return()
      }
      rv$indiv_worms_awaiting <- TRUE
      showModal(
        modalDialog(
          title = "WoRMS validation",
          div(
            class = "text-center p-4",
            div(
              class = "spinner-border text-primary",
              role = "status",
              tags$span(class = "visually-hidden", "Loading")
            ),
            p(class = "mt-3 mb-0 small", "Checking species names against WoRMS…")
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
              "Could not prepare the WoRMS list: check the reference file in data/.",
              type = "error", duration = 8
            )
            return()
          }
          w_txt <- p("Biology file mapped. Type the start of a WoRMS ", tags$em("scientificname"), " to filter. The first option keeps the imported name.", class = "small text-muted")
          showModal(
            modalDialog(
              title = "Species not found in WoRMS — biology / Individuals",
              w_txt,
              tagList(
                lapply(
                  seq_along(um),
                  function(i) {
                    uu <- um[i]
                    selectizeInput(
                      inputId = ns(paste0("indiv_worm_", i)),
                      label = tagList(strong(htmltools::htmlEscape(uu)), " — WoRMS (scientificname):"),
                      choices = ch_choices_i,
                      width = "100%",
                      options = list(
                        placeholder = "Type to filter (scientificname)…",
                        maxOptions = 50000L,
                        maxItems = 1L
                      )
                    )
                  }
                )
              ),
              footer = tagList(
                actionButton(ns("indiv_worms_apply"), "Apply", class = "btn btn-primary"),
                actionButton(ns("indiv_worms_skip"), "Continue without corrections", class = "btn btn-outline-secondary")
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
            "WoRMS: reference list not loaded (data/ folder)."
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
        paste0(nsp_i, " species · ", sumx$n_unresolved, " row(s)")
      } else {
        as.character(sumx$n_unresolved)
      }
      div(
        class = "small mb-2 p-2 rounded border",
        style = "background:#f8f9fa;",
        tags$span(HTML("&#9989;"), " Validated: ", sumx$n_validated), " · ",
        tags$span(HTML("&#9888;"), " Corrected: ", sumx$n_corrected), " · ",
        tags$span(HTML("&#10060;"), " Not found: ", unres_i)
      )
    })

    output$fq_status <- renderText({
      if (!is.null(rv$fq_err)) return(paste("Error reading the length-frequency file:", rv$fq_err))
      req(fq_std_reactive())
      s <- fq_std_reactive()
      if (length(s$missing)) {
        paste("Required columns missing:", paste(s$missing, collapse = ", "),
              "\nPlease complete the column matching shown above.")
      } else {
        "Length-frequency: ready (columns normalised)."
      }
    })

    output$bio_status <- renderText({
      if (!is.null(rv$bio_err)) return(paste("Error reading the biology file:", rv$bio_err))
      req(bio_std_reactive())
      s <- bio_std_reactive()
      if (length(s$missing)) {
        paste("Required columns missing:", paste(s$missing, collapse = ", "),
              "\nPlease complete the column matching shown above.")
      } else {
        "Biology: ready (columns normalised)."
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
        rv$join_error <- "Load both files and complete the column matching."; return()
      }
      if (length(fq$missing) || length(bio$missing)) {
        rv$join_error <- "Column matching incomplete: check the required columns of both tables."; return()
      }
      d1 <- tryCatch(indiv_fq_worms_df(),  error = function(e) NULL)
      d2 <- tryCatch(indiv_bio_worms_df(), error = function(e) NULL)
      if (is.null(d1) || is.null(d2) || !nrow(d1) || !nrow(d2)) {
        rv$join_error <- "Prepare the length-frequency and biology files (species normalisation failed). Check the required columns."
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
        rv$join_error <<- paste("Join error:", conditionMessage(e))
        NULL
      })
      if (is.null(join_attempt)) {
        shinyalert::shinyalert(
          title = "Join error",
          text  = rv$join_error %||% "Could not join the biology data to the length-frequency data.",
          type  = "error"
        )
        return()
      }
      if (nrow(join_attempt) != n_fq) {
        rv$join_error <- paste0(
          "The number of rows (", nrow(join_attempt), ") does not match the length-frequency file (",
          n_fq, "). Check for duplicates (station, espece, nombre) in the biology file."
        )
        rv$joined <- join_attempt
        shinyalert::shinyalert(
          title = "Partial join",
          text  = rv$join_error,
          type  = "warning"
        )
        return()
      }
      rv$joined <- join_attempt
      shinyalert::shinyalert(
        title = "Join successful",
        text  = paste0("Biology data joined to the length-frequency data (", n_fq, " rows)."),
        type  = "success",
        timer = 2500,
        showConfirmButton = FALSE
      )
      showNotification(
        tagList(icon("check-circle"), " Biology data joined to the length-frequency data (", n_fq, " rows)."),
        type = "message", duration = 4
      )
    })

    output$join_status <- renderText({
      if (!is.null(rv$join_error) && nzchar(rv$join_error %||% "")) return(rv$join_error)
      if (is.null(rv$joined)) return("Click \"Join Biology with Length\" after importing and matching both files.")
      paste("Rows (same as the length-frequency file):", nrow(rv$joined))
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
      validate(need(nrow(rv$joined) > 0, "Empty join: nothing to export."))
      head(export_df(), 10)
    }, striped = TRUE)

    output$dl_individuals <- downloadHandler(
      filename = function() "Individuals_LPA.xlsx",
      content  = function(file) {
        if (is.null(rv$joined) || nrow(rv$joined) == 0) {
          stop("No joined data to export. Check the join.")
        }
        writexl::write_xlsx(list(Individuals = export_df()), path = file)
      }
    )
  })
}


# --- 5. MODULE 4: Biotic_LPA ---
mod_biotic_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(tagList(icon("database", class = "me-2"), "Biotic_LPA")),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("in_missions"), "Missions_LPA.xlsx",     accept = ".xlsx", buttonLabel = "Browse…"),
        fileInput(ns("in_fish"),     "Fishstations_LPA.xlsx",  accept = ".xlsx", buttonLabel = "Browse…"),
        fileInput(ns("in_catch"),    "Catchsamples_LPA.xlsx",  accept = ".xlsx", buttonLabel = "Browse…"),
        fileInput(ns("in_indiv"),    "Individuals_LPA.xlsx",   accept = ".xlsx", buttonLabel = "Browse…"),
        actionButton(ns("process"), "Combine the data", class = "btn-primary w-100 mb-2"),
        downloadButton(ns("download_biotic"), "Download Biotic_LPA", class = "btn-success w-100"),
        width = 3
      ),
      mainPanel(
        h4(icon("table"), "Biotic_LPA input files preview (first 5 rows)"),
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
        h4(icon("database"), "Combined data preview"),
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
      withProgress(message="Reading the files...", value=0, {
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
              paste0("Warning: ", n_miss, " Individuals row(s) have no match (scientificname + serialnumber) in Catchsamples."),
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
            max_specimen = if (all(is.na(specimenid))) NA_real_ else max(specimenid, na.rm = TRUE),
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
        # Matches Bioltic_valide_model.xlsx exactly: every column is written
        # as text (inlineStr) to avoid type errors in StoX
        # (e.g. specimenid / startyear integer vs character).
        .to_chr <- function(x) {
          if (is.numeric(x)) {
            # Avoids scientific notation and stray ".0" on integers.
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

# --- 6. Main UI: one tab per module ---
ui <- tagList(
  tags$head(
    tags$style(
      htmltools::HTML("/* Selectize inside dialogs: show the menu above the dialog (bslib 5) */\n.modal .selectize-dropdown { z-index: 2005 !important; max-height: 50vh; overflow: auto; }")
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
