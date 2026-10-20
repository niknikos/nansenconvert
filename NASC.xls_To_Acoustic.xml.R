# app.R
# 0) Initial configuration
# ---------------------------------------------------------------
options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB upload limit
# ---------------------------------------------------------------
# 0) Libraries
# ---------------------------------------------------------------
library(shiny)
library(bslib)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(DT)

# ---------------------------------------------------------------
# 0.a) Acoustic settings written to <frequency>/<distance>
#      (defaults; editable in the sidebar)
# ---------------------------------------------------------------
NASC_ACOUSTIC_DEFAULTS <- list(
  freq                   = 38000,
  transceiver            = 1,
  threshold              = -70,
  pel_ch_thickness       = 10,
  upper_interpret_depth  = 10,
  lower_integrator_depth = 500
)

# ---------------------------------------------------------------
# 0.b) NASC quality control + building Acoustic_LPA
# ---------------------------------------------------------------
nasc_coord_invalid <- function(lat, lon) {
  lat <- suppressWarnings(as.numeric(lat))
  lon <- suppressWarnings(as.numeric(lon))
  is.na(lat) | is.na(lon) | abs(lat) > 90 | abs(lon) > 180
}

# Rows with a missing or out-of-range position (Echoview writes 999 / -999
# when there is no GPS fix).
nasc_rows_bad_coords <- function(df) {
  n <- nrow(df)
  if (n == 0) return(logical(0))
  if (!all(c("Lat_S", "Lon_S", "Lat_E", "Lon_E") %in% names(df))) return(rep(FALSE, n))
  nasc_coord_invalid(df$Lat_S, df$Lon_S) | nasc_coord_invalid(df$Lat_E, df$Lon_E)
}

# Rows that contribute no NASC (Sa = 0, or no Species_percent to apportion with)
nasc_rows_no_sa <- function(df) {
  n <- nrow(df)
  if (n == 0) return(logical(0))
  if (!"Sa" %in% names(df)) return(rep(FALSE, n))
  v <- suppressWarnings(as.numeric(df$Sa))
  is.na(v) | v == 0
}

# Echoview reports PRC_NASC once per region and repeats it on every species row
# of that region; Species_percent is the species' share of that region (0-100).
nasc_species_sa <- function(prc_nasc, species_percent) {
  prc <- suppressWarnings(as.numeric(prc_nasc))
  pct <- suppressWarnings(as.numeric(species_percent))
  prc * pct / 100
}

# Echoview writes dates as yyyymmdd; Excel may also return a Date or a serial number.
nasc_parse_date <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(as.Date(x))
  s <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(s)))
  ymd8 <- !is.na(s) & grepl("^[0-9]{8}$", s)
  out[ymd8] <- as.Date(s[ymd8], format = "%Y%m%d")
  serial <- !is.na(s) & !ymd8 & grepl("^[0-9]+([.][0-9]+)?$", s)
  out[serial] <- as.Date(floor(as.numeric(s[serial])), origin = "1899-12-30")
  other <- is.na(out) & !is.na(s) & nzchar(s)
  if (any(other)) {
    out[other] <- as.Date(suppressWarnings(
      lubridate::parse_date_time(s[other], orders = c("Ymd", "dmY", "mdY"))
    ))
  }
  out
}

# Seconds since midnight. Echoview writes "HH:MM:SS.ffff" (a decimal comma in
# some locales); Excel may also return a POSIXct or a fraction of a day.
nasc_parse_time <- function(x) {
  if (inherits(x, "POSIXt")) {
    lt <- as.POSIXlt(x, tz = "UTC")
    return(lt$hour * 3600 + lt$min * 60 + lt$sec)
  }
  s <- trimws(sub(",", ".", as.character(x), fixed = TRUE))
  out <- rep(NA_real_, length(s))
  hms <- !is.na(s) & grepl("^[0-9]{1,2}:[0-9]{2}(:[0-9]{2}([.][0-9]*)?)?$", s)
  out[hms] <- vapply(strsplit(s[hms], ":", fixed = TRUE), function(v) {
    v <- as.numeric(v)
    v[1] * 3600 + v[2] * 60 + if (length(v) > 2) v[3] else 0
  }, numeric(1))
  frac <- !is.na(s) & !hms & grepl("^(0?[.][0-9]+|0|1)$", s)
  out[frac] <- as.numeric(s[frac]) * 86400
  out
}

nasc_datetime <- function(date, time) {
  .POSIXct(as.numeric(nasc_parse_date(date)) * 86400 + nasc_parse_time(time), tz = "UTC")
}

# Great-circle distance in nautical miles; NA when a position is invalid.
nasc_dist_nmi <- function(lat1, lon1, lat2, lon2) {
  lat1 <- suppressWarnings(as.numeric(lat1)); lon1 <- suppressWarnings(as.numeric(lon1))
  lat2 <- suppressWarnings(as.numeric(lat2)); lon2 <- suppressWarnings(as.numeric(lon2))
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
    cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  d <- 2 * 3440.065 * asin(pmin(1, sqrt(a)))
  d[nasc_coord_invalid(lat1, lon1) | nasc_coord_invalid(lat2, lon2)] <- NA_real_
  d
}

build_lpa_from_nasc_staged <- function(df_nasc, md, settings = NASC_ACOUSTIC_DEFAULTS) {
  # Optional Echoview columns used for the sailed distance when present
  for (cn in c("VL_start", "VL_end", "Dist_S", "Dist_E")) {
    df_nasc[[cn]] <- if (cn %in% names(df_nasc)) {
      suppressWarnings(as.numeric(df_nasc[[cn]]))
    } else {
      NA_real_
    }
  }

  # One <distance> per Echoview interval cell. Interval numbers restart for
  # every EV file, so the cell is identified by its start date/time as well.
  df_nasc <- df_nasc %>%
    mutate(
      Sa       = nasc_species_sa(PRC_NASC, Species_percent),
      .t_start = nasc_datetime(Date_S, Time_S),
      .t_stop  = nasc_datetime(Date_E, Time_E),
      .cell    = paste(Date_S, Time_S, Interval, sep = "|")
    ) %>%
    arrange(.t_start, Interval, Layer)
  df_nasc$distance_id <- match(df_nasc$.cell, unique(df_nasc$.cell))

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
        Species_name %in% c("Plancton", "Plankton")          ~ 14,
        Species_name %in% c("Mesopelagique", "Mesopelagic")  ~ 15,
        Species_name %in% c("Autres", "Others")              ~ 16,
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

  dist <- df_nasc %>%
    group_by(distance_id) %>%
    summarise(
      t_start   = first(.t_start),
      t_stop    = first(.t_stop),
      lat_start = first(Lat_S),
      lon_start = first(Lon_S),
      lat_stop  = first(Lat_E),
      lon_stop  = first(Lon_E),
      vl_start  = first(VL_start),
      vl_end    = first(VL_end),
      dist_s    = first(Dist_S),
      dist_e    = first(Dist_E),
      .groups = "drop"
    )

  # Sailed distance of each interval (nmi): vessel log, then Echoview GPS
  # distance (m), then the great-circle distance between start and end.
  len_vl  <- dist$vl_end - dist$vl_start
  len_gps <- (dist$dist_e - dist$dist_s) / 1852
  len_vl[!is.na(len_vl) & len_vl <= 0]    <- NA_real_
  len_gps[!is.na(len_gps) & len_gps <= 0] <- NA_real_
  len_pos <- nasc_dist_nmi(dist$lat_start, dist$lon_start, dist$lat_stop, dist$lon_stop)
  len <- dplyr::coalesce(len_vl, len_gps, len_pos)
  len_source <- dplyr::case_when(
    !is.na(len_vl)  ~ "vessel log (VL_start/VL_end)",
    !is.na(len_gps) ~ "GPS distance (Dist_S/Dist_E)",
    !is.na(len_pos) ~ "start/end positions",
    TRUE            ~ "estimated (median interval length)"
  )
  if (any(is.na(len)) && any(!is.na(len))) {
    len[is.na(len)] <- stats::median(len, na.rm = TRUE)
  }

  # log_start: the vessel log when every interval has one, otherwise a
  # cumulative distance along the track (intervals plus the gaps between them).
  if (nrow(dist) > 0 && all(!is.na(dist$vl_start))) {
    log_start <- dist$vl_start
    log_source <- "vessel log (VL_start)"
  } else {
    gap <- nasc_dist_nmi(
      dplyr::lag(dist$lat_stop), dplyr::lag(dist$lon_stop),
      dist$lat_start, dist$lon_start
    )
    gap[is.na(gap)] <- 0
    log_start <- cumsum(gap + dplyr::coalesce(dplyr::lag(len), 0))
    log_source <- "cumulative distance along the track"
  }

  df_distances <- data.frame(
    distance_id      = dist$distance_id,
    log_start        = round(log_start, 3),
    start_time       = format(dist$t_start, "%Y-%m-%d %H:%M:%S"),
    stop_time        = format(dist$t_stop, "%Y-%m-%d %H:%M:%S"),
    integrator_dist  = round(len, 3),
    pel_ch_thickness = settings$pel_ch_thickness,
    bot_ch_thickness = NA,
    include_estimate = 1,
    lat_start        = dist$lat_start,
    lat_stop         = dist$lat_stop,
    lon_start        = dist$lon_start,
    lon_stop         = dist$lon_stop,
    stringsAsFactors = FALSE
  )

  df_frequencies <- df_nasc %>%
    group_by(distance_id) %>%
    summarise(
      num_pel_ch             = suppressWarnings(max(as.integer(Layer), na.rm = TRUE)),
      min_bot_depth          = first(Exclude_below_line_depth_min),
      max_bot_depth          = first(Exclude_below_line_range_max),
      lower_interpret_depth  = first(Exclude_below_line_range_mean),
      upper_integrator_depth = first(Exclude_above_line_depth_mean),
      .groups = "drop"
    ) %>%
    mutate(
      num_pel_ch             = ifelse(is.finite(num_pel_ch), num_pel_ch, NA_integer_),
      freq                   = settings$freq,
      transceiver            = settings$transceiver,
      threshold              = settings$threshold,
      num_bot_ch             = 0L,
      upper_interpret_depth  = settings$upper_interpret_depth,
      lower_integrator_depth = settings$lower_integrator_depth,
      quality                = 1L,
      bubble_corr            = 1
    ) %>%
    select(
      distance_id, freq, transceiver, threshold,
      num_pel_ch, num_bot_ch,
      min_bot_depth, max_bot_depth,
      upper_interpret_depth, lower_interpret_depth,
      upper_integrator_depth, lower_integrator_depth,
      quality, bubble_corr
    )

  # Every interval keeps its <ch_type>, even when it holds no NASC (true zero)
  df_ch_types <- data.frame(
    distance_id = df_distances$distance_id,
    freq        = settings$freq,
    type        = "P",
    stringsAsFactors = FALSE
  )

  # Several regions (or species mapped to the same acocat) in one cell are
  # summed into a single <sa> per acocat and channel.
  df_sa <- df_nasc %>%
    filter(!is.na(Sa), Sa > 0) %>%
    group_by(distance_id, acocat, ch = Layer) %>%
    summarise(value = sum(Sa), .groups = "drop") %>%
    mutate(freq = settings$freq, type = "P") %>%
    select(distance_id, freq, type, acocat, ch, value) %>%
    arrange(distance_id, acocat, ch)

  df_sa_by_acocat <- df_sa %>%
    distinct(distance_id, freq, type, acocat)

  list(
    nasc_raw       = df_nasc,
    nasc_processed = df_nasc,
    notes          = list(
      length_source = table(len_source),
      log_source    = log_source
    ),
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
# 1) Theme and UI
# ---------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    fg = "#333333",
    bg = "#f8f9fa"
  ),

  titlePanel("NASC → Acoustic_LPA converter"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # 1. Upload the NASC source file (CSV or XLSX)
      fileInput(
        inputId = "file_nasc",
        label = "Import NASC file (CSV or XLSX)",
        accept = c(".csv", ".xlsx"),
        buttonLabel = "Browse…"
      ),
      tags$hr(),

      # 2. Metadata entered by hand (root_metadata)
      h4("Metadata"),
      textInput("md_report_time",   "report_time",      value = ""),
      textInput("md_lsss_version",  "lsss_version",     value = ""),
      textInput("md_nation",        "nation",           value = ""),
      textInput("md_platform",      "platform (integer code)", value = ""),
      textInput("md_cruise",        "cruise",           value = ""),
      tags$hr(),

      # 3. Acoustic settings
      h4("Acoustic settings"),
      numericInput("set_freq",                   "Frequency (Hz)",                  value = NASC_ACOUSTIC_DEFAULTS$freq),
      numericInput("set_transceiver",            "Transceiver",                     value = NASC_ACOUSTIC_DEFAULTS$transceiver),
      numericInput("set_threshold",              "Sv threshold (dB)",               value = NASC_ACOUSTIC_DEFAULTS$threshold),
      numericInput("set_pel_ch_thickness",       "Pelagic channel thickness (m)",   value = NASC_ACOUSTIC_DEFAULTS$pel_ch_thickness),
      numericInput("set_upper_interpret_depth",  "Upper interpretation depth (m)",  value = NASC_ACOUSTIC_DEFAULTS$upper_interpret_depth),
      numericInput("set_lower_integrator_depth", "Lower integration depth (m)",     value = NASC_ACOUSTIC_DEFAULTS$lower_integrator_depth),
      tags$hr(),

      # 4. Action buttons
      actionButton("btn_process",       "🔄 Process file",                   class = "btn-primary"),
      tags$br(), tags$br(),
      downloadButton("dl_acoustic_lpa", "💾 Download Acoustic_LPA.xlsx",     class = "btn-success")
    ),

    mainPanel(
      width = 9,
      uiOutput("ui_row_qc"),
      # Preview of the 6 Acoustic_LPA sheets (shown after processing)
      uiOutput("ui_preview_lpa")
    )
  )
)

# ---------------------------------------------------------------
# 2) Server
# ---------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive values holding the processed data
  rv <- reactiveValues(
    nasc_raw        = NULL,
    nasc_processed  = NULL,
    lpa_sheets      = NULL,
    lpa_notes       = NULL,
    nasc_staged     = NULL,
    n_bad_coord     = 0L,
    n_no_sa         = 0L,
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

  nasc_settings <- function(input) {
    num_or <- function(x, default) if (is.null(x) || is.na(x)) default else x
    list(
      freq                   = num_or(input$set_freq,                   NASC_ACOUSTIC_DEFAULTS$freq),
      transceiver            = num_or(input$set_transceiver,            NASC_ACOUSTIC_DEFAULTS$transceiver),
      threshold              = num_or(input$set_threshold,              NASC_ACOUSTIC_DEFAULTS$threshold),
      pel_ch_thickness       = num_or(input$set_pel_ch_thickness,       NASC_ACOUSTIC_DEFAULTS$pel_ch_thickness),
      upper_interpret_depth  = num_or(input$set_upper_interpret_depth,  NASC_ACOUSTIC_DEFAULTS$upper_interpret_depth),
      lower_integrator_depth = num_or(input$set_lower_integrator_depth, NASC_ACOUSTIC_DEFAULTS$lower_integrator_depth)
    )
  }

  apply_nasc_staged <- function(df_staged) {
    res <- build_lpa_from_nasc_staged(df_staged, nasc_md(input), nasc_settings(input))
    rv$nasc_raw       <- res$nasc_raw
    rv$nasc_processed <- res$nasc_processed
    rv$lpa_sheets     <- res$lpa_sheets
    rv$lpa_notes      <- res$notes
  }

  # Metadata or settings typed after processing are applied straight away
  observeEvent(
    list(
      input$md_report_time, input$md_lsss_version, input$md_nation,
      input$md_platform, input$md_cruise,
      input$set_freq, input$set_transceiver, input$set_threshold,
      input$set_pel_ch_thickness, input$set_upper_interpret_depth,
      input$set_lower_integrator_depth
    ),
    {
      req(rv$nasc_staged)
      apply_nasc_staged(rv$nasc_staged)
    },
    ignoreInit = TRUE
  )

  # Flags that show the previews
  show_nasc_preview <- reactiveVal(FALSE)
  show_lpa_preview  <- reactiveVal(FALSE)

  # -------------------------------------------------------------
  # 2.1) Process the NASC data when "btn_process" is clicked
  # -------------------------------------------------------------
  observeEvent(input$btn_process, {
    req(input$file_nasc)

    # 1) Read the NASC file (CSV or XLSX)
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
        title = "Error",
        "Unsupported file format. Please import a CSV or XLSX file.",
        easyClose = TRUE, footer = NULL
      ))
      return(NULL)
    }

    # 2) Clean the column names
    clean_names <- colnames(df_nasc) %>%
      trimws() %>%              # remove leading/trailing spaces
      gsub("\\s+", " ", .)      # collapse repeated inner spaces
    colnames(df_nasc) <- clean_names

    # 2.a) Species_percent: match regardless of case / spaces vs underscores
    sp_key <- tolower(gsub("\\s+", "_", colnames(df_nasc)))
    sp_idx <- which(sp_key == "species_percent")
    if (length(sp_idx) == 1) {
      colnames(df_nasc)[sp_idx] <- "Species_percent"
    }

    # 3) Required columns
    required_cols <- c(
      "Date_S", "Time_S", "Date_E", "Time_E",
      "PRC_NASC",
      "Lat_S", "Lon_S", "Lat_E", "Lon_E",
      "Exclude_below_line_range_max",
      "Exclude_below_line_depth_min",
      "Exclude_above_line_depth_mean",
      "Species_percent", "Species_name","Exclude_below_line_range_mean","Layer","Interval"
    )

    # 4) Check that they are present
    missing_cols <- setdiff(required_cols, colnames(df_nasc))
    if (length(missing_cols) > 0) {
      showModal(modalDialog(
        title = "Missing columns",
        paste0(
          "The following columns are missing from the NASC file:\n",
          paste(missing_cols, collapse = ", ")
        ),
        easyClose = TRUE, footer = NULL
      ))
      return(NULL)
    }

    # 4.b) Species share of the region's NASC: PRC_NASC * Species_percent / 100
    df_nasc$Sa <- nasc_species_sa(df_nasc$PRC_NASC, df_nasc$Species_percent)

    # 5) NASC with Sa: basis for quality control and the transformations
    rv$nasc_staged <- df_nasc
    rv$qc_last_msg <- NULL
    rv$n_bad_coord <- sum(nasc_rows_bad_coords(rv$nasc_staged))
    rv$n_no_sa     <- sum(nasc_rows_no_sa(rv$nasc_staged))
    apply_nasc_staged(rv$nasc_staged)

    # Show the Acoustic_LPA preview after processing
    show_nasc_preview(FALSE)
    show_lpa_preview(TRUE)

    # Tell the user that processing is finished
    showModal(modalDialog(
      title = "✅ Processing complete",
      "The NASC file has been converted and the Acoustic_LPA sheets are ready.",
      easyClose = TRUE, footer = NULL
    ))
  })

  output$ui_row_qc <- renderUI({
    if (is.null(rv$lpa_sheets)) return(NULL)
    notes <- rv$lpa_notes
    len_src <- if (!is.null(notes$length_source) && length(notes$length_source)) {
      paste0(names(notes$length_source), ": ", as.integer(notes$length_source), collapse = "; ")
    } else {
      "-"
    }
    wellPanel(
      h4("Row quality control"),
      p(
        strong("Invalid coordinates "),
        "(999 / -999, missing or out of range in Lat_S, Lon_S, Lat_E or Lon_E): ",
        rv$n_bad_coord, " row(s)."
      ),
      actionButton("btn_drop_bad_coords", "Remove these rows (coordinates)"),
      tags$hr(),
      p(
        strong("Rows with no NASC: "), rv$n_no_sa,
        " (Sa = 0 or no Species_percent). They add no <sa> value, but their",
        " intervals are kept as zero-NASC distances."
      ),
      p(
        strong("Interval length (integrator_dist) from: "), len_src, br(),
        strong("log_start from: "), if (is.null(notes$log_source)) "-" else notes$log_source
      ),
      tags$hr(),
      if (!is.null(rv$qc_last_msg) && nzchar(rv$qc_last_msg)) {
        qc_cls <- if (grepl("^<strong>Nothing removed", rv$qc_last_msg)) {
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
        "<strong>Nothing removed</strong>: no row with invalid coordinates in the current NASC."
      showNotification(
        "No rows to remove (coordinates). The counts above are up to date.",
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
        title = "Error",
        "No rows would be left after removal.",
        easyClose = TRUE, footer = NULL
      ))
      return(invisible(NULL))
    }
    rv$nasc_staged <- df
    rv$n_bad_coord <- sum(nasc_rows_bad_coords(df))
    rv$n_no_sa     <- sum(nasc_rows_no_sa(df))
    apply_nasc_staged(df)
    show_lpa_preview(TRUE)
    msg <- sprintf(
      "<strong>✅ %d row(s) removed</strong> (invalid coordinates). NASC: <strong>%d</strong> row(s) left. Acoustic_LPA preview updated.",
      n_rem,
      nrow(df)
    )
    rv$qc_last_msg <- msg
    showNotification(
      paste0(n_rem, " row(s) removed (coordinates). NASC: ", nrow(df), " row(s)."),
      type = "message",
      duration = 12,
      closeButton = TRUE
    )
  })

  # -------------------------------------------------------------
  # 2.2) Preview of the processed NASC (when "btn_preview_nasc" is clicked)
  # -------------------------------------------------------------
  observeEvent(input$btn_preview_nasc, {
    req(rv$nasc_processed)
    show_nasc_preview(TRUE)
  })

  output$ui_preview_nasc <- renderUI({
    if (!isTRUE(show_nasc_preview())) return(NULL)
    tagList(
      h4("Processed NASC preview"),
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
  # 2.3) Preview of the 6 Acoustic_LPA sheets (when "btn_preview_lpa" is clicked)
  # -------------------------------------------------------------
  observeEvent(input$btn_preview_lpa, {
    req(rv$lpa_sheets)
    show_lpa_preview(TRUE)
  })

  output$ui_preview_lpa <- renderUI({
    if (!isTRUE(show_lpa_preview())) return(NULL)

    # One tab per sheet
    tabs <- lapply(names(rv$lpa_sheets), function(sheet_name) {
      tabPanel(
        title = sheet_name,
        div(style = "max-height:400px; overflow:auto;",
            DTOutput(paste0("tbl_", sheet_name))
        )
      )
    })

    tagList(
      h4("Acoustic_LPA sheets preview"),
      do.call(tabsetPanel, tabs),
      tags$hr()
    )
  })

  # One DTOutput per sheet
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
  # 2.4) Download Acoustic_LPA.xlsx (6 sheets)
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
# 3) Launch the application
# ---------------------------------------------------------------
shinyApp(ui = ui, server = server)
NA
shinyApp(ui = ui, server = server)
