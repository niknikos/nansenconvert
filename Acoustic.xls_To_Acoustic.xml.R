# app.R - Acoustic.xlsx -> NMD Echosounder v1 XML

# ---------------------------------------------------------------
# 0) Initial configuration
# ---------------------------------------------------------------
options(shiny.maxRequestSize = 500 * 1024^2)
# ---------------------------------------------------------------
# 1) Libraries
# ---------------------------------------------------------------
library(shiny)
library(shinyjs)
library(bslib)
library(readxl)
library(xml2)
library(dplyr)
library(purrr)
library(fontawesome)
if (!exists("validate_acoustic", mode = "function")) {
  for (h in file.path("helpers", c("code_lists.R", "taxonomy.R", "validation.R"))) source(h, local = TRUE)
}
# ---------------------------------------------------------------
# 1.b) Building the NMD Echosounder v1 XML
# ---------------------------------------------------------------
ECHOSOUNDER_NS <- "http://www.imr.no/formats/nmdechosounder/v1"

# Numbers are written in plain decimal notation: as.character(200000) gives
# "2e+05", which is not a valid xs:integer (e.g. a 200 kHz frequency).
xml_value_chr <- function(x) {
  if (is.numeric(x)) {
    s <- sub("\\.?0+$", "", sprintf("%.10f", as.double(x)))
    s[s == "-0"] <- "0"
  } else {
    s <- as.character(x)
  }
  s[is.na(x)] <- ""
  s
}

# sheets: the six Acoustic sheets (root_metadata, distances, frequencies,
# ch_types, sa_by_acocat, sa). on_progress(i, n) is called after each distance.
build_echosounder_xml <- function(sheets, on_progress = function(i, n) NULL) {
  num <- function(x) suppressWarnings(as.numeric(x))
  distances_ord    <- sheets$distances    %>% arrange(num(distance_id))
  frequencies_ord  <- sheets$frequencies  %>% arrange(num(distance_id), freq)
  ch_types_ord     <- sheets$ch_types     %>% arrange(num(distance_id), freq, type)
  sa_by_acocat_ord <- sheets$sa_by_acocat %>% arrange(num(distance_id), freq, type, acocat)
  sa_ord           <- sheets$sa           %>% arrange(num(distance_id), freq, type, acocat, ch)
  # Older Acoustic files have no log_start column: fall back to distance_id
  if (!"log_start" %in% names(distances_ord)) {
    distances_ord$log_start <- distances_ord$distance_id
  }

  prepare_data <- function(df) mutate(df, across(everything(), xml_value_chr))
  distances_ord    <- prepare_data(distances_ord)
  frequencies_ord  <- prepare_data(frequencies_ord)
  ch_types_ord     <- prepare_data(ch_types_ord)
  sa_by_acocat_ord <- prepare_data(sa_by_acocat_ord)
  sa_ord           <- prepare_data(sa_ord)
  root_meta        <- prepare_data(sheets$root_metadata)[1, ]

  # Rows of each sheet per distance; an empty table when a distance has none
  # (e.g. an interval with no NASC has no sa_by_acocat / sa rows).
  by_dist <- function(df) {
    parts <- split(df, df$distance_id)
    function(did) if (is.null(parts[[did]])) df[0, , drop = FALSE] else parts[[did]]
  }
  freq_by_dist  <- by_dist(frequencies_ord)
  cht_by_dist   <- by_dist(ch_types_ord)
  sacat_by_dist <- by_dist(sa_by_acocat_ord)
  sa_by_dist    <- by_dist(sa_ord)

  doc <- xml_new_root("echosounder_dataset", xmlns = ECHOSOUNDER_NS)
  for (nm in c("report_time", "lsss_version", "nation", "platform", "cruise")) {
    xml_add_child(doc, nm, root_meta[[nm]])
  }
  node_list <- xml_add_child(doc, "distance_list")

  total_dist <- nrow(distances_ord)
  for (i in seq_len(total_dist)) {
    dist_row <- distances_ord[i, ]
    did <- dist_row$distance_id
    node_dist <- xml_add_child(node_list, "distance")
    xml_set_attr(node_dist, "log_start", dist_row$log_start)
    xml_set_attr(node_dist, "start_time", dist_row$start_time)
    for (nm in c("stop_time", "integrator_dist", "pel_ch_thickness", "bot_ch_thickness",
                 "include_estimate", "lat_start", "lat_stop", "lon_start", "lon_stop")) {
      xml_add_child(node_dist, nm, dist_row[[nm]])
    }
    freq_data  <- freq_by_dist(did)
    cht_data   <- cht_by_dist(did)
    sacat_data <- sacat_by_dist(did)
    sa_data    <- sa_by_dist(did)
    for (frq in unique(freq_data$freq)) {
      freq_row <- freq_data[freq_data$freq == frq, ][1, ]
      node_freq <- xml_add_child(node_dist, "frequency")
      xml_set_attr(node_freq, "freq", freq_row$freq)
      xml_set_attr(node_freq, "transceiver", freq_row$transceiver)
      for (nm in c("threshold", "num_pel_ch", "num_bot_ch", "min_bot_depth", "max_bot_depth",
                   "upper_interpret_depth", "lower_interpret_depth",
                   "upper_integrator_depth", "lower_integrator_depth",
                   "quality", "bubble_corr")) {
        xml_add_child(node_freq, nm, freq_row[[nm]])
      }
      for (typ in unique(cht_data$type[cht_data$freq == frq])) {
        node_cht <- xml_add_child(node_freq, "ch_type")
        xml_set_attr(node_cht, "type", typ)
        acs <- sacat_data$acocat[sacat_data$freq == frq & sacat_data$type == typ]
        for (ac in unique(acs)) {
          node_sacat <- xml_add_child(node_cht, "sa_by_acocat")
          xml_set_attr(node_sacat, "acocat", ac)
          rows <- sa_data[sa_data$freq == frq & sa_data$type == typ & sa_data$acocat == ac, ]
          for (k in seq_len(nrow(rows))) {
            node_sa <- xml_add_child(node_sacat, "sa", rows$value[k])
            xml_set_attr(node_sa, "ch", rows$ch[k])
          }
        }
      }
    }
    on_progress(i, total_dist)
  }
  doc
}
# ---------------------------------------------------------------
# 2) User interface
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
  # -------- Progress bar colours --------
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
  # Sticky header with the progress bar
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
  # Main content
  div(
    class = "container-fluid mt-5 pt-4",
    fluidRow(
      # Sidebar: configuration
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
              label = tagList(fa("file-excel"), "Import Acoustic workbook (.xlsx)"),
              accept = c(".xlsx"),
              buttonLabel = "Browse…",
              placeholder = "Select an .xlsx file",
              width = "100%"
            ),
            fileInput(
              inputId = "aco_biotic_file",
              label = tagList(fa("fish"), "Biotic data for the trawl check (optional)"),
              accept = c(".xlsx", ".xml"),
              buttonLabel = "Browse…",
              placeholder = "Biotic.xlsx or Biotic XML",
              width = "100%"
            ),
            fileInput(
              inputId = "aco_validate_xml",
              label = tagList(fa("clipboard-check"), "…or validate an existing Acoustic XML"),
              accept = c(".xml"),
              buttonLabel = "Browse…",
              placeholder = "Select an .xml file",
              width = "100%"
            ),
            hr(),
            div(
              class = "d-grid gap-3",
              actionButton(
                inputId = "reconstruct_btn",
                label = tagList(fa("cogs"), "Build XML file"),
                class = "btn-success btn-lg"
              ),
              downloadButton(
                outputId = "download_xml",
                label = tagList(fa("download"), "Export Acoustic XML"),
                class = "btn-primary btn-lg"
              )
            ),
            checkboxInput("aco_override", "Allow the export even if validation finds errors", FALSE)
          )
        )
      ),
      # Main panel
      column(
        width = 8,
        # Activity log
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-header bg-primary text-white",
            h4(class = "mb-0", "Activity log")
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
        # Validation
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-header bg-primary text-white",
            h4(class = "mb-0", "Validation")
          ),
          div(
            class = "card-body",
            uiOutput("aco_validation_summary"),
            DT::DTOutput("aco_validation_table"),
            downloadButton("aco_download_findings", "Download findings (CSV)",
                           class = "btn-outline-secondary btn-sm mt-2")
          )
        ),
        # Data preview
        div(
          class = "card shadow-sm",
          div(
            class = "card-header bg-primary text-white",
            h4(class = "mb-0", "Data preview")
          ),
          div(
            class = "card-body p-0",
            navset_pill(
              nav_panel(
                title = tagList(fa("info-circle"), "Metadata"),
                div(class = "table-responsive", tableOutput("preview_root_metadata"))
              ),
              nav_panel(
                title = tagList(fa("ruler-horizontal"), "Distances"),
                div(class = "table-responsive", tableOutput("preview_distances"))
              ),
              nav_panel(
                title = tagList(fa("wave-square"), "Frequencies"),
                div(class = "table-responsive", tableOutput("preview_frequencies"))
              ),
              nav_panel(
                title = tagList(fa("layer-group"), "Channels"),
                div(class = "table-responsive", tableOutput("preview_ch_types"))
              ),
              nav_panel(
                title = tagList(fa("chart-bar"), "SA by ACOCAT"),
                div(class = "table-responsive", tableOutput("preview_sa_by_acocat"))
              ),
              nav_panel(
                title = tagList(fa("table"), "SA"),
                div(class = "table-responsive", tableOutput("preview_sa"))
              )
            )
          )
        )
      )
    )
  ),
  # Footer (intentionally empty)
  tags$footer(class = "mt-5 py-3 bg-light text-center"),
  # Custom JavaScript
  tags$script(HTML("
// Progress bar set-up
$(document).ready(function() {
$('#progress-header').html(`
<div class=\"progress\" style=\"height: 25px;\">
<div id=\"header-progress-bar\"
class=\"progress-bar progress-bar-striped progress-bar-animated\"
role=\"progressbar\"
style=\"width: 0%\">
<span id=\"progress-text\">Ready</span>
</div>
</div>
`);
});
// Message handler for the progress bar in the header
Shiny.addCustomMessageHandler('update_progress', function(data) {
const bar = $('#header-progress-bar');
const text = $('#progress-text');
bar.css('width', data.value + '%');
bar.attr('aria-valuenow', data.value);
text.text(data.detail || data.value + '%');
if (data.value >= 100) {
bar.removeClass('progress-bar-animated progress-bar-striped');
bar.addClass('bg-success');
text.text('Done!');
}
});
"))
)
# ---------------------------------------------------------------
# 3) Server
# ---------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive values
  xml_path <- reactiveVal(NULL)
  perf_data <- reactiveVal(NULL)
  excel_data <- reactiveVal(NULL)
  preview_data_list <- reactiveVal(NULL) # first 5 rows of each sheet
  # ---------------------------------------------------------------
  # 3.1. Excel data preview
  # ---------------------------------------------------------------
  observeEvent(input$excel_file, {
    req(input$excel_file)
    # Show a message in the log straight away
    output$status_text <- renderText(" Loading the Excel file…")
    session$sendCustomMessage("update_progress", list(value = 0, detail = "Loading Excel"))
    tryCatch({
      required_sheets <- c(
        "root_metadata", "distances", "frequencies",
        "ch_types", "sa_by_acocat", "sa"
      )
      # Sheet names
      sheet_names <- excel_sheets(input$excel_file$datapath)
      if (!all(required_sheets %in% sheet_names)) {
        stop(
          "The Excel file must contain all of the following sheets: ",
          paste(required_sheets, collapse = ", ")
        )
      }
      # Previews (first 5 rows) of each sheet
      previews <- map(
        setNames(required_sheets, required_sheets),
        ~ read_excel(input$excel_file$datapath, sheet = .x, n_max = 5)
      )
      preview_data_list(previews)
      # Full data, kept for building the XML
      full_data <- map(
        setNames(required_sheets, required_sheets),
        ~ read_excel(input$excel_file$datapath, sheet = .x)
      )
      excel_data(full_data)
      output$status_text <- renderText({
        paste0(
          " Excel file loaded.\n",
          "Sheets found: ", paste(sheet_names, collapse = ", "), "\n",
          "Preview available.\n",
          "Ready to build the XML."
        )
      })
      session$sendCustomMessage("update_progress", list(value = 20, detail = "Excel OK"))
    }, error = function(e) {
      preview_data_list(NULL)
      output$status_text <- renderText({
        paste0(" Error while importing the Excel file:\n", e$message)
      })
      session$sendCustomMessage("update_progress", list(value = 0, detail = "Excel error"))
    })
  })
  # ---------------------------------------------------------------
  # 3.1.b Preview tables
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
  # 3.2. Build the XML (progress shown in the header)
  # ---------------------------------------------------------------
  observeEvent(input$reconstruct_btn, {
    req(excel_data())
    start_time <- Sys.time()
    output$status_text <- renderText(" Building the XML…")
    session$sendCustomMessage("update_progress", list(value = 20, detail = "Starting"))
    tryCatch({
      # --- Phase 1: data preparation (20%) ---
      timing <- list()
      timing$prep_start <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 30, detail = "Preparing"))
      sheets <- excel_data()
      timing$prep_end <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 40, detail = "Data ready"))
      # --- Phase 2: XML construction (60%) ---
      timing$xml_start <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 50, detail = "Building XML"))
      doc <- build_echosounder_xml(sheets, on_progress = function(i, total_dist) {
        # Update the progress bar
        progress <- 50 + 40 * (i / total_dist)
        session$sendCustomMessage(
          "update_progress",
          list(
            value = round(progress),
            detail = paste0(i, "/", total_dist, " dist")
          )
        )
      })
      timing$xml_end <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 90, detail = "XML built"))
      # --- Phase 3: writing the file (20%) ---
      timing$write_start <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 95, detail = "Writing XML"))
      file_name <- paste0("Acoustic_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xml")
      tmpfile <- file.path(tempdir(), file_name)
      write_xml(doc, file = tmpfile, encoding = "UTF-8")
      xml_path(tmpfile)
      timing$write_end <- Sys.time()
      session$sendCustomMessage("update_progress", list(value = 100, detail = "Done!"))
      # Timings
      perf <- list(
        preparation = as.numeric(difftime(timing$prep_end, timing$prep_start, units = "secs")),
        xml = as.numeric(difftime(timing$xml_end, timing$xml_start, units = "secs")),
        writing = as.numeric(difftime(timing$write_end, timing$write_start, units = "secs")),
        total = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )
      perf_data(perf)
      output$status_text <- renderText({
        paste0(
          " XML built successfully.\n",
          "Total time: ", round(perf$total, 2), " seconds\n",
          "- Data preparation: ", round(perf$preparation, 2), " s\n",
          "- XML construction: ", round(perf$xml, 2), " s\n",
          "- Writing the file: ", round(perf$writing, 2), " s\n\n",
          "File created: ", xml_path()
        )
      })
    }, error = function(e) {
      output$status_text <- renderText({
        paste0(" Error while building the XML:\n", e$message)
      })
      session$sendCustomMessage("update_progress", list(value = 0, detail = "Error"))
    })
  })
  # ---------------------------------------------------------------
  # 3.3. Download the XML
  # ---------------------------------------------------------------
  output$download_xml <- downloadHandler(
    filename = function() {
      if (!is.null(xml_path())) {
        basename(xml_path())
      } else {
        paste0("Acoustic_", Sys.Date(), ".xml")
      }
    },
    content = function(file) {
      req(xml_path())
      file.copy(xml_path(), file, overwrite = TRUE)
    },
    contentType = "application/xml"
  )
  # ---------------------------------------------------------------
  # 3.4. Validation: sheet checks, then NMD schema and read-back once the
  #      XML is built; optional Biotic data for the trawl check
  # ---------------------------------------------------------------
  cl <- code_lists_get(session)
  # A new workbook needs a new XML
  observeEvent(input$excel_file, xml_path(NULL), priority = 10)

  acocat_level <- reactive({
    # Shared with the NASC tab when both run in the combined app
    if (is.null(input$set_acocat_level)) "specific" else input$set_acocat_level
  })

  bioticTables <- reactive({
    f <- input$aco_biotic_file
    if (is.null(f)) return(NULL)
    tryCatch({
      if (tolower(tools::file_ext(f$name)) == "xml") {
        biotic_tables_from_xml(f$datapath)
      } else {
        rd <- function(s) as.data.frame(read_excel(f$datapath, sheet = s, col_types = "text"))
        list(missions = rd("missions"), fishstations = rd("fishstations"),
             catchesamples = rd("catchesamples"), individuals = rd("individuals"))
      }
    }, error = function(e) {
      showNotification(paste("The Biotic file could not be read:", conditionMessage(e)), type = "error", duration = 8)
      NULL
    })
  })

  convertedValidation <- reactive({
    sheets <- excel_data()
    req(sheets)
    f <- validate_acoustic(sheets, cl$lists, biotic = bioticTables(), acocat_level = acocat_level())
    p <- xml_path()
    if (!is.null(p) && file.exists(p)) {
      f <- rbind(f, validate_xml_schema(p, "acoustic"),
                 readback_xml(p, "acoustic", list(distance = nrow(sheets$distances), sa = nrow(sheets$sa))))
    }
    f
  })

  uploadedValidation <- reactive({
    req(input$aco_validate_xml)
    p <- input$aco_validate_xml$datapath
    withProgress(message = "Validating the uploaded XML...", value = 0.3, {
      f_schema <- validate_xml_schema(p, "acoustic")
      sheets <- tryCatch(acoustic_sheets_from_xml(p), error = function(e) e)
      if (inherits(sheets, "error")) {
        fc <- findings_collector()
        fc$add("error", "read-back", "READBACK_FAILED", "file",
               paste("StoX's reader could not read the file:", conditionMessage(sheets)))
        return(rbind(fc$get(), f_schema))
      }
      rbind(validate_acoustic(sheets, cl$lists, biotic = bioticTables(), acocat_level = acocat_level()), f_schema)
    })
  })

  shownValidation <- reactiveVal(NULL)
  observeEvent(convertedValidation(), shownValidation(list(
    what = if (is.null(xml_path())) "the Acoustic sheets (build the XML to add the schema check)" else "the Acoustic sheets and built XML",
    f = convertedValidation())))
  observeEvent(uploadedValidation(), shownValidation(list(
    what = paste("the uploaded file", input$aco_validate_xml$name), f = uploadedValidation())))

  output$aco_validation_summary <- renderUI({
    v <- shownValidation()
    if (is.null(v)) return(p(class = "text-muted small", "Load an Acoustic workbook, or an existing Acoustic XML, to validate it."))
    tagList(
      p(class = "small mb-1", "Results for ", strong(v$what), ":"),
      findings_badges(v$f),
      if (is.null(bioticTables())) p(class = "small text-muted",
        "Add Biotic data in the sidebar to check that each acoustic category has trawl samples.")
    )
  })
  output$aco_validation_table <- DT::renderDT({
    v <- shownValidation()
    req(v)
    findings_dt(v$f)
  })
  output$aco_download_findings <- downloadHandler(
    filename = function() paste0("Acoustic_validation_", Sys.Date(), ".csv"),
    content = function(file) {
      v <- shownValidation()
      utils::write.csv(if (is.null(v)) data.frame() else v$f, file, row.names = FALSE)
    }
  )

  # Errors block the export unless overridden
  observe({
    f <- tryCatch(convertedValidation(), error = function(e) NULL)
    ok <- !is.null(xml_path()) && !is.null(f) && (sum(f$severity == "error") == 0 || isTRUE(input$aco_override))
    shinyjs::toggleState("download_xml", condition = ok)
  })
}
# ---------------------------------------------------------------
# 4) Launch the application
# ---------------------------------------------------------------
shinyApp(ui = ui, server = server)
