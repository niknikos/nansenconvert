# app.R - NansIS database -> NMDBiotic v3.1 XML
# The export itself is rnansisclient's (EAF-Nansen tool suite): this tab adds
# the connection, the choice of surveys and the downloads, and runs the same
# validation as the Biotic XML tab on every exported file.
library(shiny)
library(bslib)
library(DT)
library(shinyjs)
if (!exists("validate_biotic_xml_file", mode = "function")) {
  for (h in file.path("helpers", c("code_lists.R", "taxonomy.R", "validation.R"))) source(h, local = TRUE)
}

# Servers are typed by the user; only a copy on this computer is offered
NANSIS_HOSTS <- c("localhost")

have_rnansisclient <- function() requireNamespace("rnansisclient", quietly = TRUE)

# Last survey year rnansisclient exports (its survey numbers cover the years
# before 2022; suite decision D17)
nansis_max_year <- function() {
  tryCatch(get(".MAX_SURVEY_YEAR", envir = asNamespace("rnansisclient")), error = function(e) 2021L)
}

# Region (EEZ / high seas) choices for the inventory filter, from nansencore
nansis_region_choices <- function() {
  eez <- tryCatch(nansencore::load_eez_boundaries(), error = function(e) NULL)
  if (is.null(eez) || !nrow(eez)) return(character(0))
  codes <- as.character(eez$mrgid_eez)
  labels <- ifelse(is.na(eez$geoname) | !nzchar(eez$geoname), codes, as.character(eez$geoname))
  ord <- order(labels)
  stats::setNames(codes[ord], labels[ord])
}

# Survey numbers typed by hand: separated by commas, semicolons or spaces
nansis_parse_surveys <- function(x) {
  s <- trimws(unlist(strsplit(if (is.null(x)) "" else x, "[,;[:space:]]+")))
  unique(s[nzchar(s)])
}

nansis_zip <- function(zipfile, files) {
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(zipfile, files, mode = "cherry-pick")
  } else {
    utils::zip(zipfile, files, flags = "-j -q")
  }
}

nansis_card <- function(title, ...) {
  div(class = "card mb-3",
      div(class = "card-header fw-semibold", title),
      div(class = "card-body", ...))
}

nansis2biotic_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    titlePanel("NansIS → Biotic.xml converter"),
    if (!have_rnansisclient()) {
      div(class = "alert alert-warning",
          strong("This tab needs the rnansisclient package"), " (EAF-Nansen tool suite). Install it with ",
          code('remotes::install_git("https://git.imr.no/cdcf/nansen-toolsuite/rnansisclient.git")'),
          " and restart the app.")
    },
    fluidRow(
      column(4,
        nansis_card("1. Connect to NansIS",
          # Starts empty: selectize would otherwise pick the first host by itself
          selectizeInput(ns("host"), "Host", choices = NANSIS_HOSTS, selected = character(0),
                         options = list(create = TRUE, placeholder = "Server name or localhost",
                                        onInitialize = I("function() { this.setValue(''); }"))),
          fluidRow(
            column(5, numericInput(ns("port"), "Port", value = 5432, min = 1, max = 65535)),
            column(7, textInput(ns("dbname"), "Database", value = "nansis"))
          ),
          textInput(ns("username"), "Username", value = ""),
          passwordInput(ns("password"), "Password", value = ""),
          div(class = "d-flex gap-2",
              actionButton(ns("connect"), "Connect", class = "btn-primary"),
              actionButton(ns("disconnect"), "Disconnect", class = "btn-outline-secondary")),
          uiOutput(ns("connection_status")),
          helpText("The password is used only to open the connection; it is not saved.")
        ),
        nansis_card("2. Choose surveys",
          helpText(sprintf("rnansisclient exports surveys up to %d. Later surveys are listed but cannot be chosen.",
                           nansis_max_year())),
          numericInput(ns("vessel_type"), "Vessel type digit (4 = Dr. Fridtjof Nansen)", value = 4, min = 0, max = 9),
          selectizeInput(ns("regions"), "Regions / EEZs / high seas", choices = nansis_region_choices(),
                         multiple = TRUE, options = list(placeholder = "All regions")),
          actionButton(ns("load_inventory"), "Load survey inventory", class = "btn-outline-primary w-100 mb-2"),
          textInput(ns("survey_numbers"), "…or type survey numbers", placeholder = "e.g. 2017401, 2017402"),
          textOutput(ns("selection_summary"))
        ),
        nansis_card("3. Export",
          numericInput(ns("start_serial"), "Start serial number (one survey only, optional)", value = NA, min = 1),
          checkboxInput(ns("include_empty"), "Include no-catch stations as empty entries", value = TRUE),
          actionButton(ns("export"), "Export to Biotic XML", class = "btn-success w-100")
        ),
        nansis_card("4. Download",
          downloadButton(ns("download_xml"), "Download the shown survey's XML", class = "btn-primary w-100 mb-2"),
          checkboxInput(ns("override"), "Allow the download even if validation finds errors", FALSE),
          downloadButton(ns("download_zip"), "Download all files (ZIP)", class = "btn-outline-primary w-100"),
          helpText("The ZIP holds every exported XML, rnansisclient's reports (missing species,",
                   "code collisions, dry-run correction scripts), the validation findings and a manifest.")
        )
      ),
      column(8,
        tabsetPanel(
          id = ns("main_tabs"),
          tabPanel("Surveys", value = "surveys",
            br(),
            p(class = "text-muted small", "Load the inventory, then click rows to choose the surveys to export."),
            div(class = "d-flex gap-2 mb-2",
                actionButton(ns("select_all"), "Select all", class = "btn-sm btn-outline-secondary"),
                actionButton(ns("clear_selection"), "Clear selection", class = "btn-sm btn-outline-secondary")),
            DTOutput(ns("inventory"))
          ),
          tabPanel("Export", value = "export",
            br(),
            DTOutput(ns("results")),
            hr(),
            h5("Validation"),
            uiOutput(ns("validation_summary")),
            DTOutput(ns("validation_table")),
            div(class = "mt-2 d-flex flex-wrap gap-2",
                downloadButton(ns("download_findings"), "Download findings (CSV)", class = "btn-outline-secondary btn-sm"),
                actionButton(ns("check_worms"), "Check unresolved names in WoRMS", icon = icon("globe"),
                             class = "btn-outline-primary btn-sm")),
            uiOutput(ns("worms_result")),
            hr(),
            h5("rnansisclient export log"),
            verbatimTextOutput(ns("export_log"))
          ),
          tabPanel("Integrity", value = "integrity",
            br(),
            p(class = "text-muted small",
              "Checks the NansIS data itself for the surveys chosen in step 2. Read-only: it never changes the",
              "database, and its correction script is a dry run that ends in ROLLBACK."),
            checkboxGroupInput(ns("integrity_checks"), "Checks to run",
                               choices = if (have_rnansisclient()) {
                                 ic <- rnansisclient::list_integrity_checks(); stats::setNames(ic$id, ic$label)
                               } else character(0),
                               selected = if (have_rnansisclient()) rnansisclient::list_integrity_checks()$id else character(0)),
            actionButton(ns("run_integrity"), "Run integrity check", class = "btn-warning"),
            uiOutput(ns("integrity_status")),
            tableOutput(ns("integrity_summary")),
            uiOutput(ns("integrity_picker")),
            DTOutput(ns("integrity_detail")),
            downloadButton(ns("download_integrity"), "Download integrity report (ZIP)", class = "btn-outline-primary btn-sm mt-2")
          )
        )
      )
    )
  )
}

nansis2biotic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    cl <- code_lists_get(session)
    work_dir <- file.path(tempdir(), paste0("nansis2biotic_", session$token))
    rv <- reactiveValues(con = NULL, conn = NULL, inventory = NULL, results = NULL,
                         out_dir = NULL, integrity = NULL, integrity_dir = NULL, worms = NULL)

    connected <- reactive(!is.null(rv$con) && isTRUE(tryCatch(rnansisclient::is_connected(rv$con),
                                                              error = function(e) FALSE)))
    disconnect <- function() {
      con <- isolate(rv$con)
      if (!is.null(con)) try(suppressMessages(rnansisclient::nansis_disconnect(con)), silent = TRUE)
      rv$con <- NULL
    }
    session$onSessionEnded(function() {
      con <- isolate(rv$con)
      if (!is.null(con)) try(suppressMessages(rnansisclient::nansis_disconnect(con)), silent = TRUE)
      unlink(work_dir, recursive = TRUE)
    })

    # --- 1. Connection ------------------------------------------------------
    observeEvent(input$connect, {
      req(have_rnansisclient())
      host <- trimws(if (is.null(input$host)) "" else input$host)
      if (!nzchar(host)) {
        rv$conn <- list(ok = FALSE, msg = "Choose or type a host.")
        return()
      }
      disconnect()
      con <- tryCatch(suppressMessages(rnansisclient::nansis_connect(
        host = host, port = input$port, username = input$username, password = input$password,
        dbname = input$dbname)), error = function(e) e)
      # The password is not kept once the connection is open (or has failed)
      updateTextInput(session, "password", value = "")
      if (inherits(con, "error")) {
        rv$conn <- list(ok = FALSE, msg = paste("Connection failed:", conditionMessage(con)))
        return()
      }
      rv$con <- con
      rv$conn <- list(ok = TRUE, msg = sprintf("Connected to %s (database %s).", host, input$dbname))
    })
    observeEvent(input$disconnect, {
      disconnect()
      rv$conn <- list(ok = FALSE, msg = "Disconnected.")
    })
    output$connection_status <- renderUI({
      s <- rv$conn
      if (is.null(s)) return(p(class = "small text-muted mt-2", "Not connected."))
      p(class = paste("small mt-2", if (isTRUE(s$ok)) "text-success" else "text-danger"), s$msg)
    })

    # --- 2. Surveys ---------------------------------------------------------
    observeEvent(input$load_inventory, {
      if (!connected()) {
        showNotification("Connect to NansIS first.", type = "warning")
        return()
      }
      inv <- tryCatch(withProgress(message = "Loading the survey inventory…", value = 0.5,
        rnansisclient::list_available_surveys(rv$con, vessel_type = input$vessel_type,
                                              regions = input$regions)), error = function(e) e)
      if (inherits(inv, "error")) {
        showNotification(paste("Could not load the inventory:", conditionMessage(inv)), type = "error", duration = 10)
        return()
      }
      inv$survey <- as.character(inv$survey)
      rv$inventory <- inv
      updateTabsetPanel(session, "main_tabs", selected = "surveys")
    })

    ineligible_note <- sprintf("No, after %d", nansis_max_year())
    output$inventory <- renderDT({
      req(rv$inventory)
      df <- rv$inventory
      df$eligible <- ifelse(df$eligible, "Yes", ineligible_note)
      names(df)[names(df) == "eligible"] <- "exportable"
      datatable(df, selection = "multiple", rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE)) |>
        formatStyle("exportable", target = "row",
                    color = styleEqual(c("Yes", ineligible_note), c("inherit", "#999999")),
                    fontStyle = styleEqual(c("Yes", ineligible_note), c("normal", "italic")))
    })
    inventory_proxy <- dataTableProxy("inventory")
    observeEvent(input$select_all, {
      req(rv$inventory)
      selectRows(inventory_proxy, which(rv$inventory$eligible))
    })
    observeEvent(input$clear_selection, selectRows(inventory_proxy, NULL))
    # DT cannot make a row unselectable, so a click on a later survey is undone
    observeEvent(input$inventory_rows_selected, {
      req(rv$inventory)
      rows <- input$inventory_rows_selected
      bad <- rows[!rv$inventory$eligible[rows]]
      if (length(bad)) {
        showNotification(sprintf("Survey(s) %s are after %d and cannot be exported.",
                                 paste(rv$inventory$survey[bad], collapse = ", "), nansis_max_year()),
                         type = "warning")
        selectRows(inventory_proxy, setdiff(rows, bad))
      }
    })

    chosen_surveys <- reactive({
      picked <- character(0)
      if (!is.null(rv$inventory)) {
        rows <- input$inventory_rows_selected
        rows <- rows[rv$inventory$eligible[rows]]
        picked <- rv$inventory$survey[rows]
      }
      unique(c(picked, nansis_parse_surveys(input$survey_numbers)))
    })
    output$selection_summary <- renderText({
      n <- length(chosen_surveys())
      if (n == 0) "No surveys chosen." else sprintf("%d survey(s) chosen: %s", n, paste(chosen_surveys(), collapse = ", "))
    })

    # --- 3. Export and validation ------------------------------------------
    observeEvent(input$export, {
      if (!connected()) {
        showNotification("Connect to NansIS first.", type = "warning")
        return()
      }
      surveys <- chosen_surveys()
      if (!length(surveys)) {
        showNotification("Choose surveys in the inventory, or type survey numbers.", type = "warning")
        return()
      }
      start_serial <- if (length(surveys) == 1 && isTRUE(input$start_serial > 0)) input$start_serial else NULL
      out_dir <- file.path(work_dir, format(Sys.time(), "export_%Y%m%d_%H%M%S"))
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      lists <- cl$lists
      res <- list()
      withProgress(message = "Exporting from NansIS", value = 0, {
        for (i in seq_along(surveys)) {
          s <- surveys[i]
          incProgress(1 / length(surveys), detail = sprintf("survey %s (%d of %d)", s, i, length(surveys)))
          xml <- file.path(out_dir, sprintf("Biotic_%s.xml", s))
          r <- tryCatch(rnansisclient::export_biotic(rv$con, s, xml, start_serial_no = start_serial,
                                                     include_empty_stations = isTRUE(input$include_empty)),
                        error = function(e) list(success = FALSE, message = conditionMessage(e)))
          ok <- isTRUE(r$success) && file.exists(xml)
          res[[s]] <- list(survey = s, result = r, xml = if (ok) xml else NA_character_,
                           findings = if (ok) validate_biotic_xml_file(xml, lists) else NULL)
        }
      })
      rv$results <- res
      rv$out_dir <- out_dir
      rv$worms <- NULL
      updateTabsetPanel(session, "main_tabs", selected = "export")
    })

    results_table <- reactive({
      req(rv$results)
      do.call(rbind, lapply(rv$results, function(x) {
        r <- x$result
        n <- if (is.null(x$findings)) c(error = NA, warning = NA, info = NA) else findings_summary(x$findings)
        num <- function(v) if (is.null(v)) NA_integer_ else as.integer(v)
        data.frame(Survey = x$survey, Exported = if (isTRUE(r$success)) "Yes" else "No",
                   Stations = num(r$stations_exported), `Catch samples` = num(r$catchsamples_exported),
                   Individuals = num(r$individuals_exported), `NansIS warnings` = num(r$warning_count),
                   Errors = n[["error"]], Warnings = n[["warning"]], Notes = n[["info"]],
                   check.names = FALSE, stringsAsFactors = FALSE)
      }))
    })
    output$results <- renderDT({
      datatable(results_table(), rownames = FALSE, selection = list(mode = "single", selected = 1),
                options = list(dom = "t", pageLength = 500, ordering = FALSE))
    })
    # The survey whose validation and log are shown, and whose XML downloads
    shown <- reactive({
      req(rv$results)
      i <- input$results_rows_selected
      if (!length(i)) i <- 1
      rv$results[[min(i, length(rv$results))]]
    })

    output$validation_summary <- renderUI({
      if (is.null(rv$results)) return(p(class = "text-muted small", "Export surveys to validate them."))
      x <- shown()
      if (is.null(x$findings)) {
        return(div(class = "alert alert-danger small", "Survey ", strong(x$survey), " was not exported: ",
                   if (is.null(x$result$message)) "no reason given." else x$result$message))
      }
      tagList(p(class = "small mb-1", "Results for ", strong(basename(x$xml)), ":"), findings_badges(x$findings))
    })
    output$validation_table <- renderDT({
      x <- shown()
      req(x$findings)
      findings_dt(x$findings)
    })
    output$export_log <- renderText({
      x <- shown()
      paste(c(x$result$message, if (length(x$result$warnings) && nzchar(x$result$warnings)) c("", x$result$warnings)),
            collapse = "\n")
    })
    output$download_findings <- downloadHandler(
      filename = function() sprintf("Biotic_%s_validation.csv", shown()$survey),
      content = function(file) {
        f <- shown()$findings
        utils::write.csv(if (is.null(f)) data.frame() else f, file, row.names = FALSE)
      }
    )

    # Errors block the download unless overridden, as in the Biotic XML tab
    observe({
      x <- tryCatch(shown(), error = function(e) NULL)
      ok <- !is.null(x) && !is.na(x$xml) &&
        (sum(x$findings$severity == "error") == 0 || isTRUE(input$override))
      shinyjs::toggleState("download_xml", condition = ok)
      shinyjs::toggleState("download_zip", condition = !is.null(rv$results))
    })
    output$download_xml <- downloadHandler(
      filename = function() basename(shown()$xml),
      content = function(file) file.copy(shown()$xml, file, overwrite = TRUE),
      contentType = "application/xml"
    )
    output$download_zip <- downloadHandler(
      filename = function() format(Sys.time(), "NansIS_Biotic_%Y%m%d_%H%M.zip"),
      content = function(file) {
        req(rv$results, rv$out_dir)
        for (x in rv$results) {
          if (!is.null(x$findings)) {
            utils::write.csv(x$findings, file.path(rv$out_dir, sprintf("Biotic_%s_validation.csv", x$survey)),
                             row.names = FALSE)
          }
        }
        utils::write.csv(results_table(), file.path(rv$out_dir, "export_manifest.csv"), row.names = FALSE)
        nansis_zip(file, list.files(rv$out_dir, full.names = TRUE))
      },
      contentType = "application/zip"
    )

    observeEvent(input$check_worms, {
      f <- tryCatch(shown()$findings, error = function(e) NULL)
      nm <- if (is.null(f)) character(0) else f$where[f$check == "TAX_UNRESOLVED"]
      if (!length(nm)) {
        rv$worms <- "No unresolved names to check."
        return()
      }
      withProgress(message = "Checking names in WoRMS…", value = 0.4, rv$worms <- taxonomy_check_worms(nm))
    })
    output$worms_result <- renderUI({
      res <- rv$worms
      if (is.null(res)) return(NULL)
      if (is.character(res)) return(div(class = "alert alert-info small mt-2", res))
      tagList(h6(class = "mt-3", "WoRMS check"), tableOutput(session$ns("worms_table")))
    })
    output$worms_table <- renderTable({
      res <- rv$worms
      req(is.data.frame(res))
      res[, intersect(c("query", "worms_status", "valid_name", "valid_AphiaID", "note"), names(res)), drop = FALSE]
    }, striped = TRUE, spacing = "xs")

    # --- Integrity check ----------------------------------------------------
    observeEvent(input$run_integrity, {
      if (!connected()) {
        showNotification("Connect to NansIS first.", type = "warning")
        return()
      }
      surveys <- chosen_surveys()
      if (!length(surveys)) {
        showNotification("Choose the surveys to check in step 2.", type = "warning")
        return()
      }
      if (!length(input$integrity_checks)) {
        showNotification("Choose at least one check.", type = "warning")
        return()
      }
      dir <- file.path(work_dir, format(Sys.time(), "integrity_%Y%m%d_%H%M%S"))
      res <- tryCatch(withProgress(message = "Checking the NansIS data…", value = 0.3, {
        r <- rnansisclient::run_integrity_checks(rv$con, surveys = surveys, vessel_type = input$vessel_type,
                                                 checks = input$integrity_checks)
        incProgress(0.5, detail = "writing the report")
        r$report <- rnansisclient::write_integrity_report(r, dir, surveys = surveys,
                                                          vessel_type = input$vessel_type)
        r
      }), error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(paste("Integrity check failed:", conditionMessage(res)), type = "error", duration = 10)
        return()
      }
      res$surveys <- surveys
      rv$integrity <- res
      rv$integrity_dir <- dir
    })
    output$integrity_status <- renderUI({
      r <- rv$integrity
      if (is.null(r)) return(p(class = "text-muted small mt-2", "Not run yet."))
      s <- r$summary
      n_find <- sum(s$n_findings[s$status == "FINDINGS"], na.rm = TRUE)
      div(class = "alert alert-info small mt-3",
          sprintf("%d survey(s) checked with %d check(s): %d finding(s)%s.", length(r$surveys), nrow(s), n_find,
                  if (any(s$status == "ERROR")) sprintf("; %d check(s) could not run", sum(s$status == "ERROR")) else ""),
          if (!is.null(r$report$corrections_sql))
            span(" The report includes a dry-run correction script: review it before running it."))
    })
    output$integrity_summary <- renderTable({
      req(rv$integrity)
      s <- rv$integrity$summary
      s <- s[order(-ifelse(is.na(s$n_findings), -1, s$n_findings), s$label), c("label", "status", "n_findings")]
      names(s) <- c("Check", "Result", "Findings")
      s
    }, striped = TRUE, spacing = "xs")
    output$integrity_picker <- renderUI({
      req(rv$integrity)
      ids <- names(rv$integrity$details)
      if (!length(ids)) return(helpText("No findings to show."))
      lab <- rv$integrity$summary$label[match(ids, rv$integrity$summary$id)]
      selectInput(session$ns("integrity_pick"), "Show findings for", choices = stats::setNames(ids, lab))
    })
    output$integrity_detail <- renderDT({
      req(rv$integrity, input$integrity_pick)
      datatable(rv$integrity$details[[input$integrity_pick]], options = list(pageLength = 15, scrollX = TRUE))
    })
    observe(shinyjs::toggleState("download_integrity", condition = !is.null(rv$integrity)))
    output$download_integrity <- downloadHandler(
      filename = function() format(Sys.time(), "NansIS_integrity_%Y%m%d_%H%M.zip"),
      content = function(file) {
        req(rv$integrity_dir)
        nansis_zip(file, list.files(rv$integrity_dir, full.names = TRUE))
      },
      contentType = "application/zip"
    )
  })
}

ui <- nansis2biotic_ui("nansis")
server <- function(input, output, session) nansis2biotic_server("nansis")

# Launch the application
shinyApp(ui, server)
