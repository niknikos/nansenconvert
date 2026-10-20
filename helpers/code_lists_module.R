# "Code lists" tab: choose Nansen or Custom for each code list and edit the
# custom tables. The state lives in session$userData$code_lists (see
# code_lists_get()) so every converter reads the same lists. Custom lists are
# saved on this computer through the EAF-Nansen tool-suite state directory.

CODE_LISTS_STATE_APP <- "nansenconvert"
# Name the app used before it was renamed; lists saved under it are moved over
CODE_LISTS_STATE_APP_OLD <- "excelxmlconverter"

code_lists_state_path <- function() {
  path <- tryCatch(nansencore::toolsuite_state_file(CODE_LISTS_STATE_APP, "code_lists.csv"),
                   error = function(e) NA_character_)
  if (!is.na(path) && !file.exists(path)) {
    old <- tryCatch(nansencore::toolsuite_state_file(CODE_LISTS_STATE_APP_OLD, "code_lists.csv", create = FALSE),
                    error = function(e) NA_character_)
    if (!is.na(old) && file.exists(old)) file.copy(old, path)
  }
  path
}

code_lists_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    br(),
    fluidRow(
      column(4,
        h4("Code lists"),
        helpText("Each coded field follows either the Nansen list (the NMD reference,",
                 "or the EAF-Nansen acoustic categories for acocat) or a custom list you define."),
        selectInput(ns("list"), "Code list",
                    choices = stats::setNames(CODE_LIST_DEFS$id, CODE_LIST_DEFS$label)),
        radioButtons(ns("mode"), "Use", choices = c("Nansen" = "nansen", "Custom" = "custom"),
                     inline = TRUE),
        uiOutput(ns("used_in")),
        conditionalPanel(sprintf("input['%s'] == 'custom'", ns("mode")),
          actionButton(ns("reset_nansen"), "Fill the custom list from the Nansen list",
                       class = "btn-outline-secondary w-100 mb-2"),
          conditionalPanel(sprintf("input['%s'] == 'acocat'", ns("list")),
            actionButton(ns("reset_legacy"), "Fill it with the old LPA acocat codes",
                         class = "btn-outline-secondary w-100 mb-2")
          ),
          helpText("Right-click the table to add or remove rows. For mapped lists,",
                   "'value' is what appears in the input file; a value of * gives the code",
                   "for any value that is not listed.")
        ),
        hr(),
        h5("All code lists"),
        tableOutput(ns("summary")),
        downloadButton(ns("export"), "Export code lists (CSV)", class = "btn-primary w-100 mb-2"),
        fileInput(ns("import"), "Import code lists (CSV)", accept = ".csv", buttonLabel = "Browse…"),
        uiOutput(ns("saved_note"))
      ),
      column(8,
        uiOutput(ns("table_title")),
        conditionalPanel(sprintf("input['%s'] == 'nansen'", ns("mode")),
                         DT::DTOutput(ns("nansen_tbl"))),
        conditionalPanel(sprintf("input['%s'] == 'custom'", ns("mode")),
                         rhandsontable::rHandsontableOutput(ns("custom_tbl")))
      )
    )
  )
}

code_lists_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    state_path <- code_lists_state_path()

    # Custom lists saved on this computer in an earlier session
    if (!is.na(state_path) && file.exists(state_path)) {
      loaded <- tryCatch(code_lists_from_csv(state_path, isolate(state$lists)), error = function(e) NULL)
      if (!is.null(loaded)) state$lists <- loaded
    }
    observeEvent(state$lists, {
      if (!is.na(state_path)) try(code_lists_to_csv(state$lists, state_path), silent = TRUE)
    }, ignoreInit = TRUE)

    redraw <- reactiveVal(0)

    observeEvent(input$list, {
      updateRadioButtons(session, "mode", selected = code_list_mode(input$list, isolate(state$lists)))
    })
    observeEvent(input$mode, {
      req(input$list)
      l <- state$lists
      if (!identical(code_list_mode(input$list, l), input$mode)) {
        l[[input$list]]$mode <- input$mode
        state$lists <- l
      }
    }, ignoreInit = TRUE)

    output$used_in <- renderUI({
      req(input$list)
      d <- CODE_LIST_DEFS[CODE_LIST_DEFS$id == input$list, ]
      p(class = "small text-muted", strong("Used in: "), d$used_in)
    })

    output$table_title <- renderUI({
      req(input$list)
      d <- CODE_LIST_DEFS[CODE_LIST_DEFS$id == input$list, ]
      src <- if (input$list == "acocat") "EAF-Nansen acoustic categories (nansentaxonomy)" else "NMD reference (nansencore)"
      if (identical(input$mode, "custom")) {
        h5(d$label, "- custom list")
      } else {
        tagList(
          h5(d$label, "-", src),
          if (input$list == "acocat") p(class = "small text-muted",
            "Species get their category from nansentaxonomy; the NASC tab lists the categories used.")
        )
      }
    })

    output$nansen_tbl <- DT::renderDT({
      req(input$list)
      tab <- code_list_nansen(input$list)
      if (!CODE_LIST_DEFS$mapped[CODE_LIST_DEFS$id == input$list]) tab$value <- NULL
      DT::datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })

    output$custom_tbl <- rhandsontable::renderRHandsontable({
      req(input$list)
      redraw()
      tab <- isolate(state$lists[[input$list]]$custom)
      if (is.null(tab) || !nrow(tab)) tab <- data.frame(value = "", code = "", description = "")
      tab <- as.data.frame(lapply(tab[, c("value", "code", "description")], as.character),
                           stringsAsFactors = FALSE)
      rhandsontable::hot_context_menu(
        rhandsontable::rhandsontable(tab, stretchH = "all", rowHeaders = NULL, height = 520),
        allowRowEdit = TRUE, allowColEdit = FALSE
      )
    })

    observeEvent(input$custom_tbl, {
      req(input$list, identical(input$mode, "custom"))
      tab <- rhandsontable::hot_to_r(input$custom_tbl)
      if (!all(c("value", "code", "description") %in% names(tab))) return()
      l <- state$lists
      old <- l[[input$list]]$custom
      if (!identical(as.data.frame(old), as.data.frame(tab))) {
        l[[input$list]]$custom <- tab
        state$lists <- l
      }
    })

    reset_custom <- function(tab) {
      l <- state$lists
      l[[input$list]]$custom <- tab
      state$lists <- l
      redraw(redraw() + 1)
    }
    observeEvent(input$reset_nansen, reset_custom(code_list_nansen(input$list)))
    observeEvent(input$reset_legacy, reset_custom(ACOCAT_LEGACY_LPA))

    output$summary <- renderTable({
      l <- state$lists
      data.frame(
        List = CODE_LIST_DEFS$label,
        Uses = ifelse(vapply(CODE_LIST_DEFS$id, code_list_mode, character(1), lists = l) == "custom",
                      "Custom", "Nansen"),
        Codes = vapply(CODE_LIST_DEFS$id, function(id) nrow(code_list_table(id, l)), integer(1)),
        check.names = FALSE
      )
    }, striped = TRUE, spacing = "xs")

    output$export <- downloadHandler(
      filename = function() "code_lists.csv",
      content = function(file) code_lists_to_csv(state$lists, file)
    )

    observeEvent(input$import, {
      req(input$import)
      l <- tryCatch(code_lists_from_csv(input$import$datapath, state$lists),
                    error = function(e) { showNotification(paste("Import failed:", conditionMessage(e)),
                                                           type = "error", duration = 8); NULL })
      req(l)
      state$lists <- l
      updateRadioButtons(session, "mode", selected = code_list_mode(input$list, l))
      redraw(redraw() + 1)
      showNotification("Code lists imported.", type = "message")
    })

    output$saved_note <- renderUI({
      if (is.na(state_path)) return(NULL)
      p(class = "small text-muted", "Your lists are saved on this computer and reloaded next time.")
    })
  })
}
