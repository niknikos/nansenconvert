# "Excel file templates" tab: templates stored in data/ (sourced by app.R)

excel_model_bases <- c(
  "Length-frequency_model",
  "Biology_model",
  "Catches_model",
  "Stations_model",
  "NASC_model"
)

EXCEL_MODEL_LABELS <- c(
  "Length-frequency (Length-frequency_model)" = "Length-frequency_model",
  "Biology (Biology_model)" = "Biology_model",
  "Catches (Catches_model)" = "Catches_model",
  "Stations (Stations_model)" = "Stations_model",
  "NASC (NASC_model)" = "NASC_model"
)

resolve_excel_model_path <- function(base_name, data_dir) {
  if (is.null(data_dir) || !nzchar(data_dir) || !dir.exists(data_dir)) {
    return(NA_character_)
  }
  for (e in c(".xlsx", ".XLSX", ".xls", ".XLS")) {
    fp <- file.path(data_dir, paste0(base_name, e))
    if (file.exists(fp)) {
      return(normalizePath(fp, winslash = "/"))
    }
  }
  NA_character_
}

build_excel_model_choices <- function() {
  c("Select…" = "", EXCEL_MODEL_LABELS)
}

ui_excel_files_model <- function() {
  fluidPage(
    fluidRow(
      column(6,
        h5("Single template"),
        selectizeInput("excel_model_pick", "Choose a template", choices = build_excel_model_choices(),
          selected = "", options = list(placeholder = "Select a file…")
        ),
        helpText("Downloads one of the Excel templates stored in data/."),
        downloadButton("dl_excel_model_one", "Download template", class = "btn-primary w-100")
      ),
      column(6,
        h5("Archive"),
        helpText("Creates ", tags$code("nansenconvert_templates.zip"), " (all available templates)."),
        downloadButton("dl_excel_model_zip", "Download all templates (ZIP)", class = "btn-success w-100")
      )
    )
  )
}

excel_models_server <- function(input, output, session, data_dir) {
  output$dl_excel_model_one <- downloadHandler(
    filename = function() {
      pick <- input$excel_model_pick
      req(!is.null(pick), nzchar(pick))
      p <- resolve_excel_model_path(pick, data_dir)
      if (is.na(p) || !file.exists(p)) "template.xlsx" else basename(p)
    },
    content = function(file) {
      pick <- input$excel_model_pick
      if (is.null(pick) || !nzchar(pick)) {
        showNotification("Choose a template from the list.", type = "warning", duration = 4)
        return()
      }
      p <- resolve_excel_model_path(pick, data_dir)
      if (is.na(p) || !file.exists(p)) {
        showNotification("Template file not found in data/.", type = "error", duration = 5)
        con <- file(file, "wb")
        on.exit(close(con), add = FALSE)
        writeBin(raw(0L), con)
        return()
      }
      file.copy(p, file, overwrite = TRUE)
    }
  )

  output$dl_excel_model_zip <- downloadHandler(
    filename = function() "nansenconvert_templates.zip",
    content = function(file) {
      paths <- vapply(
        excel_model_bases, resolve_excel_model_path, character(1L), data_dir = data_dir
      )
      ok <- paths[!is.na(paths) & file.exists(paths)]
      if (length(ok) < 1L) {
        showNotification("No template in data/: the ZIP is empty.", type = "warning", duration = 6)
        con <- file(file, "wb")
        on.exit(close(con), add = FALSE)
        writeBin(raw(0L), con)
        return()
      }
      ddir <- normalizePath(dirname(ok[[1L]]), winslash = "/", mustWork = TRUE)
      nms <- basename(ok)
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zip(file, ok, mode = "cherry-pick")
        return(invisible(NULL))
      }
      owd <- getwd()
      on.exit(setwd(owd), add = TRUE)
      setwd(ddir)
      utils::zip(file, nms, zip = "zip")
    }
  )
}
