# End-to-end check in a real browser: starts the app, runs the synthetic
# templates through every tab in headless Chrome (chromote) and checks what
# each step downloads. Not part of run_tests.R; run it from the project root:
#   Rscript tests/browser_e2e.R
#   Rscript tests/browser_e2e.R --screenshots docs/img   # also saves the user guide screenshots
#   Rscript tests/browser_e2e.R --out <dir>              # keeps the downloads in <dir>
args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag) if (flag %in% args) args[which(args == flag) + 1] else NA
shot_dir <- arg_value("--screenshots")
out_dir <- if (is.na(arg_value("--out"))) tempfile("e2e_") else arg_value("--out")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if (!is.na(shot_dir)) dir.create(shot_dir, showWarnings = FALSE, recursive = TRUE)

suppressMessages(library(shiny))
for (f in c("code_lists.R", "taxonomy.R", "validation.R")) {
  suppressMessages(sys.source(file.path("helpers", f), envir = globalenv()))
}

# --- App and browser ---------------------------------------------------------
port <- 3839
base <- sprintf("http://127.0.0.1:%d/", port)
app <- processx::process$new(
  file.path(R.home("bin"), "Rscript"),
  c("-e", sprintf("shiny::runApp('.', port = %d, launch.browser = FALSE)", port)),
  stdout = file.path(out_dir, "app.log"), stderr = "2>&1"
)
on.exit(app$kill(), add = TRUE)
for (i in 1:120) {
  up <- tryCatch(suppressWarnings({ readLines(base, warn = FALSE); TRUE }), error = function(e) FALSE)
  if (up) break
  Sys.sleep(1)
}
if (!up) stop("The app did not start; see ", file.path(out_dir, "app.log"))

b <- chromote::ChromoteSession$new(width = 1400, height = 900)
on.exit(b$close(), add = TRUE)

js <- function(expr) {
  # 30 s rather than chromote's 10 s: a busy machine can stall a single call
  r <- b$Runtime$evaluate(expr, awaitPromise = TRUE, returnByValue = TRUE, timeout_ = 30)
  if (!is.null(r$exceptionDetails)) stop("JS error in: ", expr, "\n", r$exceptionDetails$exception$description)
  r$result$value
}
wait_for <- function(cond, timeout = 60, what = cond) {
  t0 <- Sys.time()
  while (!isTRUE(js(cond))) {
    if (difftime(Sys.time(), t0, units = "secs") > timeout) stop("Timed out waiting for: ", what)
    Sys.sleep(0.25)
  }
}
# Shiny is idle once the busy class has been off for a moment
idle <- function(timeout = 120) {
  Sys.sleep(0.4)
  quiet <- 0
  t0 <- Sys.time()
  while (quiet < 3) {
    quiet <- if (isTRUE(js("document.documentElement.classList.contains('shiny-busy')"))) 0 else quiet + 1
    if (difftime(Sys.time(), t0, units = "secs") > timeout) stop("The app stayed busy")
    Sys.sleep(0.3)
  }
}
q <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)
click <- function(id) { js(sprintf("document.getElementById(%s).click()", q(id))); idle() }
set_text <- function(id, value) invisible(js(sprintf("$('#' + %s).val(%s).trigger('change')", q(id), q(value))))
set_select <- function(id, value) {
  invisible(js(sprintf("(function(s){ s.addOption({value: %2$s, label: %2$s}); s.setValue(%2$s); })($('#' + %1$s)[0].selectize)",
             q(id), q(value))))
}
upload <- function(id, path) {
  root <- b$DOM$getDocument()$root$nodeId
  node <- b$DOM$querySelector(root, paste0("#", id))$nodeId
  b$DOM$setFileInputFiles(files = list(normalizePath(path)), nodeId = node)
  wait_for(sprintf("$('#' + %s + '_progress .progress-bar').text().indexOf('complete') >= 0", q(id)),
           what = paste("upload of", basename(path)))
  idle()
}
# Main or sub tab, by its visible label
tab <- function(label) {
  ok <- js(sprintf("(function(){ var a = [...document.querySelectorAll('a.nav-link')]
    .filter(a => a.innerText.trim() === %s && a.offsetParent !== null)[0];
    if (a) a.click(); return !!a; })()", q(label)))
  if (!isTRUE(ok)) stop("No tab labelled ", label)
  idle()
}
modal_open <- function() isTRUE(js("!!document.querySelector('#shiny-modal .modal.show, .modal.show')"))
close_modal <- function() { js("$('#shiny-modal').modal('hide')"); Sys.sleep(0.6) }
# A species dialog: screenshot it once, then continue without corrections
skip_species_dialog <- function(button, shot_name) {
  for (k in 1:6) {
    idle()
    if (!isTRUE(js(sprintf("!!document.getElementById(%s)", q(button))))) return(invisible(FALSE))
    shot(shot_name)
    click(button)
  }
}
download <- function(id, file) {
  href <- js(sprintf("document.getElementById(%s).getAttribute('href')", q(id)))
  if (is.null(href) || !nzchar(href) || href == "#") stop("Download link ", id, " is not ready")
  dest <- file.path(out_dir, file)
  utils::download.file(paste0(base, href), dest, mode = "wb", quiet = TRUE)
  dest
}
disabled <- function(id) isTRUE(js(sprintf(
  "(function(e){ return e.classList.contains('disabled') || e.hasAttribute('disabled'); })(document.getElementById(%s))", q(id))))
text_of <- function(id) js(sprintf("(document.getElementById(%s) || {}).innerText || ''", q(id)))
shot <- function(name, selector = "html") {
  if (is.na(shot_dir)) return(invisible())
  # Notifications and "done" alerts would cover part of the page
  js("window.scrollTo(0, 0); document.documentElement.classList.add('e2e-shot');
      if (!document.getElementById('e2e-style')) document.head.insertAdjacentHTML('beforeend',
        '<style id=\"e2e-style\">.e2e-shot #shiny-notification-panel, .e2e-shot .sweet-alert,' +
        ' .e2e-shot .sweet-overlay { display: none !important; }</style>');")
  b$screenshot(file.path(shot_dir, name), selector = selector, cliprect = c(0, 0, 1400, 900),
               scale = 1, show = FALSE, delay = 0.4)
  invisible(js("document.documentElement.classList.remove('e2e-shot')"))
}

checks <- list()
check <- function(name, ok, detail = "") {
  checks[[length(checks) + 1]] <<- data.frame(check = name, ok = isTRUE(ok), detail = detail)
  cat(if (isTRUE(ok)) "  ok    " else "  FAIL  ", name, if (nzchar(detail)) paste0(" (", detail, ")"), "\n", sep = "")
}
# Error count from a Validation panel's badges ("2 error(s) 5 warning(s) …")
n_errors <- function(summary) {
  m <- regmatches(summary, regexec("(\\d+) error", summary))[[1]]
  if (length(m) == 2) as.integer(m[2]) else NA_integer_
}
sheets <- function(path) tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
rows <- function(path, sheet = 1) tryCatch(nrow(readxl::read_excel(path, sheet = sheet)), error = function(e) 0L)
tpl <- function(f) file.path("data", f)

# --- Splash ------------------------------------------------------------------
invisible(b$Page$navigate(base, timeout_ = 60))
wait_for("!!document.getElementById('btn_launch_app')", what = "the splash screen")
idle()
shot("splash.png")
click("btn_launch_app")
Sys.sleep(0.6)

# --- Field data → Biotic workbook --------------------------------------------
cat("Field data → Biotic workbook\n")
tab("BioData_To_Biotic.xls")
tab("Fishstations")
set_select("fish-st_stationtype", "11")
click("fish-createMetadata")
wait_for("(function(e){ return !!(e && HTMLWidgets.getInstance(e) && HTMLWidgets.getInstance(e).hot); })(document.getElementById('fish-metadataTable'))",
         what = "the mission metadata table")
mission <- list(missiontype = "4", startyear = "2026", platform = "1172", missionnumber = "1",
                missiontypename = "Survey", callsignal = "SYN", platformname = "Synthetic vessel",
                cruise = "SYN2026", missionstartdate = "2026-03-10Z", missionstopdate = "2026-03-12Z",
                purpose = "Test")
# rhandsontable keeps rows as arrays: set the cells by column index, in one change
invisible(js(sprintf("(function(m){ var hot = HTMLWidgets.getInstance(document.getElementById('fish-metadataTable')).hot;
  var h = hot.getColHeader();
  hot.setDataAtCell(Object.keys(m).map(k => [0, h.indexOf(k), m[k]])); })(%s)", q(mission))))
idle()
upload("fish-file", tpl("Stations_model.xlsx"))
click("fish-injectMetadata")
shot("fishstations.png")
f_st <- download("fish-downloadStations", "Fishstations.xlsx")
f_mi <- download("fish-downloadMetadataXML", "Missions.xlsx")
n_st <- rows(tpl("Stations_model.xlsx"))
check("Fishstations.xlsx has one row per station", rows(f_st) == n_st, sprintf("%d of %d", rows(f_st), n_st))
mi <- tryCatch(readxl::read_excel(f_mi), error = function(e) NULL)
check("Missions.xlsx carries the metadata typed in", !is.null(mi) && identical(as.character(mi$cruise[1]), "SYN2026"))

tab("Catchsamples")
upload("catch-fileSP", tpl("Catches_model.xlsx"))
skip_species_dialog("catch-catch_worms_skip", "catch_species_dialog.png")
wait_for("document.querySelectorAll('#catch-previewCatch table tbody tr').length > 1", what = "the catches preview")
idle()
shot("catchsamples.png")
f_ca <- download("catch-downloadCatchsamples", "Catchsamples.xlsx")
n_ca <- rows(tpl("Catches_model.xlsx"))
check("Catchsamples.xlsx has one row per catch", rows(f_ca) == n_ca, sprintf("%d of %d", rows(f_ca), n_ca))

tab("Individuals")
upload("indiv-fq_file", tpl("Length-frequency_model.xlsx"))
skip_species_dialog("indiv-indiv_worms_skip", "individuals_species_dialog.png")
upload("indiv-bio_file", tpl("Biology_model.xlsx"))
skip_species_dialog("indiv-indiv_worms_skip", "individuals_species_dialog.png")
click("indiv-btn_join")
shot("individuals.png")
f_in <- download("indiv-dl_individuals", "Individuals.xlsx")
check("Individuals.xlsx has fish", rows(f_in) > 0, sprintf("%d fish", rows(f_in)))

tab("Biotic")
upload("biotic-in_missions", f_mi)
upload("biotic-in_fish", f_st)
upload("biotic-in_catch", f_ca)
upload("biotic-in_indiv", f_in)
click("biotic-process")
shot("biotic_workbook.png")
f_bx <- download("biotic-download_biotic", "Biotic.xlsx")
check("Biotic.xlsx has the four NMDBiotic sheets",
      setequal(sheets(f_bx), c("missions", "fishstations", "catchesamples", "individuals")),
      paste(sheets(f_bx), collapse = ", "))

# --- Biotic workbook → Biotic.xml --------------------------------------------
cat("Biotic workbook → Biotic.xml\n")
tab("Biotic.xls_To_Biotic.xml")
upload("excelFile", f_bx)
wait_for("(document.getElementById('validationSummary') || {}).innerText.length > 0", what = "the Biotic validation summary")
idle()
summary_b <- gsub("\\s+", " ", text_of("validationSummary"))
shot("biotic_xml.png")
check("Biotic validation reports no errors", identical(n_errors(summary_b), 0L), summary_b)
check("Biotic download is enabled", !disabled("downloadXML"))
f_bxml <- download("downloadXML", "Biotic.xml")
check("Biotic.xml is valid against NMDBiotic v3.1", nrow(validate_xml_schema(f_bxml, "biotic")) == 0)
rb <- readback_xml(f_bxml, "biotic")
check("Biotic.xml reads back with RstoxData", !any(rb$severity == "error"),
      paste(unique(rb$message), collapse = "; "))

# --- NansIS → Biotic.xml -----------------------------------------------------
# Only the tab itself: a real export needs a NansIS database and its login
cat("NansIS → Biotic.xml\n")
tab("Nansis2Biotic.xml")
n_regions <- js("Object.keys(($('#nansis-regions')[0] || {}).selectize ? $('#nansis-regions')[0].selectize.options : {}).length")
n_checks <- js("document.querySelectorAll('#nansis-integrity_checks input[type=checkbox]').length")
check("NansIS tab lists regions and integrity checks", n_regions > 0 && n_checks > 0,
      sprintf("%d regions, %d checks", n_regions, n_checks))
# The host must start empty, so this click cannot reach any database
host <- js("$('#nansis-host')[0].selectize.getValue()")
check("NansIS tab starts with no host chosen", identical(host, ""), sprintf("host '%s'", host))
if (identical(host, "")) click("nansis-connect")
check("NansIS tab asks for a host before connecting", grepl("host", text_of("nansis-connection_status")))
shot("nansis.png")

# --- NASC → Acoustic workbook ------------------------------------------------
cat("NASC → Acoustic workbook\n")
tab("NASC.xls_To_Acoustic.xml")
upload("file_nasc", tpl("NASC_model.xlsx"))
set_text("md_report_time", "2026-03-13 12:00:00")
set_text("md_lsss_version", "Echoview")
set_select("md_nation", "58")
set_select("md_platform", "1172")
set_text("md_cruise", "SYN2026")
idle()
click("btn_process")
wait_for("!!document.getElementById('tbl_species_pct') || document.querySelectorAll('.modal.show').length > 0",
         timeout = 120, what = "the NASC processing")
idle()
if (modal_open()) close_modal()
shot("nasc.png")
f_ax <- download("dl_acoustic_xlsx", "Acoustic.xlsx")
check("Acoustic.xlsx has the six sheets",
      setequal(sheets(f_ax), c("root_metadata", "distances", "frequencies", "ch_types", "sa_by_acocat", "sa")),
      paste(sheets(f_ax), collapse = ", "))

# --- Acoustic workbook → Acoustic XML ----------------------------------------
cat("Acoustic workbook → Acoustic XML\n")
tab("Acoustic.xls_To_Acoustic.xml")
upload("excel_file", f_ax)
upload("aco_biotic_file", f_bxml)
click("reconstruct_btn")
wait_for("(document.getElementById('aco_validation_summary') || {}).innerText.length > 0",
         timeout = 120, what = "the Acoustic validation summary")
idle()
summary_a <- gsub("\\s+", " ", text_of("aco_validation_summary"))
shot("acoustic_xml.png")
check("Acoustic validation reports no errors", identical(n_errors(summary_a), 0L), summary_a)
check("Acoustic export is enabled", !disabled("download_xml"))
f_axml <- download("download_xml", "Acoustic.xml")
check("Acoustic XML is valid against NMD Echosounder v1", nrow(validate_xml_schema(f_axml, "acoustic")) == 0)

# --- Code lists, templates, user guide ---------------------------------------
cat("Other tabs\n")
tab("Code lists")
shot("code_lists.png")
check("Code lists tab lists every code list", grepl("Nansen", text_of("code_lists-summary")))
tab("Excel file templates")
shot("templates.png")
f_zip <- download("dl_excel_model_zip", "templates.zip")
check("Templates ZIP holds the five templates", length(utils::unzip(f_zip, list = TRUE)$Name) >= 5)
tab("User_Guide")
f_pdf <- download("dl_user_guide", "User_Guide.pdf")
check("User guide downloads as a PDF", identical(readBin(f_pdf, "raw", 4), charToRaw("%PDF")))

res <- do.call(rbind, checks)
cat(sprintf("\n%d checks: %d passed, %d failed. Downloads are in %s\n", nrow(res), sum(res$ok), sum(!res$ok), out_dir))
if (any(!res$ok)) quit(status = 1)
