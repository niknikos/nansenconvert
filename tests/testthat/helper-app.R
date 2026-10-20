# Shared set-up for the tests: the project root, the helpers, and a loader
# that sources a converter script without starting it (as app.R does).

ROOT <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressMessages(library(shiny))

local({
  owd <- setwd(ROOT); on.exit(setwd(owd))
  for (f in c("code_lists.R", "taxonomy.R", "validation.R")) {
    sys.source(file.path("helpers", f), envir = globalenv())
  }
})

load_converter <- function(file) {
  env <- new.env(parent = globalenv())
  env$shinyApp <- function(ui = NULL, server = NULL, ...) {
    assign("ui", ui, envir = env); assign("server", server, envir = env); invisible(NULL)
  }
  owd <- setwd(ROOT); on.exit(setwd(owd))
  suppressMessages(sys.source(file, envir = env, chdir = TRUE))
  env
}

data_file <- function(f) file.path(ROOT, "data", f)

# The species dialogs defer their work with later::later(), which testServer
# never runs; in this test process run those callbacks straight away.
utils::assignInNamespace("later", function(func, delay = 0, loop = NULL) {
  func(); invisible(function() FALSE)
}, ns = "later")

# Runs the four BioData modules and the Biotic XML builder on the synthetic
# templates. Returns the intermediate tables and the path of the XML.
run_biotic_chain <- function(out_dir = tempfile("biotic_")) {
  dir.create(out_dir, showWarnings = FALSE)
  bio <- load_converter("BioData_To_Biotic.xls.R")
  res <- list()
  testServer(bio$mod_fishstations_server, {
    session$setInputs(st_nation = "", st_area = "", st_gear = "3500", st_gearcount = 1,
                      st_haulvalidity = "1", st_stationtype = "11",
                      st_trawldoorspread = 15, st_verticaltrawlopening = 50)
    session$setInputs(createMetadata = 1)
    metadataDF(data.frame(missiontype = "4", startyear = "2026", platform = "1172", missionnumber = "1",
                          missiontypename = "Survey", callsignal = "SYN", platformname = "Synthetic vessel",
                          cruise = "SYN2026", missionstartdate = "2026-03-10Z", missionstopdate = "2026-03-12Z",
                          purpose = "Test", stringsAsFactors = FALSE))
    session$setInputs(file = list(name = "Stations_model.xlsx", datapath = data_file("Stations_model.xlsx")))
    session$setInputs(injectMetadata = 1)
    res$stations <<- stationsData(); res$missions <<- metadataDF()
  })
  testServer(bio$mod_catchsamples_server, {
    session$setInputs(cs_conservation = "1", cs_catchproducttype = "1")
    session$setInputs(fileSP = list(name = "Catches_model.xlsx", datapath = data_file("Catches_model.xlsx")))
    if (is.null(rv_c$df)) session$setInputs(catch_worms_skip = 1)
    res$catches <<- catchSamplesData()
  })
  testServer(bio$mod_individuals_server, {
    session$setInputs(ind_lengthresolution = "2", ind_individualproducttype = "1")
    session$setInputs(fq_file = list(name = "lf.xlsx", datapath = data_file("Length-frequency_model.xlsx")))
    session$setInputs(bio_file = list(name = "bio.xlsx", datapath = data_file("Biology_model.xlsx")))
    for (k in 1:4) if (isTRUE(rv$indiv_worms_awaiting)) session$setInputs(indiv_worms_skip = k)
    session$setInputs(btn_join = 1)
    res$individuals <<- export_df()
  })
  p <- function(f) file.path(out_dir, f)
  writexl::write_xlsx(res$missions, p("m.xlsx")); writexl::write_xlsx(res$stations, p("f.xlsx"))
  writexl::write_xlsx(res$catches, p("c.xlsx")); writexl::write_xlsx(list(Individuals = res$individuals), p("i.xlsx"))
  res$biotic_xlsx <- p("Biotic_LPA.xlsx")
  testServer(bio$mod_biotic_server, {
    session$setInputs(in_missions = list(name = "m.xlsx", datapath = p("m.xlsx")),
                      in_fish = list(name = "f.xlsx", datapath = p("f.xlsx")),
                      in_catch = list(name = "c.xlsx", datapath = p("c.xlsx")),
                      in_indiv = list(name = "i.xlsx", datapath = p("i.xlsx")))
    session$setInputs(bs_sampletype = "20", bs_lengthmeasurement = "E", bs_sampleproducttype = "1")
    session$setInputs(process = 1)
    file.copy(output$download_biotic, res$biotic_xlsx, overwrite = TRUE)
  })
  bx <- load_converter("Biotic.xls_To_Biotic.xml.R")
  res$biotic_xml <- p("Biotic_LPA.xml")
  testServer(bx$server, {
    session$setInputs(excelFile = list(name = "Biotic_LPA.xlsx", datapath = res$biotic_xlsx))
    xml2::write_xml(xmlDoc(), res$biotic_xml, options = "format")
    res$validation <<- convertedValidation()
  })
  res
}

# The chain takes a while; run it once per test session
.chain <- new.env()
biotic_chain <- function() {
  if (is.null(.chain$res)) .chain$res <- run_biotic_chain()
  .chain$res
}

# A small hand-made Echoview export: two 1-nmi cells, three regions with
# known species shares, and two regions that both map to OTHER
make_nasc <- function() {
  cell <- function(interval, t1, t2, lon1, lon2) {
    data.frame(Interval = interval, Date_S = 20260310, Time_S = t1, Date_E = 20260310, Time_E = t2,
               Lat_S = 20.5, Lon_S = lon1, Lat_E = 20.5, Lon_E = lon2,
               Exclude_above_line_depth_mean = 10, Exclude_below_line_range_mean = 90,
               Exclude_below_line_range_max = 92, Exclude_below_line_depth_min = 93,
               stringsAsFactors = FALSE)
  }
  c1 <- cell(1, "07:00:00,00", "07:06:00,00", -17.5, -17.5178)
  c2 <- cell(2, "07:06:01,00", "07:12:00,00", -17.5178, -17.5356)
  row <- function(c, layer, region, prc, sp, pct) cbind(c, Layer = layer, Region_ID = region,
                                                        PRC_NASC = prc, Species_name = sp, Species_percent = pct)
  rbind(
    row(c1, 1, 1, 100, "Sardina pilchardus", 60),
    row(c1, 1, 1, 100, "Engraulis encrasicolus", 40),
    row(c1, 1, 2, 50, "Autres", 100),
    row(c1, 1, 3, 10, "Unassigned", 100),
    row(c1, 2, 4, 20, "Sardina pilchardus", 100),
    row(c2, 1, 5, 0, "Autres", 100)
  )
}

NASC_MD <- list(report_time = "2026-03-13 12:00:00", lsss_version = "Echoview", nation = "58",
                platform = "1172", cruise = "SYN2026")

read_biotic_lpa <- function(path) {
  rd <- function(s) as.data.frame(readxl::read_excel(path, sheet = s, col_types = "text"))
  list(missions = rd("missions"), fishstations = rd("fishstations"),
       catchesamples = rd("catchesamples"), individuals = rd("individuals"))
}
