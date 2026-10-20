checks_of <- function(f) unique(f$check)
nasc_conv <- load_converter("NASC.xls_To_Acoustic.xml.R")
aco_conv <- load_converter("Acoustic.xls_To_Acoustic.xml.R")
template_sheets <- function() {
  nasc_conv$build_acoustic_sheets(as.data.frame(readxl::read_excel(data_file("NASC_model.xlsx"))),
                                       NASC_MD, nasc_conv$NASC_ACOUSTIC_DEFAULTS)$acoustic_sheets
}

test_that("planted Biotic errors are found", {
  tabs <- read_biotic_workbook(biotic_chain()$biotic_xlsx)
  tabs$fishstations$serialnumber[2] <- tabs$fishstations$serialnumber[1]
  tabs$fishstations$distance[3] <- "61.2"
  tabs$fishstations$gear[4] <- "9999"
  tabs$catchesamples$catchweight[1] <- "-3"
  tabs$catchesamples$aphia[2] <- NA
  tabs$catchesamples$scientificname[3] <- "Sardina pilchardis"
  tabs$individuals$sex[1:3] <- "7"
  tabs$individuals$length[5] <- as.character(as.numeric(tabs$individuals$length[5]) * 100)
  f <- validate_biotic(tabs, check_land = FALSE)
  expect_true(all(c("XF_SERIAL_REUSED", "CATCH_ORPHAN", "STN_DISTANCE_RANGE", "STN_EFFORT_INCONSISTENT",
                    "CODE_INVALID", "CATCH_NEGATIVE", "TAX_APHIA_MISSING", "TAX_UNRESOLVED",
                    "IND_LENGTH_UNIT") %in% checks_of(f)))
  expect_true(any(f$severity == "error"))
  expect_match(f$suggestion[f$check == "TAX_UNRESOLVED"], "Sardina pilchardus")
})

test_that("a station on land and positions far from the recorded distance are flagged", {
  tabs <- read_biotic_workbook(biotic_chain()$biotic_xlsx)
  tabs$fishstations$latitudestart[1] <- "20.8"; tabs$fishstations$longitudestart[1] <- "-15.9"   # inland
  tabs$fishstations$latitudeend[2] <- as.character(as.numeric(tabs$fishstations$latitudeend[2]) + 1)
  f <- validate_biotic(tabs)
  expect_true(all(c("STN_ON_LAND", "STN_DISTANCE_POSITIONS") %in% checks_of(f)))
})

test_that("planted Acoustic errors are found", {
  sh <- template_sheets()
  sh$distances$lat_start[5] <- 999
  sh$distances$start_time[8] <- sh$distances$start_time[7]
  sh$sa$value[3] <- -10
  sh$sa$acocat[4] <- 77
  sh$root_metadata$platform <- "Nansen"
  f <- validate_acoustic(sh, biotic = list(catchesamples = data.frame(scientificname = "Boops boops")),
                         check_land = FALSE)
  expect_true(all(c("ACO_COORD_RANGE", "ACO_NASC_NEGATIVE", "ACO_ACOCAT_INVALID", "ACO_PLATFORM_INTEGER",
                    "ACO_NO_TRAWL_SUPPORT") %in% checks_of(f)))
})

test_that("Species_percent that does not add up to 100 is flagged", {
  df <- make_nasc()
  expect_equal(nrow(validate_species_percent(df)), 0)
  df$Species_percent[1] <- 80
  f <- validate_species_percent(df)
  expect_equal(f$check, "ACO_SPECIES_PERCENT")
  expect_match(f$message, "120")
})

test_that("an uploaded Acoustic XML can be validated on its own", {
  sh <- nasc_conv$build_acoustic_sheets(make_nasc(), NASC_MD, nasc_conv$NASC_ACOUSTIC_DEFAULTS)$acoustic_sheets
  f <- tempfile(fileext = ".xml"); xml2::write_xml(aco_conv$build_echosounder_xml(sh), f)
  back <- acoustic_sheets_from_xml(f)
  expect_equal(nrow(back$distances), 2)
  expect_equal(sum(back$sa$value), sum(sh$sa$value))
  expect_equal(sum(validate_acoustic(back, check_land = FALSE)$severity == "error"), 0)
})
