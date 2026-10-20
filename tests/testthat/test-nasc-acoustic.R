nasc <- load_converter("NASC.xls_To_Acoustic.xml.R")
aco <- load_converter("Acoustic.xls_To_Acoustic.xml.R")

build <- function(df = make_nasc(), ...) nasc$build_acoustic_sheets(df, NASC_MD, nasc$NASC_ACOUSTIC_DEFAULTS, ...)

test_that("species share is PRC_NASC x Species_percent / 100", {
  sa <- build()$acoustic_sheets$sa
  expect_equal(sa$value[sa$distance_id == 1 & sa$acocat == 5004 & sa$ch == 1], 60)
  expect_equal(sa$value[sa$distance_id == 1 & sa$acocat == 5041 & sa$ch == 1], 40)
})

test_that("regions with the same acocat in a cell are summed", {
  sa <- build()$acoustic_sheets$sa
  expect_equal(sa$value[sa$distance_id == 1 & sa$acocat == 1 & sa$ch == 1], 60)   # Autres 50 + Unassigned 10
  expect_false(any(duplicated(sa[, c("distance_id", "acocat", "ch")])))
  expect_equal(nrow(sa), 4)
})

test_that("zero-NASC cells stay as distances; times and log come from the data", {
  sh <- build()$acoustic_sheets
  expect_equal(nrow(sh$distances), 2)
  expect_equal(sh$distances$start_time, c("2026-03-10 07:00:00", "2026-03-10 07:06:01"))
  expect_equal(sh$distances$stop_time[2], "2026-03-10 07:12:00")
  expect_equal(sh$distances$integrator_dist, c(1, 1), tolerance = 0.01)
  expect_equal(sh$distances$log_start, c(0, 1), tolerance = 0.01)
  expect_equal(sh$frequencies$num_pel_ch, c(2L, 1L))
  expect_setequal(sh$ch_types$distance_id, 1:2)
})

test_that("a custom acocat list is used when supplied", {
  l <- code_lists_default(); l$acocat$mode <- "custom"
  sa <- build(acocat_map = acocat_map_for(unique(make_nasc()$Species_name), l))$acoustic_sheets$sa
  expect_setequal(unique(sa$acocat), c(1, 2, 16))
})

test_that("the Acoustic XML validates and reads back in StoX", {
  sh <- build()$acoustic_sheets
  f <- tempfile(fileext = ".xml")
  xml2::write_xml(aco$build_echosounder_xml(sh), f)
  expect_equal(nrow(validate_xml_schema(f, "acoustic")), 0)
  rb <- readback_xml(f, "acoustic", list(distance = 2, sa = 4))
  expect_false(any(rb$severity == "error"))
  x <- xml2::read_xml(f)
  expect_equal(unname(xml2::xml_ns(x)[1]), "http://www.imr.no/formats/nmdechosounder/v1")
})

test_that("large frequencies are written as integers", {
  sh <- build()$acoustic_sheets
  for (s in c("frequencies", "ch_types", "sa_by_acocat", "sa")) sh[[s]]$freq <- 200000
  doc <- aco$build_echosounder_xml(sh)
  expect_equal(xml2::xml_attr(xml2::xml_find_first(doc, "//*[local-name()='frequency']"), "freq"), "200000")
})

test_that("the synthetic NASC template converts to valid XML", {
  df <- as.data.frame(readxl::read_excel(data_file("NASC_model.xlsx")))
  res <- build(df)
  f <- tempfile(fileext = ".xml")
  xml2::write_xml(aco$build_echosounder_xml(res$acoustic_sheets), f)
  expect_equal(nrow(validate_xml_schema(f, "acoustic")), 0)
  expect_equal(nrow(validate_species_percent(df)), 0)
  v <- validate_acoustic(res$acoustic_sheets)
  expect_equal(sum(v$severity == "error"), 0)
})
