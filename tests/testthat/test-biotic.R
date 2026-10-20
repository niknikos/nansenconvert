test_that("the synthetic templates give a valid Biotic XML with no validation errors", {
  res <- biotic_chain()
  expect_equal(nrow(validate_xml_schema(res$biotic_xml, "biotic")), 0)
  expect_equal(sum(res$validation$severity == "error"), 0)
  x <- xml2::read_xml(res$biotic_xml)
  expect_equal(unname(xml2::xml_ns(x)[1]), "http://www.imr.no/formats/nmdbiotic/v3.1")
  expect_equal(length(xml2::xml_find_all(x, "//*[local-name()='individual']")), nrow(res$individuals))
})

test_that("sex is written as 1 = Female, 2 = Male", {
  res <- biotic_chain()
  bio <- readxl::read_excel(data_file("Biology_model.xlsx"))
  expect_true(all(res$individuals$sex %in% c("1", "2", NA)))
  expect_equal(sum(res$individuals$sex == "1", na.rm = TRUE), sum(bio$sexe == "F"))
  expect_equal(sum(res$individuals$sex == "2", na.rm = TRUE), sum(bio$sexe == "M"))
})

test_that("station values come from the data and the defaults", {
  res <- biotic_chain()
  st <- readxl::read_excel(data_file("Stations_model.xlsx"))
  expect_equal(res$stations$soaktime, round(as.numeric(st[[3]]) / 60, 3))   # Durée_chalutage, minutes
  expect_true(all(res$stations$stationtype == "11"))
  expect_true(all(is.na(res$stations$nation)))
  expect_false(any(c("logstart", "logstop") %in% names(res$stations)))
  d <- .dist_nmi(st$Latitude_filage, st$Longitude_filage, st$Latitude_virage, st$Longitude_virage)
  expect_equal(res$stations$distance, round(d, 3))
})

test_that("catch weight is the whole catch and every sample has aphia", {
  res <- biotic_chain()
  ca <- readxl::read_excel(data_file("Catches_model.xlsx"))
  expect_equal(sort(res$catches$catchweight), sort(ca$Tot_P))
  expect_true(all(!is.na(res$catches$aphia)))
  expect_true(all(res$catches$conservation == "1"))
})

test_that("lengths are rounded to the length resolution", {
  res <- biotic_chain()
  cm <- res$individuals$length * 100
  expect_true(all(abs(cm * 2 - round(cm * 2)) < 1e-6, na.rm = TRUE))
  expect_true(all(res$individuals$lengthresolution == "2"))
})
