lnd <- load_converter("Landings_To_Biotic.xls.R")

read_template <- function() lnd$landings_read(data_file("Landings_model.xlsx"))
convert <- function(d = read_template()$data, ...) lnd$landings_to_biotic(d, list(...))

test_that("columns are found by their header text, in any order", {
  raw <- readxl::read_excel(data_file("Landings_model.xlsx"), col_types = "text")
  map <- lnd$landings_map_columns(names(raw))
  expect_false(anyNA(map))
  # shuffled, upper-case and padded headers still match
  h <- toupper(paste0("  ", names(raw), "   "))[sample(ncol(raw))]
  map2 <- lnd$landings_map_columns(h)
  expect_false(anyNA(map2))
  expect_equal(h[map2["gear"]], toupper(paste0("  ", "Fishing Unit type", "   ")))
  # a column that is not there is NA; the required ones are reported
  p <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(raw[, setdiff(names(raw), "Weight (g)")], p)
  expect_true(is.na(lnd$landings_read(p)$map["w"]))
  writexl::write_xlsx(raw[, setdiff(names(raw), "Total Length (mm)")], p)
  expect_equal(lnd$landings_read(p)$missing, "tl")
})

test_that("Excel dates, written dates and a mistyped year are read", {
  x <- c("45904", "7April2026", "16-Apr-2026\t", " 30march2026 ", "30march2027", NA, "soon", rep(c("1April2026", "2april2026 "), 15))
  d <- lnd$landings_parse_dates(x)
  expect_equal(format(d[1:5]), c("2025-09-04", "2026-04-07", "2026-04-16", "2026-03-30", "2026-03-30"))
  expect_true(all(is.na(d[6:7])))
  expect_equal(attr(d, "n_year_fixed"), 1L)
})

test_that("the template converts to consistent Biotic tables", {
  r <- convert()
  tb <- r$tables
  d <- read_template()$data
  # one station per fishing unit sample (9 in the template), one sample per species
  expect_equal(nrow(tb$fishstations), 9)
  expect_equal(tb$fishstations$serialnumber, as.character(1:9))
  expect_equal(tb$fishstations$station, tb$fishstations$serialnumber)       # StoX needs a station number
  expect_false(anyDuplicated(tb$fishstations$serialnumber) > 0)
  # every fish is kept, and the keys link up
  expect_equal(nrow(tb$individuals), sum(!is.na(d$tl)))
  expect_true(all(paste(tb$individuals$serialnumber, tb$individuals$catchsampleid) %in%
                    paste(tb$catchesamples$serialnumber, tb$catchesamples$catchsampleid)))
  expect_false(anyDuplicated(tb$individuals[, c("serialnumber", "catchsampleid", "specimenid")]) > 0)
  # metres and kilograms
  len <- as.numeric(tb$individuals$length); expect_true(all(len > 0.02 & len < 0.6))
  w <- as.numeric(tb$individuals$individualweight); expect_true(all(w[!is.na(w)] > 0 & w[!is.na(w)] < 1))
  # positions are southern, inside the lake, with a source in the comment
  expect_true(all(as.numeric(tb$fishstations$latitudestart) < 0))
  expect_match(tb$fishstations$stationcomment, "position from ")
  # sex codes F/M (any case) -> 1/2, anything else empty
  expect_true(all(tb$individuals$sex %in% c("1", "2", NA)))
  expect_setequal(tb$catchesamples$scientificname, c("Lates stappersii", "Stolothrissa tanganicae", "Limnothrissa miodon"))
  expect_true(all(nzchar(tb$catchesamples$aphia)))
})

test_that("gear, test fishing, fallback positions and the written date are handled", {
  tb <- convert()$tables$fishstations
  expect_setequal(tb$gear, c("9101", "9102", "9103", "9104"))
  expect_equal(sum(tb$stationtype == "11"), 1)                               # the "Test fishing" remark
  # unit 5 has no fishing location: it takes the position of its fishing ground or landing site
  expect_true(any(grepl("position from (fishing ground|landing site)", tb$stationcomment)))
  # the unit whose date was typed as text is kept
  expect_true("2026-04-02Z" %in% tb$stationstartdate)
  # a gear with no code leaves the gear empty and says so
  r <- convert(gear_map = c(LN = "9101", RN = "9102", HL = "9103", GN = ""))
  expect_true(any(is.na(r$tables$fishstations$gear)))
  expect_true(any(grepl("no code", r$report$notes)))
})

test_that("a station without a date or a usable position is dropped and reported", {
  d <- read_template()$data
  d$date[d$unit == "2" & d$site == "SITE BRAVO"] <- NA
  d$fish_lat[d$site == "Site Charlie"] <- NA; d$fish_lon[d$site == "Site Charlie"] <- NA
  d$land_lat[d$site == "Site Charlie"] <- NA; d$land_lon[d$site == "Site Charlie"] <- NA
  r <- convert(d)
  expect_gt(nrow(r$report$dropped), 0)
  expect_setequal(r$report$dropped$reason, c("no date", "no usable position"))
  expect_equal(nrow(r$tables$fishstations) + nrow(r$report$dropped), 9)
  # a swapped latitude and longitude is put right
  d <- read_template()$data
  i <- which(d$unit == "1" & d$site == "Site Alpha")
  tmp <- d$fish_lat[i]; d$fish_lat[i] <- d$fish_lon[i]; d$fish_lon[i] <- tmp
  r <- convert(d)
  expect_true(all(abs(as.numeric(r$tables$fishstations$latitudestart)) < 9))
  expect_true(any(grepl("swapped", r$report$notes)))
})

test_that("fish without a weight get a predicted one, and every sample can be raised", {
  r <- convert()
  cs <- r$tables$catchesamples
  with_fish <- !is.na(cs$lengthsamplecount)
  expect_true(all(!is.na(cs$catchweight[with_fish])))
  expect_true(all(!is.na(cs$lengthsampleweight[with_fish])))
  expect_true(all(as.numeric(cs$lengthsampleweight[with_fish]) <= as.numeric(cs$catchweight[with_fish]) + 1e-9))
  expect_true(any(grepl("predicted from the length-weight fit", cs$catchcomment)))
  expect_true(all(r$report$lw$b > 2 & r$report$lw$b < 4))
  # without the prediction those samples cannot be raised by weight
  r0 <- convert(predict_weights = FALSE)
  expect_true(any(is.na(r0$tables$catchesamples$lengthsampleweight[!is.na(r0$tables$catchesamples$lengthsamplecount)])))
  # a reported catch below the measured weight is raised to it and says so
  d <- read_template()$data
  i <- which(d$site == "Site Alpha" & d$unit == "1")
  d$stolo[i] <- "0.001"; d$limno[i] <- "0.001"; d$lates_ad[i] <- "0.001"; d$lates_juv[i] <- "0"
  r2 <- convert(d)
  expect_true(any(grepl("below the measured weight", r2$tables$catchesamples$catchcomment)))
})

test_that("the workbook passes the Biotic XML tab without errors and StoX reads every station", {
  r <- convert()
  dir <- tempfile("landings_"); dir.create(dir)
  xl <- file.path(dir, "Biotic.xlsx"); xml <- file.path(dir, "Biotic.xml")
  writexl::write_xlsx(r$tables, xl)
  bx <- load_converter("Biotic.xls_To_Biotic.xml.R")
  res <- NULL
  testServer(bx$server, {
    session$setInputs(excelFile = list(name = "Biotic.xlsx", datapath = xl))
    xml2::write_xml(xmlDoc(), xml, options = "format")
    res <<- convertedValidation()
  })
  expect_equal(sum(res$severity == "error"), 0)
  expect_equal(nrow(validate_xml_schema(xml, "biotic")), 0)
  expect_true(any(res$check == "READBACK_OK"))

  skip_if_not_installed("RstoxData"); skip_if_not_installed("RstoxBase")
  bd <- suppressWarnings(RstoxData::ReadBiotic(xml))
  sb <- suppressWarnings(RstoxData::StoxBiotic(bd))
  # one StoX station and haul per fishing unit sample (the station number keeps them apart)
  expect_equal(nrow(sb$Station), nrow(r$tables$fishstations))
  expect_equal(nrow(sb$Haul), nrow(r$tables$fishstations))
  expect_equal(nrow(sb$Individual), nrow(r$tables$individuals))
  expect_false(anyNA(sb$Station$DateTime))
  expect_true(all(sb$Station$Latitude < 0))
  # lengths in cm, weights in g
  expect_true(all(sb$Individual$IndividualTotalLength > 2 & sb$Individual$IndividualTotalLength < 60))
  # StoX can raise every sample to the catch
  ld <- suppressWarnings(RstoxBase::LengthDistribution(sb, LengthDistributionType = "Standard", RaisingFactorPriority = "Weight"))
  expect_gt(nrow(ld[!is.na(ld$WeightedNumber), ]), 0)
})

test_that("the tab converts the template, reports it and offers the workbook and the report", {
  testServer(lnd$landings_server, {
    session$setInputs(file = list(name = "Landings_model.xlsx", datapath = data_file("Landings_model.xlsx")))
    expect_equal(gears_found(), c("GN", "HL", "LN", "RN"))
    session$setInputs(nation = "155", missiontype = "18", platform = "9999", missionnumber = "1", cruise = "TEST",
                      purpose = "test", start_time = "12:00:00", type_other = "9", type_test = "11",
                      fallback = TRUE, predict = TRUE, land_check = FALSE,
                      gear_LN = "9101", gear_RN = "9102", gear_HL = "9103", gear_GN = "9104")
    session$setInputs(process = 1)
    expect_equal(nrow(rv$res$tables$fishstations), 9)
    expect_equal(sum(rv$findings$severity == "error"), 0)
    expect_equal(rv$res$tables$missions$cruise, "TEST")
    # a gear left without a code is reported
    session$setInputs(gear_GN = "")
    session$setInputs(process = 2)
    expect_true(any(grepl("no code", rv$res$report$notes)))
    wb <- output$dl_xlsx
    expect_equal(readxl::excel_sheets(wb), c("missions", "fishstations", "catchesamples", "individuals"))
    z <- output$dl_report
    expect_true(file.exists(z))
    expect_true(all(c("dropped_stations.csv", "catch_adjustments.csv", "length_weight_fits.csv", "conversion_notes.txt") %in%
                      basename(utils::unzip(z, list = TRUE)$Name)))
  })
})
