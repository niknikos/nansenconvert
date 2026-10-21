# The Nansis2Biotic.xml tab. rnansisclient talks to a live Nansis database, so
# its database functions are replaced by fakes (as rnansisclient's own tests
# do); the fake export writes the synthetic Biotic.xml, which the tab then
# validates, offers for download and zips for real.
skip_if_not_installed("rnansisclient")
nansis <- load_converter("Nansis2Biotic.xml.R")
# Built here, not inside the fake export: the chain runs its own test sessions
synthetic_xml <- biotic_chain()$biotic_xml

# Fakes for the rnansisclient calls the tab makes; returns what the export saw
fake_nansis <- function(fail_connect = FALSE, env = parent.frame()) {
  calls <- new.env()
  calls$serial <- list()
  calls$empty <- list()
  testthat::local_mocked_bindings(
    nansis_connect = function(host, port = 5432, username, password, dbname = "nansis") {
      if (fail_connect) stop("password authentication failed for user \"", username, "\"")
      structure(list(host = host), class = "fake_nansis")
    },
    is_connected = function(con) inherits(con, "fake_nansis"),
    nansis_disconnect = function(con) invisible(NULL),
    list_available_surveys = function(con, vessel_type = 4L, regions = NULL) {
      data.frame(survey = c(2017401, 2017402, 2023401), description = c("Survey A", "Survey B", "Survey C"),
                 mindate = as.Date(c("2017-03-01", "2017-06-01", "2023-05-01")),
                 maxdate = as.Date(c("2017-03-20", "2017-06-20", "2023-05-20")),
                 n_stations = c(12L, 12L, 8L), regions = "Synthetic EEZ", eligible = c(TRUE, TRUE, FALSE))
    },
    export_biotic = function(con, survey, output_file, start_serial_no = NULL, include_empty_stations = TRUE) {
      calls$serial[[length(calls$serial) + 1]] <- start_serial_no
      calls$empty[[length(calls$empty) + 1]] <- include_empty_stations
      if (substr(survey, 1, 4) > "2021") {
        return(list(success = FALSE, message = sprintf("Survey %s is from %s. This app only supports surveys up to 2021 (inclusive).",
                                                       survey, substr(survey, 1, 4))))
      }
      file.copy(synthetic_xml, output_file, overwrite = TRUE)
      csv <- sub("[.]xml$", "_missing_species.csv", output_file)
      utils::write.csv(data.frame(species_code = "SYNTH01"), csv, row.names = FALSE)
      list(success = TRUE, message = paste("Exported to:", output_file),
           warnings = "INFO: 1 species code(s) were not found in the bundled species map.",
           stations_exported = 12L, catchsamples_exported = 55L, individuals_exported = 2655L,
           warning_count = 1L, missing_species_csv = csv)
    },
    run_integrity_checks = function(con, surveys = NULL, vessel_type = 4L, checks = NULL) {
      list(summary = data.frame(id = c("references", "gear_depth"),
                                label = c("Codes absent from their lookup table", "Gear fishing below the sea bed"),
                                core = c(TRUE, FALSE), n_findings = c(0L, 2L), status = c("PASS", "FINDINGS"),
                                message = "", stringsAsFactors = FALSE),
           details = list(gear_depth = data.frame(survey = "2017401", station = c(3L, 7L))))
    },
    write_integrity_report = function(result, output_dir, surveys = NULL, vessel_type = 4L) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      p <- file.path(output_dir, "integrity_check_summary.csv")
      utils::write.csv(result$summary, p, row.names = FALSE)
      list(check_summary = p, corrections_sql = NULL)
    },
    .package = "rnansisclient", .env = env
  )
  calls
}

connect <- function(session) {
  session$setInputs(host = "localhost", port = 5432, dbname = "nansis", username = "tester",
                    password = "test-only", include_empty = TRUE, vessel_type = 4)
  session$setInputs(connect = 1)
}

test_that("typed survey numbers are split on commas, semicolons and spaces", {
  expect_equal(nansis$nansis_parse_surveys(" 2017401, 2017402;2017403  2017401"),
               c("2017401", "2017402", "2017403"))
  expect_equal(nansis$nansis_parse_surveys(NULL), character(0))
})

test_that("connecting needs a host and reports a failed login", {
  fake_nansis(fail_connect = TRUE)
  testServer(nansis$nansis2biotic_server, {
    session$setInputs(host = "", port = 5432, dbname = "nansis", username = "tester", password = "test-only")
    session$setInputs(connect = 1)
    expect_false(rv$conn$ok)
    expect_match(rv$conn$msg, "host")
    session$setInputs(host = "localhost", connect = 2)
    expect_false(rv$conn$ok)
    expect_match(rv$conn$msg, "Connection failed")
    expect_null(rv$con)
  })
})

test_that("surveys after the cutoff cannot be chosen from the inventory", {
  fake_nansis()
  testServer(nansis$nansis2biotic_server, {
    connect(session)
    expect_true(connected())
    session$setInputs(load_inventory = 1)
    expect_equal(rv$inventory$survey, c("2017401", "2017402", "2023401"))
    session$setInputs(inventory_rows_selected = c(1, 3))
    expect_equal(chosen_surveys(), "2017401")
    session$setInputs(survey_numbers = "2017402, 2017401")
    expect_equal(chosen_surveys(), c("2017401", "2017402"))
  })
})

test_that("every exported survey is validated, and a failed one is reported", {
  calls <- fake_nansis()
  testServer(nansis$nansis2biotic_server, {
    connect(session)
    session$setInputs(survey_numbers = "2017401 2017402 2023401", start_serial = 100)
    session$setInputs(export = 1)
    tab <- results_table()
    expect_equal(tab$Survey, c("2017401", "2017402", "2023401"))
    expect_equal(tab$Exported, c("Yes", "Yes", "No"))
    expect_equal(tab$Errors[1:2], c(0, 0))
    expect_true(is.na(tab$Errors[3]))
    expect_equal(tab$Individuals[1], 2655L)
    # A start serial number only applies to a single survey
    expect_true(all(vapply(calls$serial, is.null, logical(1))))

    expect_equal(nrow(validate_xml_schema(output$download_xml, "biotic")), 0)
    zipped <- utils::unzip(output$download_zip, list = TRUE)$Name
    expect_true(all(c("Biotic_2017401.xml", "Biotic_2017402.xml", "Biotic_2017401_missing_species.csv",
                      "Biotic_2017401_validation.csv", "export_manifest.csv") %in% zipped))
    expect_false(any(grepl("2023401[.]xml$", zipped)))

    session$setInputs(results_rows_selected = 3)
    expect_match(output$export_log, "only supports surveys up to 2021")
  })
})

test_that("a single survey gets the start serial number and the no-catch choice", {
  calls <- fake_nansis()
  testServer(nansis$nansis2biotic_server, {
    connect(session)
    session$setInputs(survey_numbers = "2017401", start_serial = 100, include_empty = FALSE)
    session$setInputs(export = 1)
    expect_equal(calls$serial[[1]], 100)
    expect_false(calls$empty[[1]])
  })
})

test_that("the integrity check shows its findings and offers the report", {
  fake_nansis()
  testServer(nansis$nansis2biotic_server, {
    connect(session)
    session$setInputs(survey_numbers = "2017401", integrity_checks = c("references", "gear_depth"))
    session$setInputs(run_integrity = 1)
    expect_equal(rv$integrity$summary$n_findings, c(0L, 2L))
    expect_equal(rv$integrity$surveys, "2017401")
    expect_true("integrity_check_summary.csv" %in% utils::unzip(output$download_integrity, list = TRUE)$Name)
  })
})

test_that("validate_biotic_xml_file runs the schema, StoX's reader and the data checks", {
  f <- validate_biotic_xml_file(synthetic_xml)
  expect_equal(sum(f$severity == "error"), 0)
  expect_true("READBACK_OK" %in% f$check)
  bad <- tempfile(fileext = ".xml")
  writeLines("<missions><broken>", bad)
  expect_true(any(validate_biotic_xml_file(bad)$severity == "error"))
})
