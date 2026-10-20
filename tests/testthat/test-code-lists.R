test_that("acocat comes from the EAF-Nansen acoustic categories", {
  m <- acocat_nansen(c("Sardina pilchardus", "Engraulis encrasicolus", "Trachurus trachurus",
                       "Scomber colias", "Plancton", "Mesopelagique", "Autres", "Made-up fish"))
  expect_equal(m$acocat, c(5004, 5041, 5003, 21, 6, 5013, 1, 1))
  expect_equal(m$source[8], "not in taxonomy (OTHER)")
})

test_that("broad acocat level uses PEL1/PEL2 but keeps plankton apart", {
  m <- acocat_nansen(c("Sardina pilchardus", "Scomber colias", "Plancton"), level = "broad")
  expect_equal(m$acousticCategory, c("PEL1", "PEL2", "PLANK"))
})

test_that("sex follows the NMD codes (1 = Female, 2 = Male)", {
  expect_equal(sex_nansen(c("F", "M", "I", "f", "2", "x")), c("1", "2", NA, "1", "2", NA))
  expect_equal(code_list_map("sex", c("F", "M")), c("1", "2"))
})

test_that("custom lists map values, with * for anything else", {
  l <- code_lists_default()
  l$acocat$mode <- "custom"                       # starts from the old LPA codes
  m <- acocat_map_for(c("Sardina pilchardus", "Boops boops"), l)
  expect_equal(m$acocat, c("1", "16"))
  expect_equal(m$source, c("custom list", "custom list (*)"))
  l$sex$mode <- "custom"
  l$sex$custom <- data.frame(value = c("M", "F"), code = c("A", "B"), description = c("m", "f"))
  expect_equal(code_list_map("sex", c("m", "F", "I"), l), c("A", "B", NA))
})

test_that("code validity follows the active list", {
  expect_equal(code_list_valid("gear", c("3500", "9999", "")), c(TRUE, FALSE, TRUE))
  l <- code_lists_default()
  l$gear$mode <- "custom"
  l$gear$custom <- data.frame(value = "X1", code = "X1", description = "Test gear")
  expect_equal(code_list_valid("gear", c("3500", "X1"), l), c(FALSE, TRUE))
})

test_that("length resolution gives the length class in cm", {
  expect_equal(length_resolution_cm("2"), 0.5)
  expect_equal(length_resolution_cm("3"), 1)
  expect_true(is.na(length_resolution_cm("99")))
})

test_that("code lists survive a CSV round trip", {
  l <- code_lists_default()
  l$nation$mode <- "custom"
  l$nation$custom <- data.frame(value = "XX", code = "XX", description = "Test nation")
  f <- tempfile(fileext = ".csv")
  code_lists_to_csv(l, f)
  back <- code_lists_from_csv(f)
  expect_equal(code_list_mode("nation", back), "custom")
  expect_equal(code_list_table("nation", back)$code, "XX")
  expect_equal(code_list_mode("gear", back), "nansen")
})
