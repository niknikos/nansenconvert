test_that("names resolve to AphiaID and ITIS TSN through nansentaxonomy", {
  r <- taxonomy_resolve(c("Sardina pilchardus", "Liza aurata", "Crevettes"))
  expect_equal(r$aphia, c("126421", "126978", NA))
  expect_equal(r$catchcategory[1], "161813")
  expect_equal(r$matched_by[3], "none")
})

test_that("likely typos get a suggestion", {
  s <- taxonomy_suggest(c("Sardina pilchardis", "Crevettes"))
  expect_equal(unname(s["Sardina pilchardis"]), "Sardina pilchardus")
  expect_true(is.na(s["Crevettes"]))
})

test_that("the taxonomy reference has one row per name", {
  ref <- taxonomy_reference()
  expect_false(any(duplicated(ref$scientificname)))
  expect_true(all(c("scientificname", "aphia", "catchcategory", "status", "valid_name") %in% names(ref)))
})
