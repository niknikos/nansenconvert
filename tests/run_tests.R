# Runs the test suite from the project root:
#   Rscript tests/run_tests.R
# Uses the synthetic templates in data/ and the schemas in schemas/.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(script)) normalizePath(file.path(dirname(script), "..")) else getwd()
setwd(root)
res <- testthat::test_dir(file.path("tests", "testthat"), reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(res)
cat(sprintf("\n%d tests: %d passed, %d failed, %d skipped\n",
            sum(df$nb), sum(df$nb) - sum(df$failed) - sum(df$skipped), sum(df$failed), sum(df$skipped)))
if (sum(df$failed) > 0 || any(df$error)) quit(status = 1)
