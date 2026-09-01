# These tests read the sample workbook in data-raw/. That directory is
# gitignored and excluded from the build, so the tests are skipped on a clean
# checkout and on CRAN; they are a local convenience only. The primary
# verification is a devtools::load_all() + read against this same file.

sample_path <- function() {
  # data-raw sits next to the installed/source tree during local testing.
  candidates <- c(
    testthat::test_path("..", "..", "data-raw", "linelist_CTE Kitatumba.xlsb"),
    "data-raw/linelist_CTE Kitatumba.xlsb"
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) hit[[1L]] else candidates[[1L]]
}

test_that("list_sheets lists the workbook sheets", {
  skip_on_cran()
  f <- sample_path()
  skip_if_not(file.exists(f), "sample .xlsb not available")

  sheets <- list_sheets(f)
  expect_type(sheets, "character")
  expect_setequal(sheets, c("linelist", "occupancy", "HF_database", "Options"))
})

test_that("rxlsb returns a typed tibble", {
  skip_on_cran()
  f <- sample_path()
  skip_if_not(file.exists(f), "sample .xlsb not available")

  d <- rxlsb(f)
  expect_s3_class(d, "tbl_df")
  expect_gt(nrow(d), 0L)
  # Mixed dates + empty cells must resolve to Date, not character.
  expect_s3_class(d$date_symptom_onset, "Date")
  expect_type(d$age, "double")
})

test_that("rxlsb selects sheets by name and index", {
  skip_on_cran()
  f <- sample_path()
  skip_if_not(file.exists(f), "sample .xlsb not available")

  by_name <- rxlsb(f, sheet = "occupancy")
  by_index <- rxlsb(f, sheet = 2)
  expect_identical(names(by_name), names(by_index))
  expect_error(rxlsb(f, sheet = "nope"), "not found")
})

test_that("col_names = FALSE keeps the header row as data", {
  skip_on_cran()
  f <- sample_path()
  skip_if_not(file.exists(f), "sample .xlsb not available")

  with_names <- rxlsb(f)
  without <- rxlsb(f, col_names = FALSE)
  expect_true(all(grepl("^\\.\\.\\.[0-9]+$", names(without))))
  expect_identical(nrow(without), nrow(with_names) + 1L)
})

test_that("col_types = 'text' forces every column to character", {
  skip_on_cran()
  f <- sample_path()
  skip_if_not(file.exists(f), "sample .xlsb not available")

  d <- rxlsb(f, col_types = "text")
  expect_true(all(vapply(d, is.character, logical(1))))
  # A Date column, coerced to text, becomes an ISO string.
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", stats::na.omit(d$date_symptom_onset))))
})

test_that("col_types accepts a per-column vector, including 'skip'", {
  skip_on_cran()
  f <- sample_path()
  skip_if_not(file.exists(f), "sample .xlsb not available")

  guess <- rxlsb(f)
  n <- ncol(guess)
  types <- rep("guess", n)
  types[[1L]] <- "text"
  types[[n]] <- "skip"

  d <- rxlsb(f, col_types = types)
  expect_identical(ncol(d), n - 1L)
  expect_false(names(guess)[[n]] %in% names(d))
  expect_type(d[[1L]], "character")
})

test_that("col_types validates its values and length", {
  skip_on_cran()
  f <- sample_path()
  skip_if_not(file.exists(f), "sample .xlsb not available")

  expect_error(rxlsb(f, col_types = "bogus"), "must be one of")

  n <- ncol(rxlsb(f))
  expect_error(
    rxlsb(f, col_types = rep("text", n + 1L)),
    "must have length"
  )
})
