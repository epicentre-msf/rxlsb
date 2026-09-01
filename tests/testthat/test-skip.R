# Tests for the `skip` argument, which follows readxl's semantics: rows are
# counted from the top of the sheet, and leading blank rows are always
# skipped, so skip is a lower bound rather than an exact count. Uses the
# synthetic fixture from helper-xlsb-fixture.R; its cells are numeric (first
# data row 10, 20, then +10 per cell), so a header taken from a data row gets
# names like "30", "40".

fixture_abspath <- "C:/fixture/"

test_that("skip drops rows before the header", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  f <- build_minimal_xlsb(fixture_abspath, nrow = 4L)

  d <- rxlsb(f, skip = 1)
  expect_identical(names(d), c("30", "40"))
  expect_identical(d[[1L]], c(50, 70))
  expect_identical(d[[2L]], c(60, 80))

  no_names <- rxlsb(f, skip = 2, col_names = FALSE)
  expect_identical(names(no_names), c("...1", "...2"))
  expect_identical(no_names[[1L]], c(50, 70))
})

test_that("skip counts sheet rows and is a lower bound over leading blanks", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  # Data occupies sheet rows 3-6 (0-based 2:5); rows 1-2 are blank.
  f <- build_minimal_xlsb(fixture_abspath, nrow = 4L, data_rows = 2:5)

  # Skipping no further than the blank area changes nothing...
  expect_identical(rxlsb(f, skip = 2), rxlsb(f))
  # ...while larger values keep counting from the top of the sheet.
  d <- rxlsb(f, skip = 3)
  expect_identical(names(d), c("30", "40"))
  expect_identical(nrow(d), 2L)
})

test_that("blank rows at the skip position are also skipped", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  # Sheet row 3 (0-based 2) is blank, in the middle of the data.
  f <- build_minimal_xlsb(fixture_abspath, nrow = 4L, data_rows = c(0L, 1L, 3L, 4L))

  d <- rxlsb(f, skip = 2)
  expect_identical(names(d), c("50", "60"))
  expect_identical(nrow(d), 1L)
})

test_that("skipping past all rows returns an empty tibble", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  f <- build_minimal_xlsb(fixture_abspath, nrow = 2L)
  expect_identical(rxlsb(f, skip = 10), tibble::tibble())
})

test_that("skip validates its input", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  f <- build_minimal_xlsb(fixture_abspath)
  expect_error(rxlsb(f, skip = -1), "non-negative whole number")
  expect_error(rxlsb(f, skip = 1.5), "non-negative whole number")
  expect_error(rxlsb(f, skip = "1"), "non-negative whole number")
  expect_error(rxlsb(f, skip = c(1, 2)), "non-negative whole number")
  expect_error(rxlsb(f, skip = NA_real_), "non-negative whole number")
})
