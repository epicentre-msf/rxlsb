# Regression test for a calamine parser bug: when an .xlsb workbook's stored
# "last saved from" path (xl/workbook.bin's BrtAbsPath15 record) is exactly
# 70 characters long, calamine's record-stream walker mis-parses it and
# reports zero sheets, even though the workbook is perfectly valid (Excel and
# readxlsb both read it fine). See R/list_sheets.R (.workbook()) for the
# workaround and inst/python/rxlsb_sanitize.py for the full root-cause
# explanation. See tests/testthat/helper-xlsb-fixture.R for the fixture
# builder used below.
#
# This was originally found via a real workbook decrypted with
# rpxl::decrypt_wb() -- decryption itself is not implicated; it merely
# preserves whatever path Excel had stored, which happened to be 70 chars.

path_len <- function(n) paste0("C:/", strrep("a", n - 4L), "/")

test_that("a 70-char stored path (the calamine trigger) still reads correctly", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  f <- build_minimal_xlsb(path_len(70L))

  sheets <- list_sheets(f)
  expect_identical(sheets, "data")

  d <- rxlsb(f)
  expect_s3_class(d, "tbl_df")
  expect_identical(dim(d), c(2L, 2L))
})

test_that("nearby, non-triggering path lengths are unaffected", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  for (n in c(10L, 69L, 71L, 134L)) {
    f <- build_minimal_xlsb(path_len(n))
    expect_identical(list_sheets(f), "data", info = paste("path length", n))
    expect_identical(dim(rxlsb(f)), c(2L, 2L), info = paste("path length", n))
  }
})
