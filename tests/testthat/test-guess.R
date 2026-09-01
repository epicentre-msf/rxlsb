# Regression test: python-calamine returns whole-number cells as Python ints,
# which reticulate converts to R integer vectors. The type guesser only knew
# "numeric", so an all-integer column used to fall back to character.

test_that("integer cells guess as numeric, not character", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  f <- build_minimal_xlsb("C:/fixture/", nrow = 3L)
  d <- rxlsb(f, col_names = FALSE)
  expect_identical(d[[1L]], c(10, 30, 50))
  expect_identical(d[[2L]], c(20, 40, 60))
})
