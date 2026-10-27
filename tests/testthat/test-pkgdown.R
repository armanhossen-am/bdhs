test_that("every exported topic is listed in the pkgdown reference index", {
  skip_on_cran()
  skip_if_not_installed("pkgdown")
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "not run from the source tree")
  expect_no_error(suppressMessages(pkgdown::check_pkgdown(root)))
})
