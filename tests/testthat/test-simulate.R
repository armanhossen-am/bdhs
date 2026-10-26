test_that("bdhs_simulate() returns linked, reproducible synthetic recodes", {
  ir <- bdhs_simulate(2022, "IR", n_clusters = 6, households = 4)
  pr <- bdhs_simulate(2022, "PR", n_clusters = 6, households = 4)
  kr <- bdhs_simulate(2022, "KR", n_clusters = 6, households = 4)
  br <- bdhs_simulate(2022, "BR", n_clusters = 6, households = 4)
  expect_true(isTRUE(attr(ir, "bdhs_simulated")))
  expect_true(all(c("caseid", "v001", "v005", "v021", "v022", "v024", "v025", "awfactt") %in% names(ir)))
  expect_equal(length(unique(ir$caseid)), nrow(ir))
  # Every woman is a household member with the same line number.
  key_ir <- paste(substr(ir$caseid, 1, 12), ir$v003)
  key_pr <- paste(pr$hhid, pr$hvidx)
  expect_true(all(key_ir %in% key_pr))
  # Every child in KR is a birth in BR.
  expect_true(all(paste(kr$caseid, kr$bidx) %in% paste(br$caseid, br$bidx)))
  expect_true(all(kr$b19 < 60))
  expect_identical(bdhs_simulate(2022, "HR", seed = 9), bdhs_simulate(2022, "HR", seed = 9))
})

test_that("bdhs_simulate() follows the round's division layout", {
  n_div <- function(s) length(unique(bdhs_simulate(s, "HR", n_clusters = 40, households = 1)$hv024))
  expect_equal(n_div(1994), 5)
  expect_equal(n_div(2004), 6)
  expect_equal(n_div(2014), 7)
  expect_equal(n_div(2022), 8)
  expect_error(bdhs_simulate("spa2014"), "Can't resolve")
  expect_error(bdhs_simulate(2022, "MR"))
})

test_that("bdhs_simulate() leaves the global RNG state alone", {
  set.seed(42)
  a <- stats::runif(1)
  set.seed(42)
  invisible(bdhs_simulate(n_clusters = 3, households = 2))
  b <- stats::runif(1)
  expect_identical(a, b)
})
