# Tests on the maintainer's licensed DHS files. They never run on CRAN or CI:
# set BDHS_LOCAL_TESTS=true (and optionally BDHS_LOCAL_DIR) to run them. They
# may only assert on aggregates, never on respondent-level values.

local_data_dir <- function() {
  path.expand(Sys.getenv("BDHS_LOCAL_DIR", "~/Documents/MU/Research"))
}

skip_if_no_local_data <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not(
    identical(Sys.getenv("BDHS_LOCAL_TESTS"), "true"),
    "local DHS data tests are off (set BDHS_LOCAL_TESTS=true)"
  )
  testthat::skip_if_not(dir.exists(local_data_dir()), "BDHS_LOCAL_DIR does not exist")
}
