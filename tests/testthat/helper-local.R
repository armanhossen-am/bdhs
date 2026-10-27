# Helpers for tests that use the maintainer's own BDHS microdata. Such tests are
# skipped unless BDHS_LOCAL_TESTS=true, and may only assert on aggregates.

skip_if_no_local_data <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("BDHS_LOCAL_TESTS"), "true"),
    "Local BDHS microdata tests are disabled (set BDHS_LOCAL_TESTS=true)."
  )
  dir <- local_data_dir()
  testthat::skip_if_not(dir.exists(dir), paste("Local data directory not found:", dir))
  invisible(dir)
}

local_data_dir <- function() {
  path.expand(Sys.getenv("BDHS_LOCAL_DIR", "~/Documents/MU/Research"))
}
