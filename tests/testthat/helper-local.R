# Tests on the maintainer's own DHS files.
#
# DHS microdata cannot be redistributed, so these tests are skipped unless
# BDHS_LOCAL_TESTS=true. Files are looked up below BDHS_LOCAL_DIR (default
# ~/Documents/MU/Research). Such tests must only assert on aggregates (counts,
# names, labels) and never print or store row-level values.

local_data_dir <- function() {
  path.expand(Sys.getenv("BDHS_LOCAL_DIR", "~/Documents/MU/Research"))
}

skip_if_no_local_data <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("BDHS_LOCAL_TESTS"), "true"),
    "local DHS files not enabled (set BDHS_LOCAL_TESTS=true)"
  )
  dir <- local_data_dir()
  testthat::skip_if_not(dir.exists(dir), paste("local DHS folder not found:", dir))
  invisible(dir)
}

# Path of a local DHS file relative to BDHS_LOCAL_DIR; skips if absent.
local_data_file <- function(...) {
  skip_if_no_local_data()
  path <- file.path(local_data_dir(), ...)
  testthat::skip_if_not(file.exists(path), paste("local file not found:", path))
  path
}
