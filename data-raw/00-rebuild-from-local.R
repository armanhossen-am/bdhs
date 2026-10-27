#!/usr/bin/env Rscript
# =============================================================================
# 00-rebuild-from-local.R -- refresh everything that depends on the
# maintainer's own BDHS microdata, in one command:
#
#   1. unzip any DHS zip downloads (Stata .dta members only) into a temporary
#      folder OUTSIDE the repository, deleted at the end;
#   2. rebuild the codebooks (data/bdhs_codebook.rda, bdhs_value_labels.rda);
#   3. rebuild the validation record (R/indicator-defs-validated.R);
#   4. run the test suite including the local real-data tests.
#
# Only metadata and national aggregates are written into the package; the
# microdata are read, never copied into the repository.
#
# Usage (from the package root). Put new downloads (zips or .DTA files) in any
# folder and list it; several folders are separated by ":".
#
#   BDHS_LOCAL_DIR="~/Documents/MU/Research:~/Downloads/DHS" Rscript data-raw/00-rebuild-from-local.R
# =============================================================================

dirs <- path.expand(strsplit(Sys.getenv("BDHS_LOCAL_DIR", "~/Documents/MU/Research"), ":", fixed = TRUE)[[1]])
dirs <- dirs[dir.exists(dirs)]
if (!length(dirs)) stop("None of the BDHS_LOCAL_DIR folders exist.", call. = FALSE)

tmp <- tempfile("bdhs-unzip-")
dir.create(tmp)
run <- function(script, extra_dir = tmp) {
  env <- c(paste0("BDHS_LOCAL_DIR=", paste(c(dirs, extra_dir), collapse = ":")), "BDHS_LOCAL_TESTS=true")
  status <- system2(file.path(R.home("bin"), "Rscript"), script, env = env)
  if (!identical(status, 0L)) stop(script, " failed (exit status ", status, ").", call. = FALSE)
}

result <- tryCatch({
  zips <- list.files(dirs, pattern = "[.]zip$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  zips <- zips[grepl("^BD", basename(zips), ignore.case = TRUE)]
  for (z in zips) {
    members <- utils::unzip(z, list = TRUE)$Name
    dta <- members[grepl("[.]dta$", members, ignore.case = TRUE)]
    if (length(dta)) {
      utils::unzip(z, files = dta, exdir = file.path(tmp, tools::file_path_sans_ext(basename(z))), junkpaths = TRUE)
    }
  }
  message(length(zips), " DHS zip file(s) unpacked to a temporary folder.")

  run("data-raw/03-build-codebooks.R")
  run("data-raw/07-build-validation.R")
  run(c("-e", shQuote("devtools::document(); devtools::test()")))
  "ok"
}, finally = unlink(tmp, recursive = TRUE))

message("Rebuild finished. Review with `git diff --stat`, then commit and push.")
