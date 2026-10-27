# Synthetic DHS-like files for the data-store tests. Everything is generated
# with bdhs_simulate() and written to temporary folders.

# Point the bdhs data store at a fresh temporary folder for one test.
local_bdhs_store <- function(env = parent.frame()) {
  dir <- withr::local_tempdir("bdhs-store-", .local_envir = env)
  withr::local_envvar(BDHS_DATA_DIR = dir, .local_envir = env)
  withr::local_options(bdhs.data_dir = NULL, .local_envir = env)
  dir
}

# A synthetic recode, small but with at least a few rows in every recode.
sim_recode <- function(survey = 2022, recode = "IR", seed = 1) {
  bdhs_simulate(survey, recode, n_clusters = 6, households = 6, seed = seed)
}

write_fixture_dta <- function(dir, name, survey = 2022, recode = "IR", data = NULL, seed = 1) {
  data <- data %||% sim_recode(survey, recode, seed = seed)
  path <- file.path(dir, name)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  haven::write_dta(data, path)
  path
}

# SPSS files from the DHS Program have upper-case variable names.
write_fixture_sav <- function(dir, name, survey = 2022, recode = "IR", seed = 1) {
  data <- sim_recode(survey, recode, seed = seed)
  names(data) <- toupper(names(data))
  path <- file.path(dir, name)
  haven::write_sav(data, path)
  path
}

# Evaluate `expr`, muffling its messages; returns list(value, text).
capture_msgs <- function(expr) {
  msgs <- character()
  value <- withCallingHandlers(expr, message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  list(value = value, text = paste(msgs, collapse = "\n"))
}

skip_if_no_zip <- function() {
  testthat::skip_if(!nzchar(Sys.which(Sys.getenv("R_ZIPCMD", "zip"))), "no zip program")
}

# Zip `files` (paths relative to `root`) into `zipfile`.
write_fixture_zip <- function(zipfile, root, files) {
  skip_if_no_zip()
  withr::with_dir(root, utils::zip(normalizePath(zipfile, mustWork = FALSE), files, flags = "-q"))
  zipfile
}

# A download folder like a user's: a DHS Stata zip, a renamed file, an SPSS
# duplicate and a pooled file.
#   BDIR81DT.ZIP containing BDIR81FL.DTA (2022 IR)
#   BDHS2022KR.DTA                       (2022 KR, renamed)
#   BDIR81FL.SAV                         (2022 IR again, SPSS)
#   BDHS_2004_2022_KR_appended.dta       (pooled, must be skipped)
fixture_downloads <- function(env = parent.frame()) {
  dl <- withr::local_tempdir("dhs-downloads-", .local_envir = env)
  stage <- withr::local_tempdir("stage-", .local_envir = env)
  write_fixture_dta(stage, "BDIR81FL.DTA", 2022, "IR")
  write_fixture_zip(file.path(dl, "BDIR81DT.ZIP"), stage, "BDIR81FL.DTA")
  write_fixture_dta(dl, "BDHS2022KR.DTA", 2022, "KR")
  write_fixture_sav(dl, "BDIR81FL.SAV", 2022, "IR")
  pooled <- dplyr::bind_rows(
    haven::zap_labels(sim_recode(2004, "KR")),
    haven::zap_labels(sim_recode(2022, "KR"))
  )
  pooled$year <- ifelse(pooled$v007 == 2004, 2004, 2022)
  write_fixture_dta(dl, "BDHS_2004_2022_KR_appended.dta", data = pooled)
  dl
}
