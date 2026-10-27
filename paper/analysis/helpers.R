# Shared helpers for the paper's analysis scripts. They read the author's
# licensed BDHS files and only ever return or write aggregates.

local_bdhs_files <- function(dirs = Sys.getenv("BDHS_LOCAL_DIR", "~/Documents/MU/Research")) {
  dirs <- path.expand(strsplit(dirs, ":", fixed = TRUE)[[1]])
  f <- list.files(dirs, pattern = "[.]dta$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  ids <- do.call(rbind, lapply(f, function(x) tryCatch(suppressWarnings(bdhs_identify_file(x)), error = function(e) NULL)))
  ids <- ids[!is.na(ids$survey_id) & !ids$derived, ]
  ids[!duplicated(paste(ids$survey_id, ids$recode)), ]
}

# Read one survey x recode with the columns some indicators (and extras) need.
read_local <- function(files, survey_id, recode, indicators, extra = NULL) {
  row <- files[files$survey_id == survey_id & files$recode == recode, ]
  if (!nrow(row)) stop("No local file for ", survey_id, " ", recode, call. = FALSE)
  suppressMessages(suppressWarnings(
    bdhs_read_file(row$path[[1]], indicators = indicators, vars = extra)
  ))
}

# National (or by-group) estimate from bdhs_estimate().
est <- function(data, indicator, by = NULL, all = FALSE) {
  e <- suppressMessages(suppressWarnings(bdhs_estimate(data, indicator, by = by)))
  if (all) e else e$estimate[[1]]
}

# Official national value; `period` picks a reference period ("Three", "Two", ...).
official <- function(id, survey, period = NULL) {
  o <- suppressMessages(bdhs_official(id, survey = survey, preferred = is.null(period)))
  o <- o[o$characteristic_category == "Total", ]
  if (!is.null(period)) o <- o[grepl(period, o$by_variable_label %|NA|% "", ignore.case = TRUE), ]
  if (nrow(o)) o$value[[1]] else NA_real_
}
