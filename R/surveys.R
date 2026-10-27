# Survey rounds and survey-id resolution ---------------------------------------

# Core facts about every Bangladesh survey distributed by the DHS Program.
# The richer, API-derived catalogue is the exported `bdhs_surveys` dataset;
# this small table lives in code so that id resolution never depends on it.
.rounds <- tibble::tribble(
  ~survey_id,  ~survey_type, ~year, ~year_label, ~phase,   ~file_code, ~aliases,
  "BD1994DHS", "DHS",        1994L, "1993-94",   "DHS-III", "31",      "1993|1994|1993-94|1993-1994|93-94",
  "BD1997DHS", "DHS",        1997L, "1996-97",   "DHS-III", "3A",      "1996|1997|1996-97|1996-1997|96-97",
  "BD2000DHS", "DHS",        2000L, "1999-2000", "DHS-IV",  "41",      "1999|2000|1999-2000|1999-00|99-00",
  "BD2004DHS", "DHS",        2004L, "2004",      "DHS-IV",  "4J",      "2004",
  "BD2007DHS", "DHS",        2007L, "2007",      "DHS-V",   "51",      "2007",
  "BD2011DHS", "DHS",        2011L, "2011",      "DHS-VI",  "61",      "2011",
  "BD2014DHS", "DHS",        2014L, "2014",      "DHS-7",   "72",      "2014",
  "BD2017DHS", "DHS",        2017L, "2017-18",   "DHS-7",   "7R",      "2017|2018|2017-18|2017-2018|17-18",
  "BD2022DHS", "DHS",        2022L, "2022",      "DHS-8",   "81",      "2022",
  "BD1999SPA", "SPA",        2000L, "1999-2000", "SPA",     "4R",      "spa1999|spa2000|spa 1999|spa 1999-2000",
  "BD2014SPA", "SPA",        2014L, "2014",      "SPA",     "7A",      "spa2014|spa 2014",
  "BD2017SPA", "SPA",        2017L, "2017",      "SPA",     "7I",      "spa2017|spa 2017",
  "BD2001OTH", "OTH",        2001L, "2001",      "BMMS",    "4A",      "bmms2001|bmms 2001|2001"
)

#' Resolve survey identifiers
#'
#' Converts the many ways people refer to a BDHS round into the canonical DHS
#' survey id (for example `"BD2017DHS"`). Years, year ranges, DHS survey ids,
#' file version codes and phase-qualified codes are all accepted, so `2017`,
#' `2018`, `"2017-18"`, `"BD2017DHS"` and `"7R"` all resolve to the 2017-18
#' BDHS.
#'
#' Plain years and year ranges resolve to the main DHS round. Use the explicit
#' id (`"BD2014SPA"`) or a prefixed alias (`"spa2014"`, `"bmms2001"`) for the
#' Service Provision Assessments and the 2001 maternal mortality survey.
#'
#' @param x A vector of survey references (numeric or character). `NULL`
#'   returns every survey of the requested `type`.
#' @param type Survey types to consider: any of `"DHS"`, `"SPA"`, `"OTH"`.
#' @param error If `TRUE` (default), unresolvable references are an error;
#'   otherwise they become `NA`.
#'
#' @return A character vector of DHS survey ids, the same length as `x`.
#' @export
#' @examples
#' bdhs_survey_id(c(2022, "2017-18", "1999-2000", "BD2014DHS", "7R"))
#' bdhs_survey_id("spa2017")
#' bdhs_survey_id()
bdhs_survey_id <- function(x = NULL, type = c("DHS", "SPA", "OTH"), error = TRUE) {
  type <- toupper(type)
  rounds <- .rounds[.rounds$survey_type %in% type, ]
  if (is.null(x)) {
    return(rounds$survey_id)
  }
  if (!length(x)) {
    return(character())
  }
  key <- tolower(gsub("\\s+", " ", trimws(as.character(x))))
  out <- vapply(key, resolve_one_survey, character(1), rounds = rounds, USE.NAMES = FALSE)
  bad <- is.na(out) & !is.na(x)
  if (error && any(bad)) {
    cli::cli_abort(c(
      "Can't resolve {cli::qty(sum(bad))}survey reference{?s} {.val {as.character(x[bad])}}.",
      "i" = "Use a year ({.val 2022}), a year range ({.val 2017-18}), a DHS id ({.val BD2017DHS}) or a file code ({.val 7R}).",
      "i" = "Known surveys: {.val {rounds$survey_id}}."
    ))
  }
  out
}

resolve_one_survey <- function(key, rounds) {
  if (is.na(key) || !nzchar(key)) {
    return(NA_character_)
  }
  ids <- tolower(rounds$survey_id)
  # Exact survey id, e.g. "bd2017dhs".
  hit <- match(key, ids)
  if (!is.na(hit)) {
    return(rounds$survey_id[[hit]])
  }
  # File version code with or without the country prefix: "7r", "bd7r".
  code <- sub("^bd", "", key)
  hit <- match(code, tolower(rounds$file_code))
  if (!is.na(hit) && nchar(code) == 2L) {
    return(rounds$survey_id[[hit]])
  }
  # Aliases: years, year ranges, "spa2014", ... DHS rounds are listed first,
  # so a bare year prefers the DHS round over an SPA of the same year.
  for (i in seq_len(nrow(rounds))) {
    if (key %in% strsplit(rounds$aliases[[i]], "|", fixed = TRUE)[[1]]) {
      return(rounds$survey_id[[i]])
    }
  }
  NA_character_
}

#' Survey round facts
#'
#' A compact table of the Bangladesh surveys distributed by the DHS Program
#' with their canonical ids, labels, DHS phase and file version code. See
#' [bdhs_surveys] for the full catalogue (fieldwork dates, sample sizes and
#' more).
#'
#' @inheritParams bdhs_survey_id
#' @return A tibble with columns `survey_id`, `survey_type`, `year`,
#'   `year_label`, `phase` and `file_code`.
#' @export
#' @examples
#' bdhs_rounds()
bdhs_rounds <- function(type = "DHS") {
  type <- toupper(type)
  out <- .rounds[.rounds$survey_type %in% type, ]
  out$aliases <- NULL
  out
}

# Year label ("2017-18") for survey ids.
survey_label <- function(survey_id) {
  .rounds$year_label[match(survey_id, .rounds$survey_id)]
}

# File version code ("7R") for survey ids.
survey_file_code <- function(survey_id) {
  .rounds$file_code[match(survey_id, .rounds$survey_id)]
}

# Resolve survey id from a DHS country-phase code (v000 / hv000), e.g. "BD7".
# Several rounds share a phase code, so the interview year breaks ties.
survey_from_v000 <- function(v000, year = NULL) {
  phase_digit <- substr(toupper(trimws(v000)), 3, 3)
  dhs <- .rounds[.rounds$survey_type == "DHS", ]
  cand <- dhs[substr(dhs$file_code, 1, 1) == phase_digit, ]
  if (!nrow(cand)) {
    return(NA_character_)
  }
  if (nrow(cand) == 1L || is.null(year) || all(is.na(year))) {
    return(if (nrow(cand) == 1L) cand$survey_id else NA_character_)
  }
  year <- stats::median(year, na.rm = TRUE)
  if (year < 100) year <- year + 1900
  cand$survey_id[[which.min(abs(cand$year - year))]]
}

# Survey and recode detection for data frames -----------------------------------

#' Detect which survey round and recode a data frame comes from
#'
#' Used by functions that accept DHS data so that users rarely need to say
#' which round they are working with. The `bdhs_survey_id` attribute set by
#' [bdhs_read()] wins; otherwise a `survey_id` column (pooled data) or the DHS
#' country-phase code and interview year are used.
#'
#' @param data A data frame of DHS recode data.
#' @param survey Optional explicit survey reference, which overrides detection.
#' @param call Calling environment for error messages.
#' @return For `data_survey_id()`, a character vector of survey ids (length
#'   one unless the data are pooled). For `data_recode()`, a recode code.
#' @noRd
data_survey_id <- function(data, survey = NULL, call = rlang::caller_env()) {
  if (!is.null(survey)) {
    return(bdhs_survey_id(survey))
  }
  id <- attr(data, "bdhs_survey_id", exact = TRUE)
  if (!is.null(id)) {
    return(id)
  }
  if ("survey_id" %in% names(data)) {
    return(sort(unique(as.character(data$survey_id))))
  }
  nm <- tolower(names(data))
  v000 <- intersect(c("v000", "hv000", "mv000"), nm)
  if (length(v000)) {
    col <- names(data)[match(v000[[1]], nm)]
    yr_col <- names(data)[match(sub("000$", "007", v000[[1]]), nm)]
    yr <- if (!is.na(yr_col)) as.numeric(zap_all(data[[yr_col]])) else NULL
    codes <- unique(trimws(as.character(zap_all(data[[col]]))))
    if (length(codes) == 1L) {
      id <- survey_from_v000(codes, yr)
      if (!is.na(id)) {
        return(id)
      }
    }
  }
  cli::cli_abort(
    c(
      "Can't tell which BDHS round these data come from.",
      "i" = "Supply {.arg survey}, e.g. {.code survey = 2022}, or read the data with {.fn bdhs_read}."
    ),
    call = call
  )
}

#' @rdname data_survey_id
#' @noRd
data_recode <- function(data, recode = NULL, call = rlang::caller_env()) {
  if (!is.null(recode)) {
    return(check_recode(recode, call = call))
  }
  rec <- attr(data, "bdhs_recode", exact = TRUE)
  if (!is.null(rec)) {
    return(rec)
  }
  nm <- tolower(names(data))
  has <- function(...) all(c(...) %in% nm)
  rec <- if (has("midx") || (has("bidx") && has("caseid"))) {
    # The 2017-18 BR also carries midx, so KR vs BR is decided by the children's
    # ages: KR holds births in the last five years only (three in 1993-94).
    kr_or_br(data, nm)
  } else if (has("mv001")) {
    "MR"
  } else if (has("caseid", "v001")) {
    "IR"
  } else if (has("hvidx")) {
    "PR"
  } else if (has("hhid", "hv001")) {
    "HR"
  } else {
    NA_character_
  }
  if (is.na(rec)) {
    cli::cli_abort(
      c(
        "Can't tell which DHS recode file these data come from.",
        "i" = "Supply {.arg recode}, e.g. {.code recode = \"IR\"}."
      ),
      call = call
    )
  }
  rec
}

kr_or_br <- function(data, nm) {
  col <- function(v) data[[names(data)[match(v, nm)]]]
  if (all(c("v008", "b3") %in% nm)) {
    age <- as.numeric(zap_all(col("v008"))) - as.numeric(zap_all(col("b3")))
    if (any(!is.na(age))) {
      return(if (max(age, na.rm = TRUE) <= 60) "KR" else "BR")
    }
  }
  if ("midx" %in% nm) "KR" else "BR"
}
