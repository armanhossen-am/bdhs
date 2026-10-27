# Official DHS estimates ------------------------------------------------------------

#' Official DHS estimates for Bangladesh
#'
#' A friendly, offline accessor for [bdhs_indicators], the official estimates
#' that the DHS Program publishes for every Bangladesh DHS round (the same
#' numbers as STATcompiler and the final reports). Pick indicators by DHS id
#' or by searching their labels and definitions, choose rounds, and get a tidy
#' table back.
#'
#' Indicator ids follow the DHS API, e.g. `"FE_FRTR_W_TFR"` (total fertility
#' rate) or `"CN_NUTS_C_HA2"` (stunting). Use [bdhs_search()] to find them.
#'
#' Subnational rows follow the DHS region hierarchy: the five regions that are
#' comparable across all rounds (`"Chattogram/Sylhet"`, `"Dhaka before 2015"`,
#' `"Rajshahi/Rangpur"`, Barishal, Khulna) and, below them, the divisions of
#' each round (Chattogram, Sylhet, Dhaka, Mymensingh, Rajshahi, Rangpur). The
#' `region_id` column links to [bdhs_boundaries()] (`reg_id`).
#'
#' @param indicator Character vector of DHS indicator ids. `NULL` (default)
#'   keeps every indicator (subject to `search`).
#' @param survey Survey references understood by [bdhs_survey_id()], e.g.
#'   `2022`, `"2017-18"` or `c(2014, 2022)`. `NULL` keeps all rounds.
#' @param breakdown Which rows to return: `"national"` totals (default),
#'   `"subnational"` (regions and divisions), `"background"` characteristics
#'   (residence, education, wealth, age, ...) or `"all"`. When `characteristic`
#'   is given and `breakdown` is not, all breakdowns are searched.
#' @param characteristic Characteristic categories to keep, e.g. `"Residence"`,
#'   `"Wealth quintile"`, `"Education"` or `"Region"` (`"Division"` is
#'   accepted as a synonym). Case-insensitive.
#' @param preferred If `TRUE` (default), keep only the headline value of each
#'   estimate (the DHS `IsPreferred` flag). With `FALSE`, alternative
#'   reference periods are returned too and a `by_variable_label` column says
#'   which is which.
#' @param search A regular expression matched (case-insensitively) against
#'   indicator ids, labels, short names and definitions in
#'   [bdhs_indicator_catalog].
#'
#' @return A tibble with columns `survey_id`, `year_label`, `indicator_id`,
#'   `indicator`, `characteristic_category`, `characteristic_label`, `value`,
#'   `ci_low`, `ci_high`, `denom_weighted`, `denom_unweighted`, `region_id`
#'   and `breakdown` (plus `by_variable_label` when `preferred = FALSE`),
#'   ordered by survey year, indicator and characteristic.
#' @seealso [bdhs_search()] to find indicators, [bdhs_plot_trend()] and
#'   [bdhs_map()] to visualise them.
#' @export
#' @examples
#' # Total fertility rate in every round
#' bdhs_official("FE_FRTR_W_TFR")
#'
#' # Stunting by wealth quintile in the two latest rounds
#' bdhs_official("CN_NUTS_C_HA2", survey = c("2017-18", 2022), characteristic = "Wealth quintile")
#'
#' # Anything about anemia, by division, in 2011
#' bdhs_official(search = "anemia", survey = 2011, breakdown = "subnational")
bdhs_official <- function(indicator = NULL, survey = NULL,
                          breakdown = c("national", "subnational", "background", "all"),
                          characteristic = NULL, preferred = TRUE, search = NULL) {
  breakdown_missing <- missing(breakdown)
  breakdown <- rlang::arg_match(breakdown)
  if (breakdown_missing && !is.null(characteristic)) {
    breakdown <- "all"
  }
  if (!rlang::is_bool(preferred)) {
    cli::cli_abort("{.arg preferred} must be {.code TRUE} or {.code FALSE}.")
  }
  rows <- explore_official_rows(
    indicator = indicator, survey = survey, breakdown = breakdown,
    characteristic = characteristic, preferred = preferred, search = search
  )
  explore_official_tidy(rows, by_variable = !preferred)
}

# Internal: the matching rows of bdhs_indicators with every column (ordering
# keys, precision, level_rank, ...), ordered. Shared by plots and maps.
explore_official_rows <- function(indicator = NULL, survey = NULL, breakdown = "all",
                          characteristic = NULL, preferred = TRUE, search = NULL,
                          call = rlang::caller_env()) {
  data <- bdhs::bdhs_indicators
  keep <- rep(TRUE, nrow(data))

  ids <- explore_official_ids(indicator, search, call = call)
  if (!is.null(ids)) {
    keep <- keep & data$indicator_id %in% ids
  }
  if (!is.null(survey)) {
    sid <- bdhs_survey_id(survey)
    no_data <- setdiff(sid, unique(data$survey_id))
    if (length(no_data)) {
      cli::cli_abort(
        c(
          "No official estimates are bundled for {.val {no_data}}.",
          "i" = "The DHS Program publishes indicators for the DHS rounds {.val {unique(data$survey_id)}}."
        ),
        call = call
      )
    }
    keep <- keep & data$survey_id %in% sid
  }
  if (!identical(breakdown, "all")) {
    keep <- keep & data$breakdown %in% breakdown
  }
  if (isTRUE(preferred)) {
    keep <- keep & data$is_preferred
  }
  out <- data[keep, ]

  if (!is.null(characteristic) && nrow(out)) {
    cats <- explore_resolve_characteristic(characteristic, unique(out$characteristic_category), call = call)
    out <- out[out$characteristic_category %in% cats, ]
  }

  if (!nrow(out)) {
    cli::cli_abort(
      c(
        "No official estimates match this selection.",
        "i" = if (!is.null(ids)) {
          "{length(ids)} indicator{?s} matched, but {?has/have} no values for the requested round(s) and breakdown."
        },
        "i" = if (!is.null(ids) && length(ids) <= 3) {
          "See which rounds and breakdowns exist with {.code bdhs_official({deparse(ids)}, breakdown = \"all\")}."
        } else {
          "Find indicators with {.code bdhs_search(\"<keyword>\", where = \"indicators\")}."
        }
      ),
      call = call
    )
  }
  out[order(out$survey_year, out$indicator_order, out$characteristic_order, out$by_variable_id), ]
}

# Indicator ids selected by `indicator` and/or `search` (intersection), or NULL
# for "all".
explore_official_ids <- function(indicator = NULL, search = NULL, call = rlang::caller_env()) {
  catalog <- bdhs::bdhs_indicator_catalog
  ids <- NULL
  if (!is.null(indicator)) {
    if (!is.character(indicator) || anyNA(indicator)) {
      cli::cli_abort("{.arg indicator} must be a character vector of DHS indicator ids.", call = call)
    }
    ids <- unique(official_ids_from_registry(indicator))
    unknown <- setdiff(ids, catalog$indicator_id)
    if (length(unknown)) {
      close <- unique(unlist(lapply(unknown, function(u) {
        catalog$indicator_id[utils::adist(u, catalog$indicator_id) <= 2]
      })))
      cli::cli_abort(
        c(
          "Unknown DHS indicator id{?s} {.val {unknown}}.",
          "i" = if (length(close)) "Did you mean {.val {utils::head(close, 5)}}?",
          "i" = "Search indicators with {.code bdhs_search(\"<keyword>\", where = \"indicators\")} or use {.arg search}."
        ),
        call = call
      )
    }
  }
  if (!is.null(search)) {
    if (!rlang::is_string(search)) {
      cli::cli_abort("{.arg search} must be a single string.", call = call)
    }
    text <- paste(catalog$indicator_id, catalog$label, catalog$short_name, catalog$definition, sep = " | ")
    found <- unique(catalog$indicator_id[explore_grepl(search, text)])
    if (!length(found)) {
      cli::cli_abort(
        c(
          "No indicator id, label or definition matches {.val {search}}.",
          "i" = "Try a broader pattern, or {.code bdhs_search({.val {search}})} to search variables too."
        ),
        call = call
      )
    }
    ids <- if (is.null(ids)) found else intersect(ids, found)
    if (!length(ids)) {
      cli::cli_abort(
        c(
          "None of the indicators {.val {indicator}} matches {.arg search} {.val {search}}.",
          "i" = "Drop one of {.arg indicator} or {.arg search}."
        ),
        call = call
      )
    }
  }
  ids
}

# Map user-facing characteristic names to the categories present.
explore_resolve_characteristic <- function(characteristic, available, call = rlang::caller_env()) {
  if (!is.character(characteristic) || anyNA(characteristic)) {
    cli::cli_abort("{.arg characteristic} must be a character vector.", call = call)
  }
  aliases <- c(
    division = "region", divisions = "region", regions = "region", subnational = "region",
    wealth = "wealth quintile", "wealth index" = "wealth quintile", quintile = "wealth quintile",
    "urban/rural" = "residence", "urban-rural" = "residence", "place of residence" = "residence",
    national = "total"
  )
  key <- tolower(trimws(characteristic))
  aliased <- key %in% names(aliases)
  key[aliased] <- aliases[key[aliased]]
  low <- tolower(available)
  bad <- characteristic[!key %in% low]
  if (length(bad)) {
    cli::cli_abort(
      c(
        "No estimates for characteristic{?s} {.val {bad}} in this selection.",
        "i" = "Available: {.val {sort(available)}}."
      ),
      call = call
    )
  }
  available[low %in% key]
}

# The public, tidy shape of official estimates.
explore_official_tidy <- function(rows, by_variable = FALSE) {
  out <- tibble::tibble(
    survey_id = rows$survey_id,
    year_label = survey_label(rows$survey_id),
    indicator_id = rows$indicator_id,
    indicator = rows$indicator,
    characteristic_category = rows$characteristic_category,
    characteristic_label = explore_clean_label(rows$characteristic_label),
    value = rows$value,
    ci_low = rows$ci_low,
    ci_high = rows$ci_high,
    denom_weighted = rows$denom_weighted,
    denom_unweighted = rows$denom_unweighted,
    region_id = rows$region_id,
    breakdown = rows$breakdown
  )
  if (by_variable) {
    out$by_variable_label <- rows$by_variable_label
  }
  out
}

# The API marks sub-regions with a ".." prefix ("..Sylhet").
explore_clean_label <- function(x) {
  sub("^\\.+", "", x)
}

# Catalog facts (label, measurement type, ...) for indicator ids; the first
# catalog entry wins for the few ids the DHS Program reuses.
explore_catalog <- function(ids) {
  catalog <- bdhs::bdhs_indicator_catalog
  catalog[match(ids, catalog$indicator_id), ]
}

# Case-insensitive regex matching that falls back to a fixed-string match
# when `pattern` is not a valid regular expression.
explore_grepl <- function(pattern, x) {
  out <- tryCatch(
    grepl(pattern, x, ignore.case = TRUE, perl = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(out)) {
    out <- grepl(tolower(pattern), tolower(x), fixed = TRUE)
  }
  out & !is.na(x)
}

# bdhs indicator ids (e.g. "rh_del_csection") given where DHS indicator ids are
# expected are replaced by the DHS ids they reproduce, with a message.
official_ids_from_registry <- function(indicator) {
  reg <- indicator_registry()
  out <- character()
  for (x in indicator) {
    key <- tolower(trimws(x))
    api <- if (key %in% names(reg)) stats::na.omit(reg[[key]]$api_id) else character()
    if (length(api)) {
      cli::cli_inform("Using DHS indicator id{cli::qty(length(api))}{?s} {.val {api}} for bdhs indicator {.val {key}}.")
      out <- c(out, api)
    } else {
      out <- c(out, toupper_trim(x))
    }
  }
  out
}
