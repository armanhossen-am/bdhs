# Validation against official DHS Program values -------------------------------------

#' Validate indicator calculations against official DHS values
#'
#' Computes national estimates with [bdhs_estimate()] and compares them with
#' the official values published by the DHS Program API (bundled as
#' [bdhs_indicators]). Use it to check that your copy of the data and the
#' package's definitions reproduce the published figures before analysing
#' subgroups or pooled rounds.
#'
#' The official value is the national total (`is_total`) for the round,
#' taken for the indicator's reference period (`by_variable_label`, e.g.
#' "Three years preceding the survey") when the indicator defines one, and
#' the DHS-preferred value (`is_preferred`) otherwise. Indicators defined as
#' a sum of API categories (for example postnatal care within two days) are
#' compared with the sum.
#'
#' A computation passes when `abs(computed - official)` does not exceed
#' `tolerance` (scaled for indicators in other units, e.g. 0.1 x `tolerance`
#' for the total fertility rate) plus half a unit of the official value's
#' rounding precision.
#'
#' @param data A BDHS recode data frame for one round (or pooled rounds).
#' @param indicators Indicator ids to validate. `NULL` (default) uses every
#'   indicator for the recode and round that has an official DHS value.
#' @param tolerance Allowed absolute difference, in the units of the
#'   official value (percentage points for percentages).
#' @param survey Optional survey reference, overriding detection.
#'
#' @return A tibble with columns `survey_id`, `indicator`, `label`,
#'   `api_id`, `computed`, `official`, `diff` (computed - official),
#'   `tolerance` (effective tolerance), `pass`, `n_unweighted` and
#'   `official_n_unweighted` (the official unweighted denominator, when
#'   published).
#' @export
#' @examples
#' # Simulated data are random, so they are not expected to pass:
#' kr <- bdhs_simulate(2017, "KR")
#' bdhs_validate(kr, c("rh_del_facility", "rh_del_csection"))
bdhs_validate <- function(data, indicators = NULL, tolerance = 0.5, survey = NULL) {
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) || tolerance < 0) {
    cli::cli_abort("{.arg tolerance} must be a single non-negative number.")
  }
  recode <- data_recode(data)
  ids <- data_survey_ids(data, survey)
  if (is.null(indicators)) {
    indicators <- unique(unlist(lapply(ids, function(s) applicable_indicators(recode, s, with_api = TRUE))))
    if (!length(indicators)) {
      cli::cli_abort("No indicator with an official DHS value is available for {.val {recode}} data from {.val {ids}}.")
    }
  }
  inds <- resolve_indicators(indicators)
  no_api <- vapply(inds, function(x) all(is.na(x$api_id)), logical(1))
  if (any(no_api)) {
    cli::cli_inform("No official DHS value to compare for {.val {names(inds)[no_api]}}.")
    inds <- inds[!no_api]
  }
  if (!length(inds)) {
    return(empty_validation())
  }
  est <- bdhs_estimate(data, names(inds), survey = survey)
  if (!nrow(est)) {
    return(empty_validation())
  }
  rows <- lapply(seq_len(nrow(est)), function(i) {
    ind <- inds[[est$indicator[i]]]
    off <- official_value(ind, est$survey_id[i])
    tol <- tolerance * ind$tol_scale + 0.5 * 10^(-off$precision)
    diff <- est$estimate[i] - off$value
    tibble::tibble(
      survey_id = est$survey_id[i], indicator = ind$id, label = ind$label,
      api_id = est$api_id[i], computed = est$estimate[i], official = off$value,
      diff = diff, tolerance = tol, pass = !is.na(diff) & abs(diff) <= tol,
      n_unweighted = est$n_unweighted[i], official_n_unweighted = off$n_unweighted
    )
  })
  dplyr::bind_rows(rows)
}

empty_validation <- function() {
  tibble::tibble(
    survey_id = character(), indicator = character(), label = character(), api_id = character(),
    computed = numeric(), official = numeric(), diff = numeric(), tolerance = numeric(),
    pass = logical(), n_unweighted = integer(), official_n_unweighted = numeric()
  )
}

.official_env <- new.env(parent = emptyenv())

# National totals of the bundled DHS API values (cached).
official_totals <- function() {
  if (is.null(.official_env$totals)) {
    env <- new.env()
    utils::data("bdhs_indicators", package = "bdhs", envir = env)
    x <- env$bdhs_indicators
    x <- x[x$is_total & startsWith(x$characteristic_category, "Total"), ]
    .official_env$totals <- x
  }
  .official_env$totals
}

# Official value for an indicator and round; sums the parts of composite ids.
official_value <- function(ind, survey_id) {
  x <- official_totals()
  by <- ind$api_by
  if (length(by) > 1L || !is.null(names(by))) {
    by <- if (survey_id %in% names(by)) by[[survey_id]] else by[["default"]]
  }
  parts <- lapply(ind$api_id, function(id) {
    r <- x[x$survey_id == survey_id & x$indicator_id == id, ]
    if (!is.na(by)) {
      r <- r[!is.na(r$by_variable_label) & r$by_variable_label == by, ]
    } else if (any(r$is_preferred)) {
      r <- r[r$is_preferred, ]
    }
    if (nrow(r) > 1L) {
      r <- r[order(r$characteristic_category != "Total", r$characteristic_category != "Total 15-49"), ]
    }
    if (!nrow(r)) {
      return(NULL)
    }
    r[1, ]
  })
  if (any(vapply(parts, is.null, logical(1)))) {
    return(list(value = NA_real_, precision = 1L, n_unweighted = NA_real_))
  }
  list(
    value = sum(vapply(parts, function(p) p$value, numeric(1))),
    precision = max(vapply(parts, function(p) as.integer(p$precision %|NA|% 1L), integer(1))),
    n_unweighted = parts[[1]]$denom_unweighted
  )
}

# Rounds for which each indicator reproduced the official value with the
# maintainer's local microdata (aggregate results only; see
# tests/testthat/test-local-validation.R).
indicator_validation_status <- function() {
  .indicator_validation
}

#' The validation record shipped with bdhs
#'
#' Every indicator in [bdhs_indicator_list()] that has an official DHS value
#' was checked by computing it with [bdhs_validate()] from the maintainer's
#' own copies of the BDHS recode files and comparing it with the DHS Program
#' API value. This function returns that record: national aggregates only, no
#' microdata.
#'
#' An indicator-round passes when the computed value is within 0.5 points
#' (plus half a unit of the official value's rounding) of the official value.
#' Rounds and recodes not in the record have not been checked, usually
#' because the maintainer had no copy of that file.
#'
#' @return A tibble with columns `survey_id`, `recode`, `indicator`,
#'   `computed`, `official` and `pass`.
#' @seealso [bdhs_validate()] to check your own copy of the data.
#' @export
#' @examples
#' rec <- bdhs_validation_record()
#' table(rec$survey_id, rec$pass)
bdhs_validation_record <- function() {
  indicator_validation_status()
}
