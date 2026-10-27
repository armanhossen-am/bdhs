# Fertility and childhood mortality rates -------------------------------------------
#
# Native implementations of the DHS methods described in the Guide to DHS
# Statistics (DHS-7, 2018), ch. 5 (fertility) and ch. 8 (childhood
# mortality, synthetic cohort life table), with jackknife standard errors over
# primary sampling units as in the DHS sampling-error appendices. The DHS.rates
# package (Elkasabi 2019) implements the same methods and is used in the tests
# as an independent oracle; no code was taken from it.

.age_groups5 <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

#' Fertility rates from the birth history
#'
#' Computes the total fertility rate (TFR), age-specific fertility rates
#' (ASFR) or the general fertility rate (GFR) with the DHS method: births in
#' the reference period are classified by the mother's age at birth, and
#' woman-years of exposure in each five-year age group are counted month by
#' month within the period. Because every BDHS interviewed only ever-married
#' women, exposure is multiplied by the all-woman factor (`awfact/100`) so
#' that the rates refer to all women.
#'
#' Standard errors use the DHS jackknife: each primary sampling unit is
#' deleted in turn and the rate recomputed, giving
#' \eqn{SE = \sqrt{(k-1)/k \sum (r_{(j)} - r)^2}} for \eqn{k} clusters.
#'
#' @param data A women's recode (IR) data frame with the birth history in
#'   wide form (`b3_01`, `b3_02`, ...), the interview date `v008`, the
#'   woman's date of birth `v011`, the weight `v005`, the cluster `v021` and
#'   the all-woman factors (`awfactt`, ...).
#' @param type One of `"tfr"` (births per woman), `"asfr"` (births per 1,000
#'   women, by age group) or `"gfr"` (births per 1,000 women age 15-44).
#' @param period Length of the reference period in months before the
#'   interview (DHS standard: 36 for TFR/ASFR/GFR). The month of the
#'   interview is excluded.
#' @param by Optional character vector of grouping variables (woman-level).
#' @param se If `TRUE` (default), compute jackknife standard errors and
#'   confidence intervals.
#' @param awfact All-woman factor. `NULL` (default) chooses it from `by`
#'   (`awfactu` for `v025`, `awfactr` for `v024`, `awfacte` for `v106`,
#'   `awfactw` for `v190`, otherwise `awfactt`); `FALSE` disables the
#'   adjustment (for all-woman samples); a column name uses that column.
#' @param level Confidence level.
#' @param survey Optional survey reference, overriding detection.
#'
#' @return A tibble with columns `survey_id`, the `by` variables,
#'   `indicator`, `age_group`, `estimate`, `se`, `ci_low`, `ci_high`,
#'   `n_births` (unweighted births in the period), `exposure` (weighted
#'   woman-years), `n_women`, `n_psu` and `period`.
#' @seealso [bdhs_mortality()], [bdhs_estimate()]
#' @export
#' @examples
#' ir <- bdhs_simulate(2017, "IR")
#' bdhs_fertility(ir)
#' bdhs_fertility(ir, "asfr", se = FALSE)
#' bdhs_fertility(ir, "tfr", by = "v025", se = FALSE)
bdhs_fertility <- function(data, type = c("tfr", "asfr", "gfr"), period = 36, by = NULL, se = TRUE,
                           awfact = NULL, level = 0.95, survey = NULL) {
  type <- rlang::arg_match(type)
  check_period(period, "months")
  check_by(data, by)
  b3_vars <- grep("^b3_[0-9]+$", names(data), value = TRUE)
  needed <- c("v005", "v008", "v011", "v021")
  missing_vars <- setdiff(needed, names(data))
  if (!length(b3_vars) || length(missing_vars)) {
    cli::cli_abort(c(
      "{.arg data} must be a women's recode (IR) with the birth history in wide form.",
      "x" = "Missing: {.field {c(if (!length(b3_vars)) 'b3_01, b3_02, ...', missing_vars)}}."
    ))
  }
  aw_var <- resolve_awfact(data, awfact, by)
  out <- lapply(survey_row_groups(data, survey), function(g) {
    d <- data[g$rows, , drop = FALSE]
    comp <- fertility_components(d, b3_vars, period, aw_var)
    res <- rate_by_groups(d, by, comp, function(B, E) fertility_stat(B, E, type), se, level)
    res$period <- as.integer(period)
    tibble::add_column(res, survey_id = g$survey_id, .before = 1)
  })
  out <- dplyr::bind_rows(out)
  labels <- switch(type,
    tfr = "15-49",
    gfr = "15-44",
    asfr = .age_groups5
  )
  n_stat <- length(labels)
  out$indicator <- type
  out$age_group <- rep(labels, length.out = nrow(out))
  names(out)[names(out) == "n_events"] <- "n_births"
  names(out)[names(out) == "denominator"] <- "exposure"
  names(out)[names(out) == "n_units"] <- "n_women"
  cols <- c("survey_id", by, "indicator", "age_group", "estimate", "se", "ci_low", "ci_high",
            "n_births", "exposure", "n_women", "n_psu", "period")
  stopifnot(nrow(out) %% n_stat == 0L)
  out[, cols]
}

#' Childhood mortality rates
#'
#' Computes neonatal (NNMR), postneonatal (PNNMR), infant (IMR), child (CMR,
#' 4q1) and under-five (U5MR) mortality rates per 1,000 live births with the
#' DHS synthetic cohort life-table method: probabilities of dying are
#' estimated for the age segments 0, 1-2, 3-5, 6-11, 12-23, 24-35, 36-47 and
#' 48-59 months from the births and deaths in the reference period, counting
#' half of the exposure (and deaths) of cohorts only partly exposed at the
#' start of the period, and half of the exposure of cohorts partly exposed at
#' its end. Standard errors use the DHS jackknife over clusters.
#'
#' @param data A births recode (BR, one row per birth with `b3`, `b7`) or a
#'   women's recode (IR, wide birth history `b3_01`, `b7_01`, ...), with
#'   `v008`, `v005` and `v021`.
#' @param period Length of the reference period in **years** before the
#'   interview (DHS standard: 5; 10 for subgroups).
#' @param rates Which rates to return.
#' @param by Optional character vector of grouping variables (birth- or
#'   woman-level).
#' @inheritParams bdhs_fertility
#'
#' @return A tibble with columns `survey_id`, the `by` variables,
#'   `indicator`, `estimate` (per 1,000 live births), `se`, `ci_low`,
#'   `ci_high`, `n_births` (unweighted births contributing to the rates),
#'   `n_deaths`, `n_psu` and `period` (years).
#' @seealso [bdhs_fertility()], [bdhs_estimate()]
#' @export
#' @examples
#' br <- bdhs_simulate(2017, "BR")
#' bdhs_mortality(br, se = FALSE)
#' bdhs_mortality(br, period = 10, rates = c("imr", "u5mr"), by = "v025")
bdhs_mortality <- function(data, period = 5, rates = c("nnmr", "pnnmr", "imr", "cmr", "u5mr"), by = NULL,
                           se = TRUE, level = 0.95, survey = NULL) {
  rates <- rlang::arg_match(rates, multiple = TRUE)
  check_period(period, "years")
  check_by(data, by)
  births <- births_long(data, by)
  ids <- data_survey_ids(data, survey)
  groups <- if (!is_pooled(data)) {
    list(list(survey_id = ids, rows = seq_len(nrow(births))))
  } else {
    lapply(ids, function(s) list(survey_id = s, rows = which(as.character(births$survey_id) == s)))
  }
  out <- lapply(groups, function(g) {
    d <- births[g$rows, , drop = FALSE]
    comp <- mortality_components(d, period * 12)
    res <- rate_by_groups(d, by, comp, function(D, E) mortality_stat(D, E, rates), se, level)
    res$period <- period
    tibble::add_column(res, survey_id = g$survey_id, .before = 1)
  })
  out <- dplyr::bind_rows(out)
  out$indicator <- rep(rates, length.out = nrow(out))
  names(out)[names(out) == "n_units"] <- "n_births"
  out[, c("survey_id", by, "indicator", "estimate", "se", "ci_low", "ci_high", "n_births", "n_deaths", "n_psu", "period")]
}

# Internals -----------------------------------------------------------------------

check_period <- function(period, unit, call = rlang::caller_env()) {
  if (!is.numeric(period) || length(period) != 1L || is.na(period) || period <= 0) {
    cli::cli_abort("{.arg period} must be a single positive number of {unit}.", call = call)
  }
}

check_by <- function(data, by, call = rlang::caller_env()) {
  if (is.null(by)) {
    return(invisible())
  }
  if (!is.character(by)) {
    cli::cli_abort("{.arg by} must be a character vector of variable names.", call = call)
  }
  bad <- setdiff(by, names(data))
  if (length(bad)) {
    cli::cli_abort("Grouping variable{?s} {.field {bad}} not found in {.arg data}.", call = call)
  }
}

# All-woman factor column for an ever-married sample.
resolve_awfact <- function(data, awfact, by, call = rlang::caller_env()) {
  if (isFALSE(awfact)) {
    return(NULL)
  }
  if (is.character(awfact)) {
    if (!awfact %in% names(data)) {
      cli::cli_abort("All-woman factor {.field {awfact}} not found in {.arg data}.", call = call)
    }
    return(awfact)
  }
  ever_married <- any(grepl("^aw", names(data)))
  if (!ever_married) {
    return(NULL)
  }
  aw <- awfact_for_by(by)
  if (!aw %in% names(data)) {
    cli::cli_warn("All-woman factor {.field {aw}} not found; using {.field awfactt}.", call = call)
    aw <- "awfactt"
  }
  if (!aw %in% names(data)) {
    cli::cli_abort(c(
      "These data are from an ever-married sample but have no all-woman factors.",
      "i" = "Keep {.field awfactt} (and {.field awfactu}, {.field awfactr}, ... for subgroups), or set {.code awfact = FALSE}."
    ), call = call)
  }
  aw
}

# DHS all-woman factor matching a background characteristic.
awfact_for_by <- function(by) {
  if (length(by) != 1L) {
    return("awfactt")
  }
  switch(by,
    v025 = , v102 = "awfactu",
    v024 = , v101 = "awfactr",
    v106 = , v149 = "awfacte",
    v190 = "awfactw",
    "awfactt"
  )
}

# Births and exposure by five-year age group for each woman.
fertility_components <- function(d, b3_vars, period, aw_var) {
  n <- nrow(d)
  v008 <- dv(d, "v008")
  v011 <- dv(d, "v011")
  births <- matrix(0, n, 7L)
  for (v in b3_vars) {
    b3 <- dv(d, v)
    since <- v008 - b3
    g <- floor((b3 - v011) / 60) - 2
    ok <- !is.na(since) & since >= 1 & since <= period & !is.na(g) & g >= 1 & g <= 7
    if (any(ok)) {
      idx <- cbind(which(ok), g[ok])
      births[idx] <- births[idx] + 1
    }
  }
  lower <- v008 - period
  upper <- v008 - 1
  exposure <- vapply(1:7, function(g) {
    lo <- v011 + 60 * (g + 2)
    hi <- lo + 59
    pmax(0, pmin(upper, hi) - pmax(lower, lo) + 1) / 12
  }, numeric(n))
  if (n == 1L) exposure <- matrix(exposure, nrow = 1L)
  aw <- if (is.null(aw_var)) rep(1, n) else dv(d, aw_var) / 100
  w <- dv(d, "v005") / 1e6
  list(
    psu = as.character(zap_all(d$v021)), w = w,
    num = births, den = exposure * aw,
    n_events = rowSums(births), keep = !is.na(w) & w > 0 & !is.na(v008) & !is.na(v011)
  )
}

fertility_stat <- function(B, E, type) {
  asfr <- B / E
  switch(type,
    tfr = 5 * sum(asfr),
    gfr = 1000 * sum(B) / sum(E[1:6]),
    asfr = 1000 * asfr
  )
}

.mort_segments <- list(lower = c(0, 1, 3, 6, 12, 24, 36, 48), upper = c(1, 3, 6, 12, 24, 36, 48, 60))

# One row per birth (BR, or IR reshaped), with design and grouping columns.
births_long <- function(data, by) {
  keep <- unique(c("v005", "v008", "v021", "v022", "survey_id", by))
  if (all(c("b3", "b7") %in% names(data))) {
    out <- data[, intersect(c(keep, "b3", "b7"), names(data)), drop = FALSE]
  } else {
    b3_vars <- grep("^b3_[0-9]+$", names(data), value = TRUE)
    if (!length(b3_vars)) {
      cli::cli_abort(c(
        "{.arg data} must be a births recode (BR) or a women's recode (IR) with the birth history.",
        "x" = "No {.field b3} or {.field b3_01}, {.field b3_02}, ... columns found."
      ), call = rlang::caller_env())
    }
    if (!length(grep("^b7_[0-9]+$", names(data)))) {
      cli::cli_abort(c(
        "{.arg data} has a birth history without ages at death.",
        "x" = "No {.field b7_01}, {.field b7_02}, ... columns found."
      ), call = rlang::caller_env())
    }
    base <- data[, intersect(keep, names(data)), drop = FALSE]
    parts <- lapply(b3_vars, function(v) {
      i <- sub("^b3_", "", v)
      b3 <- dv(data, v)
      sel <- !is.na(b3)
      part <- base[sel, , drop = FALSE]
      part$b3 <- b3[sel]
      part$b7 <- dv(data, paste0("b7_", i))[sel]
      part
    })
    out <- dplyr::bind_rows(parts)
  }
  missing_vars <- setdiff(c("v005", "v008", "v021"), names(out))
  if (length(missing_vars)) {
    cli::cli_abort("Variable{?s} {.field {missing_vars}} not found in {.arg data}.", call = rlang::caller_env())
  }
  out
}

# Deaths and exposure in each age segment for each birth.
mortality_components <- function(d, months) {
  b3 <- dv(d, "b3")
  b7 <- dv(d, "b7")
  tu <- dv(d, "v008")
  tl <- tu - months
  n <- nrow(d)
  seg <- .mort_segments
  den <- matrix(0, n, 8L)
  num <- matrix(0, n, 8L)
  for (s in 1:8) {
    a <- seg$lower[[s]]
    b <- seg$upper[[s]]
    survived <- is.na(b7) | b7 >= a
    died <- !is.na(b7) & b7 >= a & b7 < b
    coh_a <- !is.na(b3) & b3 >= tl - b & b3 < tl - a
    coh_b <- !is.na(b3) & b3 >= tl - a & b3 < tu - b
    coh_c <- !is.na(b3) & b3 >= tu - b & b3 < tu - a
    den[, s] <- survived * (0.5 * coh_a + coh_b + 0.5 * coh_c)
    num[, s] <- died * (0.5 * coh_a + coh_b + coh_c)
  }
  w <- dv(d, "v005") / 1e6
  in_window <- rowSums(den) > 0 | rowSums(num) > 0
  list(
    psu = as.character(zap_all(d$v021)), w = w, num = num, den = den,
    n_events = as.numeric(rowSums(num) > 0), keep = !is.na(w) & w > 0 & in_window
  )
}

mortality_stat <- function(D, E, rates) {
  q <- D / E
  q[!is.finite(q)] <- 0
  surv <- function(i) prod(1 - q[i])
  all <- c(
    nnmr = 1000 * q[[1]],
    imr = 1000 * (1 - surv(1:4)),
    cmr = 1000 * (1 - surv(5:8)),
    u5mr = 1000 * (1 - surv(1:8))
  )
  all[["pnnmr"]] <- all[["imr"]] - all[["nnmr"]]
  unname(all[rates])
}

# Apply a rate statistic overall or by groups, with jackknife SEs over PSUs.
rate_by_groups <- function(d, by, comp, stat, se, level) {
  keep <- comp$keep
  groups <- if (is.null(by)) {
    list(list(rows = which(keep), values = NULL))
  } else {
    gv <- lapply(by, function(v) as_group_values(d[[v]]))
    names(gv) <- by
    as_key <- function(x) addNA(if (is.factor(x)) x else factor(x, levels = sort(unique(x))), ifany = TRUE)
    key <- do.call(interaction, c(lapply(gv, as_key), list(drop = TRUE, lex.order = TRUE)))
    lapply(levels(key), function(k) {
      rows <- which(keep & key == k)
      first <- which(key == k)[[1]]
      list(rows = rows, values = lapply(gv, function(x) x[first]))
    })
  }
  res <- lapply(groups, function(g) {
    r <- jackknife_rate(comp, g$rows, stat, se, level)
    if (!is.null(g$values)) {
      vals <- lapply(g$values, function(x) rep(x, nrow(r)))
      r <- tibble::as_tibble(c(vals, as.list(r)))
    }
    r
  })
  dplyr::bind_rows(res)
}

# Group labels: value labels for labelled vectors, values otherwise.
as_group_values <- function(x) {
  if (inherits(x, "haven_labelled")) {
    return(haven::as_factor(x, levels = "labels"))
  }
  x
}

jackknife_rate <- function(comp, rows, stat, se, level) {
  w <- comp$w[rows]
  psu <- comp$psu[rows]
  num <- comp$num[rows, , drop = FALSE] * w
  den <- comp$den[rows, , drop = FALSE] * w
  tot_num <- colSums(num)
  tot_den <- colSums(den)
  est <- stat(tot_num, tot_den)
  n_psu <- length(unique(psu))
  se_val <- rep(NA_real_, length(est))
  if (se && n_psu > 1L) {
    psu_num <- rowsum(num, psu, reorder = FALSE)
    psu_den <- rowsum(den, psu, reorder = FALSE)
    reps <- vapply(seq_len(nrow(psu_num)), function(j) {
      stat(tot_num - psu_num[j, ], tot_den - psu_den[j, ])
    }, numeric(length(est)))
    reps <- matrix(reps, nrow = length(est))
    k <- n_psu
    se_val <- sqrt((k - 1) / k * rowSums((reps - est)^2, na.rm = TRUE))
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  tibble::tibble(
    estimate = est, se = se_val,
    ci_low = pmax(0, est - z * se_val), ci_high = est + z * se_val,
    n_events = sum(comp$n_events[rows]), denominator = sum(den),
    n_units = length(rows), n_psu = n_psu,
    n_deaths = sum(rowSums(comp$num[rows, , drop = FALSE]) > 0)
  )
}
