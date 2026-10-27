# Design-based estimation of indicators ----------------------------------------------

#' Estimate indicators with correct survey standard errors
#'
#' Computes indicators from the registry ([bdhs_indicator_list()]) for one
#' or several BDHS rounds, nationally or by subgroups, using the complex
#' survey design of the data ([bdhs_design()]): clusters, sampling-error
#' strata and sample weights. Standard errors use Taylor linearisation
#' (proportions, means, all-women ratios) or the DHS jackknife (medians,
#' fertility and mortality rates).
#'
#' * Proportions and means are design-based means over the indicator's
#'   denominator population (domain estimation, so standard errors account
#'   for the full design).
#' * Indicators whose denominator is *all* women (for example child marriage
#'   or teenage childbearing) use the ratio estimator
#'   \eqn{\sum w y / \sum w \, awfact / 100}, because only ever-married women
#'   were interviewed. The all-woman factor matching a single `by` variable is
#'   used (`awfactu` for urban/rural, `awfactr` for division, `awfacte` for
#'   education, `awfactw` for wealth); other groupings use `awfactt`.
#' * Medians (age at first marriage) use the DHS interpolation on completed
#'   ages and jackknife standard errors.
#' * Rates call [bdhs_fertility()] and [bdhs_mortality()].
#'
#' Singleton strata are handled with `options(survey.lonely.psu = "adjust")`,
#' set only while the function runs.
#'
#' @param data A BDHS recode data frame (from `bdhs_read()`,
#'   [bdhs_simulate()] or pooled rounds with a `survey_id` column) with DHS
#'   variable names.
#' @param indicators Character vector of indicator ids (see
#'   [bdhs_indicator_list()]). `NULL` estimates every indicator available for
#'   the recode and round.
#' @param by Optional character vector of grouping variables. Labelled
#'   variables are shown with their value labels.
#' @param survey Optional survey reference, overriding detection.
#' @param level Confidence level.
#' @param scale Multiplier for proportions (100 gives percentages, as in DHS
#'   reports). Means, medians and rates are never rescaled.
#' @param ci_method `"logit"` (default) computes confidence intervals for
#'   proportions on the logit scale (as [survey::svyciprop()]), which keeps
#'   them inside 0-100; `"wald"` uses estimate +/- t * SE. Means, medians and
#'   rates always use Wald intervals.
#' @param design_args A list of further arguments passed to [bdhs_design()],
#'   e.g. `list(strata = "design")` or `list(weight = "dv")`.
#'
#' @return A tibble of class `bdhs_estimate` with columns `survey_id`,
#'   `indicator`, `label`, the `by` variables, `estimate`, `se`, `ci_low`,
#'   `ci_high`, `n_unweighted` (records in the denominator), `n_weighted`
#'   (weighted denominator; for all-women indicators the estimated number of
#'   all women) and `api_id` (DHS Program API indicator id for comparison
#'   with [bdhs_indicators]).
#' @seealso [bdhs_validate()] to compare with the official values.
#' @export
#' @examples
#' kr <- bdhs_simulate(2017, "KR")
#' bdhs_estimate(kr, c("rh_del_facility", "rh_del_csection"))
#' bdhs_estimate(kr, "rh_del_facility", by = "v025")
#'
#' ir <- bdhs_simulate(2017, "IR")
#' bdhs_estimate(ir, c("fp_cpr_modern", "ms_married_by_18"), by = "v025")
bdhs_estimate <- function(data, indicators, by = NULL, survey = NULL, level = 0.95, scale = 100,
                          ci_method = c("logit", "wald"), design_args = list()) {
  ci_method <- rlang::arg_match(ci_method)
  check_by(data, by)
  by <- setdiff(by, "survey_id")
  if (!length(by)) by <- NULL
  if (!is.list(design_args)) {
    cli::cli_abort("{.arg design_args} must be a list of arguments for {.fn bdhs_design}.")
  }
  recode <- data_recode(data)
  explicit <- !missing(indicators) && !is.null(indicators)
  if (!explicit) indicators <- applicable_indicators(recode)
  inds <- resolve_indicators(indicators)
  wrong <- !vapply(inds, function(x) recode %in% x$recode, logical(1))
  if (any(wrong)) {
    abort_wrong_recode(inds, wrong, recode)
  }
  old <- options(survey.lonely.psu = "adjust")
  on.exit(options(old), add = TRUE)

  out <- list()
  for (g in survey_row_groups(data, survey)) {
    ok <- vapply(inds, function(x) g$survey_id %in% x$rounds, logical(1))
    if (any(!ok) && explicit) {
      skipped <- names(inds)[!ok]
      cli::cli_warn("{.val {g$survey_id}}: skipping {cli::qty(length(skipped))}indicator{?s} not available in this round: {.val {skipped}}.")
    }
    if (!any(ok)) next
    d <- if (length(g$rows) == nrow(data)) data else data[g$rows, , drop = FALSE]
    out[[length(out) + 1L]] <- estimate_survey(
      d, inds[ok], g$survey_id, recode, by, level, scale, ci_method, design_args
    )
  }
  res <- if (length(out)) dplyr::bind_rows(out) else empty_estimate(by)
  by_labels <- vapply(by, function(b) var_label(data[[b]]), character(1))
  new_bdhs_estimate(res, level = level, scale = scale, by_labels = by_labels)
}

new_bdhs_estimate <- function(x, level = 0.95, scale = 100, by_labels = NULL) {
  x <- tibble::as_tibble(x)
  attr(x, "level") <- level
  attr(x, "scale") <- scale
  # Variable labels of the `by` columns (named by column), used by plot().
  attr(x, "by_labels") <- by_labels
  class(x) <- c("bdhs_estimate", setdiff(class(x), "bdhs_estimate"))
  x
}

empty_estimate <- function(by) {
  cols <- list(survey_id = character(), indicator = character(), label = character())
  for (b in by) cols[[b]] <- character()
  tibble::as_tibble(c(cols, list(
    estimate = numeric(), se = numeric(), ci_low = numeric(), ci_high = numeric(),
    n_unweighted = integer(), n_weighted = numeric(), api_id = character()
  )))
}

#' @export
print.bdhs_estimate <- function(x, ...) {
  cat(estimate_header(x), "\n", sep = "")
  NextMethod()
}

estimate_header <- function(x) {
  n_ind <- length(unique(x$indicator))
  surveys <- paste(unique(x$survey_id), collapse = ", ")
  paste0("# BDHS estimates: ", n_ind, " indicator", if (n_ind != 1) "s", " | ", surveys, " | ",
         format(100 * (attr(x, "level") %||% 0.95)), "% CI")
}

# Estimates for one survey round.
estimate_survey <- function(d, inds, survey_id, recode, by, level, scale, ci_method, design_args) {
  is_rate <- vapply(inds, function(x) x$type == "rate", logical(1))
  res <- list()

  if (any(!is_rate)) {
    plain <- inds[!is_rate]
    ycols <- lapply(plain, indicator_values, d = d, survey_id = survey_id)
    ynames <- paste0(".bdhs_y", seq_along(plain))
    byn <- if (is.null(by)) NULL else paste0(".bdhs_by_", seq_along(by))
    aw_var <- awfact_for_by(by)
    aw_needed <- any(vapply(plain, function(x) !is.null(x$all_women), logical(1)))

    design_cols <- unique(c(unlist(.design_vars), "survey_id", "d005", "v000", "v007", "hv000", "hv007"))
    wcol <- design_args$weight
    if (is.character(wcol) && length(wcol) == 1L && !wcol %in% c("default", "dv")) design_cols <- c(design_cols, wcol)
    slim <- as.data.frame(d[, intersect(design_cols, names(d)), drop = FALSE])
    for (i in seq_along(plain)) slim[[ynames[i]]] <- ycols[[i]]
    for (i in seq_along(by)) slim[[byn[i]]] <- as_group_values(d[[by[i]]])
    if (aw_needed) {
      if (!aw_var %in% names(d)) {
        if ("awfactt" %in% names(d)) {
          cli::cli_warn("All-woman factor {.field {aw_var}} not found; using {.field awfactt}.")
          aw_var <- "awfactt"
        } else {
          cli::cli_warn("No all-woman factor in the data: all-women indicators are computed for interviewed women only.")
          aw_var <- NULL
        }
      }
      slim$.bdhs_aw <- if (is.null(aw_var)) 1 else dv(d, aw_var) / 100
    }
    attr(slim, "bdhs_survey_id") <- survey_id
    attr(slim, "bdhs_recode") <- recode
    des <- do.call(bdhs_design, c(list(slim, survey = survey_id, recode = recode), design_args))
    q <- stats::qt(1 - (1 - level) / 2, df = max(1, survey::degf(des)))

    for (i in seq_along(plain)) {
      ind <- plain[[i]]
      quiet <- !is.null(attr(ycols[[i]], "bdhs_missing"))
      est <- estimate_plain(des, ind, ynames[i], byn, q, ci_method, scale, quiet)
      if (!is.null(by)) names(est)[match(byn, names(est))] <- by
      res[[ind$id]] <- add_indicator_cols(est, ind, survey_id, by)
    }
  }
  for (ind in inds[is_rate]) {
    est <- ind$rate(d, by, level, survey_id)
    res[[ind$id]] <- add_indicator_cols(est, ind, survey_id, by)
  }
  dplyr::bind_rows(res[names(inds)])
}

add_indicator_cols <- function(est, ind, survey_id, by) {
  est <- tibble::as_tibble(est)
  n <- nrow(est)
  front <- tibble::tibble(survey_id = rep(survey_id, n), indicator = rep(ind$id, n), label = rep(ind$label, n))
  api <- if (all(is.na(ind$api_id))) NA_character_ else paste(ind$api_id, collapse = "+")
  out <- dplyr::bind_cols(front, est[, c(by, "estimate", "se", "ci_low", "ci_high", "n_unweighted", "n_weighted"), drop = FALSE])
  out$n_unweighted <- as.integer(out$n_unweighted)
  out$api_id <- rep(api, n)
  out
}

# Proportion, mean, all-women ratio or median for one indicator.
estimate_plain <- function(des, ind, yname, byn, q, ci_method, scale, quiet = FALSE) {
  vars <- des$variables
  keep <- !is.na(vars[[yname]])
  if (!any(keep)) {
    if (!quiet) cli::cli_warn("{.val {ind$id}}: no records in the denominator.")
    return(na_estimate(vars, byn))
  }
  sub <- des[keep, ]
  sv <- sub$variables
  aw <- !is.null(ind$all_women) && ".bdhs_aw" %in% names(sv)
  counts <- group_counts(sv, byn, aw)

  if (ind$type == "median") {
    est <- median_estimate(sv, yname, byn, aw, ind$params$min_age)
  } else {
    f <- stats::as.formula(paste0("~", yname))
    if (is.null(byn)) {
      r <- if (aw) survey::svyratio(f, ~.bdhs_aw, sub) else survey::svymean(f, sub)
      est <- data.frame(estimate = as.numeric(stats::coef(r)), se = as.numeric(survey::SE(r)))
    } else {
      byf <- stats::as.formula(paste0("~", paste(byn, collapse = " + ")))
      r <- if (aw) {
        survey::svyby(f, byf, sub, survey::svyratio, denominator = ~.bdhs_aw, keep.var = TRUE, drop.empty.groups = TRUE)
      } else {
        survey::svyby(f, byf, sub, survey::svymean, keep.var = TRUE, drop.empty.groups = TRUE)
      }
      est <- as.data.frame(r)[, byn, drop = FALSE]
      est$estimate <- as.numeric(stats::coef(r))
      est$se <- as.numeric(survey::SE(r))
    }
  }
  is_prop <- ind$type == "proportion"
  ci <- ci_bounds(est$estimate, est$se, q, if (is_prop) ci_method else "wald")
  est$ci_low <- ci$low
  est$ci_high <- ci$high
  if (is_prop) {
    for (v in c("estimate", "se", "ci_low", "ci_high")) est[[v]] <- est[[v]] * scale
  }
  if (is.null(byn)) {
    est$n_unweighted <- counts$n_unweighted
    est$n_weighted <- counts$n_weighted
  } else {
    est <- dplyr::left_join(est, counts, by = byn)
  }
  est
}

na_estimate <- function(vars, byn) {
  base <- data.frame(estimate = NA_real_, se = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
                     n_unweighted = 0L, n_weighted = 0)
  if (is.null(byn)) {
    return(base)
  }
  base[0, , drop = FALSE]
}

group_counts <- function(sv, byn, aw) {
  w <- sv$.bdhs_wt
  if (aw) w <- w * sv$.bdhs_aw
  if (is.null(byn)) {
    return(data.frame(n_unweighted = nrow(sv), n_weighted = sum(w)))
  }
  df <- sv[, byn, drop = FALSE]
  df$.w <- w
  df <- dplyr::summarise(dplyr::group_by(df, dplyr::across(dplyr::all_of(byn))),
                         n_unweighted = dplyr::n(), n_weighted = sum(.data$.w), .groups = "drop")
  as.data.frame(df)
}

ci_bounds <- function(est, se, q, method) {
  low <- est - q * se
  high <- est + q * se
  if (method == "logit") {
    ok <- !is.na(est) & !is.na(se) & est > 0 & est < 1
    lg <- stats::qlogis(est[ok])
    s <- se[ok] / (est[ok] * (1 - est[ok]))
    low[ok] <- stats::plogis(lg - q * s)
    high[ok] <- stats::plogis(lg + q * s)
  }
  list(low = low, high = high)
}

# DHS median (e.g. age at first marriage) with jackknife SE over PSUs.
# P(a) = share married before exact age a among all (or interviewed) women;
# median = a + (0.5 - P(a)) / (P(a + 1) - P(a)) for the first a with
# P(a + 1) >= 0.5; not computed when P(min_age) < 0.5.
median_estimate <- function(sv, yname, byn, aw, min_age) {
  y <- sv[[yname]]
  w <- sv$.bdhs_wt
  den_w <- if (aw) w * sv$.bdhs_aw else w
  psu <- as.character(sv$.bdhs_psu)
  ages <- 1:80
  num <- outer(y, ages, `<`) * w
  stat <- function(n, d) dhs_median(n / d, ages, min_age)
  one <- function(rows) {
    tn <- colSums(num[rows, , drop = FALSE])
    td <- sum(den_w[rows])
    est <- stat(tn, td)
    p <- psu[rows]
    k <- length(unique(p))
    se <- NA_real_
    if (k > 1L && !is.na(est)) {
      pn <- rowsum(num[rows, , drop = FALSE], p, reorder = FALSE)
      pd <- rowsum(den_w[rows], p, reorder = FALSE)
      reps <- vapply(seq_len(nrow(pn)), function(j) stat(tn - pn[j, ], td - pd[j, 1]), numeric(1))
      se <- sqrt((k - 1) / k * sum((reps - est)^2, na.rm = TRUE))
    }
    c(estimate = est, se = se)
  }
  if (is.null(byn)) {
    r <- one(seq_len(nrow(sv)))
    return(data.frame(estimate = r[["estimate"]], se = r[["se"]]))
  }
  key <- interaction(sv[, byn, drop = FALSE], drop = TRUE, lex.order = TRUE)
  groups <- split(seq_len(nrow(sv)), key)
  vals <- lapply(groups, function(rows) {
    r <- one(rows)
    first <- sv[rows[[1]], byn, drop = FALSE]
    cbind(first, data.frame(estimate = r[["estimate"]], se = r[["se"]]))
  })
  out <- do.call(rbind, vals)
  rownames(out) <- NULL
  out
}

# p[i] = proportion with event before exact age ages[i].
dhs_median <- function(p, ages, min_age) {
  if (!is.null(min_age) && p[match(min_age, ages)] < 0.5) {
    return(NA_real_)
  }
  i <- which(p >= 0.5)[1]
  if (is.na(i)) {
    return(NA_real_)
  }
  lower <- if (i > 1) p[i - 1] else 0
  # p[i] refers to exact age ages[i], so the median lies in [ages[i] - 1, ages[i]).
  (ages[i] - 1) + (0.5 - lower) / (p[i] - lower)
}
