# Comparisons between rounds and between subgroups ----------------------------------

#' Compare indicators between survey rounds and between subgroups
#'
#' Tests differences of indicators between BDHS rounds (for example the
#' change in caesarean deliveries from 2017-18 to 2022) and between groups
#' within a round (for example urban versus rural), with design-based
#' standard errors, confidence intervals and z tests.
#'
#' **Between rounds.** BDHS rounds are independent samples, so the
#' difference \eqn{d = \hat\theta_2 - \hat\theta_1} has
#' \eqn{SE(d) = \sqrt{SE_1^2 + SE_2^2}}; \eqn{z = d / SE(d)} and the p-value
#' is two-sided from the standard normal. The annualized change is
#' \eqn{d / (t_2 - t_1)}, where \eqn{t} is the mid-point of each survey's
#' fieldwork (from [bdhs_surveys]), with standard error \eqn{SE(d) / (t_2 -
#' t_1)}. Rates (fertility, mortality) and medians use their jackknife
#' standard errors.
#'
#' **Between groups within a round.** Groups of the same survey are not
#' independent (they share strata and clusters), so their estimates are
#' computed jointly with [survey::svyby()] (`covmat = TRUE`) and the
#' difference uses [survey::svycontrast()], which accounts for the
#' covariance. Indicators with all-women denominators use the ratio
#' estimator of [bdhs_estimate()] with the all-woman factor of the `by`
#' variable. Within-round contrasts need the microdata; they are not computed
#' from `bdhs_estimate` objects, nor for rates and medians.
#'
#' **Divisions.** Bangladesh had 5 divisions in 1993-94, 6 to 2007, 7 in
#' 2011-2014 and 8 from 2017-18, and the codes of `v024` changed. Comparing
#' `v024` (or `division_surveyed`) across rounds with different layouts
#' compares different areas under the same name; a warning recommends the
#' harmonized `division_trend6` from [bdhs_harmonize()].
#'
#' @param ... Either data frames of BDHS recode data (one per round, or one
#'   pooled data frame with a `survey_id` column, see [bdhs_pool()]), or
#'   `bdhs_estimate` objects from [bdhs_estimate()] (not a mix).
#' @param indicators Indicator ids (see [bdhs_indicator_list()]). Required for
#'   data frames; for `bdhs_estimate` inputs it optionally selects
#'   indicators.
#' @param by Optional single grouping variable. For `bdhs_estimate` inputs it
#'   is detected from the columns.
#' @param level Confidence level of the intervals of the differences.
#' @param rounds Which pairs of rounds to compare: `"consecutive"` (default),
#'   `"first_last"` or `"all"` pairs.
#' @param groups Contrasts between groups within a round: `"reference"`
#'   (every group minus the first group, default) or `"pairwise"`.
#' @param design_args A list of further arguments passed to [bdhs_design()].
#'
#' @return A tibble of class `bdhs_comparison` with one row per comparison
#'   and columns `comparison` (`"rounds"` or `"groups"`), `indicator`,
#'   `label`, `by` (grouping variable or `NA`), `survey_1`, `survey_2`,
#'   `group_1`, `group_2`, `estimate_1`, `estimate_2`, `difference`
#'   (second minus first), `se`, `ci_low`, `ci_high`, `z`, `p_value`,
#'   `years` (between survey mid-points), `annual_change` and
#'   `annual_change_se` (per year; `NA` for group contrasts). Proportions are
#'   on the 0-100 scale, so differences are in percentage points.
#' @seealso [bdhs_estimate()], [bdhs_inequality()], [bdhs_decompose()].
#' @export
#' @examples
#' kr17 <- bdhs_simulate(2017, "KR")
#' kr22 <- bdhs_simulate(2022, "KR")
#' bdhs_compare(kr17, kr22, indicators = "rh_del_csection")
#'
#' # Urban-rural gap within each round and its change between rounds
#' bdhs_compare(kr17, kr22, indicators = "rh_del_csection", by = "v025")
#'
#' # From saved estimates
#' e1 <- bdhs_estimate(kr17, "rh_del_facility")
#' e2 <- bdhs_estimate(kr22, "rh_del_facility")
#' bdhs_compare(e1, e2)
bdhs_compare <- function(..., indicators = NULL, by = NULL, level = 0.95,
                         rounds = c("consecutive", "first_last", "all"),
                         groups = c("reference", "pairwise"), design_args = list()) {
  rounds <- rlang::arg_match(rounds)
  groups <- rlang::arg_match(groups)
  check_level(level)
  inputs <- list(...)
  if (!length(inputs)) {
    cli::cli_abort("Supply data frames or {.cls bdhs_estimate} objects to compare.")
  }
  if (!is.null(by) && (!is.character(by) || length(by) != 1L)) {
    cli::cli_abort("{.arg by} must be a single variable name.")
  }
  is_est <- vapply(inputs, inherits, logical(1), "bdhs_estimate")
  is_df <- vapply(inputs, is.data.frame, logical(1))
  if (!all(is_df)) {
    cli::cli_abort("Every input must be a data frame or a {.cls bdhs_estimate} object.")
  }
  if (any(is_est) && !all(is_est)) {
    cli::cli_abort("Supply either data frames or {.cls bdhs_estimate} objects, not a mix.")
  }

  micro <- NULL
  if (all(is_est)) {
    est <- dplyr::bind_rows(lapply(inputs, function(x) tibble::as_tibble(unclass_estimate(x))))
    est_by <- estimate_by_cols(est)
    if (length(est_by) > 1L) {
      cli::cli_abort("{.fn bdhs_compare} handles estimates with a single {.arg by} variable, not {.field {est_by}}.")
    }
    if (!is.null(by) && !identical(by, est_by)) {
      cli::cli_abort("{.arg by} = {.val {by}} does not match the grouping of the estimates ({.val {est_by}}).")
    }
    by <- if (length(est_by)) est_by else NULL
    if (!is.null(indicators)) {
      indicators <- translate_indicator_ids(indicators)
      est <- est[est$indicator %in% indicators, , drop = FALSE]
    }
  } else {
    if (is.null(indicators)) {
      cli::cli_abort("Supply {.arg indicators} to compare, e.g. {.code indicators = \"rh_del_csection\"}.")
    }
    micro <- inputs
    est <- dplyr::bind_rows(lapply(inputs, function(d) {
      tibble::as_tibble(unclass_estimate(bdhs_estimate(d, indicators, by = by, level = level, design_args = design_args)))
    }))
  }
  if (!nrow(est)) {
    cli::cli_abort("Nothing to compare: no estimates for these indicators.")
  }
  dup <- est[, c("survey_id", "indicator", by), drop = FALSE]
  if (anyDuplicated(dup)) {
    cli::cli_abort("The same round appears more than once in the inputs ({.val {unique(est$survey_id[duplicated(dup)])}}).")
  }
  if (!is.null(by)) warn_division_layout(by, unique(est$survey_id))

  out <- list(compare_rounds(est, by, level, rounds))
  if (!is.null(by) && !is.null(micro)) {
    out <- c(out, lapply(micro, compare_groups_data, indicators = indicators, by = by,
                         level = level, groups = groups, design_args = design_args))
  } else if (!is.null(by)) {
    cli::cli_inform(c("i" = "Group contrasts within a round need the microdata (the covariance of the groups); only between-round differences are computed."))
  }
  res <- dplyr::bind_rows(out)
  if (!nrow(res)) res <- empty_comparison()
  res <- tibble::as_tibble(res)
  attr(res, "level") <- level
  class(res) <- c("bdhs_comparison", class(res))
  res
}

#' @export
print.bdhs_comparison <- function(x, ...) {
  if (!"comparison" %in% names(x)) {
    return(NextMethod())
  }
  n_r <- sum(x$comparison == "rounds")
  n_g <- sum(x$comparison == "groups")
  cat("# BDHS comparisons: ", n_r, " between rounds, ", n_g, " between groups | ",
      format(100 * (attr(x, "level") %||% 0.95)), "% CI\n", sep = "")
  NextMethod()
}

empty_comparison <- function() {
  tibble::tibble(
    comparison = character(), indicator = character(), label = character(), by = character(),
    survey_1 = character(), survey_2 = character(), group_1 = character(), group_2 = character(),
    estimate_1 = numeric(), estimate_2 = numeric(), difference = numeric(), se = numeric(),
    ci_low = numeric(), ci_high = numeric(), z = numeric(), p_value = numeric(),
    years = numeric(), annual_change = numeric(), annual_change_se = numeric()
  )
}

unclass_estimate <- function(x) {
  class(x) <- setdiff(class(x), "bdhs_estimate")
  x
}

.estimate_cols <- c(
  "survey_id", "indicator", "label", "estimate", "se", "ci_low", "ci_high",
  "n_unweighted", "n_weighted", "api_id"
)

estimate_by_cols <- function(est) setdiff(names(est), .estimate_cols)

check_level <- function(level, call = rlang::caller_env()) {
  if (!is.numeric(level) || length(level) != 1L || is.na(level) || level <= 0 || level >= 1) {
    cli::cli_abort("{.arg level} must be a single number between 0 and 1.", call = call)
  }
}

# Difference of two independent estimates (and z test).
independent_difference <- function(e1, e2, se1, se2, level) {
  d <- e2 - e1
  se <- sqrt(se1^2 + se2^2)
  diff_stats(d, se, level)
}

diff_stats <- function(d, se, level) {
  q <- stats::qnorm(1 - (1 - level) / 2)
  z <- d / se
  z[!is.finite(z)] <- NA_real_
  list(difference = d, se = se, ci_low = d - q * se, ci_high = d + q * se,
       z = z, p_value = 2 * stats::pnorm(-abs(z)))
}

# Mid-point of the fieldwork of each survey, in decimal years.
survey_mid_year <- function(survey_id) {
  s <- bdhs::bdhs_surveys
  i <- match(survey_id, s$survey_id)
  start <- s$fieldwork_start[i]
  end <- s$fieldwork_end[i]
  mid <- start + (end - start) / 2
  yr <- as.numeric(format(mid, "%Y"))
  frac <- (as.numeric(format(mid, "%j")) - 0.5) / 365.25
  out <- yr + frac
  fallback <- is.na(out)
  if (any(fallback)) {
    out[fallback] <- suppressWarnings(as.numeric(substr(survey_id[fallback], 3, 6))) + 0.5
  }
  out
}

# Number of divisions in the v024 layout of a round.
division_layout <- function(survey_id) {
  yr <- suppressWarnings(as.numeric(substr(survey_id, 3, 6)))
  ifelse(yr <= 1994, 5L, ifelse(yr <= 2007, 6L, ifelse(yr <= 2014, 7L, 8L)))
}

.division_vars <- c("v024", "hv024", "mv024", "v101", "hv101", "division_surveyed", "region", "division")

warn_division_layout <- function(by, survey_ids, call = rlang::caller_env()) {
  if (!by %in% .division_vars || length(survey_ids) < 2L) {
    return(invisible())
  }
  lay <- division_layout(survey_ids)
  if (length(unique(lay)) > 1L) {
    cli::cli_warn(c(
      "{.field {by}} has different division layouts in these rounds ({paste(sort(unique(lay)), collapse = ', ')} divisions); between-round comparisons by division compare different areas.",
      "i" = "Use the harmonized {.field division_trend6}: {.code bdhs_harmonize(data, vars = \"division_trend6\")}."
    ), call = call)
  }
}

# Pairs of rounds (indices into rounds sorted by time).
round_pairs <- function(n, rounds) {
  if (n < 2L) {
    return(matrix(integer(), ncol = 2))
  }
  switch(rounds,
    consecutive = cbind(seq_len(n - 1L), 2:n),
    first_last = matrix(c(1L, n), ncol = 2),
    all = t(utils::combn(n, 2))
  )
}

compare_rounds <- function(est, by, level, rounds) {
  est$.group <- if (is.null(by)) NA_character_ else as.character(est[[by]])
  keys <- unique(est[, c("indicator", ".group")])
  out <- list()
  for (k in seq_len(nrow(keys))) {
    sub <- est[est$indicator == keys$indicator[k] & (is.na(keys$.group[k]) & is.na(est$.group) |
                                                      est$.group %in% keys$.group[k]), , drop = FALSE]
    sub <- sub[!is.na(sub$estimate), , drop = FALSE]
    if (nrow(sub) < 2L) next
    mid <- survey_mid_year(sub$survey_id)
    sub <- sub[order(mid), , drop = FALSE]
    mid <- sort(mid)
    pr <- round_pairs(nrow(sub), rounds)
    a <- sub[pr[, 1], , drop = FALSE]
    b <- sub[pr[, 2], , drop = FALSE]
    st <- independent_difference(a$estimate, b$estimate, a$se, b$se, level)
    years <- mid[pr[, 2]] - mid[pr[, 1]]
    out[[length(out) + 1L]] <- tibble::tibble(
      comparison = "rounds", indicator = a$indicator, label = a$label,
      by = by %||% NA_character_,
      survey_1 = a$survey_id, survey_2 = b$survey_id,
      group_1 = a$.group, group_2 = b$.group,
      estimate_1 = a$estimate, estimate_2 = b$estimate,
      difference = st$difference, se = st$se, ci_low = st$ci_low, ci_high = st$ci_high,
      z = st$z, p_value = st$p_value, years = years,
      annual_change = st$difference / years, annual_change_se = st$se / years
    )
  }
  dplyr::bind_rows(out)
}

# Group contrasts within each round of one data frame.
compare_groups_data <- function(data, indicators, by, level, groups, design_args) {
  recode <- data_recode(data)
  inds <- resolve_indicators(indicators)
  old <- options(survey.lonely.psu = "adjust")
  on.exit(options(old), add = TRUE)
  out <- list()
  for (g in survey_row_groups(data)) {
    d <- if (length(g$rows) == nrow(data)) data else data[g$rows, , drop = FALSE]
    for (ind in inds) {
      if (!g$survey_id %in% ind$rounds || !recode %in% ind$recode) next
      if (ind$type %in% c("rate", "median")) {
        cli::cli_inform("{.val {ind$id}}: group contrasts within a round are not available for {ind$type}s.")
        next
      }
      gb <- group_estimates(d, ind, g$survey_id, recode, by, design_args)
      if (is.null(gb)) next
      out[[length(out) + 1L]] <- group_contrasts(gb, ind, g$survey_id, by, level, groups)
    }
  }
  dplyr::bind_rows(out)
}

# Design for one indicator in one round: the indicator in `.bdhs_y`, the
# group in `.bdhs_g` and, for all-women indicators, the all-woman factor in
# `.bdhs_aw` (as in bdhs_estimate()). Restricted (as a domain) to records in
# the denominator with a non-missing group. Returns NULL when empty.
indicator_design <- function(d, ind, survey_id, recode, by = NULL, design_args = list(), aw_by = by,
                             extra = list()) {
  y <- suppressWarnings(indicator_values(ind, d, survey_id))
  design_cols <- unique(c(unlist(.design_vars), "survey_id", "d005", "md005", "hv028",
                          "v000", "v007", "hv000", "hv007"))
  wcol <- design_args$weight
  if (is.character(wcol) && length(wcol) == 1L && !wcol %in% c("default", "dv")) design_cols <- c(design_cols, wcol)
  slim <- as.data.frame(d[, intersect(design_cols, names(d)), drop = FALSE])
  slim$.bdhs_y <- y
  if (!is.null(by)) slim$.bdhs_g <- as_group_values(d[[by]])
  for (nm in names(extra)) slim[[nm]] <- extra[[nm]]
  aw <- !is.null(ind$all_women)
  if (aw) {
    aw_var <- if (isTRUE(ind$params$aw_total)) "awfactt" else awfact_for_by(aw_by)
    if (!aw_var %in% names(d)) aw_var <- if ("awfactt" %in% names(d)) "awfactt" else NULL
    slim$.bdhs_aw <- if (is.null(aw_var)) 1 else dv(d, aw_var) / 100
  }
  attr(slim, "bdhs_survey_id") <- survey_id
  attr(slim, "bdhs_recode") <- recode
  des <- do.call(bdhs_design, c(list(slim, survey = survey_id, recode = recode), design_args))
  v <- des$variables
  keep <- !is.na(v$.bdhs_y)
  if (!is.null(by)) keep <- keep & !is.na(v$.bdhs_g)
  if (!any(keep)) {
    return(NULL)
  }
  des <- des[keep, ]
  if (!is.null(by) && is.factor(des$variables$.bdhs_g)) {
    des$variables$.bdhs_g <- droplevels(des$variables$.bdhs_g)
  }
  attr(des, "bdhs_all_women") <- aw
  des
}

# Joint group estimates (svyby with covariance).
group_estimates <- function(d, ind, survey_id, recode, by, design_args) {
  des <- indicator_design(d, ind, survey_id, recode, by, design_args)
  if (is.null(des)) {
    return(NULL)
  }
  aw <- isTRUE(attr(des, "bdhs_all_women"))
  if (length(unique(des$variables$.bdhs_g)) < 2L) {
    return(NULL)
  }
  r <- if (aw) {
    survey::svyby(~.bdhs_y, ~.bdhs_g, des, survey::svyratio, denominator = ~.bdhs_aw,
                  covmat = TRUE, drop.empty.groups = TRUE)
  } else {
    survey::svyby(~.bdhs_y, ~.bdhs_g, des, survey::svymean, covmat = TRUE, drop.empty.groups = TRUE)
  }
  r
}

group_contrasts <- function(r, ind, survey_id, by, level, groups) {
  lev <- as.character(as.data.frame(r)$.bdhs_g)
  cf <- stats::coef(r)
  names(cf) <- lev
  V <- stats::vcov(r)
  scale <- if (ind$type == "proportion") 100 else 1
  n <- length(lev)
  pairs <- if (groups == "reference") cbind(1L, seq_len(n)[-1]) else t(utils::combn(n, 2))
  out <- lapply(seq_len(nrow(pairs)), function(k) {
    i <- pairs[k, 1]
    j <- pairs[k, 2]
    w <- numeric(n)
    w[i] <- -1
    w[j] <- 1
    con <- survey::svycontrast(r, w)
    d <- as.numeric(stats::coef(con)) * scale
    se <- as.numeric(survey::SE(con)) * scale
    st <- diff_stats(d, se, level)
    tibble::tibble(
      comparison = "groups", indicator = ind$id, label = ind$label, by = by,
      survey_1 = survey_id, survey_2 = survey_id, group_1 = lev[i], group_2 = lev[j],
      estimate_1 = cf[[i]] * scale, estimate_2 = cf[[j]] * scale,
      difference = st$difference, se = st$se, ci_low = st$ci_low, ci_high = st$ci_high,
      z = st$z, p_value = st$p_value, years = NA_real_, annual_change = NA_real_,
      annual_change_se = NA_real_
    )
  })
  dplyr::bind_rows(out)
}
