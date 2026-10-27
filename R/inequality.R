# Socioeconomic inequality measures --------------------------------------------------

.inequality_measures <- c("difference", "ratio", "sii", "rii", "ci", "eci")

.inequality_labels <- c(
  difference = "Difference (highest - lowest)",
  ratio = "Ratio (highest / lowest)",
  sii = "Slope index of inequality",
  rii = "Relative index of inequality",
  ci = "Concentration index",
  eci = "Erreygers concentration index"
)

#' Socioeconomic inequality in an indicator
#'
#' Computes the standard summary measures of inequality of an indicator
#' across ordered socioeconomic groups (by default household wealth
#' quintiles), with design-based standard errors and confidence intervals,
#' for every round in the data.
#'
#' Let the groups be ordered from poorest (1) to richest (\eqn{K}), with
#' weighted population shares \eqn{p_k} in the indicator's denominator
#' population, and let \eqn{R_k = \sum_{j<k} p_j + p_k / 2} be the weighted
#' ridit (fractional rank) of group \eqn{k}, so that \eqn{R} runs from 0
#' (poorest) to 1 (richest).
#'
#' * `difference`: \eqn{\mu_K - \mu_1} (richest minus poorest group), and
#'   `ratio`: \eqn{\mu_K / \mu_1}. The group means are estimated jointly
#'   ([survey::svyby()] with `covmat = TRUE`) and the measures with
#'   [survey::svycontrast()] (delta method; the ratio's interval is computed
#'   on the log scale).
#' * `sii`, the slope index of inequality: the slope \eqn{\beta} of the
#'   design-based linear regression \eqn{E(y) = \alpha + \beta R} of the
#'   0/1 outcome on the ridit ([survey::svyglm()]); the absolute difference
#'   between the top and the bottom of the socioeconomic scale (Barros and
#'   Victora 2013).
#' * `rii`, the relative index of inequality: \eqn{\exp(\beta)} from the
#'   log-link (quasi-Poisson) regression \eqn{\log E(y) = \alpha + \beta R},
#'   the ratio of the predicted values at the top and the bottom of the scale
#'   (Barros and Victora 2013).
#' * `ci`, the concentration index, in its covariance ("convenient
#'   regression") form with design weights, \eqn{C = 2\,\mathrm{cov}_w(y,
#'   R) / \mu} (Kakwani, Wagstaff and van Doorslaer 1997; Wagstaff, Paci and
#'   van Doorslaer 1991). It is written as a smooth function of survey totals,
#'   \eqn{C = 2 T_{yR} / T_y - 2 T_R / N}, and its standard error is
#'   obtained by linearization with [survey::svycontrast()].
#' * `eci`, Erreygers' corrected concentration index for a bounded outcome
#'   in \eqn{[a, b]}: \eqn{E = 4 \mu C / (b - a) = 8\,\mathrm{cov}_w(y, R)}
#'   for a binary outcome (Erreygers 2009); only for proportions.
#'
#' Positive values of `difference`, `sii`, `ci` and `eci` (and `ratio`,
#' `rii` above 1) mean that the indicator is higher among the richer. Ridits
#' are treated as fixed (conditional on the observed group shares), as is
#' usual. Proportions are on the 0-100 scale, so `difference` and `sii` are
#' in percentage points; the other measures are scale-free.
#'
#' Indicators with all-women denominators (ratio estimators) get the
#' `difference` and `ratio` only. Lonely PSUs are handled with
#' `options(survey.lonely.psu = "adjust")`, set while the function runs.
#'
#' @param data A BDHS recode data frame (one round or pooled rounds).
#' @param indicator A single indicator id (see [bdhs_indicator_list()]); a
#'   proportion or a mean.
#' @param by Ordered grouping variable, lowest group first. `NULL` (default)
#'   uses the harmonized `wealth_quintile` if present, else `v190` (`hv270`
#'   in household files). Labelled variables are ordered by their codes and
#'   factors by their levels.
#' @param measures Measures to compute, any of `"difference"`, `"ratio"`,
#'   `"sii"`, `"rii"`, `"ci"` and `"eci"`.
#' @param level Confidence level.
#' @param survey Optional survey reference, overriding detection.
#' @param design_args A list of further arguments passed to [bdhs_design()].
#'
#' @return A tibble of class `bdhs_inequality` with columns `survey_id`,
#'   `indicator`, `label`, `by`, `measure`, `measure_label`, `estimate`,
#'   `se`, `ci_low`, `ci_high`, `lowest`, `highest` (labels of the extreme
#'   groups) and `n_groups`.
#' @references
#' Barros AJD, Victora CG (2013). Measuring coverage in MNCH: determining and
#' interpreting inequalities in coverage of maternal, newborn, and child
#' health interventions. *PLoS Medicine* 10(5): e1001390.
#'
#' Erreygers G (2009). Correcting the concentration index. *Journal of Health
#' Economics* 28(2): 504-515.
#'
#' Kakwani N, Wagstaff A, van Doorslaer E (1997). Socioeconomic inequalities
#' in health: measurement, computation, and statistical inference. *Journal
#' of Econometrics* 77(1): 87-103.
#'
#' Wagstaff A, Paci P, van Doorslaer E (1991). On the measurement of
#' inequalities in health. *Social Science & Medicine* 33(5): 545-557.
#' @seealso [bdhs_inequality_official()] for the same gap measures from the
#'   official estimates; [bdhs_compare()].
#' @export
#' @examples
#' kr <- bdhs_simulate(2022, "KR", n_clusters = 40)
#' bdhs_inequality(kr, "rh_del_csection")
#' bdhs_inequality(kr, "rh_del_facility", by = "v106", measures = c("difference", "sii"))
bdhs_inequality <- function(data, indicator, by = NULL, measures = .inequality_measures,
                            level = 0.95, survey = NULL, design_args = list()) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame of BDHS recode data, not {.obj_type_friendly {data}}.")
  }
  check_level(level)
  measures <- rlang::arg_match(measures, .inequality_measures, multiple = TRUE)
  if (!is.character(indicator) || length(indicator) != 1L) {
    cli::cli_abort("{.arg indicator} must be a single indicator id.")
  }
  ind <- resolve_indicators(indicator)[[1]]
  if (!ind$type %in% c("proportion", "mean")) {
    cli::cli_abort("Inequality measures need a proportion or a mean; {.val {ind$id}} is a {ind$type}.")
  }
  recode <- data_recode(data)
  if (!recode %in% ind$recode) abort_wrong_recode(list(ind), TRUE, recode)
  by <- by %||% first_present(data, c("wealth_quintile", "v190", "hv270"))
  if (is.null(by)) {
    cli::cli_abort(c(
      "No wealth quintile variable ({.field wealth_quintile}, {.field v190} or {.field hv270}) in {.arg data}.",
      "i" = "Supply an ordered grouping variable with {.arg by}."
    ))
  }
  check_by(data, by)
  old <- options(survey.lonely.psu = "adjust")
  on.exit(options(old), add = TRUE)

  out <- list()
  for (g in survey_row_groups(data, survey)) {
    if (!g$survey_id %in% ind$rounds) {
      cli::cli_warn("{.val {ind$id}} is not available for {.val {g$survey_id}}; skipped.")
      next
    }
    d <- if (length(g$rows) == nrow(data)) data else data[g$rows, , drop = FALSE]
    out[[length(out) + 1L]] <- inequality_round(d, ind, g$survey_id, recode, by, measures, level, design_args)
  }
  res <- dplyr::bind_rows(out)
  attr(res, "level") <- level
  class(res) <- c("bdhs_inequality", class(res))
  res
}

#' @export
print.bdhs_inequality <- function(x, ...) {
  if (all(c("indicator", "by", "survey_id") %in% names(x))) cat("# BDHS inequality: ", paste(unique(x$indicator), collapse = ", "), " by ",
      paste(unique(x$by), collapse = ", "), " | ", paste(unique(x$survey_id), collapse = ", "), "\n", sep = "")
  NextMethod()
}

inequality_round <- function(d, ind, survey_id, recode, by, measures, level, design_args) {
  ord <- as_codes(d[[by]])
  des <- indicator_design(d, ind, survey_id, recode, by, design_args, extra = list(.bdhs_ord = ord))
  if (is.null(des)) {
    cli::cli_warn("{.val {ind$id}}: no records in the denominator in {.val {survey_id}}.")
    return(NULL)
  }
  aw <- isTRUE(attr(des, "bdhs_all_women"))
  v <- des$variables
  w <- stats::weights(des)
  scale <- if (ind$type == "proportion") 100 else 1
  q <- stats::qt(1 - (1 - level) / 2, df = max(1, survey::degf(des)))

  # Group order and labels, poorest first.
  lab <- as.character(v$.bdhs_g)
  labs_u <- unique(lab)
  tab <- data.frame(
    label = labs_u,
    w = vapply(labs_u, function(l) sum(w[lab == l]), numeric(1)),
    ord = vapply(labs_u, function(l) stats::median(v$.bdhs_ord[lab == l], na.rm = TRUE), numeric(1))
  )
  tab <- tab[order(tab$ord), , drop = FALSE]
  if (nrow(tab) < 2L) {
    cli::cli_warn("{.val {ind$id}}: fewer than two groups of {.field {by}} in {.val {survey_id}}.")
    return(NULL)
  }
  p <- tab$w / sum(tab$w)
  tab$ridit <- cumsum(p) - p / 2
  lowest <- tab$label[1]
  highest <- tab$label[nrow(tab)]
  des$variables$.bdhs_ridit <- tab$ridit[match(as.character(v$.bdhs_g), tab$label)]

  row <- function(measure, est, se, low, high) {
    tibble::tibble(
      survey_id = survey_id, indicator = ind$id, label = ind$label, by = by,
      measure = measure, measure_label = unname(.inequality_labels[measure]),
      estimate = est, se = se, ci_low = low, ci_high = high,
      lowest = lowest, highest = highest, n_groups = nrow(tab)
    )
  }
  rows <- list()

  if (any(c("difference", "ratio") %in% measures)) {
    r <- if (aw) {
      survey::svyby(~.bdhs_y, ~.bdhs_g, des, survey::svyratio, denominator = ~.bdhs_aw,
                    covmat = TRUE, drop.empty.groups = TRUE)
    } else {
      survey::svyby(~.bdhs_y, ~.bdhs_g, des, survey::svymean, covmat = TRUE, drop.empty.groups = TRUE)
    }
    labs <- as.character(as.data.frame(r)$.bdhs_g)
    w_diff <- (labs == highest) - (labs == lowest)
    if ("difference" %in% measures) {
      con <- survey::svycontrast(r, w_diff)
      e <- as.numeric(stats::coef(con)) * scale
      s <- as.numeric(survey::SE(con)) * scale
      rows$difference <- row("difference", e, s, e - q * s, e + q * s)
    }
    if ("ratio" %in% measures) {
      cf <- stats::coef(r)
      V <- stats::vcov(r)
      hi <- which(labs == highest)
      lo <- which(labs == lowest)
      e <- cf[[hi]] / cf[[lo]]
      grad <- c(-cf[[hi]] / cf[[lo]]^2, 1 / cf[[lo]])
      s <- sqrt(as.numeric(t(grad) %*% V[c(lo, hi), c(lo, hi)] %*% grad))
      if (!is.finite(e)) s <- NA_real_
      ci <- if (is.finite(e) && e > 0) exp(log(e) + c(-1, 1) * q * s / e) else c(NA_real_, NA_real_)
      rows$ratio <- row("ratio", e, s, ci[1], ci[2])
    }
  }

  reg <- intersect(measures, c("sii", "rii", "ci", "eci"))
  if (length(reg) && aw) {
    cli::cli_inform("{.val {ind$id}} has an all-women denominator: only {.val difference} and {.val ratio} are computed.")
    reg <- character()
  }
  if ("sii" %in% reg) {
    fit <- survey::svyglm(.bdhs_y ~ .bdhs_ridit, design = des, family = stats::gaussian())
    b <- stats::coef(fit)[[".bdhs_ridit"]] * scale
    s <- sqrt(stats::vcov(fit)[".bdhs_ridit", ".bdhs_ridit"]) * scale
    rows$sii <- row("sii", b, s, b - q * s, b + q * s)
  }
  if ("rii" %in% reg) {
    fit <- tryCatch(
      suppressWarnings(survey::svyglm(.bdhs_y ~ .bdhs_ridit, design = des, family = stats::quasipoisson(link = "log"))),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      rows$rii <- row("rii", NA_real_, NA_real_, NA_real_, NA_real_)
    } else {
      b <- stats::coef(fit)[[".bdhs_ridit"]]
      s <- sqrt(stats::vcov(fit)[".bdhs_ridit", ".bdhs_ridit"])
      rows$rii <- row("rii", exp(b), exp(b) * s, exp(b - q * s), exp(b + q * s))
    }
  }
  if (any(c("ci", "eci") %in% reg)) {
    des$variables$.ci_yr <- des$variables$.bdhs_y * des$variables$.bdhs_ridit
    des$variables$.ci_one <- 1
    tot <- survey::svytotal(~ .bdhs_y + .ci_yr + .bdhs_ridit + .ci_one, des)
    if ("ci" %in% reg) {
      con <- survey::svycontrast(tot, quote(2 * .ci_yr / .bdhs_y - 2 * .bdhs_ridit / .ci_one))
      e <- as.numeric(stats::coef(con))
      s <- as.numeric(survey::SE(con))
      rows$ci <- row("ci", e, s, e - q * s, e + q * s)
    }
    if ("eci" %in% reg) {
      if (ind$type != "proportion") {
        cli::cli_inform("{.val eci} (Erreygers) needs a bounded binary outcome; skipped for {.val {ind$id}}.")
      } else {
        con <- survey::svycontrast(tot, quote(8 * (.ci_yr / .ci_one - .bdhs_y * .bdhs_ridit / .ci_one^2)))
        e <- as.numeric(stats::coef(con))
        s <- as.numeric(survey::SE(con))
        rows$eci <- row("eci", e, s, e - q * s, e + q * s)
      }
    }
  }
  dplyr::bind_rows(rows[intersect(measures, names(rows))])
}

#' Inequality gaps from the official estimates
#'
#' Computes the inequality measures of [bdhs_inequality()] from the official
#' DHS estimates by wealth quintile ([bdhs_official()]), for every round
#' with such estimates. These are point estimates only (the official
#' tables carry no covariances). The measures follow from the group values
#' \eqn{\mu_k} and the weighted denominators \eqn{p_k} of the five
#' quintiles: `difference` \eqn{\mu_5 - \mu_1}, `ratio` \eqn{\mu_5 / \mu_1},
#' `sii` and `rii` from the weighted (least squares and log-link) regression
#' of \eqn{\mu_k} on the ridit \eqn{R_k}, which equals the individual-level
#' regression because the ridit is constant within groups, `ci`
#' \eqn{= 2 \sum_k p_k \mu_k R_k / \mu - 1} and `eci` \eqn{= 4 \mu C} (for
#' percentages, with \eqn{\mu} as a proportion). Official values are
#' rounded, so the measures are approximate.
#'
#' @param indicator A DHS indicator id (e.g. `"RH_DELA_C_CSC"`) or a bdhs
#'   indicator id with a DHS equivalent (e.g. `"rh_del_csection"`).
#' @param survey Optional survey references; `NULL` uses every round.
#' @param measures Measures to compute (see [bdhs_inequality()]).
#' @param period Reference period of the official values, as in the
#'   `by_variable_label` column of `bdhs_official(preferred = FALSE)` (e.g.
#'   `"Three years preceding the survey"`). `NULL` (default) uses the period
#'   that bdhs computes for a bdhs indicator id (so the result is comparable
#'   with [bdhs_inequality()] on the microdata), and the DHS headline
#'   (preferred) value for a DHS id.
#'
#' @return A tibble of class `bdhs_inequality` with columns `survey_id`,
#'   `indicator`, `label`, `by` (`"Wealth quintile"`), `measure`,
#'   `measure_label`, `estimate`, `lowest`, `highest` and `n_groups`;
#'   `se`, `ci_low` and `ci_high` are `NA`.
#' @inherit bdhs_inequality references
#' @export
#' @examples
#' bdhs_inequality_official("RH_DELA_C_CSC")
#' bdhs_inequality_official("rh_del_facility", survey = c(2004, 2022),
#'                          measures = c("difference", "ratio"))
bdhs_inequality_official <- function(indicator, survey = NULL, measures = .inequality_measures, period = NULL) {
  measures <- rlang::arg_match(measures, .inequality_measures, multiple = TRUE)
  if (!is.character(indicator) || length(indicator) != 1L) {
    cli::cli_abort("{.arg indicator} must be a single indicator id.")
  }
  api <- official_id(indicator)
  reg <- indicator_registry()
  ind <- if (!indicator %in% bdhs::bdhs_indicators$indicator_id && tolower(indicator) %in% names(reg)) reg[[tolower(indicator)]]
  all_rows <- bdhs_official(api, survey = survey, characteristic = "Wealth quintile", preferred = FALSE)
  all_rows <- all_rows[all_rows$characteristic_category == "Wealth quintile", , drop = FALSE]
  pref <- bdhs_official(api, survey = survey, characteristic = "Wealth quintile")
  pref <- pref[pref$characteristic_category == "Wealth quintile", , drop = FALSE]
  rows <- dplyr::bind_rows(lapply(unique(all_rows$survey_id), function(s) {
    want <- period
    if (is.null(want) && !is.null(ind)) {
      by <- ind$api_by
      if (length(by) > 1L || !is.null(names(by))) {
        by <- if (s %in% names(by)) by[[s]] else by[["default"]]
      }
      if (!is.na(by)) want <- by
    }
    hit <- if (is.null(want)) NULL else all_rows[all_rows$survey_id == s & all_rows$by_variable_label %in% want, , drop = FALSE]
    if (is.null(hit) || !nrow(hit)) hit <- pref[pref$survey_id == s, , drop = FALSE]
    hit
  }))
  if (!nrow(rows)) {
    cli::cli_abort("No official estimates by wealth quintile for {.val {api}}.")
  }
  cat <- bdhs::bdhs_indicator_catalog
  is_pct <- identical(cat$measurement_type[match(api, cat$indicator_id)], "Percent")
  order_q <- c("Lowest", "Second", "Middle", "Fourth", "Highest")
  out <- list()
  for (s in unique(rows$survey_id)) {
    r <- rows[rows$survey_id == s, , drop = FALSE]
    r <- r[match(order_q, r$characteristic_label), , drop = FALSE]
    if (anyNA(r$value)) next
    w <- r$denom_weighted
    if (anyNA(w)) {
      cli::cli_inform("{.val {s}}: no weighted denominators by quintile; assuming equal shares.")
      w <- rep(1, 5)
    }
    m <- grouped_inequality(r$value, w, is_pct)
    out[[s]] <- tibble::tibble(
      survey_id = s, indicator = api, label = r$indicator[1], by = "Wealth quintile",
      measure = names(m), measure_label = unname(.inequality_labels[names(m)]),
      estimate = unname(m), se = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
      lowest = "Lowest", highest = "Highest", n_groups = 5L
    )
  }
  res <- dplyr::bind_rows(out)
  res <- res[res$measure %in% measures, , drop = FALSE]
  res <- res[order(match(res$survey_id, res$survey_id), match(res$measure, .inequality_measures)), , drop = FALSE]
  attr(res, "level") <- NA_real_
  class(res) <- c("bdhs_inequality", class(res))
  res
}

# DHS API id for an indicator given as API id or registry id.
official_id <- function(indicator, call = rlang::caller_env()) {
  if (indicator %in% bdhs::bdhs_indicators$indicator_id) {
    return(indicator)
  }
  reg <- indicator_registry()
  if (tolower(indicator) %in% names(reg)) {
    api <- reg[[tolower(indicator)]]$api_id
    api <- api[!is.na(api)]
    if (length(api) == 1L) {
      return(api)
    }
    cli::cli_abort("{.val {indicator}} has no single DHS indicator id to look up.", call = call)
  }
  toupper(indicator)
}

# Inequality measures from group values (poorest first) and group sizes.
grouped_inequality <- function(mu, w, percent = TRUE) {
  p <- w / sum(w)
  R <- cumsum(p) - p / 2
  k <- length(mu)
  m <- sum(p * mu)
  sii <- sum(p * (R - sum(p * R)) * (mu - m)) / sum(p * (R - sum(p * R))^2)
  rii <- tryCatch({
    fit <- suppressWarnings(stats::glm(mu ~ R, weights = p, family = stats::quasipoisson(link = "log")))
    exp(stats::coef(fit)[["R"]])
  }, error = function(e) NA_real_)
  ci <- 2 * sum(p * mu * R) / m - 1
  out <- c(difference = mu[k] - mu[1], ratio = mu[k] / mu[1], sii = sii, rii = rii, ci = ci)
  if (percent) out <- c(out, eci = 4 * (m / 100) * ci)
  out
}

#' @rdname bdhs_inequality
#' @param x,object A `bdhs_inequality` result.
#' @param ... Unused.
#' @export
plot.bdhs_inequality <- function(x, ...) {
  autoplot.bdhs_inequality(x, ...)
}

#' @rdname bdhs_inequality
#' @exportS3Method ggplot2::autoplot
autoplot.bdhs_inequality <- function(object, ...) {
  check_installed("ggplot2", "to plot inequality measures")
  d <- tibble::as_tibble(as.data.frame(object))
  if (!nrow(d)) cli::cli_abort("{.arg object} has no rows to plot.")
  d$year <- survey_mid_year(d$survey_id)
  d$measure_label <- factor(d$measure_label, levels = unique(d$measure_label))
  ref <- data.frame(measure_label = levels(d$measure_label))
  ref$y <- ifelse(grepl("^Ratio|^Relative", ref$measure_label), 1, 0)
  ref$measure_label <- factor(ref$measure_label, levels = levels(d$measure_label))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$estimate)) +
    ggplot2::geom_hline(data = ref, ggplot2::aes(yintercept = .data$y), colour = "grey60", linetype = "dashed") +
    ggplot2::geom_line(colour = trend_palette()[1]) +
    ggplot2::geom_point(colour = trend_palette()[1], size = 2)
  if (any(!is.na(d$ci_low))) {
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
                                    width = 0.3, colour = trend_palette()[1], na.rm = TRUE)
  }
  p + ggplot2::facet_wrap(ggplot2::vars(.data$measure_label), scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL, title = d$label[1],
                  subtitle = paste("Inequality by", d$by[1])) +
    explore_theme()
}
