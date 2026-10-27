# Kitagawa decomposition of change between rounds -------------------------------------

#' Decompose the change of an indicator between two rounds
#'
#' Splits the change of an indicator between two BDHS rounds into a
#' **composition** effect (the population moved between groups, e.g. more
#' mothers with secondary education) and a **within-group** (rate) effect
#' (the indicator changed inside the groups), following Kitagawa (1955) with
#' the symmetric weighting of Das Gupta (1993).
#'
#' With group shares \eqn{c_{tg}} (weighted share of the indicator's
#' denominator population in group \eqn{g} in round \eqn{t}) and group rates
#' \eqn{r_{tg}}, the overall level is \eqn{\bar r_t = \sum_g c_{tg} r_{tg}}
#' and
#' \deqn{\bar r_2 - \bar r_1 = \sum_g (c_{2g} - c_{1g}) \frac{r_{1g} +
#' r_{2g}}{2} + \sum_g (r_{2g} - r_{1g}) \frac{c_{1g} + c_{2g}}{2},}
#' the composition and within-group effects. The decomposition is exact (no
#' interaction term), and each term is also reported by group.
#'
#' **Standard errors.** Every term is a smooth function of weighted survey
#' totals of the two rounds, which are independent samples, so its variance
#' is the sum of the variances from each round. These are estimated with the
#' delete-one-PSU jackknife within each round (the DHS Program's method for
#' non-linear statistics such as rates): \eqn{v_t = (k_t - 1) / k_t
#' \sum_j (\hat\theta_{(j)} - \hat\theta)^2} over the \eqn{k_t} PSUs of round
#' \eqn{t}, holding the other round at its full-sample value. The jackknife
#' accounts for the clustering and the randomness of both the shares and the
#' rates.
#'
#' The groups must mean the same in both rounds: use harmonized variables
#' (`mother_edu_level`, `wealth_quintile`, `residence`, `division_trend6`,
#' see [bdhs_harmonized_vars()]). If `by` names a harmonized variable that is
#' not yet in the data, it is created with [bdhs_harmonize()].
#'
#' @param data1,data2 Data frames of the same recode from two rounds (earlier
#'   round first).
#' @param indicator A single indicator id (a proportion or a mean without an
#'   all-women denominator).
#' @param by A categorical grouping variable present (or harmonizable) in
#'   both data frames.
#' @param level Confidence level.
#' @param se `"jackknife"` (default) or `"none"`.
#'
#' @return A tibble of class `bdhs_decomposition` with columns `component`
#'   (`"composition"`, `"within"`, `"total"`), `group` (a group, or `"All"`
#'   for the sum over groups), `estimate`, `se`, `ci_low`, `ci_high` and
#'   `percent` (share of the total change). Proportions are on the 0-100
#'   scale. The attribute `groups` holds the shares and rates by group and
#'   round; `survey_1`, `survey_2`, `indicator` and `by` describe the
#'   decomposition.
#' @references
#' Kitagawa EM (1955). Components of a difference between two rates.
#' *Journal of the American Statistical Association* 50(272): 1168-1194.
#'
#' Das Gupta P (1993). *Standardization and Decomposition of Rates: A User's
#' Manual*. US Bureau of the Census, Current Population Reports P23-186.
#' @export
#' @examples
#' kr14 <- bdhs_simulate(2014, "KR")
#' kr22 <- bdhs_simulate(2022, "KR")
#' dec <- bdhs_decompose(kr14, kr22, "rh_del_csection", by = "residence")
#' dec
#' attr(dec, "groups")
bdhs_decompose <- function(data1, data2, indicator, by, level = 0.95, se = c("jackknife", "none")) {
  se <- rlang::arg_match(se)
  check_level(level)
  if (!is.data.frame(data1) || !is.data.frame(data2)) {
    cli::cli_abort("{.arg data1} and {.arg data2} must be data frames of BDHS recode data.")
  }
  if (!is.character(by) || length(by) != 1L) {
    cli::cli_abort("{.arg by} must be a single variable name.")
  }
  if (!is.character(indicator) || length(indicator) != 1L) {
    cli::cli_abort("{.arg indicator} must be a single indicator id.")
  }
  ind <- resolve_indicators(indicator)[[1]]
  if (!ind$type %in% c("proportion", "mean") || !is.null(ind$all_women)) {
    cli::cli_abort("{.fn bdhs_decompose} needs a proportion or mean without an all-women denominator; {.val {ind$id}} is not one.")
  }
  if (is_pooled(data1) || is_pooled(data2)) {
    cli::cli_abort("{.arg data1} and {.arg data2} must each hold a single round.")
  }
  data1 <- decompose_by_column(data1, by)
  data2 <- decompose_by_column(data2, by)
  old <- options(survey.lonely.psu = "adjust")
  on.exit(options(old), add = TRUE)
  t1 <- decompose_totals(data1, ind, by)
  t2 <- decompose_totals(data2, ind, by)
  if (identical(t1$survey_id, t2$survey_id)) {
    cli::cli_abort("{.arg data1} and {.arg data2} are the same round ({.val {t1$survey_id}}).")
  }
  groups <- union(t1$groups, t2$groups)
  only <- c(setdiff(groups, t1$groups), setdiff(groups, t2$groups))
  if (length(only)) {
    cli::cli_warn("Group{?s} {.val {only}} {?is/are} present in one round only; {?its/their} rate is taken from the other round.")
  }
  scale <- if (ind$type == "proportion") 100 else 1
  t1 <- align_totals(t1, groups)
  t2 <- align_totals(t2, groups)

  theta <- function(Y1, W1, Y2, W2) {
    c1 <- W1 / sum(W1)
    c2 <- W2 / sum(W2)
    r1 <- ifelse(W1 > 0, Y1 / W1, NA_real_)
    r2 <- ifelse(W2 > 0, Y2 / W2, NA_real_)
    r1[is.na(r1)] <- r2[is.na(r1)]
    r2[is.na(r2)] <- r1[is.na(r2)]
    comp <- (c2 - c1) * (r1 + r2) / 2
    within <- (r2 - r1) * (c1 + c2) / 2
    c(comp, sum(comp), within, sum(within), sum(comp) + sum(within)) * scale
  }
  est <- theta(t1$Y, t1$W, t2$Y, t2$W)
  ses <- rep(NA_real_, length(est))
  if (se == "jackknife") {
    v1 <- jackknife_var(t1, function(Y, W) theta(Y, W, t2$Y, t2$W), est)
    v2 <- jackknife_var(t2, function(Y, W) theta(t1$Y, t1$W, Y, W), est)
    ses <- sqrt(v1 + v2)
  }
  q <- stats::qnorm(1 - (1 - level) / 2)
  ng <- length(groups)
  total <- est[length(est)]
  res <- tibble::tibble(
    component = c(rep("composition", ng + 1L), rep("within", ng + 1L), "total"),
    group = c(groups, "All", groups, "All", "All"),
    estimate = est, se = ses, ci_low = est - q * ses, ci_high = est + q * ses,
    percent = 100 * est / total
  )
  rates <- tibble::tibble(
    group = groups,
    share_1 = 100 * t1$W / sum(t1$W), share_2 = 100 * t2$W / sum(t2$W),
    rate_1 = scale * t1$Y / t1$W, rate_2 = scale * t2$Y / t2$W,
    n_1 = as.integer(t1$n), n_2 = as.integer(t2$n)
  )
  attr(res, "groups") <- rates
  attr(res, "survey_1") <- t1$survey_id
  attr(res, "survey_2") <- t2$survey_id
  attr(res, "indicator") <- ind$id
  attr(res, "label") <- ind$label
  attr(res, "by") <- by
  attr(res, "level") <- level
  class(res) <- c("bdhs_decomposition", class(res))
  res
}

#' @export
print.bdhs_decomposition <- function(x, ...) {
  cat("# Kitagawa decomposition: ", attr(x, "indicator"), " by ", attr(x, "by"), ", ",
      attr(x, "survey_1"), " -> ", attr(x, "survey_2"), "\n", sep = "")
  NextMethod()
}

# Add a harmonized `by` column when needed.
decompose_by_column <- function(data, by, call = rlang::caller_env()) {
  if (by %in% names(data)) {
    return(data)
  }
  hv <- bdhs_harmonized_vars()
  if (by %in% hv[[1]]) {
    return(suppressMessages(bdhs_harmonize(data, vars = by, warn = FALSE)))
  }
  check_by(data, by, call = call)
  data
}

# Weighted totals by group and PSU for one round.
decompose_totals <- function(data, ind, by) {
  recode <- data_recode(data)
  if (!recode %in% ind$recode) abort_wrong_recode(list(ind), TRUE, recode)
  sid <- data_survey_id(data)
  des <- indicator_design(data, ind, sid, recode, by)
  if (is.null(des)) {
    cli::cli_abort("{.val {ind$id}}: no records in the denominator in {.val {sid}}.")
  }
  v <- des$variables
  w <- stats::weights(des)
  g <- v$.bdhs_g
  lev <- if (is.factor(g)) levels(droplevels(g)) else sort(unique(as.character(g)))
  g <- factor(as.character(g), levels = lev)
  psu <- as.character(v$.bdhs_psu)
  Yp <- rowsum(outer(w * v$.bdhs_y, rep(1, length(lev))) * (outer(as.integer(g), seq_along(lev), `==`)), psu)
  Wp <- rowsum(outer(w, rep(1, length(lev))) * (outer(as.integer(g), seq_along(lev), `==`)), psu)
  list(
    survey_id = sid, groups = lev, Y = colSums(Yp), W = colSums(Wp),
    Yp = Yp, Wp = Wp, n = as.integer(table(g))
  )
}

align_totals <- function(t, groups) {
  idx <- match(groups, t$groups)
  pick <- function(x) {
    out <- if (is.matrix(x)) matrix(0, nrow(x), length(groups)) else numeric(length(groups))
    if (is.matrix(x)) out[, !is.na(idx)] <- x[, idx[!is.na(idx)], drop = FALSE] else out[!is.na(idx)] <- x[idx[!is.na(idx)]]
    out
  }
  t$Y <- pick(t$Y)
  t$W <- pick(t$W)
  t$Yp <- pick(t$Yp)
  t$Wp <- pick(t$Wp)
  t$n <- pick(t$n)
  t$groups <- groups
  t
}

# Delete-one-PSU jackknife variance of f(Y, W) over the PSUs of one round.
jackknife_var <- function(t, f, est) {
  k <- nrow(t$Yp)
  if (k < 2L) {
    return(rep(NA_real_, length(est)))
  }
  reps <- vapply(seq_len(k), function(j) f(t$Y - t$Yp[j, ], t$W - t$Wp[j, ]), numeric(length(est)))
  reps <- matrix(reps, nrow = length(est))
  (k - 1) / k * rowSums((reps - est)^2)
}

#' @rdname bdhs_decompose
#' @param x,object A `bdhs_decomposition` result.
#' @param ... Unused.
#' @export
plot.bdhs_decomposition <- function(x, ...) {
  autoplot.bdhs_decomposition(x, ...)
}

#' @rdname bdhs_decompose
#' @exportS3Method ggplot2::autoplot
autoplot.bdhs_decomposition <- function(object, ...) {
  check_installed("ggplot2", "to plot decompositions")
  d <- tibble::as_tibble(as.data.frame(object))
  d <- d[d$component != "total", , drop = FALSE]
  d$group <- factor(d$group, levels = unique(c(setdiff(d$group, "All"), "All")))
  d$component <- factor(d$component, levels = c("composition", "within"),
                        labels = c("Composition", "Within-group"))
  total <- object$estimate[object$component == "total"]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$group, y = .data$estimate, fill = .data$component)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
                           position = ggplot2::position_dodge(width = 0.8), width = 0.2, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = trend_palette()[1:2], name = NULL) +
    ggplot2::labs(
      x = attr(object, "by"), y = "Contribution to the change",
      title = attr(object, "label"),
      subtitle = paste0(survey_label(attr(object, "survey_1")) %|NA|% attr(object, "survey_1"), " to ",
                        survey_label(attr(object, "survey_2")) %|NA|% attr(object, "survey_2"),
                        ": total change ", format(round(total, 1), nsmall = 1))
    ) +
    explore_theme()
}
