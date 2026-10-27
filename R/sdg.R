# SDG tracker from the official estimates ---------------------------------------------

# SDG indicators that the BDHS measures, with the official DHS indicator id,
# the direction of progress and a 2030 target where one exists. Sources:
# UN SDG global indicator framework and metadata repository
# (https://unstats.un.org/sdgs/metadata/); WHO/UNICEF (2017) "The extension
# of the 2025 maternal, infant and young child nutrition targets to 2030"
# (WHA global nutrition targets); WHO (2015) "Strategies toward ending
# preventable maternal mortality (EPMM)" / Every Newborn Action Plan coverage
# targets; Fabic MS et al. (2015) Bull World Health Organ 93:678 (demand
# satisfied benchmark of 75%).
sdg_targets <- function() {
  tibble::tribble(
    ~id, ~sdg, ~name, ~dhs_id, ~direction, ~target, ~target_relative, ~target_source,
    "stunting", "2.2.1", "Stunting, children under 5 (%)", "CN_NUTS_C_HA2", "lower", NA_real_, 0.5,
    "WHA/WHO-UNICEF 2030 extension: 50% fewer stunted children than in 2012 (applied to prevalence, baseline BDHS 2011)",
    "wasting", "2.2.2", "Wasting, children under 5 (%)", "CN_NUTS_C_WH2", "lower", 3, NA_real_,
    "WHA/WHO-UNICEF 2030 extension: reduce and maintain wasting below 3%",
    "u5mr", "3.2.1", "Under-five mortality rate (per 1,000)", "CM_ECMR_C_U5M", "lower", 25, NA_real_,
    "SDG target 3.2: at least as low as 25 per 1,000 live births",
    "nmr", "3.2.2", "Neonatal mortality rate (per 1,000)", "CM_ECMR_C_NNR", "lower", 12, NA_real_,
    "SDG target 3.2: at least as low as 12 per 1,000 live births",
    "sba", "3.1.2", "Births attended by skilled personnel (%)", "RH_DELA_C_SKP", "higher", 90, NA_real_,
    "No numeric SDG target; EPMM/ENAP coverage target of 90%",
    "demand_satisfied", "3.7.1", "Demand for family planning satisfied by modern methods (%)", "FP_NADM_W_PDM", "higher", 75, NA_real_,
    "No numeric SDG target; 75% benchmark (Fabic et al. 2015)",
    "mcpr", "3.7.1", "Modern contraceptive prevalence, married women (%)", "FP_CUSM_W_MOD", "higher", NA_real_, NA_real_,
    "Context for 3.7.1; no target",
    "abr", "3.7.2", "Adolescent birth rate, 15-19 (per 1,000 women)", "FE_FRTR_W_A15", "lower", NA_real_, NA_real_,
    "No numeric global target (ASFR 15-19, 3 years before the survey)",
    "married_by_15", "5.3.1", "Women 20-24 first married by 15 (%)", "MA_MBAY_W_B15", "lower", 0, NA_real_,
    "SDG target 5.3: eliminate child marriage",
    "married_by_18", "5.3.1", "Women 20-24 first married by 18 (%)", "MA_MBAY_W_B18", "lower", 0, NA_real_,
    "SDG target 5.3: eliminate child marriage",
    "improved_water", "6.1.1", "Population using an improved water source (%)", "WS_SRCE_P_IMP", "higher", 100, NA_real_,
    "SDG target 6.1: universal access (DHS improved source, proxy for safely managed)",
    "improved_sanitation", "6.2.1", "Population using improved sanitation (%)", "WS_TLET_P_IMP", "higher", 100, NA_real_,
    "SDG target 6.2: universal access (DHS improved facility, proxy for safely managed)"
  )
}

#' SDG tracker for Bangladesh from the official DHS estimates
#'
#' Tracks the Sustainable Development Goal indicators that the BDHS
#' measures, using the bundled official estimates ([bdhs_official()]):
#' stunting and wasting (SDG 2.2.1, 2.2.2), under-five and neonatal
#' mortality (3.2.1, 3.2.2), skilled birth attendance (3.1.2), family
#' planning (3.7.1), the adolescent birth rate (3.7.2), child marriage
#' (5.3.1) and improved water and sanitation (proxies for 6.1.1, 6.2.1).
#'
#' * The average annual rate of change (UNICEF/WHO) between the `baseline`
#'   round and the latest round is \eqn{AARC = 1 - (x_2 / x_1)^{1 / (t_2 -
#'   t_1)}}, in percent per year, with \eqn{t} the mid-point of fieldwork.
#'   For indicators that should fall it is computed on the value; for
#'   coverage indicators that should rise it is computed on the shortfall
#'   \eqn{100 - x}. Positive values always mean progress.
#' * The required AARC is the rate that brings the latest value to the
#'   target by `target_year`; it is `NA` for elimination or universal targets
#'   (0% or 100%), which an exponential trend never reaches exactly.
#' * The projection continues the current AARC to `target_year`; an
#'   indicator is on track when the target is already met or the projection,
#'   rounded to whole units, meets it.
#'
#' Targets and their sources are in the `target_source` column; where the
#' SDG framework has no numeric target a widely used benchmark is given, or
#' none.
#'
#' @param indicators Optional ids (`"stunting"`, `"u5mr"`, ...), SDG
#'   numbers (`"3.2.1"`) or DHS ids to keep. `NULL` keeps all.
#' @param baseline Survey reference of the baseline round for the AARC
#'   (default 2014, the round closest to the start of the SDG period).
#' @param target_year Target year (default 2030).
#'
#' @return A tibble of class `bdhs_sdg` with columns `id`, `sdg`, `name`,
#'   `dhs_id`, `direction`, `target`, `baseline_survey`, `baseline_value`,
#'   `latest_survey`, `latest_value`, `aarc`, `required_aarc`,
#'   `projected`, `on_track` and `target_source`. The attribute `series`
#'   holds every round's value.
#' @export
#' @examples
#' bdhs_sdg()
#' bdhs_sdg(c("stunting", "u5mr"))
bdhs_sdg <- function(indicators = NULL, baseline = 2014, target_year = 2030) {
  tg <- sdg_targets()
  if (!is.null(indicators)) {
    keep <- tg$id %in% indicators | tg$sdg %in% indicators | tg$dhs_id %in% toupper(indicators)
    unknown <- indicators[!indicators %in% c(tg$id, tg$sdg) & !toupper(indicators) %in% tg$dhs_id]
    if (length(unknown)) {
      cli::cli_abort(c("Unknown SDG indicator{?s}: {.val {unknown}}.", "i" = "Choose from {.val {tg$id}}."))
    }
    tg <- tg[keep, , drop = FALSE]
  }
  if (!is.numeric(target_year) || length(target_year) != 1L) {
    cli::cli_abort("{.arg target_year} must be a single year.")
  }
  base_id <- bdhs_survey_id(baseline)
  off <- bdhs_official(tg$dhs_id)
  off <- off[!is.na(off$value), , drop = FALSE]
  series <- tibble::tibble(
    id = tg$id[match(off$indicator_id, tg$dhs_id)], dhs_id = off$indicator_id,
    survey_id = off$survey_id, year = survey_mid_year(off$survey_id), value = off$value
  )
  series <- series[order(match(series$id, tg$id), series$year), , drop = FALSE]

  rows <- lapply(seq_len(nrow(tg)), function(i) {
    s <- series[series$id == tg$id[i], , drop = FALSE]
    latest <- s[nrow(s), , drop = FALSE]
    b <- s[s$survey_id == base_id, , drop = FALSE]
    target <- tg$target[i]
    if (!is.na(tg$target_relative[i])) {
      ref <- s$value[s$survey_id == "BD2011DHS"]
      target <- if (length(ref)) round(tg$target_relative[i] * ref, 1) else NA_real_
    }
    higher <- tg$direction[i] == "higher"
    tr <- function(x) if (higher) 100 - x else x
    aarc <- if (nrow(b) && nrow(latest) && latest$year > b$year) {
      aarc_rate(tr(b$value), tr(latest$value), latest$year - b$year)
    } else {
      NA_real_
    }
    horizon <- target_year - latest$year
    req <- NA_real_
    projected <- NA_real_
    if (nrow(latest)) {
      projected <- if (is.na(aarc)) NA_real_ else {
        p <- tr(latest$value) * (1 - aarc / 100)^horizon
        if (higher) 100 - p else p
      }
      if (!is.na(target) && tr(target) > 0 && horizon > 0) {
        req <- max(0, aarc_rate(tr(latest$value), tr(target), horizon))
      }
    }
    met <- if (higher) latest$value >= target else latest$value <= target
    proj_met <- if (higher) round(projected) >= target else round(projected) <= target
    on_track <- if (is.na(target)) NA else isTRUE(met) || isTRUE(proj_met)
    tibble::tibble(
      id = tg$id[i], sdg = tg$sdg[i], name = tg$name[i], dhs_id = tg$dhs_id[i],
      direction = tg$direction[i], target = target,
      baseline_survey = if (nrow(b)) b$survey_id else NA_character_,
      baseline_value = if (nrow(b)) b$value else NA_real_,
      latest_survey = latest$survey_id, latest_value = latest$value,
      aarc = aarc, required_aarc = req, projected = projected, on_track = on_track,
      target_source = tg$target_source[i]
    )
  })
  res <- dplyr::bind_rows(rows)
  attr(res, "series") <- series
  attr(res, "target_year") <- target_year
  class(res) <- c("bdhs_sdg", class(res))
  res
}

#' @export
print.bdhs_sdg <- function(x, ...) {
  cat("# BDHS SDG tracker (official DHS estimates) | target year ", attr(x, "target_year") %||% 2030,
      " | AARC in % per year\n", sep = "")
  NextMethod()
}

# Average annual rate of reduction (percent per year) from x1 to x2 over t years.
aarc_rate <- function(x1, x2, t) {
  out <- 100 * (1 - (x2 / x1)^(1 / t))
  out[!is.finite(out)] <- NA_real_
  out
}

#' @rdname bdhs_sdg
#' @param x,object A `bdhs_sdg` result.
#' @param ... Unused.
#' @export
plot.bdhs_sdg <- function(x, ...) {
  autoplot.bdhs_sdg(x, ...)
}

#' @rdname bdhs_sdg
#' @exportS3Method ggplot2::autoplot
autoplot.bdhs_sdg <- function(object, ...) {
  check_installed("ggplot2", "to plot the SDG tracker")
  s <- attr(object, "series")
  s <- s[s$id %in% object$id, , drop = FALSE]
  if (!nrow(s)) cli::cli_abort("{.arg object} has no values to plot.")
  ty <- attr(object, "target_year") %||% 2030
  facet <- function(id) {
    i <- match(id, object$id)
    factor(paste0(object$sdg[i], " ", object$name[i]), levels = unique(paste0(object$sdg, " ", object$name)))
  }
  s$panel <- facet(s$id)
  tgt <- object[!is.na(object$target), , drop = FALSE]
  tgt$panel <- facet(tgt$id)
  proj <- object[!is.na(object$projected), , drop = FALSE]
  latest_year <- survey_mid_year(proj$latest_survey)
  pr <- tibble::tibble(
    panel = facet(rep(proj$id, 2)), year = c(latest_year, rep(ty, nrow(proj))),
    value = c(proj$latest_value, proj$projected), id = rep(proj$id, 2)
  )
  ggplot2::ggplot(s, ggplot2::aes(x = .data$year, y = .data$value)) +
    ggplot2::geom_hline(data = tgt, ggplot2::aes(yintercept = .data$target), colour = trend_palette()[2], linetype = "dashed") +
    ggplot2::geom_line(data = pr, ggplot2::aes(group = .data$id), colour = "grey50", linetype = "dotted") +
    ggplot2::geom_line(colour = trend_palette()[1]) +
    ggplot2::geom_point(colour = trend_palette()[1], size = 1.8) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), scales = "free_y",
                        labeller = ggplot2::label_wrap_gen(width = 32)) +
    ggplot2::labs(x = NULL, y = NULL, title = "Bangladesh SDG indicators (BDHS)",
                  subtitle = paste0("Official DHS estimates; dashed: target; dotted: current AARC projected to ", ty),
                  caption = "Source: DHS Program API via bdhs. AARC = average annual rate of change.") +
    explore_theme()
}
