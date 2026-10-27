# Plots ------------------------------------------------------------------------------------

#' Plot the trend of an official DHS indicator
#'
#' Draws an official indicator from [bdhs_indicators] across survey rounds,
#' nationally or by a background characteristic, with 95% confidence
#' intervals where the DHS Program publishes them. Survey rounds are placed
#' on a true time scale and labelled with their survey years.
#'
#' Subnational trends come in two flavours. `by = "Region"` uses the five
#' regions that are comparable across every round (Barishal,
#' Chattogram/Sylhet, Dhaka before 2015 (incl. Mymensingh), Khulna,
#' Rajshahi/Rangpur). `by = "Division"` uses the divisions as surveyed in
#' each round (Sylhet from 1996-97, Rangpur from 2011, Mymensingh from
#' 2017-18); with more than eight series these are drawn as small multiples.
#'
#' @param indicator One or more DHS indicator ids (see [bdhs_search()]).
#'   Several indicators are drawn in separate panels.
#' @param by `NULL` (default) for the national value, or a characteristic
#'   category such as `"Residence"`, `"Wealth quintile"`, `"Education"`,
#'   `"Region"` or `"Division"`.
#' @param survey Optional survey references to restrict the rounds shown.
#' @param ci If `TRUE` (default), draw 95% confidence intervals when the DHS
#'   Program publishes them for the indicator.
#'
#' @return A ggplot object.
#' @seealso [bdhs_official()] for the numbers behind the plot.
#' @export
#' @examplesIf rlang::is_installed("ggplot2")
#' bdhs_plot_trend("FE_FRTR_W_TFR")
#' bdhs_plot_trend("CN_NUTS_C_HA2", by = "Residence")
#' bdhs_plot_trend("CM_ECMR_C_U5M", by = "Region")
bdhs_plot_trend <- function(indicator, by = NULL, survey = NULL, ci = TRUE) {
  check_installed("ggplot2", "to plot indicator trends")
  if (missing(indicator) || !is.character(indicator) || !length(indicator)) {
    cli::cli_abort("{.arg indicator} must be one or more DHS indicator ids, e.g. {.val FE_FRTR_W_TFR}.")
  }
  if (!is.null(by) && !rlang::is_string(by)) {
    cli::cli_abort("{.arg by} must be a single characteristic category, e.g. {.val Residence}.")
  }
  rows <- explore_official_rows(indicator, survey, breakdown = "all", preferred = TRUE)
  rows <- trend_select(rows, by)
  rows <- rows[!is.na(rows$value), ]

  d <- tibble::tibble(
    survey_id = rows$survey_id,
    year = rows$survey_year,
    indicator = rows$indicator,
    group = explore_clean_label(rows$characteristic_label),
    value = rows$value,
    ci_low = rows$ci_low,
    ci_high = rows$ci_high
  )
  group_levels <- unique(d$group[order(rows$characteristic_order)])
  indicator_levels <- unique(d$indicator[order(rows$indicator_order)])
  d <- d[!duplicated(d[, c("survey_id", "indicator", "group")]), ]
  d$group <- factor(d$group, levels = group_levels)
  d$indicator <- factor(d$indicator, levels = indicator_levels)

  n_groups <- nlevels(d$group)
  n_ind <- nlevels(d$indicator)
  facet_groups <- n_groups > length(trend_palette())
  if (facet_groups && n_ind > 1L) {
    cli::cli_abort(c(
      "Too many series to draw several indicators by {.val {by}}.",
      "i" = "Plot one indicator at a time."
    ))
  }
  coloured <- n_groups > 1L && !facet_groups
  show_ci <- isTRUE(ci) && any(!is.na(d$ci_low) & !is.na(d$ci_high))
  dodge <- if (coloured && show_ci) ggplot2::position_dodge(width = 0.7) else ggplot2::position_identity()

  aes_main <- if (coloured) {
    ggplot2::aes(x = .data$year, y = .data$value, colour = .data$group, group = .data$group)
  } else {
    ggplot2::aes(x = .data$year, y = .data$value, group = .data$group)
  }
  fixed <- if (coloured) list() else list(colour = "#2a78d6")
  p <- ggplot2::ggplot(d, aes_main)
  if (show_ci) {
    p <- p + rlang::exec(
      ggplot2::geom_errorbar,
      data = d[!is.na(d$ci_low) & !is.na(d$ci_high), ],
      mapping = ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
      width = 0.4, linewidth = 0.4, position = dodge, !!!fixed
    )
  }
  # Lines only join groups observed in at least two rounds.
  n_obs <- table(paste(d$indicator, d$group))
  line_data <- d[n_obs[paste(d$indicator, d$group)] > 1L, ]
  p <- p +
    rlang::exec(ggplot2::geom_line, data = line_data, linewidth = 0.7, position = dodge, !!!fixed) +
    rlang::exec(ggplot2::geom_point, size = 2, position = dodge, !!!fixed)
  if (coloured) {
    p <- p + ggplot2::scale_colour_manual(values = trend_palette()[seq_len(n_groups)], name = NULL)
  }

  years <- sort(unique(d$year))
  year_labels <- trend_short_label(survey_label(d$survey_id[match(years, d$year)]))
  axis_guide <- if (facet_groups) {
    ggplot2::guide_axis(check.overlap = TRUE, angle = 90)
  } else {
    ggplot2::guide_axis(check.overlap = TRUE)
  }
  p <- p + ggplot2::scale_x_continuous(
    breaks = years, labels = year_labels, expand = ggplot2::expansion(add = 1), guide = axis_guide
  )

  if (facet_groups) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$group))
  } else if (n_ind > 1L) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$indicator), scales = "free_y")
  }

  cat_info <- explore_catalog(unique(rows$indicator_id))
  units <- unique(stats::na.omit(cat_info$measurement_type))
  span <- trend_short_label(survey_label(d$survey_id[match(range(d$year), d$year)]))
  subtitle <- paste0(
    if (!is.null(by)) paste0("By ", trend_by_text(by, rows), ", ") else "",
    "Bangladesh DHS ", if (span[[1]] == span[[2]]) span[[1]] else paste(span, collapse = " to ")
  )
  caption <- paste0(
    "Source: DHS Program API (official estimates bundled in bdhs).",
    if (show_ci) " Bars: 95% confidence intervals." else ""
  )
  p +
    ggplot2::labs(
      title = if (n_ind == 1L) levels(d$indicator) else "Official DHS indicators",
      subtitle = subtitle,
      x = NULL,
      y = if (length(units) == 1L) units else "Value",
      caption = caption
    ) +
    explore_theme()
}

# Keep the rows of one breakdown for a trend plot.
trend_select <- function(rows, by, call = rlang::caller_env()) {
  if (is.null(by)) {
    out <- rows[rows$breakdown == "national", ]
    total <- grepl("^Total", out$characteristic_category)
    if (any(total)) out <- out[total, ]
    if (!nrow(out)) {
      cli::cli_abort(
        c(
          "No national estimates for this indicator.",
          "i" = "Available breakdowns: {.val {sort(unique(rows$characteristic_category))}}."
        ),
        call = call
      )
    }
    return(out)
  }
  key <- tolower(trimws(by))
  if (key %in% c("region", "regions")) {
    return(trend_require(rows[rows$breakdown == "subnational" & !startsWith(rows$characteristic_label, "."), ], rows, by, call))
  }
  if (key %in% c("division", "divisions")) {
    return(trend_require(rows[rows$breakdown == "subnational" & rows$level_rank %in% 1L, ], rows, by, call))
  }
  available <- unique(rows$characteristic_category[rows$breakdown != "subnational"])
  cats <- tryCatch(
    explore_resolve_characteristic(by, available, call = call),
    error = function(e) {
      cli::cli_abort(
        c(
          "No estimates of this indicator by {.val {by}}.",
          "i" = "Available: {.val {sort(c(available, if (any(rows$breakdown == 'subnational')) c('Region', 'Division')))}}."
        ),
        call = call
      )
    }
  )
  rows[rows$characteristic_category %in% cats, ]
}

trend_require <- function(out, rows, by, call) {
  if (!nrow(out)) {
    cli::cli_abort(
      c(
        "No subnational estimates for this indicator.",
        "i" = "Available: {.val {sort(unique(rows$characteristic_category))}}."
      ),
      call = call
    )
  }
  out
}

# Subtitle wording for the breakdown: the category name as published.
trend_by_text <- function(by, rows) {
  cats <- unique(rows$characteristic_category)
  if (tolower(by) %in% c("region", "regions", "division", "divisions") || length(cats) != 1L) {
    return(tolower(by))
  }
  tolower(cats)
}

# "1999-2000" -> "1999-00" for compact axes.
trend_short_label <- function(x) {
  sub("^(\\d{4})-\\d{2}(\\d{2})$", "\\1-\\2", x)
}

# Categorical palette (fixed order, colour-blind checked).
trend_palette <- function() {
  c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
}

# Shared, quiet theme for bdhs plots.
explore_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "grey40", size = base_size * 0.75),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

# Plot method for bdhs_estimate() results ---------------------------------------------

#' Plot survey estimates
#'
#' Point-and-interval plot of the estimates returned by [bdhs_estimate()]:
#' one panel per indicator, one row per group (when `by` was used), and one
#' colour per survey round when several rounds are present. Intervals are
#' the 95% confidence intervals of the estimates.
#'
#' @param x,object A `bdhs_estimate` tibble from [bdhs_estimate()].
#' @param ... Unused.
#'
#' @return A ggplot object.
#' @export
#' @examplesIf rlang::is_installed("ggplot2")
#' est <- tibble::tibble(
#'   survey_id = "BD2022DHS", indicator = "anc4", label = "ANC 4+ visits",
#'   v025 = c("urban", "rural"), estimate = c(54.1, 37.9), se = c(2.1, 1.4),
#'   ci_low = c(50, 35.2), ci_high = c(58.2, 40.6), n_unweighted = c(1200L, 3400L),
#'   n_weighted = c(1250, 3350), api_id = "RH_ANCN_W_N4P"
#' )
#' class(est) <- c("bdhs_estimate", class(est))
#' plot(est)
plot.bdhs_estimate <- function(x, ...) {
  autoplot.bdhs_estimate(x, ...)
}

#' @rdname plot.bdhs_estimate
#' @exportS3Method ggplot2::autoplot
autoplot.bdhs_estimate <- function(object, ...) {
  check_installed("ggplot2", "to plot estimates")
  d <- tibble::as_tibble(as.data.frame(object))
  needed <- c("estimate", "ci_low", "ci_high")
  if (!all(needed %in% names(d))) {
    cli::cli_abort("{.arg object} must have columns {.field {needed}}.")
  }
  if (!nrow(d)) {
    cli::cli_abort("{.arg object} has no rows to plot.")
  }
  known <- c(
    "survey_id", "indicator", "label", "estimate", "se", "ci_low", "ci_high",
    "n_unweighted", "n_weighted", "api_id"
  )
  by_cols <- setdiff(names(d), known)
  facet <- if ("label" %in% names(d)) d$label else if ("indicator" %in% names(d)) d$indicator else rep("Estimate", nrow(d))
  facet[is.na(facet)] <- if ("indicator" %in% names(d)) d$indicator[is.na(facet)] else "Estimate"
  d$.facet <- factor(facet, levels = unique(facet))
  surveys <- if ("survey_id" %in% names(d)) unique(d$survey_id) else character()
  d$.survey <- if (length(surveys)) {
    lab <- survey_label(d$survey_id) %|NA|% d$survey_id
    factor(lab, levels = unique(lab[order(search_survey_year(d$survey_id))]))
  } else {
    factor("")
  }
  multi_survey <- length(surveys) > 1L

  if (length(by_cols)) {
    parts <- lapply(d[by_cols], explore_group_values)
    group <- do.call(paste, c(parts, sep = " / "))
    lev <- unique(group[do.call(order, lapply(d[by_cols], explore_group_order))])
  } else if (multi_survey) {
    group <- as.character(d$.survey)
    lev <- levels(d$.survey)
  } else {
    group <- rep("Total", nrow(d))
    lev <- "Total"
  }
  d$.group <- factor(group, levels = rev(lev))
  colour_by_survey <- multi_survey && length(by_cols) > 0L
  dodge <- if (colour_by_survey) ggplot2::position_dodge(width = 0.6) else ggplot2::position_identity()

  aes_main <- if (colour_by_survey) {
    ggplot2::aes(x = .data$estimate, y = .data$.group, colour = .data$.survey)
  } else {
    ggplot2::aes(x = .data$estimate, y = .data$.group)
  }
  fixed <- if (colour_by_survey) list() else list(colour = "#2a78d6")
  p <- ggplot2::ggplot(d, aes_main) +
    rlang::exec(
      ggplot2::geom_errorbar,
      mapping = ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high),
      orientation = "y", width = 0.25, linewidth = 0.5, position = dodge, na.rm = TRUE, !!!fixed
    ) +
    rlang::exec(ggplot2::geom_point, size = 2.2, position = dodge, na.rm = TRUE, !!!fixed)
  if (colour_by_survey) {
    p <- p + ggplot2::scale_colour_manual(values = trend_palette()[seq_along(surveys)], name = NULL)
  }
  if (nlevels(d$.facet) > 1L) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$.facet), scales = "free_x")
  }
  survey_text <- if (length(surveys)) {
    paste("Bangladesh DHS", paste(levels(d$.survey), collapse = ", "))
  } else {
    NULL
  }
  p +
    ggplot2::labs(
      title = if (nlevels(d$.facet) == 1L) levels(d$.facet) else "Survey estimates",
      subtitle = survey_text,
      x = "Estimate (95% CI)",
      y = if (length(by_cols)) paste(estimate_axis_titles(object, by_cols), collapse = " / ") else NULL,
      caption = "Estimates computed with bdhs from DHS microdata; bars show 95% confidence intervals."
    ) +
    explore_theme() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(colour = "grey90"),
      panel.grid.major.y = ggplot2::element_blank()
    )
}

# Display values of a by-group column (labels for labelled/factor columns).
explore_group_values <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::as_factor(x, levels = "default")
  }
  out <- as.character(x)
  out[is.na(out)] <- "(missing)"
  out
}

# Sort key of a by-group column: codes or factor order, not alphabetical.
explore_group_order <- function(x) {
  if (is.factor(x) || inherits(x, "haven_labelled") || is.numeric(x)) {
    return(as_codes(x))
  }
  match(x, unique(x))
}

# Axis titles for by-columns: the variable label recorded by bdhs_estimate()
# (e.g. "wealth index combined" for v190), else the column name.
estimate_axis_titles <- function(object, by_cols) {
  labs <- attr(object, "by_labels", exact = TRUE)
  out <- by_cols
  if (length(labs)) {
    hit <- by_cols %in% names(labs) & !is.na(labs[by_cols])
    out[hit] <- labs[by_cols[hit]]
  }
  out
}
