# Boundaries and maps -------------------------------------------------------------------

#' Division boundaries of a survey round
#'
#' The subnational (division) boundaries used for each Bangladesh survey, as
#' an sf object. Boundaries change between rounds as divisions were created
#' (Sylhet 1995, Rangpur 2010, Mymensingh 2015), so use the boundaries of
#' the round you are mapping.
#'
#' Boundaries come from the DHS Program Spatial Data Repository
#' (<https://spatialdata.dhsprogram.com/home/>, ICF), licensed under the Open
#' Database License (ODbL) 1.0. `region_code` is the region code used in the
#' recode files (`v024`/`hv024`), and `reg_id` links to the `region_id` of
#' official subnational estimates in [bdhs_official()].
#'
#' @param survey Survey references (see [bdhs_survey_id()]); several are
#'   allowed, and `NULL` returns every round.
#'
#' @return An sf object (EPSG:4326) with columns `survey_id`, `region_code`,
#'   `region_name`, `unit_id` (stable 4-digit code of the geographic unit
#'   across rounds), `reg_id` and `geometry`.
#' @seealso [bdhs_map()]
#' @export
#' @examplesIf rlang::is_installed("sf")
#' b <- bdhs_boundaries(2022)
#' b
#' table(bdhs_boundaries(NULL)$survey_id)
bdhs_boundaries <- function(survey = 2022) {
  check_installed("sf", "to work with division boundaries")
  all <- map_read_boundaries()
  if (is.null(survey)) {
    sid <- unique(all$survey_id)
  } else {
    sid <- bdhs_survey_id(survey, type = c("DHS", "SPA", "OTH"))
    missing <- setdiff(sid, all$survey_id)
    if (length(missing)) {
      cli::cli_abort(c(
        "No boundaries are bundled for {.val {missing}}.",
        "i" = "Available: {.val {unique(all$survey_id)}}."
      ))
    }
  }
  out <- all[all$survey_id %in% sid, ]
  out <- out[order(match(out$survey_id, sid), out$region_code), ]
  out <- out[, c("survey_id", "region_code", "region_name", "unit_id", "reg_id")]
  rownames(out) <- NULL
  attr(out, "source") <- attr(all, "source", exact = TRUE)
  out
}

#' Map indicators by division
#'
#' Draws a choropleth of an indicator by division with the boundaries of the
#' survey round. `x` is either an official DHS indicator id, mapped with the
#' subnational estimates bundled in [bdhs_indicators], or a data frame of
#' your own estimates by region, such as the output of
#' `bdhs_estimate(..., by = "v024")`.
#'
#' A data frame needs a region column (`region_code`, `v024`, `hv024`,
#' `mv024`, `region`, `division` or `region_name`) holding region codes as in
#' the recode files or division names (old and new spellings such as
#' "Barisal"/"Barishal" are both understood), and a value column (`value`
#' argument; by default `estimate` or `value`). The survey round comes from
#' `survey`, a `survey_id` column (several rounds give one panel each) or the
#' `bdhs_survey_id` attribute.
#'
#' @param x An official indicator id (e.g. `"CN_NUTS_C_HA2"`) or a data frame
#'   of estimates by region.
#' @param survey Survey references. For an indicator id, defaults to the most
#'   recent round with subnational estimates; several rounds are drawn as
#'   panels sharing one colour scale.
#' @param value For a data frame, the name of the column to map.
#' @param title Plot title; defaults to the indicator label.
#'
#' @return A ggplot object.
#' @seealso [bdhs_boundaries()], [bdhs_official()]
#' @export
#' @examplesIf rlang::is_installed(c("ggplot2", "sf"))
#' bdhs_map("CN_NUTS_C_HA2")
#' bdhs_map("FE_FRTR_W_TFR", survey = c(2014, 2022))
#'
#' # Your own estimates by division (v024 codes of the 2022 round)
#' est <- data.frame(v024 = 1:8, estimate = c(25, 30, 22, 21, 31, 23, 24, 34))
#' bdhs_map(est, survey = 2022, title = "My indicator")
bdhs_map <- function(x, survey = NULL, value = NULL, title = NULL) {
  check_installed(c("ggplot2", "sf"), "to draw maps")
  info <- if (is.data.frame(x)) {
    map_user_data(x, survey = survey, value = value)
  } else if (rlang::is_string(x)) {
    map_official_data(x, survey = survey)
  } else {
    cli::cli_abort("{.arg x} must be an official indicator id or a data frame of estimates by region.")
  }
  d <- info$data
  sid <- unique(d$survey_id)
  shp <- bdhs_boundaries(sid)
  shp$value <- d$value[match(paste(shp$survey_id, shp[[info$key]]), paste(d$survey_id, d$key))]
  unmatched <- setdiff(paste(d$survey_id, d$key), paste(shp$survey_id, shp[[info$key]]))
  if (length(unmatched) && is.data.frame(x)) {
    cli::cli_warn("{length(unmatched)} row{?s} of {.arg x} did not match a division of the survey and {?was/were} dropped.")
  }
  if (all(is.na(shp$value))) {
    cli::cli_abort("No values could be matched to the division boundaries.")
  }
  shp$year_label <- factor(
    survey_label(shp$survey_id) %|NA|% shp$survey_id,
    levels = unique(survey_label(sid) %|NA|% sid)
  )
  labels <- map_label_points(shp, digits = info$digits)

  p <- ggplot2::ggplot(shp) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$value), colour = "white", linewidth = 0.3) +
    map_label_layer(labels) +
    ggplot2::scale_fill_viridis_c(name = info$legend, na.value = "grey85") +
    ggplot2::coord_sf(datum = NA, clip = "off")
  if (length(sid) > 1L) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$year_label))
  }
  p +
    ggplot2::labs(
      title = title %||% info$title,
      subtitle = paste0("Bangladesh DHS ", paste(levels(shp$year_label), collapse = ", "), ", by division"),
      x = NULL, y = NULL,
      caption = paste0(
        info$source, "\n",
        "Boundaries: The DHS Program Spatial Data Repository (ICF), ODbL 1.0."
      )
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "grey40", size = 8.25),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}

# Internals ------------------------------------------------------------------------------

map_read_boundaries <- function() {
  if (is.null(.explore_cache$boundaries)) {
    path <- system.file("extdata", "bdhs_boundaries.rds", package = "bdhs")
    if (!nzchar(path)) {
      cli::cli_abort("The bundled boundaries file is missing; reinstall {.pkg bdhs}.")
    }
    .explore_cache$boundaries <- readRDS(path)
  }
  .explore_cache$boundaries
}

# Official subnational estimates of one indicator, keyed by reg_id.
map_official_data <- function(indicator, survey = NULL, call = rlang::caller_env()) {
  rows <- explore_official_rows(indicator, survey = survey, breakdown = "subnational", preferred = TRUE, call = call)
  rows <- rows[!is.na(rows$region_id) & !is.na(rows$value), ]
  if (!nrow(rows)) {
    cli::cli_abort("{.val {indicator}} has no subnational estimates for this selection.", call = call)
  }
  if (is.null(survey)) {
    rows <- rows[rows$survey_year == max(rows$survey_year), ]
  }
  # One indicator per id (the DHS Program reuses a few ids) and one row per region.
  first <- rows$indicator_order == rows$indicator_order[[1]]
  if (!all(first)) {
    cli::cli_inform(c("i" = "{.val {indicator}} has several definitions; mapping {.val {rows$indicator[[1]]}}."))
    rows <- rows[first, ]
  }
  # Keep the divisions of each round (the API also lists the parent regions
  # of the comparable trend series) and one row per division.
  shp <- map_read_boundaries()
  rows <- rows[rows$region_id %in% shp$reg_id, ]
  rows <- rows[!duplicated(rows[, c("survey_id", "region_id")]), ]
  if (!nrow(rows)) {
    cli::cli_abort("The subnational estimates of {.val {indicator}} do not match the bundled boundaries.", call = call)
  }
  cat_info <- explore_catalog(rows$indicator_id[[1]])
  digits <- rows$precision[[1]]
  list(
    data = tibble::tibble(survey_id = rows$survey_id, key = rows$region_id, value = rows$value),
    key = "reg_id",
    title = rows$indicator[[1]],
    legend = cat_info$measurement_type %|NA|% "Value",
    digits = if (is.na(digits)) NULL else digits,
    source = "Source: DHS Program API (official estimates bundled in bdhs)."
  )
}

# User estimates by region: detect the region and value columns and the round.
map_user_data <- function(x, survey = NULL, value = NULL, call = rlang::caller_env()) {
  names(x) <- tolower(names(x))
  region_col <- first_present(x, c(
    "region_code", "v024", "hv024", "mv024", "region", "division",
    "region_name", "division_name", "division_surveyed"
  ))
  if (is.null(region_col)) {
    cli::cli_abort(
      c(
        "Can't find a region column in {.arg x}.",
        "i" = "Name it {.field region_code} (codes) or {.field region_name} (division names), or use {.field v024}."
      ),
      call = call
    )
  }
  value_col <- if (!is.null(value)) tolower(value) else first_present(x, c("estimate", "value"))
  if (is.null(value_col) || !value_col %in% names(x)) {
    cli::cli_abort(
      c(
        "Can't find the value column to map.",
        "i" = "Name it with {.arg value}, e.g. {.code value = \"estimate\"}."
      ),
      call = call
    )
  }
  if (!is.numeric(x[[value_col]])) {
    cli::cli_abort("Column {.field {value_col}} must be numeric.", call = call)
  }

  sid <- if (!is.null(survey)) {
    bdhs_survey_id(survey, type = c("DHS", "SPA", "OTH"))
  } else if ("survey_id" %in% names(x)) {
    unique(as.character(x$survey_id))
  } else {
    attr(x, "bdhs_survey_id", exact = TRUE)
  }
  if (is.null(sid) || !length(sid) || anyNA(sid)) {
    cli::cli_abort(
      c(
        "Can't tell which survey round {.arg x} comes from.",
        "i" = "Supply {.arg survey}, e.g. {.code survey = 2022}."
      ),
      call = call
    )
  }
  if ("survey_id" %in% names(x)) {
    x <- x[as.character(x$survey_id) %in% sid, ]
  } else if (length(sid) > 1L) {
    cli::cli_abort("Give a single {.arg survey} or a {.field survey_id} column to map several rounds.", call = call)
  } else {
    x$survey_id <- sid
  }
  if ("indicator" %in% names(x) && length(unique(x$indicator)) > 1L) {
    cli::cli_abort(
      c(
        "{.arg x} holds several indicators: {.val {unique(x$indicator)}}.",
        "i" = "Filter it to one indicator before mapping."
      ),
      call = call
    )
  }

  region <- x[[region_col]]
  use_codes <- inherits(region, "haven_labelled") || is.numeric(region) ||
    (is.character(region) && all(grepl("^\\s*\\d+\\s*$", region[!is.na(region)])))
  shp <- map_read_boundaries()
  if (use_codes) {
    key <- as.integer(as_codes(region))
    key_col <- "region_code"
  } else {
    names_key <- explore_region_key(as.character(region))
    shp_rows <- shp[shp$survey_id %in% sid, ]
    lookup <- paste(shp_rows$survey_id, explore_region_key(shp_rows$region_name))
    key <- shp_rows$region_code[match(paste(x$survey_id, names_key), lookup)]
    key_col <- "region_code"
    bad <- unique(as.character(region)[is.na(key) & !is.na(region)])
    if (length(bad)) {
      cli::cli_warn("Unrecognised division name{?s} {.val {bad}} {?was/were} dropped.", call = call)
    }
  }
  d <- tibble::tibble(survey_id = as.character(x$survey_id), key = key, value = as.numeric(x[[value_col]]))
  d <- d[!is.na(d$key), ]
  if (anyDuplicated(d[, c("survey_id", "key")])) {
    cli::cli_abort(
      c(
        "{.arg x} has more than one row per division.",
        "i" = "Map one estimate per division: filter other grouping variables first."
      ),
      call = call
    )
  }
  label <- if ("label" %in% names(x) && length(unique(x$label)) == 1L) unique(x$label) else value_col
  is_estimate <- inherits(x, "bdhs_estimate") || all(c("estimate", "ci_low") %in% names(x))
  list(
    data = d,
    key = key_col,
    title = label,
    legend = if (identical(label, value_col)) value_col else "Estimate",
    digits = NULL,
    source = if (is_estimate) "Estimates computed with bdhs from DHS microdata." else "Estimates: user-supplied data."
  )
}

# Normalised division key: first word, lower case, modern spelling.
explore_region_key <- function(x) {
  k <- tolower(x)
  k <- sub("^[^a-z]+", "", k)
  k <- sub("[^a-z].*$", "", k)
  spelling <- c(barisal = "barishal", chittagong = "chattogram", rajshani = "rajshahi")
  hit <- k %in% names(spelling)
  k[hit] <- spelling[k[hit]]
  k
}

# Division name and value on a translucent white box, readable on any fill.
map_label_layer <- function(labels) {
  no_border <- if (utils::packageVersion("ggplot2") >= "3.5.0") list(linewidth = 0) else list(label.size = 0)
  rlang::exec(
    ggplot2::geom_label,
    data = labels,
    mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$text),
    inherit.aes = FALSE, size = 2.6, lineheight = 0.9, colour = "grey10", fill = "#FFFFFFB3",
    label.padding = ggplot2::unit(0.12, "lines"), !!!no_border
  )
}

# Label positions: a point inside each polygon, kept inside the map frame.
map_label_points <- function(shp, digits = NULL) {
  pts <- suppressWarnings(suppressMessages(sf::st_point_on_surface(sf::st_geometry(shp))))
  xy <- sf::st_coordinates(pts)
  # Keep labels of coastal divisions (Chattogram) inside the map frame.
  bbox <- sf::st_bbox(shp)
  margin <- 0.1 * (bbox[["xmax"]] - bbox[["xmin"]])
  xy[, "X"] <- pmin(pmax(xy[, "X"], bbox[["xmin"]] + margin), bbox[["xmax"]] - margin)
  v <- shp$value
  if (is.null(digits)) {
    digits <- if (all(is.na(v))) {
      1L
    } else if (all(v == round(v), na.rm = TRUE)) {
      0L
    } else if (max(abs(v), na.rm = TRUE) < 1) {
      2L
    } else {
      1L
    }
  }
  value_text <- ifelse(is.na(v), "n/a", formatC(v, format = "f", digits = digits))
  name <- sub("\\s*\\(.*$", "", shp$region_name)
  tibble::tibble(
    x = xy[, "X"],
    y = xy[, "Y"],
    year_label = shp$year_label,
    text = paste0(name, "\n", value_text)
  )
}
