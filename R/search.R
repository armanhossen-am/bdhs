# Searching the bundled metadata -----------------------------------------------------

#' Search variables, indicators and harmonized variables
#'
#' One search box for everything bundled with the package: variable names and
#' labels of the DHS recode files ([bdhs_codebook]), official indicator ids,
#' labels and definitions ([bdhs_indicator_catalog]) and the cross-round
#' harmonized variables used by [bdhs_harmonize()]. Works offline and needs no
#' microdata.
#'
#' @param pattern A regular expression, matched case-insensitively. Invalid
#'   regular expressions are matched as plain text.
#' @param where Where to search: `"all"` (default), or any of `"variables"`,
#'   `"indicators"` and `"harmonized"`.
#' @param survey Optional survey references (see [bdhs_survey_id()]); keeps
#'   results available in at least one of these rounds.
#' @param recode Optional recode codes (e.g. `"IR"`, `"KR"`); restricts
#'   variables and harmonized variables to these recodes. Indicators are not
#'   affected.
#' @param value_labels If `TRUE`, also search the value labels of variables
#'   (e.g. `"pentavalent"` finds variables with such a category).
#'
#' @return A tibble of class `bdhs_search` with columns `source`
#'   (`"variable"`, `"indicator"` or `"harmonized"`), `id` (variable name,
#'   indicator id or harmonized name), `label`, `recode` and `surveys` (comma
#'   separated survey ids). It prints compactly; use `print(x, n = Inf)` to
#'   see every match or [tibble::as_tibble()] for a plain tibble.
#' @seealso [bdhs_variable()] for one variable across rounds,
#'   [bdhs_official()] for the official estimates of an indicator.
#' @export
#' @examples
#' bdhs_search("stunt")
#' bdhs_search("region|division", where = "variables", recode = "IR")
#' bdhs_search("anemia", where = "indicators", survey = 2011)
bdhs_search <- function(pattern, where = c("all", "variables", "indicators", "harmonized"),
                        survey = NULL, recode = NULL, value_labels = FALSE) {
  if (!rlang::is_string(pattern) || !nzchar(pattern)) {
    cli::cli_abort("{.arg pattern} must be a single non-empty string.")
  }
  where <- rlang::arg_match(where, multiple = TRUE)
  if ("all" %in% where) {
    where <- c("variables", "indicators", "harmonized")
  }
  sid <- if (!is.null(survey)) bdhs_survey_id(survey) else NULL
  recode <- if (!is.null(recode)) toupper_trim(recode) else NULL

  parts <- list(
    variables = if ("variables" %in% where) search_variables(pattern, sid, recode, value_labels),
    indicators = if ("indicators" %in% where) search_indicators(pattern, sid),
    harmonized = if ("harmonized" %in% where) search_harmonized(pattern, sid, recode)
  )
  out <- do.call(dplyr::bind_rows, c(list(search_empty()), unname(parts)))
  out <- out[, c("source", "id", "label", "recode", "surveys")]
  structure(out, class = c("bdhs_search", class(out)), pattern = pattern)
}

#' @export
print.bdhs_search <- function(x, ..., n = 10) {
  needed <- c("source", "id", "label", "recode", "surveys")
  if (!all(needed %in% names(x))) {
    return(NextMethod())
  }
  pattern <- attr(x, "pattern", exact = TRUE) %||% ""
  cli::cat_line(cli::style_bold(paste0(
    "# bdhs search for \"", pattern, "\": ", nrow(x), " match", if (nrow(x) != 1L) "es"
  )))
  if (!nrow(x)) {
    cli::cat_line("No matches. Try a shorter or broader pattern, or `value_labels = TRUE`.")
    return(invisible(x))
  }
  titles <- c(variable = "Variables", indicator = "Indicators", harmonized = "Harmonized variables")
  width <- cli::console_width()
  truncated <- FALSE
  for (src in intersect(names(titles), unique(x$source))) {
    rows <- x[x$source == src, ]
    cli::cat_line()
    cli::cat_line(cli::rule(left = paste0(titles[[src]], " (", nrow(rows), ")"), width = min(width, 80)))
    shown <- utils::head(rows, n)
    id <- if (src == "variable") paste0(shown$id, " [", shown$recode, "]") else shown$id
    id <- format(id)
    surveys <- vapply(shown$surveys, search_format_surveys, character(1), USE.NAMES = FALSE)
    room <- max(20L, width - nchar(id[[1]]) - 4L - min(28L, max(nchar(surveys))))
    label <- search_truncate(shown$label, room)
    cli::cat_line(paste0(id, "  ", format(label), "  ", cli::col_grey(surveys)))
    if (nrow(rows) > n) {
      truncated <- TRUE
      cli::cat_line(cli::col_grey(paste0("... and ", nrow(rows) - n, " more")))
    }
  }
  if (truncated) {
    cli::cat_line()
    cli::cat_line(cli::col_grey("# Use `print(x, n = Inf)` to see every match."))
  }
  invisible(x)
}

#' One variable across survey rounds
#'
#' Shows how a DHS recode variable is labelled and coded in every round in
#' which it appears, using the bundled [bdhs_codebook] and
#' [bdhs_value_labels]. Label changes between rounds (e.g. `v024` "region" vs
#' "division", "barisal" vs "barishal") are flagged so you know when a
#' cross-round comparison needs care.
#'
#' @param name A single variable name, e.g. `"v024"` (case-insensitive).
#' @param recode Optional recode codes to restrict to, e.g. `"IR"`.
#'
#' @return A tibble with one row per survey round and recode: `survey_id`,
#'   `recode`, `label`, `n_valid` (number of non-missing values in the source
#'   file), `value_labels` (compact `"value=label; ..."` string) and
#'   `label_drift` (`TRUE` when the variable label or value labels differ from
#'   those of the most recent round).
#' @seealso [bdhs_search()] to find variables.
#' @export
#' @examples
#' bdhs_variable("v024")
#' bdhs_variable("h3", recode = "KR")
bdhs_variable <- function(name, recode = NULL) {
  if (!rlang::is_string(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single variable name.")
  }
  name <- tolower(trimws(name))
  recode <- if (!is.null(recode)) toupper_trim(recode) else NULL
  cb <- bdhs::bdhs_codebook
  rows <- cb[cb$variable == name, ]
  if (!nrow(rows)) {
    cli::cli_abort(c(
      "Variable {.field {name}} is not in the bundled codebook.",
      "i" = "Search names and labels with {.code bdhs_search({.val {name}})}."
    ))
  }
  if (!is.null(recode)) {
    found_in <- sort(unique(rows$recode))
    rows <- rows[rows$recode %in% recode, ]
    if (!nrow(rows)) {
      cli::cli_abort(c(
        "Variable {.field {name}} is not in recode{?s} {.val {recode}}.",
        "i" = "It appears in {.val {found_in}}."
      ))
    }
  }
  rows <- rows[!duplicated(rows[, c("survey_id", "recode")]), ]
  rows <- rows[order(search_survey_year(rows$survey_id), match(rows$recode, bdhs_recodes()$recode)), ]

  vl <- bdhs::bdhs_value_labels
  vl <- vl[vl$variable == name & vl$survey_id %in% rows$survey_id, ]
  vl <- vl[!duplicated(vl[, c("survey_id", "recode", "value", "label")]), ]
  vl <- vl[order(vl$survey_id, vl$recode, vl$value), ]
  vl_key <- paste(vl$survey_id, vl$recode)
  vl_string <- vapply(split(seq_len(nrow(vl)), vl_key), function(i) {
    describe_label_string(vl$value[i], vl$label[i])
  }, character(1))
  value_labels <- unname(vl_string[paste(rows$survey_id, rows$recode)])
  value_labels[is.na(value_labels)] <- ""

  norm <- function(x) tolower(gsub("\\s+", " ", trimws(x %|NA|% "")))
  signature <- paste(norm(rows$label), norm(value_labels), sep = "\r")
  latest <- signature[[nrow(rows)]]
  tibble::tibble(
    survey_id = rows$survey_id,
    recode = rows$recode,
    label = rows$label,
    n_valid = rows$n_valid,
    value_labels = value_labels,
    label_drift = signature != latest
  )
}

# Internals ----------------------------------------------------------------------------

search_empty <- function() {
  tibble::tibble(
    source = character(), id = character(), label = character(),
    recode = character(), surveys = character()
  )
}

# Survey year (for chronological ordering) of survey ids.
search_survey_year <- function(survey_id) {
  .rounds$year[match(survey_id, .rounds$survey_id)]
}

# Collapse survey ids into a chronologically ordered comma-separated string.
search_collapse_surveys <- function(survey_id) {
  ids <- unique(survey_id)
  paste(ids[order(search_survey_year(ids))], collapse = ",")
}

# "BD1994DHS,BD2022DHS" -> "1993-94, 2022"; all nine DHS rounds -> "all 9 DHS rounds".
search_format_surveys <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return("")
  }
  ids <- strsplit(x, ",", fixed = TRUE)[[1]]
  dhs <- bdhs_survey_id(type = "DHS")
  if (setequal(ids, dhs)) {
    return("all 9 DHS rounds")
  }
  lab <- survey_label(ids) %|NA|% ids
  lab <- sub("^(\\d{4})-\\d{2}(\\d{2})$", "\\1-\\2", lab)
  paste(lab, collapse = ", ")
}

search_truncate <- function(x, width) {
  x <- x %|NA|% ""
  long <- nchar(x) > width
  x[long] <- paste0(substr(x[long], 1, width - 3L), "...")
  x
}

# Rank: exact id match first, then id/label matches, then everything else.
search_rank <- function(pattern, id, label) {
  exact <- tolower(id) == tolower(pattern)
  direct <- explore_grepl(pattern, id) | explore_grepl(pattern, label)
  ifelse(exact, 0L, ifelse(direct, 1L, 2L))
}

search_variables <- function(pattern, survey_id = NULL, recode = NULL, value_labels = FALSE) {
  cb <- bdhs::bdhs_codebook
  if (!is.null(survey_id)) cb <- cb[cb$survey_id %in% survey_id, ]
  if (!is.null(recode)) cb <- cb[cb$recode %in% recode, ]
  if (!nrow(cb)) {
    return(NULL)
  }
  cb$key <- paste(cb$variable, cb$recode, sep = "\r")
  cb$pos <- seq_len(nrow(cb))
  hit <- explore_grepl(pattern, cb$variable) | explore_grepl(pattern, cb$label)
  keys <- unique(cb$key[hit])
  if (isTRUE(value_labels)) {
    vl <- bdhs::bdhs_value_labels
    vl <- vl[explore_grepl(pattern, vl$label), ]
    keys <- union(keys, paste(vl$variable, vl$recode, sep = "\r"))
  }
  rows <- cb[cb$key %in% keys, ]
  if (!nrow(rows)) {
    return(NULL)
  }
  rows$year <- search_survey_year(rows$survey_id)
  # Latest round first within each variable, so its label is the one shown.
  rows <- rows[order(rows$key, -rows$year), ]
  out <- rows[!duplicated(rows$key), c("key", "variable", "recode", "label")]
  names(out)[names(out) == "variable"] <- "id"
  all_labels <- tapply(rows$label, rows$key, function(l) paste(unique(l), collapse = " | "))
  surveys <- tapply(rows$survey_id, rows$key, search_collapse_surveys)
  pos <- tapply(rows$pos, rows$key, min)
  out$all_labels <- unname(all_labels[out$key])
  out$surveys <- unname(surveys[out$key])
  out$pos <- unname(pos[out$key])
  out$rank <- search_rank(pattern, out$id, out$all_labels)
  out <- out[order(out$rank, match(out$recode, bdhs_recodes()$recode), out$pos), ]
  tibble::tibble(source = "variable", id = out$id, label = out$label, recode = out$recode, surveys = out$surveys)
}

search_indicators <- function(pattern, survey_id = NULL) {
  catalog <- bdhs::bdhs_indicator_catalog
  text <- paste(catalog$short_name, catalog$definition, sep = " | ")
  hit <- explore_grepl(pattern, catalog$indicator_id) | explore_grepl(pattern, catalog$label) |
    explore_grepl(pattern, text)
  if (!is.null(survey_id)) {
    avail <- vapply(strsplit(catalog$surveys %|NA|% "", ",", fixed = TRUE), function(s) any(s %in% survey_id), logical(1))
    hit <- hit & avail
  }
  rows <- catalog[hit, ]
  if (!nrow(rows)) {
    return(NULL)
  }
  rank <- search_rank(pattern, rows$indicator_id, rows$label)
  rows <- rows[order(rank, rows$indicator_order), ]
  tibble::tibble(
    source = "indicator", id = rows$indicator_id, label = rows$label,
    recode = NA_character_, surveys = rows$surveys
  )
}

search_harmonized <- function(pattern, survey_id = NULL, recode = NULL) {
  vars <- search_crosswalk_vars()
  if (!length(vars)) {
    return(NULL)
  }
  info <- lapply(vars, function(v) {
    rounds <- v$rounds %||% list()
    avail <- vapply(rounds, function(r) !identical(r$method, "unavailable"), logical(1))
    files <- unique(unlist(lapply(rounds[avail], function(r) r$file)))
    src_vars <- unique(unlist(lapply(rounds[avail], function(r) r$var)))
    list(
      id = v$name %||% NA_character_,
      label = v$label %||% NA_character_,
      recode = if (length(files)) paste(files, collapse = ",") else NA_character_,
      files = files,
      surveys = search_collapse_surveys(names(rounds)[avail]),
      survey_ids = names(rounds)[avail],
      text = paste(c(v$name, v$label, v$notes, src_vars), collapse = " | ")
    )
  })
  id <- vapply(info, `[[`, character(1), "id")
  label <- vapply(info, `[[`, character(1), "label")
  text <- vapply(info, `[[`, character(1), "text")
  hit <- explore_grepl(pattern, text)
  if (!is.null(survey_id)) {
    hit <- hit & vapply(info, function(i) any(i$survey_ids %in% survey_id), logical(1))
  }
  if (!is.null(recode)) {
    hit <- hit & vapply(info, function(i) any(i$files %in% recode), logical(1))
  }
  if (!any(hit)) {
    return(NULL)
  }
  rank <- search_rank(pattern, id, label)
  idx <- which(hit)
  idx <- idx[order(rank[idx], idx)]
  tibble::tibble(
    source = "harmonized", id = id[idx], label = label[idx],
    recode = vapply(info[idx], `[[`, character(1), "recode"),
    surveys = vapply(info[idx], `[[`, character(1), "surveys")
  )
}

# The harmonized variable definitions shipped in inst/extdata/crosswalk.rds.
.explore_cache <- new.env(parent = emptyenv())

search_crosswalk_vars <- function() {
  if (is.null(.explore_cache$crosswalk_vars)) {
    path <- system.file("extdata", "crosswalk.rds", package = "bdhs")
    .explore_cache$crosswalk_vars <- if (nzchar(path)) readRDS(path)$variables else list()
  }
  .explore_cache$crosswalk_vars
}
