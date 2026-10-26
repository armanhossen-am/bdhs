# Describing data frames ----------------------------------------------------------------

#' Codebook of a data frame
#'
#' Summarises every column of a data frame: its variable label, type, number
#' of valid and missing values and its declared value labels. Works on any
#' data frame, including data from [bdhs_read()] and [bdhs_simulate()].
#'
#' Only metadata is reported. No data values are shown, apart from the
#' declared label sets (value labels of labelled columns and factor levels),
#' so the result is safe to share even for licensed microdata.
#'
#' @param data A data frame.
#'
#' @return A tibble with one row per column: `variable`, `label`, `type`,
#'   `n_valid`, `n_missing`, `n_value_labels` and `value_labels` (compact
#'   `"value=label; ..."` string for labelled columns, `"level; ..."` for
#'   factors). The `bdhs_survey_id` and `bdhs_recode` attributes of `data`
#'   are kept.
#' @seealso [bdhs_variable()] for a variable across rounds in the bundled
#'   codebook.
#' @export
#' @examples
#' ir <- bdhs_simulate(2022, "IR", n_clusters = 4, households = 5)
#' bdhs_describe(ir)
bdhs_describe <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.")
  }
  cols <- as.list(data)
  n_missing <- vapply(cols, function(x) sum(is.na(x)), integer(1), USE.NAMES = FALSE)
  label_sets <- lapply(cols, describe_label_set)
  out <- tibble::tibble(
    variable = names(data),
    label = vapply(cols, var_label, character(1), USE.NAMES = FALSE),
    type = vapply(cols, describe_type, character(1), USE.NAMES = FALSE),
    n_valid = nrow(data) - n_missing,
    n_missing = n_missing,
    n_value_labels = vapply(label_sets, function(s) length(s$labels), integer(1), USE.NAMES = FALSE),
    value_labels = vapply(label_sets, function(s) s$string, character(1), USE.NAMES = FALSE)
  )
  for (a in c("bdhs_survey_id", "bdhs_recode")) {
    attr(out, a) <- attr(data, a, exact = TRUE)
  }
  out
}

# Compact type names, consistent with the bundled codebook.
describe_type <- function(x) {
  if (inherits(x, "haven_labelled")) {
    return("haven_labelled")
  }
  if (is.ordered(x)) {
    return("ordered")
  }
  if (is.factor(x)) {
    return("factor")
  }
  if (inherits(x, "Date")) {
    return("Date")
  }
  if (inherits(x, "POSIXt")) {
    return("datetime")
  }
  if (is.character(x)) {
    return("character")
  }
  if (is.logical(x)) {
    return("logical")
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (is.numeric(x)) {
    return("numeric")
  }
  class(x)[[1]]
}

# Declared label set of a column: value labels or factor levels (never data).
describe_label_set <- function(x) {
  if (is.factor(x)) {
    lev <- levels(x)
    return(list(labels = lev, string = paste(lev, collapse = "; ")))
  }
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels) || !length(labels)) {
    return(list(labels = character(), string = ""))
  }
  list(labels = names(labels), string = describe_label_string(unname(labels), names(labels)))
}

# "0=no; 1=yes" from values and labels; tagged missing values show as NA(a).
describe_label_string <- function(values, labels) {
  if (!length(values)) {
    return("")
  }
  shown <- if (is.numeric(values)) {
    out <- vapply(values, function(v) {
      if (is.na(v)) {
        return("NA")
      }
      format(v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
    }, character(1))
    if (is.double(values)) {
      tagged <- haven::is_tagged_na(values)
      out[tagged] <- paste0("NA(", haven::na_tag(values[tagged]), ")")
    }
    out
  } else {
    as.character(values)
  }
  paste0(shown, "=", labels, collapse = "; ")
}
