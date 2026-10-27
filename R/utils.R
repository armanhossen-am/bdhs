# Internal helpers shared across the package. Nothing here is exported.

check_installed <- function(pkg, reason) {
  rlang::check_installed(pkg, reason = reason)
}

# Upper-case, trimmed character vector; NULL stays NULL.
toupper_trim <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  toupper(trimws(as.character(x)))
}

# Attach the haven "label" attribute to a column without touching its class.
set_var_label <- function(x, label) {
  attr(x, "label") <- label
  x
}

# The variable label of a column, or NA.
var_label <- function(x) {
  lab <- attr(x, "label", exact = TRUE)
  if (is.null(lab) || length(lab) != 1L) NA_character_ else as.character(lab)
}

# Strip haven classes while keeping the underlying codes.
zap_all <- function(x) {
  haven::zap_labels(haven::zap_missing(x))
}

# Turn a vector into a plain numeric code vector, whatever its class.
as_codes <- function(x) {
  if (is.factor(x)) {
    return(as.integer(x))
  }
  as.numeric(zap_all(x))
}

# Lower-case column names, which is how bdhs returns DHS recode files.
lower_names <- function(df) {
  names(df) <- tolower(names(df))
  df
}

# First column of `candidates` that exists in `data`, or NULL.
first_present <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit)) hit[[1]] else NULL
}

# Format a vector for cli messages: {.val a}, {.val b}.
cli_vals <- function(x) {
  paste0("{.val ", x, "}", collapse = ", ")
}

# Package-level options with defaults.
bdhs_opt <- function(name, default = NULL) {
  getOption(paste0("bdhs.", name), default)
}

`%|NA|%` <- function(x, y) {
  ifelse(is.na(x), y, x)
}

# Escape braces so arbitrary text (file names, labels) can go into cli messages.
cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}
