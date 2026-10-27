# DHS special codes -> NA ------------------------------------------------------------

# Value-label patterns that mark DHS special codes (matched case-insensitively
# against the label text). "flush, don't know where" is a real category, so the
# don't-know pattern only matches labels that *start* with it.
.na_label_patterns <- c(
  missing = "\\bmissing\\b",
  dont_know = "^(don['’]?t|dont|do not) know\\b|^dk\\b",
  not_dejure = "\\bnot (a )?de[- ]?jure\\b|\\bnon[- ]?de[- ]?jure\\b",
  flagged = "\\bflagged\\b",
  implausible = "implausible|plausible limits?",
  inconsistent = "\\binconsistent\\b",
  not_applicable = "\\bnot applicable\\b|^n/?a$"
)

# Anthropometry: z-scores, weights, heights and BMI use 9994-9999 for
# "not present", "refused", "out of plausible limits", "flagged" and
# "missing" (not always labelled, e.g. 9999 in 2007/2011).
.anthro_pattern <- "^(hw[0-9]+[a-z]?|hc[0-9]+[a-z]?|ha[0-9]+[a-z]?|hb[0-9]+[a-z]?|v4(3[7-9]|4[0-9])[a-z]?)$"
.anthro_threshold <- 9994

# Label regex for the requested kinds of special codes.
na_label_regex <- function(dont_know = TRUE) {
  pat <- .na_label_patterns
  if (!dont_know) {
    pat <- pat[names(pat) != "dont_know"]
  }
  paste0("(", pat, ")", collapse = "|")
}

# Codes of a labelled vector whose labels mark special codes.
special_label_codes <- function(x, dont_know = TRUE) {
  labs <- attr(x, "labels", exact = TRUE)
  if (is.null(labs) || !length(labs) || is.character(labs)) {
    return(numeric())
  }
  hit <- grepl(na_label_regex(dont_know), tolower(trimws(names(labs))), perl = TRUE)
  unname(as.numeric(labs[hit]))
}

#' Set DHS special codes to `NA`
#'
#' DHS recode files store "missing", "don't know", "not a de jure resident",
#' "flagged" and similar answers as ordinary codes (often 8/9, 98/99,
#' 997/998 or 9996-9999). Left in place they silently distort means and
#' regressions. `bdhs_na()` turns them into `NA`:
#'
#' * labelled values whose labels say *missing*, *don't know* / *dk*,
#'   *not (a) de jure resident*, *flagged*, *implausible* / *out of plausible
#'   limits*, *inconsistent* or *not applicable*;
#' * anthropometry codes 9994-9999 in child and adult height, weight, z-score
#'   and BMI variables (`hw*`, `hc*`, `ha*`, `hb*`, `v437`-`v449`), which are
#'   not always labelled;
#' * Stata extended missing values (`haven::tagged_na()`, e.g. `.a` in 2022
#'   files) and SPSS user-defined missing values, which become ordinary `NA`.
#'
#' Codes that are substantive answers keep their value, including categories
#' such as "flush to don't know where" (toilet type) or "other".
#'
#' @param x A vector (labelled, numeric or factor) or a data frame.
#' @param cols For data frames, the columns to clean (tidyselect, e.g.
#'   `c(v133, hw70)` or `starts_with("hw")`). `NULL` (default) cleans every
#'   column.
#' @param dont_know `"na"` (default) turns "don't know" answers into `NA`;
#'   `"keep"` keeps them, for indicators where DHS counts "don't know" as a
#'   valid answer (usually as "no").
#' @return `x` with special codes replaced by `NA`. Labelled vectors stay
#'   labelled; the labels of the removed codes are dropped.
#' @export
#' @examples
#' x <- haven::labelled(
#'   c(1, 2, 8, 9, 1),
#'   c(yes = 1, no = 2, "don't know" = 8, missing = 9)
#' )
#' bdhs_na(x)
#' bdhs_na(x, dont_know = "keep")
#'
#' kr <- bdhs_simulate(2022, "KR")
#' summary(kr$hw70)
#' summary(bdhs_na(kr, cols = hw70)$hw70)
bdhs_na <- function(x, cols = NULL, dont_know = c("na", "keep")) {
  dont_know <- rlang::arg_match(dont_know)
  dk <- dont_know == "na"
  if (!is.data.frame(x)) {
    nm <- rlang::as_label(rlang::enexpr(x))
    return(na_vector(x, name = nm, dont_know = dk))
  }
  cols_q <- rlang::enquo(cols)
  sel <- if (rlang::quo_is_null(cols_q)) {
    names(x)
  } else {
    names(dplyr::select(x[0, , drop = FALSE], !!cols_q))
  }
  n_changed <- 0
  touched <- character()
  for (nm in sel) {
    old <- x[[nm]]
    new <- na_vector(old, name = nm, dont_know = dk)
    k <- sum(is.na(new)) - sum(is.na(as_plain(old)))
    if (k > 0) {
      n_changed <- n_changed + k
      touched <- c(touched, nm)
    }
    if (!identical(old, new, num.eq = FALSE, single.NA = FALSE)) {
      x[[nm]] <- new
    }
  }
  if (n_changed > 0) {
    cli::cli_inform(c(
      "i" = "Set {n_changed} special code{?s} to {.val NA} in {length(touched)} column{?s}: {.var {touched}}."
    ))
  }
  x
}

# NA count must ignore tagged NAs etc., which are already NA.
as_plain <- function(x) {
  if (is.factor(x) || is.character(x)) x else unclass(x)
}

# Set elements to a plain NA, whatever the class (labelled, tagged NA).
set_na <- function(x, idx) {
  cls <- oldClass(x)
  y <- unclass(x)
  y[idx] <- NA
  class(y) <- cls
  y
}

# Clean one vector. `name` (the column name) enables the anthropometry rule.
na_vector <- function(x, name = NULL, dont_know = TRUE) {
  if (is.null(x) || is.list(x)) {
    return(x)
  }
  if (is.factor(x)) {
    bad <- grepl(na_label_regex(dont_know), tolower(trimws(levels(x))), perl = TRUE)
    if (any(bad)) {
      x[x %in% levels(x)[bad]] <- NA
      x <- factor(x, levels = levels(x)[!bad], ordered = is.ordered(x))
    }
    return(x)
  }
  if (inherits(x, "haven_labelled_spss")) {
    x <- haven::zap_missing(x)
  }
  if (is.character(x) || is.logical(x)) {
    return(x)
  }
  if (is.double(x)) {
    tagged <- haven::is_tagged_na(x)
    if (any(tagged)) {
      x <- set_na(x, tagged)
      labs <- attr(x, "labels", exact = TRUE)
      if (!is.null(labs) && is.double(labs)) {
        attr(x, "labels") <- labs[!haven::is_tagged_na(labs)]
      }
    }
  }
  codes <- special_label_codes(x, dont_know)
  vals <- as.numeric(unclass(x))
  drop <- if (length(codes)) vals %in% codes else logical(length(vals))
  if (!is.null(name) && length(name) == 1L && grepl(.anthro_pattern, tolower(name))) {
    drop <- drop | (!is.na(vals) & vals >= .anthro_threshold)
  }
  if (any(drop)) {
    x <- set_na(x, drop)
  }
  if (length(codes)) {
    labs <- attr(x, "labels", exact = TRUE)
    attr(x, "labels") <- labs[!as.numeric(labs) %in% codes]
  }
  x
}
