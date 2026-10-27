# Indicator registry ------------------------------------------------------------
#
# Every indicator the package can compute is an object created by
# new_indicator(). Definitions live in R/indicator-defs-*.R, grouped by topic,
# and are assembled lazily into the registry the first time it is needed.
#
# The calculations port the logic of the DHS Program's "DHS-Indicators-R" code
# (https://github.com/DHSProgram/DHS-Indicators-R) and the Guide to DHS
# Statistics (DHS-7, 2018), re-implemented for this package and adapted to
# Bangladesh-specific questionnaire codes. Every round-specific rule was checked
# against the official values published by the DHS Program API wherever the
# maintainer's local files allowed it (see bdhs_validate()).

.dhs_rounds <- c(
  "BD1994DHS", "BD1997DHS", "BD2000DHS", "BD2004DHS", "BD2007DHS",
  "BD2011DHS", "BD2014DHS", "BD2017DHS", "BD2022DHS"
)

.indicator_types <- c("proportion", "mean", "median", "rate")

# Create one indicator definition (internal).
#
# universe(d, survey) returns a logical vector (the denominator population),
# value(d, survey) a numeric vector (0/1 for proportions). Both receive the
# data with lower-case DHS names and the canonical survey id, so round-specific
# codes are handled inside the functions. `all_women` names the all-woman
# factor for indicators whose denominator is all women although only
# ever-married women were interviewed (ratio estimator
# sum(w * y) / sum(w * awfact / 100)). `api_by` is the DHS API
# ByVariableLabel (time window) to compare with; a named vector gives
# round-specific labels, with the unnamed/"default" element as fallback.
new_indicator <- function(id, label, recode, topic, universe = NULL, value = NULL,
                          type = "proportion", api_id = NA_character_, api_by = NA_character_,
                          all_women = NULL, window = NA_integer_, rounds = .dhs_rounds,
                          vars = character(), definition = NA_character_, notes = NA_character_,
                          reference = NA_character_, params = list(), rate = NULL,
                          tol_scale = 1, requires = character()) {
  stopifnot(
    is.character(id), length(id) == 1L, grepl("^[a-z][a-z0-9_]*$", id),
    is.character(label), length(label) == 1L,
    all(recode %in% c("IR", "KR", "BR", "PR", "HR", "MR")),
    type %in% .indicator_types,
    all(rounds %in% .dhs_rounds)
  )
  if (type == "rate") {
    stopifnot(is.function(rate))
  } else {
    stopifnot(is.function(universe), is.function(value))
  }
  structure(
    list(
      id = id, label = label, recode = recode, topic = topic, type = type,
      universe = universe, value = value, rate = rate,
      api_id = api_id, api_by = api_by, all_women = all_women,
      window = as.integer(window), rounds = rounds, vars = vars,
      definition = definition, notes = notes, reference = reference,
      params = params, tol_scale = tol_scale, requires = requires
    ),
    class = "bdhs_indicator"
  )
}

#' @export
print.bdhs_indicator <- function(x, ...) {
  cat("<bdhs_indicator> ", x$id, ": ", x$label, "\n", sep = "")
  cat("* Recode: ", paste(x$recode, collapse = ", "), "; type: ", x$type, "; topic: ", x$topic, "\n", sep = "")
  cat("* DHS API id: ", paste(x$api_id, collapse = " + "), "\n", sep = "")
  cat("* Rounds: ", paste(x$rounds, collapse = ", "), "\n", sep = "")
  if (!is.na(x$definition)) cat(strwrap(x$definition, prefix = "  "), sep = "\n")
  invisible(x)
}

abort_wrong_recode <- function(inds, wrong, recode, call = rlang::caller_env()) {
  bad <- names(inds)[wrong]
  need <- unique(unlist(lapply(inds[wrong], `[[`, "recode")))
  cli::cli_abort(c(
    "{cli::qty(length(bad))}Indicator{?s} {.val {bad}} {?is/are} not computed from {recode} data.",
    "i" = "Use the {.val {need}} recode{cli::qty(length(need))}{?s}."
  ), call = call)
}

.registry_env <- new.env(parent = emptyenv())

# All indicator definitions, as a named list (built once per session).
indicator_registry <- function() {
  if (is.null(.registry_env$registry)) {
    defs <- c(
      defs_family_planning(), defs_marriage_fertility(), defs_maternal(),
      defs_child_health(), defs_nutrition(), defs_household(),
      defs_education_empowerment(), defs_biomarkers(), defs_rates()
    )
    ids <- vapply(defs, `[[`, character(1), "id")
    if (anyDuplicated(ids)) {
      cli::cli_abort("Duplicated indicator id{?s}: {.val {ids[duplicated(ids)]}}.", .internal = TRUE)
    }
    names(defs) <- ids
    .registry_env$registry <- defs
  }
  .registry_env$registry
}

# Resolve indicator ids (or bdhs_indicator objects) to definitions.
resolve_indicators <- function(indicators, call = rlang::caller_env()) {
  reg <- indicator_registry()
  if (inherits(indicators, "bdhs_indicator")) {
    return(list(indicators))
  }
  if (is.list(indicators) && all(vapply(indicators, inherits, logical(1), "bdhs_indicator"))) {
    return(indicators)
  }
  if (!is.character(indicators) || !length(indicators)) {
    cli::cli_abort("{.arg indicators} must be a character vector of indicator ids.", call = call)
  }
  indicators <- translate_indicator_ids(indicators, reg)
  bad <- setdiff(indicators, names(reg))
  if (length(bad)) {
    close <- unique(unlist(lapply(tolower(bad), function(b) {
      names(reg)[utils::adist(b, names(reg)) <= 3]
    })))
    cli::cli_abort(
      c(
        "Unknown indicator id{?s}: {.val {bad}}.",
        "i" = if (length(close)) "Did you mean {.val {utils::head(close, 5)}}?",
        "i" = "See {.fn bdhs_indicator_list} or {.code bdhs_search(\"<keyword>\", where = \"computable\")}."
      ),
      call = call
    )
  }
  reg[unique(indicators)]
}

# Map ids given in another case ("RH_DEL_CSECTION") or as DHS API ids
# ("RH_DELA_C_CSC") to registry ids, telling the user about API translations.
translate_indicator_ids <- function(ids, reg = indicator_registry()) {
  lower <- tolower(ids)
  ids[lower %in% names(reg)] <- lower[lower %in% names(reg)]
  unknown <- which(!ids %in% names(reg))
  if (!length(unknown)) {
    return(ids)
  }
  from_api <- character()
  for (i in unknown) {
    hit <- names(reg)[vapply(reg, function(x) toupper(ids[[i]]) %in% x$api_id, logical(1))]
    if (length(hit) == 1L) {
      from_api[[ids[[i]]]] <- hit
      ids[[i]] <- hit
    }
  }
  if (length(from_api)) {
    cli::cli_inform(
      "Using bdhs indicator{cli::qty(length(from_api))}{?s} {.val {unname(from_api)}} for DHS indicator id{cli::qty(length(from_api))}{?s} {.val {names(from_api)}}."
    )
  }
  ids
}

# Indicators applicable to a recode (and optionally a survey round).
applicable_indicators <- function(recode, survey_id = NULL, with_api = FALSE) {
  reg <- indicator_registry()
  keep <- vapply(reg, function(ind) {
    ok <- recode %in% ind$recode
    if (!is.null(survey_id)) ok <- ok && all(survey_id %in% ind$rounds)
    if (with_api) ok <- ok && !all(is.na(ind$api_id))
    ok
  }, logical(1))
  names(reg)[keep]
}

#' List the indicators bdhs can compute
#'
#' Returns the registry of indicators that [bdhs_estimate()],
#' [bdhs_add_indicators()] and [bdhs_validate()] understand: what each one
#' measures, which recode file it is computed from, the denominator rules,
#' the matching DHS Program API indicator and the survey rounds it supports.
#'
#' Definitions follow the Guide to DHS Statistics and port the logic of the
#' DHS Program's DHS-Indicators-R code
#' (<https://github.com/DHSProgram/DHS-Indicators-R>), adapted to the
#' Bangladesh questionnaires (for example the country-specific list of
#' medically trained providers, which changes between rounds, and the 2022
#' long/short women's questionnaire design). The `validated` column lists the
#' rounds for which the package's computation was checked against the
#' official DHS value with the maintainer's copy of the microdata.
#'
#' @param topic Optional topic(s) to keep, e.g. `"family_planning"`,
#'   `"maternal_health"`, `"child_health"`, `"nutrition"`. Matching is
#'   case-insensitive.
#' @param recode Optional recode code(s) (`"IR"`, `"KR"`, `"BR"`, `"PR"`, ...)
#'   to keep only indicators computed from those files.
#'
#' @return A tibble with one row per indicator and columns `id`, `label`,
#'   `topic`, `recode`, `type` (`"proportion"`, `"mean"`, `"median"` or
#'   `"rate"`), `api_id`, `all_women` (all-woman factor used for all-women
#'   denominators, or `NA`), `window` (reference period in months, if any),
#'   `rounds` and `validated` (list columns of survey ids), `definition`,
#'   `notes` and `reference`.
#' @export
#' @examples
#' bdhs_indicator_list()
#' bdhs_indicator_list(topic = "family_planning")
#' bdhs_indicator_list(recode = "KR")
bdhs_indicator_list <- function(topic = NULL, recode = NULL) {
  reg <- indicator_registry()
  val <- indicator_validation_status()
  field <- function(f, type = character(1)) unname(vapply(reg, f, type))
  out <- tibble::tibble(
    id = names(reg),
    label = field(function(x) x$label),
    topic = field(function(x) x$topic),
    recode = field(function(x) paste(x$recode, collapse = ", ")),
    type = field(function(x) x$type),
    api_id = field(function(x) paste(x$api_id, collapse = " + ")),
    all_women = field(function(x) x$all_women %||% NA_character_),
    window = field(function(x) x$window, integer(1)),
    rounds = unname(lapply(reg, `[[`, "rounds")),
    validated = lapply(names(reg), function(id) unique(val$survey_id[val$indicator == id & val$pass])),
    definition = field(function(x) as.character(x$definition)),
    notes = field(function(x) as.character(x$notes)),
    reference = field(function(x) as.character(x$reference))
  )
  out$api_id[out$api_id == "NA"] <- NA_character_
  if (!is.null(topic)) {
    out <- out[tolower(out$topic) %in% tolower(topic), ]
  }
  if (!is.null(recode)) {
    recode <- toupper_trim(recode)
    keep <- vapply(reg[out$id], function(x) any(x$recode %in% recode), logical(1))
    out <- out[keep, ]
  }
  out
}

#' Add indicator variables to BDHS microdata
#'
#' Computes indicators from the registry ([bdhs_indicator_list()]) and adds
#' them as columns named by the indicator id. Each column holds the
#' indicator's value (0/1 for proportions) for records in the indicator's
#' denominator population and `NA` for everyone else, so that
#' `mean(x, na.rm = TRUE)` weighted by the sample weight gives the
#' indicator. Use [bdhs_estimate()] for estimates with correct standard
#' errors and for indicators with all-women denominators.
#'
#' Round-specific coding (for example which providers count as skilled in
#' each BDHS round) is applied automatically using the survey round of the
#' data. Rates (fertility and mortality) are not record-level variables and
#' are skipped; use [bdhs_fertility()] and [bdhs_mortality()].
#'
#' @param data A BDHS recode data frame, as returned by `bdhs_read()` or
#'   [bdhs_simulate()], with DHS variable names. Pooled data (a `survey_id`
#'   column) are handled round by round.
#' @param indicators Character vector of indicator ids.
#' @param survey Optional survey reference, overriding detection.
#'
#' @return `data` with one numeric column per indicator. Each column carries
#'   the attributes `label` and, for all-women indicators, `bdhs_all_women`
#'   (the name of the all-woman factor needed for the denominator).
#' @export
#' @examples
#' kr <- bdhs_simulate(2017, "KR")
#' kr <- bdhs_add_indicators(kr, c("rh_del_facility", "ch_vac_bcg"))
#' table(kr$rh_del_facility, useNA = "ifany")
bdhs_add_indicators <- function(data, indicators, survey = NULL) {
  inds <- resolve_indicators(indicators)
  recode <- data_recode(data)
  is_rate <- vapply(inds, function(x) x$type == "rate", logical(1))
  if (any(is_rate)) {
    cli::cli_inform(c(
      "!" = "Skipping rate indicator{?s} {.val {names(inds)[is_rate]}}.",
      "i" = "Use {.fn bdhs_fertility} or {.fn bdhs_mortality} for rates."
    ))
    inds <- inds[!is_rate]
  }
  wrong <- !vapply(inds, function(x) recode %in% x$recode, logical(1))
  if (any(wrong)) {
    abort_wrong_recode(inds, wrong, recode)
  }
  groups <- survey_row_groups(data, survey)
  for (ind in inds) {
    col <- rep(NA_real_, nrow(data))
    for (g in groups) {
      if (!g$survey_id %in% ind$rounds) {
        cli::cli_warn("{.val {ind$id}} is not available for {.val {g$survey_id}}; values set to {.code NA}.")
        next
      }
      d <- data[g$rows, , drop = FALSE]
      col[g$rows] <- indicator_values(ind, d, g$survey_id)
    }
    attr(col, "label") <- ind$label
    if (!is.null(ind$all_women)) attr(col, "bdhs_all_women") <- ind$all_women
    data[[ind$id]] <- col
  }
  data
}

# Survey rounds in the data. A `survey_id` column with several rounds (pooled
# data) takes precedence over a single-round attribute left over from binding.
data_survey_ids <- function(data, survey = NULL, call = rlang::caller_env()) {
  if (is_pooled(data)) {
    ids <- bdhs_survey_id(sort(unique(as.character(data$survey_id))))
    if (is.null(survey)) {
      return(ids)
    }
    # On pooled data `survey` selects rounds; it never relabels other rounds.
    want <- intersect(bdhs_survey_id(survey), ids)
    if (!length(want)) {
      cli::cli_abort(
        "None of the requested surveys are in these pooled data, which contain {.val {ids}}.",
        call = call
      )
    }
    return(want)
  }
  data_survey_id(data, survey)
}

# TRUE for data stacked from several rounds (a survey_id column with more
# than one value).
is_pooled <- function(data) {
  "survey_id" %in% names(data) && length(unique(stats::na.omit(as.character(data$survey_id)))) > 1L
}

# Row groups by survey round: list of list(survey_id, rows).
survey_row_groups <- function(data, survey = NULL, call = rlang::caller_env()) {
  ids <- data_survey_ids(data, survey, call = call)
  if (!is_pooled(data)) {
    return(list(list(survey_id = ids, rows = seq_len(nrow(data)))))
  }
  sid <- as.character(data$survey_id)
  lapply(ids, function(s) list(survey_id = s, rows = which(sid == s)))
}

# Value of an indicator, NA outside its universe. When a required variable is
# absent (`requires`: names, or alternatives separated by "|"), all values are
# NA and the result carries the attribute "bdhs_missing".
indicator_values <- function(ind, d, survey_id) {
  if (!nrow(d)) {
    return(numeric())
  }
  present <- vapply(strsplit(ind$requires, "|", fixed = TRUE), function(alt) any(alt %in% names(d)), logical(1))
  if (length(present) && !all(present)) {
    miss <- ind$requires[!present]
    cli::cli_warn("{.val {ind$id}}: {cli::qty(length(miss))}required variable{?s} {.field {miss}} not found in the data; values set to {.code NA}.")
    out <- rep(NA_real_, nrow(d))
    attr(out, "bdhs_missing") <- miss
    return(out)
  }
  u <- ind$universe(d, survey_id)
  u[is.na(u)] <- FALSE
  y <- as.numeric(ind$value(d, survey_id))
  if (length(u) != nrow(d) || length(y) != nrow(d)) {
    cli::cli_abort("Indicator {.val {ind$id}} returned a vector of the wrong length.", .internal = TRUE)
  }
  y[!u] <- NA_real_
  y
}

# Helpers used by the indicator definitions ------------------------------------

# Numeric DHS codes of a variable; an all-NA vector when the variable is absent.
dv <- function(d, var) {
  x <- d[[var]]
  if (is.null(x)) {
    return(rep(NA_real_, nrow(d)))
  }
  if (is.factor(x)) {
    cli::cli_abort(c(
      "Indicator calculations need the numeric DHS codes, but {.field {var}} is a factor.",
      "i" = "Read the data with value labels kept as codes (the default of {.fn bdhs_read})."
    ), call = NULL)
  }
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }
  as.numeric(zap_all(x))
}

# Does `d` have `var` with at least one non-missing value?
has_var <- function(d, var) {
  !is.null(d[[var]]) && any(!is.na(d[[var]]))
}

# TRUE where x is one of `codes` (missing is FALSE).
in_codes <- function(x, codes) {
  !is.na(x) & x %in% codes
}

# 1 if any of `vars` equals one of `codes`, else 0.
any_of_vars <- function(d, vars, codes = 1) {
  out <- rep(FALSE, nrow(d))
  for (v in vars) out <- out | in_codes(dv(d, v), codes)
  as.numeric(out)
}

# Variables of the form <prefix><letter> present in d, e.g. h32a..h32x.
letter_vars <- function(d, prefix, letters = base::letters[1:24]) {
  intersect(paste0(prefix, letters), names(d))
}

# Child's age in months (b19 when available, else interview date - birth date).
child_age <- function(d) {
  a <- dv(d, "b19")
  if (all(is.na(a))) a <- dv(d, "v008") - dv(d, "b3")
  a
}

# Women interviewed with the full (long) questionnaire. The 2022 BDHS used a
# short questionnaire (sections 1-2 only: background and birth history) in one
# third of the households (sqtype == 2); modules such as contraception,
# marriage, fertility preferences and empowerment are therefore missing for
# those women and their denominators must exclude them.
long_questionnaire <- function(d, survey_id, keys = c("v313", "v511", "v626a")) {
  if (!identical(survey_id, "BD2022DHS")) {
    return(rep(TRUE, nrow(d)))
  }
  if (!is.null(d[["sqtype"]])) {
    return(in_codes(dv(d, "sqtype"), 1))
  }
  keys <- intersect(keys, names(d))
  if (!length(keys)) {
    return(rep(TRUE, nrow(d)))
  }
  out <- rep(FALSE, nrow(d))
  for (k in keys) out <- out | !is.na(dv(d, k))
  out
}

# Columns needed to compute the given indicators (design, identifiers, dates
# and each indicator's variables), restricted to `available` names. Used to
# read only the necessary columns of large recode files.
indicator_columns <- function(indicators, available, by = NULL) {
  inds <- resolve_indicators(indicators)
  base <- c(
    unlist(.design_vars), "v000", "v007", "hv000", "hv007", "caseid", "hhid", "midx", "bidx", "hvidx",
    "survey_id", "v008", "v011", "v012", "v502", "b19", "b3", "b7", "b5", "sqtype", "d005",
    grep("^awfact", available, value = TRUE)
  )
  vars <- unique(c(base, by, unlist(lapply(inds, `[[`, "vars"))))
  if (any(vapply(inds, function(x) x$type == "rate", logical(1)))) {
    vars <- c(vars, grep("^b[37]_[0-9]+$", available, value = TRUE))
  }
  intersect(available, vars)
}

# Survey-specific value: x is a named list keyed by survey id with "default".
by_round <- function(x, survey_id) {
  if (survey_id %in% names(x)) x[[survey_id]] else x[["default"]]
}
