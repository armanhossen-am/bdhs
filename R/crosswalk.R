# Cross-round harmonization rules (the crosswalk) ----------------------------------
#
# The rules live in inst/extdata/crosswalk.rds, a plain parse of
# data-raw/inputs/crosswalk.json (built by data-raw/04-build-crosswalk.R).
# They are normalised once per session into R-friendly objects and cached.

.bdhs_cache <- new.env(parent = emptyenv())

.cw_methods <- c(
  "map", "numeric", "range_map", "any_of", "expr", "derive", "constant",
  "unavailable", "external_file"
)

# The normalised crosswalk (cached).
crosswalk <- function() {
  cw <- .bdhs_cache$crosswalk
  if (is.null(cw)) {
    path <- system.file("extdata", "crosswalk.rds", package = "bdhs")
    if (!nzchar(path)) {
      cli::cli_abort("The bundled harmonization rules ({.file crosswalk.rds}) are missing; reinstall {.pkg bdhs}.")
    }
    cw <- cw_normalize(readRDS(path))
    .bdhs_cache$crosswalk <- cw
  }
  cw
}

# Normalisation ----------------------------------------------------------------

# NULL -> NA so that records become rectangular.
null_na <- function(x) if (is.null(x)) NA else x

# A list of JSON records (list of named lists) -> data frame.
records_df <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (!length(x)) {
    return(data.frame())
  }
  cols <- unique(unlist(lapply(x, names)))
  out <- lapply(cols, function(col) {
    vals <- lapply(x, function(r) null_na(r[[col]]))
    unlist(vals, use.names = FALSE)
  })
  names(out) <- cols
  as.data.frame(out, stringsAsFactors = FALSE)
}

# JSON object of scalars -> named character vector.
named_chr <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  out <- vapply(x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
  stats::setNames(out, names(x))
}

num_vec <- function(x) {
  if (is.null(x)) NULL else as.numeric(unlist(x, use.names = FALSE))
}

chr_vec <- function(x) {
  if (is.null(x)) NULL else as.character(unlist(x, use.names = FALSE))
}

cw_normalize_rule <- function(r) {
  r$method <- as.character(r[["method"]])
  # Always present (possibly empty) so that `$var` never partially matches
  # `$vars`.
  r[["var"]] <- chr_vec(r[["var"]]) %||% character()
  r[["vars"]] <- chr_vec(r[["vars"]]) %||% character()
  if (!is.null(r$map)) {
    r$map <- named_chr(r$map)
    r$map_codes <- as.numeric(names(r$map))
  }
  r$na_codes <- num_vec(r$na_codes)
  r$true_codes <- num_vec(r$true_codes)
  r$valid_range <- num_vec(r$valid_range)
  r$divide_by <- num_vec(r$divide_by)
  r$value_label_check <- named_chr(r$value_label_check)
  r$label_check <- named_chr(r$label_check)
  if (!is.null(r$ranges)) {
    r$ranges <- records_df(r$ranges)
  }
  if (!is.null(r$apply)) {
    r$apply <- cw_normalize_rule(r$apply)
  }
  r
}

cw_normalize <- function(raw) {
  vars <- lapply(raw$variables, function(v) {
    v$levels <- if (!is.null(v$levels)) records_df(v$levels)
    v$notes <- chr_vec(v$notes)
    hh <- v$pr_hr_equivalent
    v$hh_var <- if (is.null(hh)) NA_character_ else sub("^\\s*([A-Za-z0-9_]+).*$", "\\1", hh)
    if (!is.null(v$derive)) {
      v$derive$from <- as.character(v$derive$from)
      if (!is.null(v$derive$map)) {
        v$derive$map <- named_chr(v$derive$map)
      }
    }
    v$rounds <- lapply(v$rounds, cw_normalize_rule)
    v
  })
  names(vars) <- vapply(vars, function(v) v$name, character(1))
  lookups <- lapply(raw$lookups, records_df)
  list(meta = raw$meta, lookups = lookups, vars = vars)
}

# Consistency checks used by data-raw/04-build-crosswalk.R and the tests.
cw_check <- function(cw) {
  problems <- character()
  add <- function(...) problems <<- c(problems, paste0(...))
  for (v in cw$vars) {
    lv <- if (!is.null(v$levels)) v$levels$value
    if (v$type == "factor" && !length(lv)) add(v$name, ": factor without levels")
    if (!v$type %in% c("factor", "binary", "integer", "numeric", "character")) add(v$name, ": unknown type ", v$type)
    for (sid in names(v$rounds)) {
      r <- v$rounds[[sid]]
      where <- paste0(v$name, " [", sid, "]")
      if (!r$method %in% .cw_methods) add(where, ": unknown method ", r$method)
      rules <- list(r)
      if (!is.null(r$apply)) rules <- c(rules, list(r$apply))
      for (rr in rules) {
        vals <- switch(rr$method,
          map = unname(rr$map),
          range_map = rr$ranges$level,
          NULL
        )
        if (rr$method == "map" && anyNA(rr$map_codes)) add(where, ": non-numeric map code")
        if (!is.null(vals) && v$type == "factor" && !all(vals %in% lv)) {
          add(where, ": values not in levels: ", paste(setdiff(vals, lv), collapse = ", "))
        }
        if (!is.null(vals) && v$type == "binary" && !all(vals %in% c("0", "1"))) {
          add(where, ": binary map values must be 0/1")
        }
      }
      if (r$method %in% c("map", "numeric", "range_map") && !length(r$var)) add(where, ": missing `var`")
      if (r$method %in% c("any_of", "expr") && !length(r$vars)) add(where, ": missing `vars`")
      if (r$method == "any_of" && !length(r$true_codes)) add(where, ": missing `true_codes`")
      if (r$method == "expr") {
        ok <- tryCatch(is.call(str2lang(r$expr)) || is.name(str2lang(r$expr)), error = function(e) FALSE)
        if (!ok) add(where, ": expression does not parse")
        free <- expr_names(str2lang(r$expr))
        unknown <- setdiff(free, c(r$vars, "lookup"))
        if (length(unknown)) add(where, ": expression uses unlisted names ", paste(unknown, collapse = ", "))
      }
      if (r$method == "derive") {
        d <- v$derive
        if (is.null(d) || !d$from %in% names(cw$vars)) add(where, ": derive without a valid `from`")
        if (!is.null(d) && is.null(d$map) && is.null(d$expr)) add(where, ": derive needs `map` or `expr`")
      }
    }
  }
  problems
}

# Names an expression reads (the right-hand side of `$` is a field, not a
# variable).
expr_names <- function(e) {
  if (is.name(e)) {
    return(as.character(e))
  }
  if (!is.call(e)) {
    return(character())
  }
  args <- as.list(e)[-1]
  if (identical(e[[1]], as.name("$")) || identical(e[[1]], as.name("@"))) {
    args <- args[1]
  }
  unique(unlist(lapply(args, expr_names)))
}

# Rule resolution ---------------------------------------------------------------

# DHS design/geography variables that the household files carry under an `h`
# prefix with identical meaning and codes (v001 -> hv001, v008 -> hv008,
# v024 -> hv024, ...). v009-v019 and v027+ are NOT mirrors (v012 is the
# woman's age, hv012 the number of de jure household members).
is_hh_mirror <- function(x) grepl("^v0(0[0-8]|2[0-6])$", x)

# Does a harmonized variable apply to a recode at all? Derived variables
# apply wherever their source variable does.
cw_applies <- function(spec, recode, cw = crosswalk()) {
  if (!is.null(spec$derive) && !is.null(cw$vars[[spec$derive$from]])) {
    return(cw_applies(cw$vars[[spec$derive$from]], recode, cw))
  }
  unit <- spec$unit
  if (identical(unit, "any")) {
    return(TRUE)
  }
  switch(recode,
    KR = TRUE,
    BR = unit %in% c("woman/birth", "birth"),
    IR = unit %in% c("woman/birth"),
    HR = ,
    PR = {
      hh <- spec$hh_var
      if (!is.na(hh)) {
        recode == "PR" || !grepl("^(hv1[0-9]{2}|hc[0-9])", hh)
      } else {
        # Rules that only read design/geography variables (v024, v008, ...)
        # work on the household files through their hv equivalents.
        mirrored <- vapply(spec$rounds, function(r) {
          s <- c(r$var, r$vars)
          length(s) > 0 && all(is_hh_mirror(s))
        }, logical(1))
        unit == "woman/birth" && any(mirrored)
      }
    },
    FALSE
  )
}

# The rule for one variable, round and recode, with `src` mapping the names
# used by the rule to the data columns to read. NULL when the variable has no
# rule for that round.
cw_rule <- function(spec, survey_id, recode) {
  rule <- spec$rounds[[survey_id]]
  if (is.null(rule)) {
    return(NULL)
  }
  names_used <- unique(c(rule$var, rule$vars))
  src <- stats::setNames(names_used, names_used)
  if (recode %in% c("HR", "PR") && length(src)) {
    hh <- spec$hh_var
    if (!is.na(hh) && rule$method %in% c("map", "numeric", "range_map")) {
      src[[rule$var]] <- hh
    } else {
      mirror <- is_hh_mirror(src)
      src[mirror] <- paste0("h", src[mirror])
    }
  }
  rule$src <- src
  rule
}

# Catalog ------------------------------------------------------------------------

rule_source <- function(r) {
  switch(r$method,
    constant = paste0("constant ", encodeString(as.character(r$value), quote = "\"")),
    unavailable = "not available",
    external_file = paste0(r$var, " (", r$file, " file)"),
    derive = paste0("derived from ", r$from),
    paste(c(r$var, r$vars), collapse = ", ")
  )
}

#' Harmonized variables available across BDHS rounds
#'
#' Lists the variables that [bdhs_harmonize()] can create, with their type,
#' levels, comparability across rounds and, for each round, the DHS source
#' variable(s) and rule used. The rules were derived from the value labels and
#' code frequencies of every recode file and validated against official DHS
#' estimates; see the `notes` column for known breaks (for example the
#' 2007 change in the ARI definition or the 2022 move of "tin" walls to a new
#' code).
#'
#' Variables are defined for the children's recode (KR); rules on woman-level
#' `v` variables also apply to the women's (IR) and births (BR) recodes, and
#' variables with a `household_var` apply to the household (HR) and household
#' member (PR) recodes.
#'
#' @param survey Optional survey reference(s) (see [bdhs_survey_id()]). When
#'   given, the result has one row per variable and round, with the rule used
#'   in that round.
#' @return A tibble. Without `survey`: one row per variable with columns
#'   `name`, `label`, `type` (`factor`, `binary` = 0/1 integer, `integer`,
#'   `numeric`, `character`), `unit`, `comparability`, `levels` (a list of
#'   tibbles with `value` and `label`), `rounds` (survey ids where the
#'   variable is in the recode file), `external_rounds` (rounds where it is in
#'   a separate WI/HW file), `sources` (per-round source summary),
#'   `household_var` and `notes`. With `survey`: `name`, `label`, `type`,
#'   `unit`, `comparability`, `survey_id`, `method`, `source`, `available` and
#'   `note`.
#' @export
#' @examples
#' vars <- bdhs_harmonized_vars()
#' vars[, c("name", "type", "comparability")]
#'
#' # Rules used in one round
#' bdhs_harmonized_vars(2022)[, c("name", "method", "source", "available")]
bdhs_harmonized_vars <- function(survey = NULL) {
  cw <- crosswalk()
  specs <- cw$vars
  if (is.null(survey)) {
    return(tibble::tibble(
      name = names(specs),
      label = vapply(specs, function(v) v$label, character(1), USE.NAMES = FALSE),
      type = vapply(specs, function(v) v$type, character(1), USE.NAMES = FALSE),
      unit = vapply(specs, function(v) v$unit, character(1), USE.NAMES = FALSE),
      comparability = vapply(specs, function(v) v$comparability, character(1), USE.NAMES = FALSE),
      levels = unname(lapply(specs, function(v) {
        if (is.null(v$levels)) NULL else tibble::as_tibble(v$levels[c("value", "label")])
      })),
      rounds = unname(lapply(specs, function(v) {
        ok <- vapply(v$rounds, function(r) !r$method %in% c("unavailable", "external_file"), logical(1))
        names(v$rounds)[ok]
      })),
      external_rounds = unname(lapply(specs, function(v) {
        ok <- vapply(v$rounds, function(r) r$method == "external_file", logical(1))
        names(v$rounds)[ok]
      })),
      sources = vapply(specs, function(v) {
        paste0(survey_label(names(v$rounds)), ": ", vapply(v$rounds, rule_source, character(1)), collapse = "; ")
      }, character(1), USE.NAMES = FALSE),
      household_var = vapply(specs, function(v) v$hh_var, character(1), USE.NAMES = FALSE),
      notes = vapply(specs, function(v) paste(v$notes, collapse = " "), character(1), USE.NAMES = FALSE)
    ))
  }
  ids <- bdhs_survey_id(survey)
  rows <- lapply(specs, function(v) {
    rules <- lapply(ids, function(id) v$rounds[[id]])
    method <- vapply(rules, function(r) if (is.null(r)) "none" else r$method, character(1))
    tibble::tibble(
      name = v$name,
      label = v$label,
      type = v$type,
      unit = v$unit,
      comparability = v$comparability,
      survey_id = ids,
      method = method,
      source = vapply(rules, function(r) if (is.null(r)) NA_character_ else rule_source(r), character(1)),
      available = !method %in% c("none", "unavailable", "external_file"),
      note = vapply(rules, function(r) {
        if (is.null(r)) {
          "No verified rule for this round."
        } else {
          paste(c(r$reason, r$note), collapse = " ")
        }
      }, character(1))
    )
  })
  out <- dplyr::bind_rows(rows)
  out$note[!nzchar(out$note)] <- NA_character_
  out
}
