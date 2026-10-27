# Cross-round harmonization -----------------------------------------------------------

#' Harmonize BDHS variables across survey rounds
#'
#' Creates analysis-ready variables that mean the same thing in every BDHS
#' round, from 1993-94 to 2022. DHS codes change between rounds (new
#' divisions, re-ordered region codes, new answer categories, letters of the
#' delivery-assistance variables, ...) and some value labels in the files are
#' wrong; the bundled crosswalk records, for every round, which source
#' variables and codes to use. See [bdhs_harmonized_vars()] for the catalogue.
#'
#' The data can come from one round (detected from the `bdhs_survey_id`
#' attribute or `v000`/`v007`) or from several rounds stacked with
#' [bdhs_pool()] (a `survey_id` column), in which case every row is
#' harmonized with the rules of its own round.
#'
#' * Factors get the documented levels (in documented order); binary
#'   variables are integer 0/1; numeric variables are on their natural scale
#'   (z-scores divided by 100, weights by 1e6, ...).
#' * Variables whose source variables are absent from `data` are skipped (one
#'   message lists them).
#' * Rounds where a variable was not collected, or where it lives in a separate
#'   WI (wealth index) or HW (WHO height/weight) file, give `NA` with a
#'   message. Merge those files first with [bdhs_merge()] and the variable is
#'   computed from them.
#' * Codes not covered by the rules become `NA` with a warning, and value
#'   labels that no longer match the expected meaning (label drift, e.g. data
#'   attributed to the wrong round) are reported, unless `warn = FALSE`.
#'   Codes whose labels mark them as missing / don't know / not de jure are
#'   set to `NA` silently.
#'
#' Rules are written for the children's recode (KR); woman-level rules also
#' apply to the women's (IR) and births (BR) recodes, and variables with a
#' household equivalent (`household_var` in [bdhs_harmonized_vars()]) to the
#' household (HR) and household member (PR) recodes.
#'
#' @param data A data frame of BDHS recode data with the original DHS codes
#'   (haven-labelled or plain numeric), as returned by [bdhs_read()],
#'   [bdhs_simulate()] or [bdhs_pool()].
#' @param vars Names of harmonized variables to create (see
#'   [bdhs_harmonized_vars()]). `NULL` (default) creates every variable that
#'   applies to the recode.
#' @param survey Optional survey reference for data from a single round,
#'   overriding detection.
#' @param recode Optional recode code (`"KR"`, `"IR"`, ...), overriding
#'   detection.
#' @param prefix A prefix for the new column names, e.g. `"h_"`.
#' @param warn If `TRUE` (default), warn about unexpected codes and label
#'   drift.
#' @return `data` with the harmonized columns added (or replaced) and its
#'   attributes kept. The attribute `bdhs_harmonized` lists the harmonized
#'   columns.
#' @export
#' @examples
#' kr <- bdhs_simulate(2014, "KR")
#' h <- bdhs_harmonize(kr, vars = c("division_trend6", "residence", "child_age_months"))
#' table(h$division_trend6)
#'
#' # Pooled rounds are harmonized round by round
#' pooled <- bdhs_pool(bdhs_simulate(2011, "IR"), bdhs_simulate(2022, "IR"))
#' h <- bdhs_harmonize(pooled, vars = c("division_surveyed", "division_trend6"))
#' table(h$survey_id, h$division_trend6)
bdhs_harmonize <- function(data, vars = NULL, survey = NULL, recode = NULL, prefix = "", warn = TRUE) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.")
  }
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    cli::cli_abort("{.arg prefix} must be a single string.")
  }
  cw <- crosswalk()
  recode <- data_recode(data, recode)
  sid <- harmonize_survey_rows(data, survey)
  n <- nrow(data)
  rounds <- unique(sid)
  row_sets <- if (length(rounds) == 1L) {
    stats::setNames(list(NULL), rounds)
  } else {
    split(seq_len(n), factor(sid, levels = rounds))
  }

  all_vars <- names(cw$vars)
  explicit <- !is.null(vars)
  if (explicit) {
    vars <- unique(as.character(vars))
    bad <- setdiff(vars, all_vars)
    if (length(bad)) {
      cli::cli_abort(c(
        "Unknown harmonized variable{?s} {.val {bad}}.",
        "i" = "See {.run bdhs::bdhs_harmonized_vars()} for the available variables."
      ))
    }
  } else {
    vars <- all_vars[vapply(cw$vars, cw_applies, logical(1), recode = recode, cw = cw)]
  }
  # Derived variables need their source variable, computed but not returned
  # unless requested.
  needed <- vars
  for (v in vars) {
    from <- cw$vars[[v]]$derive$from
    if (!is.null(from)) needed <- union(needed, from)
  }
  base_vars <- needed[vapply(cw$vars[needed], function(s) is.null(s$derive), logical(1))]
  derived_vars <- setdiff(needed, base_vars)

  log <- hz_log()
  results <- list()
  for (v in base_vars) {
    results[[v]] <- harmonize_one(cw, cw$vars[[v]], data, row_sets, recode, n, log)
  }
  for (v in derived_vars) {
    results[[v]] <- harmonize_derived(cw$vars[[v]], results[[cw$vars[[v]]$derive$from]], row_sets, n)
  }

  # Which variables to return: produced in at least one round, or NA-only
  # because of unavailable/external rounds when the variable applies.
  keep <- vapply(vars, function(v) {
    st <- results[[v]]$status
    any(st == "ok") ||
      (any(st %in% c("unavailable", "external")) && (explicit || cw_applies(cw$vars[[v]], recode)))
  }, logical(1))
  out_vars <- vars[keep]

  harmonize_report(cw, vars, out_vars, results, rounds, log, explicit, warn)

  new_cols <- lapply(out_vars, function(v) finish_column(results[[v]]$values, cw$vars[[v]]))
  new_names <- if (length(out_vars)) paste0(prefix, out_vars) else character()
  names(new_cols) <- new_names

  clash <- intersect(new_names, names(data))
  prev <- attr(data, "bdhs_harmonized", exact = TRUE)
  clash <- setdiff(clash, c(prev, "survey_id"))
  if (length(clash)) {
    cli::cli_inform(c("!" = "Replacing existing column{?s} {.var {clash}} with harmonized version{?s}."))
  }
  out <- add_columns(data, new_cols)
  attr(out, "bdhs_harmonized") <- union(prev, new_names)
  out
}

# Survey id of every row (or one id for single-round data).
harmonize_survey_rows <- function(data, survey, call = rlang::caller_env()) {
  if (!is.null(survey)) {
    id <- bdhs_survey_id(survey)
    if (length(id) != 1L) {
      cli::cli_abort("{.arg survey} must refer to a single round.", call = call)
    }
    return(id)
  }
  if ("survey_id" %in% names(data)) {
    s <- as.character(zap_all(data$survey_id))
    u <- unique(s)
    if (anyNA(u)) {
      cli::cli_abort("The {.field survey_id} column has missing values.", call = call)
    }
    ids <- bdhs_survey_id(u, error = FALSE)
    if (anyNA(ids)) {
      cli::cli_abort("Unknown survey id{?s} {.val {u[is.na(ids)]}} in the {.field survey_id} column.", call = call)
    }
    if (length(u) == 1L) {
      return(ids)
    }
    return(ids[match(s, u)])
  }
  id <- data_survey_id(data, call = call)
  if (length(id) != 1L) {
    cli::cli_abort("Can't tell which round each row comes from; add a {.field survey_id} column.", call = call)
  }
  id
}

# Issue collector (an environment so helpers can append).
hz_log <- function() {
  e <- new.env(parent = emptyenv())
  e$issues <- list()
  e
}

hz_issue <- function(log, kind, var, survey_id, msg) {
  log$issues[[length(log$issues) + 1L]] <- list(kind = kind, var = var, survey_id = survey_id, msg = msg)
}

# Harmonize one (non-derived) variable over all rounds.
# Returns list(values = <vector of length n, character for factors>,
#              status = named character per round, detail = named list).
harmonize_one <- function(cw, spec, data, row_sets, recode, n, log) {
  type <- spec$type
  values <- empty_values(type, n)
  status <- stats::setNames(character(length(row_sets)), names(row_sets))
  detail <- stats::setNames(vector("list", length(row_sets)), names(row_sets))
  applies <- cw_applies(spec, recode)
  for (sid in names(row_sets)) {
    rows <- row_sets[[sid]]
    m <- if (is.null(rows)) n else length(rows)
    rule <- cw_rule(spec, sid, recode)
    if (is.null(rule)) {
      status[[sid]] <- "norule"
      next
    }
    if (rule$method == "unavailable") {
      status[[sid]] <- "unavailable"
      detail[[sid]] <- rule$reason
      next
    }
    if (rule$method == "external_file") {
      if (!is.null(rule$apply) && rule$var %in% names(data)) {
        ext_var <- rule$var
        rule <- rule$apply
        rule[["var"]] <- ext_var
        rule[["src"]] <- stats::setNames(ext_var, ext_var)
      } else {
        status[[sid]] <- "external"
        detail[[sid]] <- rule
        next
      }
    }
    if (rule$method == "derive") {
      status[[sid]] <- "norule"
      next
    }
    src <- rule$src
    missing_src <- src[!src %in% names(data)]
    if (length(missing_src)) {
      status[[sid]] <- if (applies) "absent" else "notapplicable"
      detail[[sid]] <- unname(missing_src)
      next
    }
    cols <- lapply(src, function(col) {
      x <- data[[col]]
      if (is.null(rows)) x else x[rows]
    })
    is_fct <- vapply(cols, is.factor, logical(1))
    if (any(is_fct)) {
      status[[sid]] <- "factor"
      detail[[sid]] <- unname(src[is_fct])
      next
    }
    res <- apply_rule(rule, cols, spec, sid, recode, m, cw, log)
    if (is.null(rows)) values <- res else values[rows] <- res
    status[[sid]] <- "ok"
  }
  list(values = values, status = status, detail = detail)
}

empty_values <- function(type, n) {
  switch(type,
    factor = ,
    character = rep(NA_character_, n),
    binary = ,
    integer = rep(NA_integer_, n),
    rep(NA_real_, n)
  )
}

# Apply one rule to the source columns of one round.
apply_rule <- function(rule, cols, spec, sid, recode, m, cw, log) {
  check_labels(rule, cols, spec, sid, recode, log)
  out <- switch(rule$method,
    constant = rep(rule$value, m),
    map = rule_map(rule, cols[[1]], spec, sid, log),
    numeric = rule_numeric(rule, cols[[1]]),
    range_map = rule_range_map(rule, cols[[1]], spec, sid, log),
    any_of = rule_any_of(rule, cols),
    expr = rule_expr(rule, cols, cw, m),
    cli::cli_abort("Unknown rule method {.val {rule$method}} for {.field {spec$name}}.", .internal = TRUE)
  )
  coerce_type(out, spec, sid, log)
}

# Codes of a source column as numbers (labels, tagged NAs and SPSS missing
# values stripped).
src_codes <- function(x) {
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }
  as.numeric(zap_all(x))
}

report_unknown <- function(codes, unknown, spec, sid, var, log) {
  if (!any(unknown)) {
    return(invisible())
  }
  tab <- table(codes[unknown])
  shown <- utils::head(tab, 8)
  hz_issue(log, "unknown", spec$name, sid, paste0(
    "{.field ", spec$name, "} [", sid, "]: unexpected code", if (length(tab) > 1) "s" else "",
    " of ", var, " set to NA: ",
    paste0(names(shown), " (n=", as.integer(shown), ")", collapse = ", "),
    if (length(tab) > length(shown)) ", ..." else ""
  ))
}

rule_map <- function(rule, x, spec, sid, log) {
  codes <- src_codes(x)
  i <- match(codes, rule$map_codes)
  out <- unname(rule$map)[i]
  na_hit <- codes %in% rule$na_codes
  out[na_hit] <- NA_character_
  unknown <- !is.na(codes) & is.na(i) & !na_hit & !codes %in% special_label_codes(x)
  report_unknown(codes, unknown, spec, sid, rule$var, log)
  out
}

rule_numeric <- function(rule, x) {
  codes <- src_codes(x)
  drop <- codes %in% c(rule$na_codes, special_label_codes(x))
  if (length(rule$valid_range) == 2L) {
    drop <- drop | (!is.na(codes) & (codes < rule$valid_range[[1]] | codes > rule$valid_range[[2]]))
  }
  codes[drop] <- NA
  if (length(rule$divide_by)) {
    codes <- codes / rule$divide_by
  }
  codes
}

rule_range_map <- function(rule, x, spec, sid, log) {
  codes <- src_codes(x)
  out <- rep(NA_character_, length(codes))
  r <- rule$ranges
  for (k in seq_len(nrow(r))) {
    out[!is.na(codes) & codes >= r$from[[k]] & codes <= r$to[[k]]] <- r$level[[k]]
  }
  na_hit <- codes %in% rule$na_codes
  out[na_hit] <- NA_character_
  unknown <- !is.na(codes) & is.na(out) & !na_hit & !codes %in% special_label_codes(x)
  report_unknown(codes, unknown, spec, sid, rule$var, log)
  out
}

rule_any_of <- function(rule, cols) {
  codes <- lapply(cols, src_codes)
  hit <- Reduce(`|`, lapply(codes, function(z) !is.na(z) & z %in% rule$true_codes))
  any_obs <- Reduce(`|`, lapply(codes, function(z) !is.na(z)))
  out <- as.integer(hit)
  out[!any_obs] <- NA_integer_
  out
}

rule_expr <- function(rule, cols, cw, m) {
  mask <- lapply(cols, function(x) if (is.character(x)) x else src_codes(x))
  env <- new.env(parent = baseenv())
  env$lookup <- cw$lookups
  res <- rlang::eval_tidy(rlang::parse_expr(rule$expr), data = mask, env = env)
  if (length(res) == 1L && m != 1L) {
    res <- rep(res, m)
  }
  if (length(res) != m) {
    cli::cli_abort("Rule expression {.code {rule$expr}} returned {length(res)} value{?s} for {m} row{?s}.",
      .internal = TRUE
    )
  }
  if (is.factor(res)) as.character(res) else res
}

# Coerce rule output to the variable type (factor values stay character until
# finish_column()).
coerce_type <- function(x, spec, sid, log) {
  switch(spec$type,
    factor = {
      x <- as.character(x)
      bad <- !is.na(x) & !x %in% spec$levels$value
      if (any(bad)) {
        hz_issue(log, "unknown", spec$name, sid, paste0(
          "{.field ", spec$name, "} [", sid, "]: rule produced undocumented value",
          if (length(unique(x[bad])) > 1) "s" else "", " ",
          paste(unique(x[bad]), collapse = ", "), "; set to NA"
        ))
        x[bad] <- NA_character_
      }
      x
    },
    binary = {
      x <- suppressWarnings(as.integer(as.numeric(x)))
      x[!is.na(x) & !x %in% c(0L, 1L)] <- NA_integer_
      x
    },
    integer = suppressWarnings(as.integer(round(as.numeric(x)))),
    numeric = suppressWarnings(as.numeric(x)),
    character = as.character(x),
    x
  )
}

# Value-label and variable-label drift checks.
check_labels <- function(rule, cols, spec, sid, recode, log) {
  if (!is.null(rule$value_label_check) && length(cols) >= 1L) {
    check_ok <- TRUE
    if (!is.null(rule$check_in)) {
      where <- toupper(sub(":.*$", "", rule$check_in))
      check_ok <- recode == where || (where == "HR" && recode == "PR")
    }
    labs <- attr(cols[[1]], "labels", exact = TRUE)
    if (check_ok && !is.null(labs) && length(labs) && !is.character(labs)) {
      lab_codes <- as.numeric(labs)
      for (code in names(rule$value_label_check)) {
        lab <- names(labs)[match(as.numeric(code), lab_codes)]
        if (length(lab) && !is.na(lab) && !grepl(rule$value_label_check[[code]], lab, ignore.case = TRUE, perl = TRUE)) {
          hz_issue(log, "drift", spec$name, sid, paste0(
            "{.field ", spec$name, "} [", sid, "]: code ", code, " of ", rule$var,
            " is labelled \"", cli_escape(lab), "\", expected /", cli_escape(rule$value_label_check[[code]]), "/"
          ))
        }
      }
    }
  }
  if (!is.null(rule$label_check)) {
    for (v in names(rule$label_check)) {
      col <- cols[[v]]
      lab <- if (is.null(col)) NA_character_ else var_label(col)
      if (!is.na(lab) && !grepl(rule$label_check[[v]], lab, ignore.case = TRUE, perl = TRUE)) {
        hz_issue(log, "drift", spec$name, sid, paste0(
          "{.field ", spec$name, "} [", sid, "]: ", v, " is labelled \"", cli_escape(lab),
          "\", expected /", cli_escape(rule$label_check[[v]]), "/"
        ))
      }
    }
  }
}

# Variables derived from another harmonized variable.
harmonize_derived <- function(spec, from, row_sets, n) {
  d <- spec$derive
  x <- from$values
  values <- if (!is.null(d$map)) {
    as.numeric(unname(d$map)[match(as.character(x), names(d$map))])
  } else {
    env <- new.env(parent = baseenv())
    mask <- stats::setNames(list(x), d$from)
    rlang::eval_tidy(rlang::parse_expr(d$expr), data = mask, env = env)
  }
  values <- coerce_type(values, spec, NA_character_, hz_log())
  status <- from$status
  # Rounds where the rule says "derive" inherit the source status; other
  # methods are not used for derived variables.
  list(values = values, status = status, detail = from$detail)
}

# Final vector: factors with documented levels, labels attached.
finish_column <- function(x, spec) {
  if (spec$type == "factor") {
    x <- factor(x, levels = spec$levels$value)
  }
  attr(x, "label") <- spec$label
  x
}

# Add or replace columns of a data frame without copying the others, keeping
# the class and attributes of `data`.
add_columns <- function(data, cols) {
  if (!length(cols)) {
    return(data)
  }
  cls <- class(data)
  out <- unclass(data)
  out[names(cols)] <- cols
  class(out) <- cls
  out
}

# One message for skipped variables, one for NA rounds, one warning for codes
# and label drift.
harmonize_report <- function(cw, vars, out_vars, results, rounds, log, explicit, warn) {
  st <- lapply(vars, function(v) results[[v]]$status)
  names(st) <- vars
  all_absent <- vars[vapply(st, function(s) all(s %in% c("absent", "notapplicable", "norule")) &&
    any(s %in% c("absent", "notapplicable")), logical(1))]
  if (!explicit) {
    all_absent <- intersect(all_absent, vars[vapply(st, function(s) any(s == "absent"), logical(1))])
  }
  if (length(all_absent)) {
    cli::cli_inform(c(
      "i" = "Skipped {length(all_absent)} variable{?s} whose source variables are not in {.arg data}: {.field {all_absent}}."
    ))
  }
  fct <- vars[vapply(st, function(s) any(s == "factor"), logical(1))]
  if (length(fct)) {
    srcs <- unique(unlist(lapply(fct, function(v) {
      det <- results[[v]]$detail
      unlist(det[results[[v]]$status == "factor"])
    })))
    cli::cli_inform(c(
      "!" = "Could not harmonize {.field {fct}} in some rounds: source column{?s} {.var {srcs}} {?is a factor/are factors}.",
      "i" = "Harmonization needs the original DHS codes; keep value labels as {.cls haven_labelled} (the default of {.fn bdhs_read})."
    ))
  }
  norule <- rounds[vapply(rounds, function(r) all(vapply(st, function(s) s[[r]] == "norule", logical(1))), logical(1))]
  if (length(norule)) {
    covered <- unique(unlist(lapply(cw$vars, function(v) names(v$rounds))))
    cli::cli_inform(c(
      "!" = "No verified harmonization rules for {.val {norule}}; {cli::qty(length(norule))}{?its/their} rows are {.val NA}.",
      "i" = "Rules cover {.val {covered}}."
    ))
  }
  bullets <- character()
  for (v in out_vars) {
    s <- st[[v]]
    if (all(s %in% c("absent", "notapplicable", "norule"))) next
    det <- results[[v]]$detail
    for (r in names(s)) {
      b <- switch(s[[r]],
        unavailable = paste0("{.field ", v, "} [", r, "]: not available (", cli_escape(det[[r]] %||% "not collected"), ")"),
        external = paste0(
          "{.field ", v, "} [", r, "]: in the separate ", det[[r]]$file, " file (", det[[r]]$var,
          "); merge it with {.fn bdhs_merge} first"
        ),
        absent = paste0("{.field ", v, "} [", r, "]: source ", paste(det[[r]], collapse = ", "), " not in the data"),
        factor = paste0(
          "{.field ", v, "} [", r, "]: source ", paste(det[[r]], collapse = ", "),
          " is a factor; harmonization needs the original DHS codes"
        ),
        norule = if (!r %in% norule) paste0("{.field ", v, "} [", r, "]: no rule for this round"),
        NULL
      )
      if (!is.null(b)) bullets <- c(bullets, b)
    }
  }
  if (length(bullets)) {
    names(bullets) <- rep("*", length(bullets))
    cli::cli_inform(c("i" = "Some harmonized values are {.val NA} by design:", bullets))
  }
  if (warn && length(log$issues)) {
    msgs <- vapply(log$issues, function(i) i$msg, character(1))
    names(msgs) <- rep("!", length(msgs))
    cli::cli_warn(c("Harmonization found unexpected codes or labels:", msgs))
  }
}

# Escape braces so free text can go through cli.
