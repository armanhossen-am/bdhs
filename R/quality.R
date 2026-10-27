# Data-quality diagnostics ----------------------------------------------------------------

quality_checks <- c(
  "age_heaping", "age_displacement", "birth_date_completeness", "births_by_year",
  "age_at_death", "anthropometry", "completeness"
)

quality_default_vars <- c(
  "v012", "v024", "v025", "v106", "v190", "v130", "b4", "b10", "m14", "m15", "m17", "h2", "h3",
  "hw70", "hw71", "hw72", "hv024", "hv025", "hv104", "hv105", "hv106", "hv270", "hc70", "hc71",
  "hc72", "ha40"
)

#' Data-quality diagnostics for DHS recode files
#'
#' Computes the standard DHS data-quality indicators (the "Data quality
#' tables" of the final report appendices and the DHS Methodological Reports
#' on data quality) for whichever recode file is supplied. Each check uses
#' the variables it needs and is skipped, with a message, when they are
#' absent, so the same call works on PR, KR, BR and IR files.
#'
#' The checks are:
#'
#' * `"age_heaping"` (PR, `hv105`, `hv104`): Whipple's index (ages 23-62,
#'   terminal digits 0 and 5; 100 = no preference, 500 = every age ends in 0
#'   or 5) and Myers' blended index (ages 10-89; 0 = no preference, 90 = every
#'   age ends in the same digit), for both sexes and by sex, and the share of
#'   members with age missing or "don't know". The single-year age
#'   distribution is kept for [plot()][plot.bdhs_quality].
#' * `"age_displacement"` (PR): ratios of the numbers aged 14 to 15, 50 to 49
#'   and 50 to 51 (men also 55 to 54). Interviewers who move women out of the
#'   eligible age range (15-49) inflate the counts at 14 and 50; values near 1
#'   are expected. A high 50/49 ratio with an equally high 50/51 ratio points
#'   to heaping on 50 rather than displacement.
#' * `"birth_date_completeness"` (BR, KR, or IR birth history, `b10`):
#'   percentage of births with month and year of birth reported (`b10` codes
#'   0 "month, year and day" and 1 "month and year", the same in every BDHS
#'   round), overall, for living and dead children and for births in the last
#'   5 and 15 years, plus the percentage of living children whose birth date
#'   (hence age) was partly imputed.
#' * `"births_by_year"` (`b3`, `v008`): births by calendar year and the
#'   calendar-year ratio \eqn{100 B_x / ((B_{x-1} + B_{x+1}) / 2)}, also by
#'   completed years before the interview. A ratio well below 100 just inside
#'   the five-year window (calendar year interview year - 5, or 4 years before
#'   the interview) together with one above 100 just outside it suggests births
#'   were displaced out of the window that triggers the health questions.
#'   KR files only hold births of the last 5 years (3 in 1993-94), so only
#'   interior years are available; `births_year4_vs_year3` (births 48-59 vs
#'   36-47 months before the interview) then gives a window-edge check.
#' * `"age_at_death"` (`b6`, `b7`): share of deaths with age at death reported
#'   in days, months and years, or missing, and the heaping index at 12
#'   months, \eqn{100 D_{12} / ((D_{11} + D_{13}) / 2)}, where \eqn{D_{12}}
#'   counts deaths reported at 12 months or "1 year" (and the same index from
#'   the imputed months in `b7`).
#' * `"anthropometry"`: for children (PR `hc*`, KR/BR `hw*`) and for women
#'   (`ha*`) and men (`hb*`) in PR: percentage with weight or height missing,
#'   percentage of z-scores flagged by DHS (codes 9996-9998), percentage of the
#'   remaining z-scores outside the WHO plausible limits (height-for-age -6 to
#'   6, weight-for-age -6 to 5, weight-for-height -5 to 5), mean and standard
#'   deviation of the valid z-scores (WHO expects an SD of about 1.0-1.2;
#'   larger values point to measurement error), and digit preference of the
#'   terminal (decimal) digit of weight and height: percentage ending in 0,
#'   in 0 or 5 (expected 20) and the index of dissimilarity from a uniform
#'   digit distribution (0 = none, 90 = all on one digit).
#' * `"completeness"`: percentage missing (`NA`) and percentage coded as
#'   "don't know", "missing" or "inconsistent" (read from the value labels) for
#'   key variables (`vars`). `NA` includes rows skipped by design (for example
#'   `m14` for births outside the reference period).
#'
#' All statistics are unweighted, as in the DHS data-quality tables. For PR
#' files the household population is the de facto population (`hv103 == 1`)
#' by default.
#'
#' @param data A recode data frame from [bdhs_read()] (numeric codes, not
#'   factors), or pooled data from [bdhs_pool()].
#' @param checks Checks to run (see Details); `NULL` runs all those the data
#'   support.
#' @param survey Optional survey reference when `data` does not carry one.
#' @param population For household members (PR): `"de_facto"` (default,
#'   `hv103 == 1`), `"de_jure"` (`hv102 == 1`) or `"all"`.
#' @param vars Variables for the `"completeness"` check; `NULL` uses a
#'   default list of key variables, keeping those present.
#'
#' @return A tibble of class `bdhs_quality` with columns `survey_id`,
#'   `recode`, `check`, `statistic`, `group` (sex, year, variable, ...),
#'   `value`, `benchmark` (the value expected with good data),
#'   `interpretation` and `n` (number of cases behind the value). The
#'   attribute `profiles` holds the single-year age distribution (`age`) and
#'   births by calendar year (`births`), drawn by [plot()][plot.bdhs_quality].
#' @references Pullum, T. W. (2006). An assessment of age and date reporting
#'   in the DHS surveys, 1985-2003. DHS Methodological Reports No. 5.
#'
#'   Pullum, T. W. and Staveteig, S. (2017). An assessment of the quality and
#'   consistency of age and date reporting in DHS surveys, 2000-2015. DHS
#'   Methodological Reports No. 19.
#' @seealso [plot.bdhs_quality()]
#' @export
#' @examples
#' pr <- bdhs_simulate(2022, "PR")
#' q <- bdhs_quality(pr, checks = c("age_heaping", "anthropometry"))
#' q[q$statistic %in% c("whipple", "myers"), c("statistic", "group", "value")]
#' br <- bdhs_simulate(2022, "BR")
#' bdhs_quality(br, checks = "births_by_year")
bdhs_quality <- function(data, checks = NULL, survey = NULL,
                         population = c("de_facto", "de_jure", "all"), vars = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, e.g. from {.fn bdhs_read}.")
  }
  population <- rlang::arg_match(population)
  checks <- if (is.null(checks)) quality_checks else unique(rlang::arg_match(checks, quality_checks, multiple = TRUE))
  if (!is.null(vars) && !is.character(vars)) {
    cli::cli_abort("{.arg vars} must be a character vector of variable names.")
  }
  recode <- tryCatch(data_recode(data), error = function(e) NA_character_)
  d_all <- lower_names(data)
  groups <- tryCatch(
    survey_row_groups(data, survey),
    error = function(e) list(list(survey_id = NA_character_, rows = seq_len(nrow(data))))
  )

  rows <- list()
  profiles <- list(age = list(), births = list())
  skipped <- list()
  for (g in groups) {
    d <- d_all[g$rows, , drop = FALSE]
    ctx <- list(survey_id = g$survey_id, recode = recode, population = population, vars = vars)
    births <- NULL
    if (any(c("birth_date_completeness", "births_by_year", "age_at_death") %in% checks)) {
      births <- quality_births(d)
    }
    for (chk in checks) {
      res <- switch(chk,
        age_heaping = quality_age_heaping(d, ctx),
        age_displacement = quality_age_displacement(d, ctx),
        birth_date_completeness = quality_birth_dates(births, ctx),
        births_by_year = quality_births_by_year(births, d, ctx),
        age_at_death = quality_age_at_death(births, ctx),
        anthropometry = quality_anthropometry(d, ctx),
        completeness = quality_completeness(d, ctx)
      )
      if (!is.null(res$missing)) {
        skipped[[chk]] <- res$missing
        next
      }
      if (!is.null(res$rows) && nrow(res$rows)) {
        res$rows$survey_id <- g$survey_id
        res$rows$recode <- recode
        rows[[length(rows) + 1L]] <- res$rows
      }
      for (p in names(res$profile)) {
        prof <- res$profile[[p]]
        prof$survey_id <- rep(g$survey_id, nrow(prof))
        profiles[[p]][[length(profiles[[p]]) + 1L]] <- prof
      }
    }
  }
  if (length(skipped)) {
    msg <- vapply(names(skipped), function(k) {
      paste0("{.val ", k, "}: needs ", cli_escape(skipped[[k]]))
    }, character(1))
    cli::cli_inform(c("i" = "Skipped checks the data do not support:", stats::setNames(msg, rep("*", length(msg)))))
  }
  cols <- c("survey_id", "recode", "check", "statistic", "group", "value", "benchmark", "interpretation", "n")
  out <- if (length(rows)) dplyr::bind_rows(rows) else quality_rows(character(), character(), numeric(), numeric())
  out$survey_id <- as.character(out$survey_id)
  out$recode <- as.character(out$recode)
  out <- tibble::as_tibble(out[, cols])
  profiles <- lapply(profiles, function(p) {
    if (!length(p)) {
      return(NULL)
    }
    p <- dplyr::bind_rows(p)
    p[, c("survey_id", setdiff(names(p), "survey_id"))]
  })
  attr(out, "profiles") <- profiles
  class(out) <- c("bdhs_quality", class(out))
  out
}

# Helpers -----------------------------------------------------------------------------------

quality_rows <- function(check, statistic, value, n, group = NA_character_,
                         benchmark = NA_real_, interpretation = NA_character_) {
  k <- max(length(statistic), length(value))
  tibble::tibble(
    check = rep_len(as.character(check), k),
    statistic = rep_len(as.character(statistic), k),
    group = rep_len(as.character(group), k),
    value = rep_len(as.numeric(value), k),
    benchmark = rep_len(as.numeric(benchmark), k),
    interpretation = rep_len(as.character(interpretation), k),
    n = rep_len(as.numeric(n), k)
  )
}

quality_missing <- function(vars) {
  list(missing = paste(vars, collapse = ", "))
}

pct <- function(x, universe = rep(TRUE, length(x))) {
  x <- x[universe & !is.na(universe)]
  if (!length(x)) {
    return(NA_real_)
  }
  100 * mean(x, na.rm = TRUE)
}

safe_ratio <- function(a, b) {
  if (is.na(b) || b == 0) NA_real_ else a / b
}

# Household members in the requested population.
quality_members <- function(d, population) {
  keep <- rep(TRUE, nrow(d))
  if (population == "de_facto" && "hv103" %in% names(d)) {
    keep <- dv(d, "hv103") %in% 1
  } else if (population == "de_jure" && "hv102" %in% names(d)) {
    keep <- dv(d, "hv102") %in% 1
  }
  keep
}

# Whipple's index: ages 23-62, preference for terminal digits 0 and 5.
whipple_index <- function(age) {
  age <- age[!is.na(age) & age >= 23 & age <= 62]
  if (!length(age)) {
    return(NA_real_)
  }
  100 * sum(age %% 5 == 0) / (length(age) / 5)
}

# Myers' blended index over ages [lo, hi]: two series of decades started
# 10 years apart, weighted d + 1 and 9 - d.
myers_index <- function(age, lo = 10, hi = 89) {
  age <- age[!is.na(age)]
  s1 <- s2 <- numeric(10)
  for (dg in 0:9) {
    s1[dg + 1] <- sum(age >= lo & age <= hi - 10 & age %% 10 == dg)
    s2[dg + 1] <- sum(age >= lo + 10 & age <= hi & age %% 10 == dg)
  }
  blended <- (0:9 + 1) * s1 + (9 - 0:9) * s2
  if (sum(blended) == 0) {
    return(NA_real_)
  }
  0.5 * sum(abs(100 * blended / sum(blended) - 10))
}

whipple_interpretation <- function(w) {
  if (is.na(w)) {
    return(NA_character_)
  }
  # United Nations categories for Whipple's index.
  if (w < 105) "highly accurate" else if (w < 110) "fairly accurate" else if (w < 125) "approximate" else if (w < 175) "rough" else "very rough"
}

# Age heaping --------------------------------------------------------------------------------

quality_age_heaping <- function(d, ctx) {
  if (!"hv105" %in% names(d)) {
    return(quality_missing("hv105 (household member age, PR)"))
  }
  keep <- quality_members(d, ctx$population)
  age_all <- dv(d, "hv105")[keep]
  sex <- if ("hv104" %in% names(d)) dv(d, "hv104")[keep] else rep(NA_real_, length(age_all))
  valid <- !is.na(age_all) & age_all >= 0 & age_all <= 95
  sets <- list(total = rep(TRUE, length(age_all)), male = sex %in% 1, female = sex %in% 2)
  if (!any(!is.na(sex))) sets <- sets["total"]
  rows <- list()
  for (s in names(sets)) {
    a <- age_all[valid & sets[[s]]]
    w <- whipple_index(a)
    rows[[length(rows) + 1L]] <- quality_rows("age_heaping", "whipple", w, sum(a >= 23 & a <= 62),
      group = s, benchmark = 100, interpretation = whipple_interpretation(w))
    rows[[length(rows) + 1L]] <- quality_rows("age_heaping", "myers", myers_index(a), sum(a >= 10 & a <= 89),
      group = s, benchmark = 0)
  }
  miss <- !valid
  rows[[length(rows) + 1L]] <- quality_rows("age_heaping", "pct_age_missing", pct(miss), length(miss),
    group = "total", benchmark = 0)
  a <- age_all[valid]
  sx <- sex[valid]
  sx_lab <- ifelse(sx %in% 1, "male", ifelse(sx %in% 2, "female", NA_character_))
  prof <- as.data.frame(table(sex = sx_lab, age = a), stringsAsFactors = FALSE)
  prof <- tibble::tibble(sex = prof$sex, age = as.integer(prof$age), n = as.integer(prof$Freq))
  list(rows = dplyr::bind_rows(rows), profile = list(age = prof))
}

# Age displacement --------------------------------------------------------------------------

quality_age_displacement <- function(d, ctx) {
  if (!all(c("hv105", "hv104") %in% names(d))) {
    return(quality_missing("hv105 and hv104 (household member age and sex, PR)"))
  }
  keep <- quality_members(d, ctx$population)
  age <- dv(d, "hv105")[keep]
  sex <- dv(d, "hv104")[keep]
  out <- list()
  specs <- list(
    female = list(c(14, 15), c(50, 49), c(50, 51)),
    male = list(c(14, 15), c(50, 49), c(50, 51), c(55, 54))
  )
  for (s in names(specs)) {
    code <- if (s == "male") 1 else 2
    cnt <- function(a) sum(age %in% a & sex %in% code)
    for (p in specs[[s]]) {
      a <- cnt(p[1])
      b <- cnt(p[2])
      r <- safe_ratio(a, b)
      edge <- p[1] %in% c(14, 50, 55) && p[2] %in% c(15, 49, 54)
      interp <- if (is.na(r)) {
        NA_character_
      } else if (r > 1.15 && edge) {
        paste0("excess at ", p[1], ": possible displacement out of the eligible ages")
      } else if (r > 1.15) {
        paste0("excess at ", p[1], " (heaping)")
      } else if (r < 0.85) {
        paste0("deficit at ", p[1], " relative to ", p[2])
      } else {
        "no marked excess"
      }
      out[[length(out) + 1L]] <- quality_rows("age_displacement", paste0("ratio_", p[1], "_to_", p[2]), r, a + b,
        group = s, benchmark = 1, interpretation = interp)
    }
  }
  list(rows = dplyr::bind_rows(out))
}

# Births (long form) -----------------------------------------------------------------------

# A long data frame of births from BR/KR (one row per birth) or from the wide
# birth history of IR (b3_01 ... b3_20). NULL when there is no birth history.
quality_births <- function(d) {
  nm <- names(d)
  pick <- c("b3", "b5", "b6", "b7", "b10", "m15")
  if ("b3" %in% nm) {
    out <- lapply(pick, function(v) if (v %in% nm) dv(d, v) else rep(NA_real_, nrow(d)))
    names(out) <- pick
    out$v008 <- if ("v008" %in% nm) dv(d, "v008") else rep(NA_real_, nrow(d))
    out <- tibble::as_tibble(out)
    attr(out, "has") <- intersect(c(pick, "v008"), nm)
    attr(out, "wide") <- FALSE
    return(out)
  }
  slots <- sub("^b3_", "", grep("^b3_[0-9]+$", nm, value = TRUE))
  if (!length(slots)) {
    return(NULL)
  }
  v008 <- if ("v008" %in% nm) dv(d, "v008") else rep(NA_real_, nrow(d))
  parts <- lapply(slots, function(s) {
    cols <- lapply(pick[1:5], function(v) {
      col <- paste0(v, "_", s)
      if (col %in% nm) dv(d, col) else rep(NA_real_, nrow(d))
    })
    names(cols) <- pick[1:5]
    cols$m15 <- rep(NA_real_, nrow(d))
    cols$v008 <- v008
    tb <- tibble::as_tibble(cols)
    tb[!is.na(tb$b3), , drop = FALSE]
  })
  out <- dplyr::bind_rows(parts)
  has <- c(pick[1:5][paste0(pick[1:5], "_", slots[[1]]) %in% nm], if ("v008" %in% nm) "v008")
  attr(out, "has") <- has
  attr(out, "wide") <- TRUE
  out
}

cmc_year <- function(cmc) 1900 + (cmc - 1) %/% 12

# Birth date completeness --------------------------------------------------------------------

quality_birth_dates <- function(b, ctx) {
  if (is.null(b) || !"b10" %in% attr(b, "has")) {
    return(quality_missing("b10 (completeness of birth date; BR, KR or IR)"))
  }
  complete <- b$b10 %in% c(0, 1)
  known <- !is.na(b$b10)
  alive <- b$b5 %in% 1
  dead <- b$b5 %in% 0
  age_mo <- b$v008 - b$b3
  sets <- list(
    all = known,
    living = known & alive,
    dead = known & dead,
    last_5_years = known & !is.na(age_mo) & age_mo < 60,
    last_15_years = known & !is.na(age_mo) & age_mo < 180
  )
  if (!"b5" %in% attr(b, "has")) sets$living <- sets$dead <- NULL
  if (!all(c("b3", "v008") %in% attr(b, "has"))) sets$last_5_years <- sets$last_15_years <- NULL
  rows <- lapply(names(sets), function(s) {
    quality_rows("birth_date_completeness", "pct_month_and_year", pct(complete, sets[[s]]), sum(sets[[s]]),
      group = s, benchmark = 100)
  })
  if (!is.null(sets$living)) {
    rows[[length(rows) + 1L]] <- quality_rows("birth_date_completeness", "pct_living_age_imputed",
      pct(!complete, sets$living), sum(sets$living), group = "living", benchmark = 0)
  }
  list(rows = dplyr::bind_rows(rows))
}

# Births by calendar year ---------------------------------------------------------------------

quality_births_by_year <- function(b, d, ctx) {
  if (is.null(b) || !all(c("b3", "v008") %in% attr(b, "has"))) {
    return(quality_missing("b3 and v008 (dates of birth and interview; BR, KR or IR)"))
  }
  b <- b[!is.na(b$b3) & !is.na(b$v008), , drop = FALSE]
  if (!nrow(b)) {
    return(list(rows = NULL))
  }
  age_mo <- b$v008 - b$b3
  # KR holds births in the last 5 years (3 in 1993-94) only.
  # A few births just outside the window can appear, so use shares, not the maximum.
  truncated <- mean(age_mo >= 60) < 0.05
  window <- if (!truncated) max(age_mo) + 1 else if (mean(age_mo >= 36) < 0.05) 36 else 60
  first_int_year <- cmc_year(min(b$v008))
  year <- cmc_year(b$b3)
  rows <- list()

  # Calendar years: ratios only for years whose neighbours are fully observed.
  counts <- table(year)
  yrs <- as.integer(names(counts))
  cnt <- stats::setNames(as.numeric(counts), yrs)
  lo_full <- if (truncated) cmc_year(max(b$v008) - window + 1) + 1 else min(yrs)
  cand <- yrs[yrs - 1 >= lo_full & yrs + 1 <= first_int_year - 1 & yrs >= first_int_year - 21]
  cutoff <- first_int_year - 5
  for (x in cand) {
    nb <- c(cnt[as.character(x - 1)], cnt[as.character(x + 1)])
    nb[is.na(nb)] <- 0
    r <- if (sum(nb) > 0) 200 * cnt[[as.character(x)]] / sum(nb) else NA_real_
    rows[[length(rows) + 1L]] <- quality_rows("births_by_year", "calendar_year_ratio", r, cnt[[as.character(x)]],
      group = as.character(x), benchmark = 100, interpretation = cyr_interpretation(r, x, cutoff))
  }

  # Completed years before the interview.
  k <- age_mo %/% 12
  kc <- table(factor(k, levels = 0:max(k)))
  kmax_full <- if (truncated) floor(window / 12) - 1 else min(max(k), 25)
  for (kk in seq_len(max(0, min(kmax_full - 1, 20)))) {
    r <- 200 * kc[[kk + 1]] / (kc[[kk]] + kc[[kk + 2]])
    rows[[length(rows) + 1L]] <- quality_rows("births_by_year", "years_before_ratio", r, kc[[kk + 1]],
      group = as.character(kk), benchmark = 100,
      interpretation = if (kk %in% 4:5) cyr_interpretation(r, kk, NA) else NA_character_)
  }
  if (kmax_full >= 4) {
    r <- 100 * safe_ratio(kc[["4"]], kc[["3"]])
    rows[[length(rows) + 1L]] <- quality_rows("births_by_year", "births_year4_vs_year3", r, kc[["4"]] + kc[["3"]],
      group = "48-59 vs 36-47 months", benchmark = 100,
      interpretation = if (!is.na(r) && r < 90) "deficit just inside the 5-year window" else NA_character_)
  }
  if ("m15" %in% attr(b, "has")) {
    u <- age_mo < 60
    rows[[length(rows) + 1L]] <- quality_rows("births_by_year", "pct_last5_with_m15",
      pct(!is.na(b$m15) & !b$m15 %in% c(98, 99), u), sum(u), group = "births 0-59 months",
      interpretation = "place of delivery reported; m15 covers fewer than 5 years in some rounds")
  }
  prof <- tibble::tibble(year = yrs, n = as.integer(cnt), partial = yrs >= first_int_year | (truncated & yrs < lo_full),
    cutoff_year = cutoff)
  list(rows = dplyr::bind_rows(rows), profile = list(births = prof))
}

cyr_interpretation <- function(r, x, cutoff) {
  if (is.na(r)) {
    return(NA_character_)
  }
  tag <- if (r < 90) "deficit" else if (r > 110) "excess" else return(NA_character_)
  if (!is.na(cutoff) && x == cutoff) {
    paste(tag, "in the first year of the 5-year window")
  } else if (!is.na(cutoff) && x == cutoff - 1) {
    paste(tag, "in the year before the 5-year window")
  } else {
    tag
  }
}

# Age at death -------------------------------------------------------------------------------

quality_age_at_death <- function(b, ctx) {
  if (is.null(b) || !"b6" %in% attr(b, "has")) {
    return(quality_missing("b6 (age at death; BR, KR or IR)"))
  }
  b6 <- b$b6[!is.na(b$b6)]
  b7 <- b$b7[!is.na(b$b6)]
  n <- length(b6)
  if (!n) {
    return(list(rows = NULL))
  }
  unit <- b6 %/% 100
  num <- b6 %% 100
  missing <- b6 >= 997 | num == 99 | !unit %in% 1:3
  ok <- !missing
  rows <- list(
    quality_rows("age_at_death", "pct_reported_days", pct(unit == 1 & ok), n, group = "deaths"),
    quality_rows("age_at_death", "pct_reported_months", pct(unit == 2 & ok), n, group = "deaths"),
    quality_rows("age_at_death", "pct_reported_years", pct(unit == 3 & ok), n, group = "deaths"),
    quality_rows("age_at_death", "pct_missing", pct(missing), n, group = "deaths", benchmark = 0)
  )
  d11 <- sum(b6 == 211)
  d13 <- sum(b6 == 213)
  d12 <- sum(b6 %in% c(212, 301))
  h <- 100 * safe_ratio(d12, (d11 + d13) / 2)
  rows[[5]] <- quality_rows("age_at_death", "heaping_12_months", h, d11 + d12 + d13,
    group = "reported (12 months or 1 year)", benchmark = 100, interpretation = heaping_interpretation(h))
  rows[[6]] <- quality_rows("age_at_death", "pct_12_months_as_1_year", 100 * safe_ratio(sum(b6 == 301), d12), d12,
    group = "reported")
  if ("b7" %in% attr(b, "has")) {
    i12 <- sum(b7 %in% 12)
    hi <- 100 * safe_ratio(i12, (sum(b7 %in% 11) + sum(b7 %in% 13)) / 2)
    rows[[7]] <- quality_rows("age_at_death", "heaping_12_months", hi, sum(b7 %in% 11:13),
      group = "imputed months (b7)", benchmark = 100, interpretation = heaping_interpretation(hi))
  }
  list(rows = dplyr::bind_rows(rows))
}

heaping_interpretation <- function(h) {
  if (is.na(h)) NA_character_ else if (h > 150) "strong heaping at 12 months" else if (h > 110) "some heaping at 12 months" else "little heaping"
}

# Anthropometry ------------------------------------------------------------------------------

anthro_specs <- function() {
  list(
    list(prefix = "hc", who = "children", age = "hc1"),
    list(prefix = "hw", who = "children", age = "hw1"),
    list(prefix = "ha", who = "women", age = "ha1"),
    list(prefix = "hb", who = "men", age = "hb1")
  )
}

quality_anthropometry <- function(d, ctx) {
  nm <- names(d)
  rows <- list()
  for (s in anthro_specs()) {
    p <- s$prefix
    vars <- paste0(p, c("2", "3", "70", "71", "72"))
    if (!any(vars %in% nm)) next
    u <- if (s$age %in% nm) !is.na(dv(d, s$age)) & dv(d, s$age) < 9990 else rep(TRUE, nrow(d))
    if (s$who == "children") {
      if (s$age %in% nm) u <- u & dv(d, s$age) < 60
      if (p == "hw" && "b5" %in% nm) u <- u & dv(d, "b5") %in% 1
    }
    if (p != "hw") u <- u & quality_members(d, ctx$population)
    grp <- s$who
    measures <- c(weight = "2", height = "3")
    for (label in names(measures)) {
      v <- paste0(p, measures[[label]])
      if (!v %in% nm) next
      x <- dv(d, v)[u]
      valid <- !is.na(x) & x < 9990
      rows[[length(rows) + 1L]] <- quality_rows("anthropometry", paste0("pct_missing_", label), pct(!valid),
        length(x), group = grp, benchmark = 0)
      dig <- round(x[valid]) %% 10
      if (length(dig)) {
        dist <- tabulate(dig + 1, 10) / length(dig) * 100
        rows[[length(rows) + 1L]] <- quality_rows("anthropometry",
          paste0(label, "_terminal_digit_", c("pct_0", "pct_0_or_5", "dissimilarity")),
          c(dist[1], dist[1] + dist[6], 0.5 * sum(abs(dist - 10))), length(dig), group = grp,
          benchmark = c(10, 20, 0))
      }
    }
    z_spec <- list(c("70", "haz", -6, 6), c("71", "waz", -6, 5), c("72", "whz", -5, 5))
    # z-scores are only for children (adult ha70-ha72 are not WHO z-scores).
    if (s$who != "children") z_spec <- list()
    for (z in z_spec) {
      v <- paste0(p, z[1])
      if (!v %in% nm) next
      x <- dv(d, v)[u]
      have <- !is.na(x)
      flagged <- have & x >= 9996
      zs <- x[have & !flagged] / 100
      out <- zs < as.numeric(z[3]) | zs > as.numeric(z[4])
      sd_z <- if (length(zs) > 1) stats::sd(zs) else NA_real_
      rows[[length(rows) + 1L]] <- quality_rows("anthropometry",
        paste0(z[2], c("_pct_missing", "_pct_flagged", "_pct_out_of_who_range", "_mean", "_sd")),
        c(pct(!have), pct(flagged, have), pct(out), if (length(zs)) mean(zs) else NA_real_, sd_z),
        c(length(x), sum(have), length(zs), length(zs), length(zs)),
        group = grp, benchmark = c(0, 0, 0, NA, 1),
        interpretation = c(NA, NA, NA, NA, if (is.na(sd_z)) NA else if (sd_z > 1.2) "SD above the WHO range (1.0-1.2): possible measurement error" else if (sd_z < 0.8) "unusually small SD" else "SD within the expected range"))
    }
  }
  if (!length(rows)) {
    return(quality_missing("hc2/hc3/hc70-hc72 (PR), hw2/hw3/hw70-hw72 (KR/BR) or ha2/ha3 (PR)"))
  }
  list(rows = dplyr::bind_rows(rows))
}

# Completeness of key variables ------------------------------------------------------------------

quality_completeness <- function(d, ctx) {
  vars <- if (is.null(ctx$vars)) intersect(quality_default_vars, names(d)) else tolower(ctx$vars)
  absent <- setdiff(vars, names(d))
  if (length(absent) && !is.null(ctx$vars)) {
    cli::cli_warn("Variable{?s} {.field {absent}} not found; skipped in the completeness check.")
  }
  vars <- intersect(vars, names(d))
  if (!length(vars)) {
    return(quality_missing("at least one key variable (e.g. v106, v190, hv105, m15)"))
  }
  rows <- lapply(vars, function(v) {
    col <- d[[v]]
    x <- if (is.factor(col)) as.character(col) else zap_all(col)
    na <- is.na(x)
    labs <- attr(col, "labels", exact = TRUE)
    dk_codes <- if (length(labs)) unname(labs[grepl("don.?t know|^dk$|missing|inconsistent|not stated", names(labs), ignore.case = TRUE)]) else numeric()
    dk <- !na & x %in% dk_codes
    quality_rows("completeness", c("pct_na", "pct_dk_or_missing_code"), c(pct(na), pct(dk, !na)),
      c(length(x), sum(!na)), group = v, benchmark = 0,
      interpretation = c("includes rows skipped by design", NA))
  })
  list(rows = dplyr::bind_rows(rows))
}

# Plots --------------------------------------------------------------------------------------

#' Plot data-quality profiles
#'
#' Draws the single-year age distribution of the household population (ages
#' ending in 0 or 5 highlighted) or the number of births by calendar year
#' (with the first year of the five-year window marked) from
#' [bdhs_quality()] results.
#'
#' @param x,object A `bdhs_quality` result.
#' @param type `"age"` (default) or `"births"`.
#' @param ... Unused.
#' @return A ggplot object.
#' @export
#' @examplesIf rlang::is_installed("ggplot2")
#' q <- bdhs_quality(bdhs_simulate(2022, "PR"), checks = "age_heaping")
#' plot(q)
plot.bdhs_quality <- function(x, type = c("age", "births"), ...) {
  autoplot.bdhs_quality(x, type = type, ...)
}

#' @rdname plot.bdhs_quality
#' @exportS3Method ggplot2::autoplot
autoplot.bdhs_quality <- function(object, type = c("age", "births"), ...) {
  check_installed("ggplot2", "to plot data-quality profiles")
  type <- rlang::arg_match(type)
  prof <- attr(object, "profiles", exact = TRUE)[[type]]
  if (is.null(prof) || !nrow(prof)) {
    need <- if (type == "age") "\"age_heaping\" on a PR file" else "\"births_by_year\" on a BR, KR or IR file"
    cli::cli_abort("No {type} profile in {.arg object}; run {.fn bdhs_quality} with {need}.")
  }
  ids <- unique(prof$survey_id)
  lab <- ifelse(is.na(ids), "", survey_label(ids) %|NA|% ids)
  prof$.survey <- factor(lab[match(prof$survey_id, ids)], levels = unique(lab))
  multi <- length(ids) > 1L
  if (type == "age") {
    prof$.digit <- ifelse(prof$age %% 5 == 0, "Ends in 0 or 5", "Other")
    prof$sex <- ifelse(is.na(prof$sex), "Total", tools::toTitleCase(prof$sex))
    p <- ggplot2::ggplot(prof, ggplot2::aes(x = .data$age, y = .data$n, fill = .data$.digit)) +
      ggplot2::geom_col(width = 0.9) +
      ggplot2::scale_fill_manual(values = c("Ends in 0 or 5" = "#d6612a", Other = "#2a78d6"), name = NULL) +
      ggplot2::labs(title = "Household population by single year of age", x = "Age (years)", y = "Members")
    p <- p + if (multi) {
      ggplot2::facet_grid(ggplot2::vars(.data$.survey), ggplot2::vars(.data$sex), scales = "free_y")
    } else {
      ggplot2::facet_wrap(ggplot2::vars(.data$sex))
    }
  } else {
    prof$.status <- ifelse(prof$partial, "Partly observed", "Complete year")
    cut <- unique(prof[, c(".survey", "cutoff_year")])
    p <- ggplot2::ggplot(prof, ggplot2::aes(x = .data$year, y = .data$n, fill = .data$.status)) +
      ggplot2::geom_col(width = 0.85) +
      ggplot2::geom_vline(data = cut, ggplot2::aes(xintercept = .data$cutoff_year - 0.5),
        linetype = "dashed", colour = "grey30") +
      ggplot2::scale_fill_manual(values = c("Complete year" = "#2a78d6", "Partly observed" = "#a9c6ec"), name = NULL) +
      ggplot2::labs(title = "Births by calendar year", x = "Calendar year of birth", y = "Births",
        caption = "Dashed line: start of the five-year window (first interview year - 5).")
    if (multi) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$.survey), scales = "free")
  }
  if (!multi && nzchar(levels(prof$.survey)[1])) {
    p <- p + ggplot2::labs(subtitle = paste("Bangladesh DHS", levels(prof$.survey)[1]))
  }
  p + explore_theme() + ggplot2::theme(legend.position = "top")
}
