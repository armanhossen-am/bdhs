# Pooling survey rounds -------------------------------------------------------------------

# Fallback for the bundled `bdhs_pool_population` dataset (used until it is
# available): female population 15-49 (UN World Population Prospects 2024,
# interpolated to the mean interview date), number of women interviewed
# (DHS API) and the share of women 15-49 who were ever married (DHS API
# indicator MA_MSTA_W_NMA). Source: data-raw/inputs/bdhs_pooling_population.csv.
.pool_population_fallback <- data.frame(
  survey_id = c(
    "BD1994DHS", "BD1997DHS", "BD2000DHS", "BD2004DHS", "BD2007DHS",
    "BD2011DHS", "BD2014DHS", "BD2017DHS", "BD2022DHS"
  ),
  female_15_49 = c(
    28096489, 30782776, 33689291, 36965636, 39019159,
    41427050, 43360901, 45459595, 48342561
  ),
  n_women_sample = c(9640, 9127, 10544, 11440, 10996, 17842, 17863, 20127, 30078),
  p_ever_married = c(0.852, 0.839, 0.827, 0.834, 0.841, 0.853, 0.846, 0.851, 0.844),
  stringsAsFactors = FALSE
)

# The population table: the bundled dataset when present, else the fallback.
pool_population <- function() {
  ns <- asNamespace("bdhs")
  lazy <- ns[[".__NAMESPACE__."]][["lazydata"]]
  pop <- if (is.environment(lazy)) get0("bdhs_pool_population", envir = lazy, inherits = FALSE)
  if (is.null(pop)) {
    pop <- get0("bdhs_pool_population", envir = ns, inherits = FALSE)
  }
  if (is.null(pop) || !all(c("survey_id", "female_15_49", "n_women_sample") %in% names(pop))) {
    return(.pool_population_fallback)
  }
  pop <- as.data.frame(pop)
  if (!"p_ever_married" %in% names(pop)) {
    pop$p_ever_married <- .pool_population_fallback$p_ever_married[
      match(pop$survey_id, .pool_population_fallback$survey_id)
    ]
  }
  pop
}

#' Pool several BDHS rounds into one dataset
#'
#' Stacks data from several survey rounds of the same recode type, adds a
#' `survey_id` column and a pooled analysis weight `pool_wt`, and reconciles
#' columns whose value labels changed between rounds. Use it for trend
#' analysis and pooled models; [bdhs_design()] then builds a design with
#' round-specific clusters and strata and uses `pool_wt` automatically.
#'
#' @section Pooled weights:
#' DHS weights are *normalized*: within each round they average 1, so they sum
#' to the round's sample size (see the Guide to DHS Statistics, "Sampling
#' weights" and "Normalization of weights"). Three choices are offered for
#' `pool_wt`:
#'
#' * `"normalized"` (default): `v005 / 1e6` (`hv005`, `mv005` for household
#'   and men's files). Rounds contribute in proportion to their sample size.
#'   Fine for most pooled models with survey fixed effects.
#' * `"denormalized"`: the weights are scaled back to population totals, so
#'   each round contributes in proportion to the population it represents:
#'   `pool_wt = v005 / 1e6 * N / n`, where `N` is the population the sample
#'   represents (female population aged 15-49 times the proportion ever
#'   married, because every BDHS women's sample is of ever-married women) and
#'   `n` the number of women interviewed. This is the de-normalization
#'   recommended by the DHS Program for pooling surveys (DHS Sampling and
#'   Household Listing Manual, ICF 2012; Guide to DHS Statistics, Croft et
#'   al. 2018). Population figures come from the bundled
#'   `bdhs_pool_population` table (UN World Population Prospects 2024 and the
#'   DHS API). Household and member files use the same factor, which
#'   approximates the inverse sampling fraction of households.
#' * `"equal"`: each round gets the same total weight (`n_total / k` for `k`
#'   rounds), so each round counts equally whatever its size.
#'
#' Variance estimation is unaffected by a constant rescaling within rounds;
#' only the relative contribution of the rounds changes.
#'
#' @param ... Data frames to pool (one round each, or already pooled data),
#'   or a single list of data frames.
#' @param weights How to build `pool_wt`: `"normalized"`, `"denormalized"`
#'   or `"equal"` (see Details).
#' @param harmonize If `TRUE`, run [bdhs_harmonize()] on every round before
#'   stacking, so that comparable variables are available in every round.
#' @param vars Optional columns to keep. With `harmonize = TRUE`, names of
#'   harmonized variables (see [bdhs_harmonized_vars()]) to create; raw DHS
#'   columns named in `vars` are kept as well. Identifier and design columns
#'   are always kept. `NULL` keeps every column.
#' @return A tibble with `survey_id` as first column, the stacked data and
#'   `pool_wt`, with attributes `bdhs_recode`, `bdhs_pooled = TRUE` and
#'   `bdhs_pool_weights`.
#' @references
#' Croft, Trevor N., Aileen M. J. Marshall, Courtney K. Allen, et al. 2018.
#' *Guide to DHS Statistics.* Rockville, Maryland, USA: ICF.
#' <https://dhsprogram.com/data/Guide-to-DHS-Statistics/>
#'
#' ICF International. 2012. *Demographic and Health Survey Sampling and
#' Household Listing Manual.* MEASURE DHS, Calverton, Maryland, USA: ICF
#' International.
#' @export
#' @examples
#' p <- bdhs_pool(bdhs_simulate(2014, "KR"), bdhs_simulate(2022, "KR"))
#' table(p$survey_id)
#' tapply(p$pool_wt, p$survey_id, sum)
#'
#' p2 <- bdhs_pool(
#'   list(bdhs_simulate(2014, "KR"), bdhs_simulate(2022, "KR")),
#'   weights = "equal", harmonize = TRUE,
#'   vars = c("division_trend6", "residence", "child_age_months")
#' )
#' tapply(p2$pool_wt, p2$survey_id, sum)
bdhs_pool <- function(..., weights = c("normalized", "denormalized", "equal"), harmonize = FALSE, vars = NULL) {
  weights <- rlang::arg_match(weights)
  dots <- list(...)
  if (length(dots) == 1L && is.list(dots[[1]]) && !is.data.frame(dots[[1]])) {
    dots <- dots[[1]]
  }
  if (length(dots) < 1L || !all(vapply(dots, is.data.frame, logical(1)))) {
    cli::cli_abort("Supply data frames to pool, either in {.arg ...} or as one list.")
  }
  dots <- unname(dots)

  recodes <- vapply(dots, function(d) data_recode(d), character(1))
  if (length(unique(recodes)) > 1L) {
    cli::cli_abort(c(
      "All data must come from the same recode type.",
      "i" = "Got {.val {recodes}}. Pool each recode separately and link them with {.fn bdhs_merge}."
    ))
  }
  recode <- recodes[[1]]

  sids <- lapply(dots, pool_survey_rows)
  all_ids <- unlist(lapply(sids, unique))
  if (anyDuplicated(all_ids)) {
    cli::cli_abort("Round{?s} {.val {unique(all_ids[duplicated(all_ids)])}} appear{?s/} more than once.")
  }

  if (harmonize) {
    hvars <- if (is.null(vars)) NULL else intersect(vars, names(crosswalk()$vars))
    if (!is.null(vars) && !length(hvars)) {
      cli::cli_abort("None of {.arg vars} is a harmonized variable; see {.fn bdhs_harmonized_vars}.")
    }
    # Harmonize round by round (labels are still intact), quietly; one
    # summary message follows.
    dots <- lapply(seq_along(dots), function(i) {
      d <- dots[[i]]
      if (!"survey_id" %in% names(d)) d <- add_columns(d, list(survey_id = sids[[i]]))
      suppressMessages(bdhs_harmonize(d, vars = hvars, recode = recode))
    })
  }

  dv <- .design_vars[[design_prefix(recode)]]
  if (!is.null(vars)) {
    always <- c(
      "survey_id", "caseid", "hhid", "hvidx", "bidx", "midx", "b16", "d005", "hv028", "awfactt",
      paste0(design_prefix(recode), c("000", "001", "002", "003")),
      unname(dv[c("psu", "strata", "strata_design", "region", "residence", "weight")])
    )
    harmonized <- unique(unlist(lapply(dots, function(d) attr(d, "bdhs_harmonized", exact = TRUE))))
    keep_cols <- unique(c(always, harmonized, vars))
    dots <- lapply(dots, function(d) {
      d[, intersect(names(d), keep_cols), drop = FALSE]
    })
  }

  dots <- reconcile_columns(dots)

  wt <- lapply(seq_along(dots), function(i) {
    d <- dots[[i]]
    w <- if (dv[["weight"]] %in% names(d)) as.numeric(zap_all(d[[dv[["weight"]]]])) / 1e6 else rep(NA_real_, nrow(d))
    w
  })
  if (all(vapply(wt, function(w) all(is.na(w)), logical(1)))) {
    cli::cli_warn("Weight variable {.field {dv[['weight']]}} not found; {.field pool_wt} is {.val NA}.")
  }

  pieces <- lapply(seq_along(dots), function(i) {
    d <- dots[[i]]
    d$survey_id <- NULL
    tibble::as_tibble(c(list(survey_id = rep_len(sids[[i]], nrow(d))), as.list(d)))
  })
  out <- dplyr::bind_rows(pieces)
  # Plain columns lose their variable labels when combined: restore them
  # (working on the bare list avoids a tibble copy per column).
  cls <- class(out)
  out <- unclass(out)
  for (nm in names(out)) {
    if (is.null(attr(out[[nm]], "label", exact = TRUE))) {
      for (pc in pieces) {
        lab <- if (nm %in% names(pc)) attr(pc[[nm]], "label", exact = TRUE)
        if (!is.null(lab)) {
          attr(out[[nm]], "label") <- lab
          break
        }
      }
    }
  }
  class(out) <- cls
  sid_all <- out$survey_id
  w <- unlist(wt)
  out$pool_wt <- pool_weights(w, sid_all, weights)
  attr(out$pool_wt, "label") <- paste0("Pooled weight (", weights, ")")

  attr(out, "bdhs_recode") <- recode
  attr(out, "bdhs_pooled") <- TRUE
  attr(out, "bdhs_pool_weights") <- weights
  harmonized <- unique(unlist(lapply(dots, function(d) attr(d, "bdhs_harmonized", exact = TRUE))))
  if (length(harmonized)) {
    attr(out, "bdhs_harmonized") <- harmonized
    pool_harmonize_summary(out, harmonized)
  }
  out
}

# Which harmonized variables are entirely NA in some rounds.
pool_harmonize_summary <- function(out, harmonized) {
  harmonized <- intersect(setdiff(harmonized, "survey_id"), names(out))
  rounds <- unique(out$survey_id)
  gaps <- character()
  for (v in harmonized) {
    all_na <- tapply(is.na(out[[v]]), factor(out$survey_id, levels = rounds), all)
    if (any(all_na)) {
      gaps <- c(gaps, paste0("{.field ", v, "}: ", paste(survey_label(rounds[all_na]), collapse = ", ")))
    }
  }
  msg <- c("i" = "Harmonized {length(harmonized)} variable{?s} in {length(rounds)} round{?s}.")
  if (length(gaps)) {
    names(gaps) <- rep("*", length(gaps))
    msg <- c(msg, "!" = "Missing (all {.val NA}) in some rounds:", gaps,
      "i" = "See {.run bdhs::bdhs_harmonized_vars()} for availability by round."
    )
  }
  cli::cli_inform(msg)
}

pool_survey_rows <- function(d) {
  if ("survey_id" %in% names(d)) {
    s <- as.character(zap_all(d$survey_id))
    u <- unique(s)
    ids <- bdhs_survey_id(u, error = FALSE)
    if (anyNA(ids)) {
      cli::cli_abort("Unknown survey id{?s} {.val {u[is.na(ids)]}} in a {.field survey_id} column.")
    }
    return(if (length(u) == 1L) rep(ids, nrow(d)) else ids[match(s, u)])
  }
  id <- data_survey_id(d)
  if (length(id) != 1L) {
    cli::cli_abort("Each data frame must come from one round or carry a {.field survey_id} column.")
  }
  rep(id, nrow(d))
}

pool_weights <- function(w, sid, method) {
  out <- switch(method,
    normalized = w,
    equal = {
      k <- length(unique(sid))
      tot <- tapply(w, sid, sum, na.rm = TRUE)
      w * (length(w) / k) / tot[sid]
    },
    denormalized = {
      pop <- pool_population()
      i <- match(sid, pop$survey_id)
      if (anyNA(i)) {
        cli::cli_abort("No population figures for {.val {unique(sid[is.na(i)])}}; use {.code weights = \"normalized\"} or {.code \"equal\"}.")
      }
      p_em <- pop$p_ever_married[i]
      p_em[is.na(p_em)] <- 1
      w * pop$female_15_49[i] * p_em / pop$n_women_sample[i]
    }
  )
  as.numeric(out)
}

# Make columns stackable across rounds: labelled columns with different value
# labels lose their labels (codes kept), and type clashes become character.
reconcile_columns <- function(dots) {
  cols <- unique(unlist(lapply(dots, names)))
  zapped <- character()
  to_chr <- character()
  for (nm in cols) {
    has <- which(vapply(dots, function(d) nm %in% names(d), logical(1)))
    if (length(has) < 2L) next
    xs <- lapply(dots[has], function(d) d[[nm]])
    chr <- vapply(xs, function(x) is.character(x) || is.factor(x), logical(1))
    if (any(chr) && !all(chr)) {
      for (i in has) dots[[i]][[nm]] <- as.character(haven::as_factor(dots[[i]][[nm]], levels = "values"))
      to_chr <- c(to_chr, nm)
      next
    }
    lab <- vapply(xs, function(x) inherits(x, "haven_labelled"), logical(1))
    if (!any(lab)) next
    labs <- lapply(xs, function(x) attr(x, "labels", exact = TRUE))
    same <- all(lab) && all(vapply(labs[-1], identical, logical(1), labs[[1]]))
    if (!same) {
      for (i in has) {
        x <- dots[[i]][[nm]]
        vl <- attr(x, "label", exact = TRUE)
        x <- zap_all(x)
        attr(x, "label") <- vl
        dots[[i]][[nm]] <- x
      }
      zapped <- c(zapped, nm)
    }
  }
  if (length(zapped)) {
    cli::cli_inform(c(
      "i" = "Value labels differ between rounds for {length(zapped)} column{?s}; kept the codes and dropped the labels: {.var {zapped}}.",
      "i" = "DHS codes are not always comparable across rounds; use {.code harmonize = TRUE} or {.fn bdhs_harmonize} for comparable variables."
    ))
  }
  if (length(to_chr)) {
    cli::cli_inform(c("i" = "Converted {length(to_chr)} column{?s} with mixed types to character: {.var {to_chr}}."))
  }
  dots
}
