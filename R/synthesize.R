# Calibrated synthetic BDHS data ----------------------------------------------------

#' Calibrated synthetic BDHS data
#'
#' Builds **synthetic** recode data with the structure of [bdhs_simulate()]
#' whose weighted estimates reproduce the *official* DHS values of a curated
#' set of indicators ([bdhs_official()]), nationally and by division,
#' residence and (optionally) wealth quintile. Only published aggregates are
#' used, never microdata, so the result can be shared, bundled with papers and
#' used for teaching, testing and fully reproducible worked examples whose
#' headline numbers match the DHS reports.
#'
#' @section Method:
#' 1. **Sample design.** The number of clusters is the real number of the
#'    round (from [bdhs_surveys]) times `scale`; households per cluster are
#'    chosen so the number of women approximates the real sample. Clusters are
#'    allocated to division x residence strata in proportion to the official
#'    *unweighted* denominators of a reference indicator (births in the last
#'    3 years for KR, currently married women for IR, the de jure population
#'    for PR), with at least two PSUs per stratum. After simulation the
#'    weights are post-stratified so that the *weighted* reference population
#'    is distributed across strata as in the official weighted denominators
#'    (the joint division x residence table is the product of the published
#'    margins, which is the only information available from aggregates).
#'    Weights are normalised to the DHS convention (mean `v005` = 1,000,000).
#' 2. **Cell model.** For every target indicator the probability in cell
#'    \eqn{c} (division x residence, x wealth quintile when requested) is
#'    \eqn{p_c = \mathrm{logit}^{-1}(a + b_{div} + c_{res} [+ d_{wq}])}. The
#'    coefficients are fitted by iterative proportional fitting on the logit
#'    scale (logit raking) so that the weighted synthetic margins equal the
#'    official margins exactly, using the synthetic weighted denominators of
#'    each cell (the indicator's own universe function, including all-woman
#'    factors for all-women indicators).
#' 3. **Deterministic assignment.** Within each cell the eligible records are
#'    ordered by a key and the first \eqn{k_c} receive the outcome, with
#'    \eqn{k_c} chosen so that the weighted cell proportion is closest to
#'    \eqn{p_c}; a greedy controlled-rounding step then moves the \eqn{k_c}
#'    by single records to minimise the squared deviations of all calibrated
#'    margins (national included). Results are therefore exact up to the
#'    granularity of single records, not subject to Monte Carlo noise. Keys
#'    combine a mild wealth gradient with random noise, except for
#'    anthropometric targets, where the record's own z-score or BMI is the key
#'    and the cell's distribution is shifted (location shift) so that its
#'    shape is kept.
#' 4. **Coherence.** Source variables are rewritten consistently: caesarean
#'    births are facility births, delivery assistance (`m3*`) follows the
#'    place of delivery, modern users are a subset of users, unmet need only
#'    occurs among non-users, method (`v312`), method type (`v313`) and need
#'    status (`v626a`) agree, and electricity is a household attribute.
#'
#' @section What is and is not reproduced:
#' Reproduced: the weighted value of every calibrated indicator nationally and
#' for each calibrated margin (see [bdhs_synthetic_fidelity()]), typically
#' within a few hundredths of a percentage point at `scale = 1` (official
#' values are rounded to 0.1). The national value is implied by the margins
#' and the synthetic population shares; it can deviate slightly when an
#' indicator's universe is distributed across divisions differently from the
#' reference population.
#'
#' **Not** reproduced: joint distributions beyond the additive logit model
#' (e.g. urban Sylhet), associations between targets or with non-target
#' variables (they follow the simple simulation model), every non-target
#' variable, fertility and mortality rates, and standard errors (the design
#' is realistic in size and structure but intra-cluster correlation is not
#' calibrated, so SEs are only indicative). Calibration is done per recode:
#' synthesised KR, IR and PR files share their structure but are not
#' calibrated jointly, so linked values (e.g. a woman's BMI in IR and PR) can
#' differ. The 2022 short-questionnaire design (`sqtype`) is not reproduced.
#'
#' **Synthetic data must never be used for substantive conclusions**; they
#' are for teaching, software testing and reproducible demonstrations.
#'
#' @param survey A survey reference understood by [bdhs_survey_id()] (a DHS
#'   round, 1993-94 to 2022).
#' @param recode `"KR"` (children), `"IR"` (women) or `"PR"` (household
#'   members).
#' @param targets Indicator ids to calibrate (see Details). `NULL` uses every
#'   supported indicator of the recode with an official value in the round.
#' @param margins Margins to calibrate in addition to the national value: any
#'   of `"division"`, `"residence"` and `"wealth"`. `NULL` or `character()`
#'   calibrates national values only.
#' @param scale Sample size relative to the real survey (clusters are scaled;
#'   households per cluster are kept). Small values give fast, coarser data.
#' @param seed Random seed. The global random number generator state is left
#'   untouched.
#'
#' @details Supported targets (each depends on one source variable that the
#'   synthesiser controls):
#'   * KR: `rh_del_facility` (`m15`), `rh_del_csection` (`m17`),
#'     `rh_anc_4plus` (`m14`), `ch_vac_bcg` (`h2`), `ch_vac_dpt3`
#'     (`h3`/`h5`/`h7`), `ch_vac_measles` (`h9`), `ch_diarrhoea` (`h11`).
#'     Vaccinations are not available for 2022 and caesarean section not for
#'     1993-94. For 1993-94 the KR file is restricted to births in the last 3
#'     years, as in the real file.
#'   * IR: `fp_cpr_any`, `fp_cpr_modern` (`v312`/`v313`), `fp_unmet_total`
#'     (`v626a`), `ms_married_by_18` (`v511`, all-women denominator through
#'     `awfactt`), and `nt_wm_thin` (`v445`) on request.
#'   * PR: `nt_stunting` (`hc70`), `nt_wasting` (`hc72`), `nt_underweight`
#'     (`hc71`), `nt_wm_thin` (`ha40`), `hh_electricity` (`hv206`).
#'
#'   Official values use the indicator's reference period
#'   (`by_variable_label`, e.g. births in the last 3 years). Divisions are
#'   matched to the divisions as surveyed in the round (older rounds publish
#'   Dhaka and Rajshahi under "Dhaka before 2015" and "Rajshahi/Rangpur"). A
#'   margin without published values for every category is skipped for that
#'   indicator, and so is a margin whose published denominators do not add
#'   up (within 2%) to the national denominator, because it then refers to a
#'   different population than the national value (this happens for the
#'   1996-97 and 1999-2000 child anthropometry by residence).
#'
#' @return A tibble like [bdhs_simulate()] output, with attributes
#'   `bdhs_survey_id`, `bdhs_recode`, `bdhs_simulated = TRUE`,
#'   `bdhs_synthetic = TRUE`, `bdhs_targets` (the official values used: one
#'   row per indicator, margin and category), `bdhs_synthetic_design` (seed,
#'   scale, clusters, households per cluster, reference indicator, and
#'   `inconsistent_margins`: published breakdowns that were not calibrated
#'   because their denominators do not add up to the national one) and
#'   `bdhs_fidelity` (the result of [bdhs_synthetic_fidelity()]).
#' @seealso [bdhs_synthetic_fidelity()], [bdhs_simulate()], [bdhs_official()].
#' @export
#' @examples
#' \donttest{
#' kr <- bdhs_synthesize(2017, "KR", scale = 0.2)
#' bdhs_estimate(kr, "rh_del_csection", by = "v025")
#' bdhs_official("RH_DELA_C_CSC", survey = 2017, characteristic = "Residence")
#' bdhs_synthetic_fidelity(kr)
#' }
bdhs_synthesize <- function(survey = 2022, recode = c("KR", "IR", "PR"), targets = NULL,
                            margins = c("division", "residence"), scale = 1, seed = 1) {
  survey_id <- bdhs_survey_id(survey, type = "DHS")
  if (length(survey_id) != 1L) {
    cli::cli_abort("{.arg survey} must refer to a single survey round.")
  }
  recode <- toupper(recode)
  recode <- rlang::arg_match(recode, c("KR", "IR", "PR"))
  if (is.null(margins)) margins <- character()
  if (!is.character(margins) || !all(margins %in% c("division", "residence", "wealth"))) {
    cli::cli_abort("{.arg margins} must be any of {.val {c('division', 'residence', 'wealth')}}.")
  }
  margins <- unique(margins)
  if (!is.numeric(scale) || length(scale) != 1L || is.na(scale) || scale <= 0) {
    cli::cli_abort("{.arg scale} must be a single positive number.")
  }
  info <- sim_round_info(survey_id)
  targets <- synth_resolve_targets(targets, recode, survey_id)
  official <- lapply(targets, synth_official, survey_id = survey_id, info = info, margins = margins)
  names(official) <- targets

  design <- synth_design(survey_id, recode, scale, info)
  data <- withr_seed(seed, {
    world <- sim_world(survey_id, design$n_clusters, design$households, strata = design$strata)
    d <- tibble::as_tibble(world[[tolower(recode)]])
    if (recode == "KR" && survey_id == "BD1994DHS") {
      d <- d[dv(d, "b19") < 36, ] # the 1993-94 KR covers births in the last 3 years
    }
    d <- synth_poststratify(d, design, survey_id, info, targets, official)
    if ("ms_married_by_18" %in% targets) {
      d <- synth_aw_feasible(d, official[["ms_married_by_18"]], survey_id, info)
    }
    for (id in targets) {
      d <- synth_calibrate(d, id, official[[id]], survey_id, targets)
    }
    d
  })

  tg <- dplyr::bind_rows(lapply(targets, function(id) {
    o <- official[[id]]
    tibble::tibble(
      survey_id = survey_id, indicator = id,
      api_id = indicator_registry()[[id]]$api_id[[1]],
      margin = o$margin, category = o$category, official_label = o$label, official = o$value
    )
  }))
  attr(data, "bdhs_survey_id") <- survey_id
  attr(data, "bdhs_recode") <- recode
  attr(data, "bdhs_simulated") <- TRUE
  attr(data, "bdhs_synthetic") <- TRUE
  attr(data, "bdhs_targets") <- tg
  attr(data, "bdhs_synthetic_design") <- list(
    seed = seed, scale = scale, margins = margins, n_clusters = design$n_clusters,
    households = design$households, reference = design$reference,
    inconsistent_margins = Filter(length, lapply(official, attr, "dropped"))
  )
  attr(data, "bdhs_fidelity") <- bdhs_synthetic_fidelity(data)
  data
}

#' Fidelity of calibrated synthetic data to the official values
#'
#' Re-estimates every calibrated indicator of data made by
#' [bdhs_synthesize()] with [bdhs_estimate()] (nationally and by each
#' calibrated margin) and compares it with the official DHS value it was
#' calibrated to.
#'
#' @param data Data returned by [bdhs_synthesize()] (possibly modified; the
#'   comparison is recomputed from the data).
#'
#' @return A tibble with columns `survey_id`, `indicator`, `api_id`,
#'   `margin` (`"national"`, `"division"`, `"residence"`, `"wealth"`),
#'   `category`, `official`, `synthetic`, `diff` (synthetic minus official,
#'   percentage points) and `n_unweighted`.
#' @seealso [bdhs_synthesize()]
#' @export
#' @examples
#' \donttest{
#' ir <- bdhs_synthesize(2022, "IR", scale = 0.2)
#' fid <- bdhs_synthetic_fidelity(ir)
#' max(abs(fid$diff))
#' }
bdhs_synthetic_fidelity <- function(data) {
  tg <- attr(data, "bdhs_targets")
  if (!isTRUE(attr(data, "bdhs_synthetic")) || is.null(tg)) {
    cli::cli_abort(c(
      "{.arg data} is not calibrated synthetic data.",
      "i" = "Create it with {.fn bdhs_synthesize}."
    ))
  }
  survey_id <- attr(data, "bdhs_survey_id")
  pre <- synth_prefix(data)
  ids <- unique(tg$indicator)
  by_var <- c(division = paste0(pre, "024"), residence = paste0(pre, "025"), wealth = synth_wealth_var(pre))
  res <- list()
  for (m in unique(tg$margin)) {
    if (m == "national") {
      e <- bdhs_estimate(data, ids, survey = survey_id)
      cat_vals <- rep("Total", nrow(e))
    } else {
      e <- bdhs_estimate(data, ids, by = by_var[[m]], survey = survey_id)
      cat_vals <- as.character(e[[by_var[[m]]]])
    }
    res[[m]] <- tibble::tibble(
      indicator = e$indicator, margin = m, category = cat_vals,
      synthetic = e$estimate, n_unweighted = e$n_unweighted
    )
  }
  est <- dplyr::bind_rows(res)
  out <- dplyr::left_join(tg, est, by = c("indicator", "margin", "category"))
  tibble::tibble(
    survey_id = out$survey_id, indicator = out$indicator, api_id = out$api_id,
    margin = out$margin, category = out$category, official = out$official,
    synthetic = out$synthetic, diff = out$synthetic - out$official,
    n_unweighted = out$n_unweighted
  )
}

# Targets ---------------------------------------------------------------------------

.synth_defaults <- list(
  KR = c("rh_del_facility", "rh_del_csection", "rh_anc_4plus", "ch_vac_bcg", "ch_vac_dpt3",
         "ch_vac_measles", "ch_diarrhoea"),
  IR = c("fp_cpr_any", "fp_cpr_modern", "fp_unmet_total", "ms_married_by_18"),
  PR = c("nt_stunting", "nt_wasting", "nt_underweight", "nt_wm_thin", "hh_electricity")
)
.synth_supported <- list(
  KR = .synth_defaults$KR,
  IR = c(.synth_defaults$IR, "nt_wm_thin"),
  PR = .synth_defaults$PR
)
# Reference indicator whose official denominators define the population
# distribution across strata.
.synth_reference <- c(KR = "rh_del_facility", IR = "fp_cpr_modern", PR = "hh_electricity")

synth_resolve_targets <- function(targets, recode, survey_id, call = rlang::caller_env()) {
  reg <- indicator_registry()
  supported <- .synth_supported[[recode]]
  explicit <- !is.null(targets)
  if (!explicit) targets <- .synth_defaults[[recode]]
  if (!is.character(targets) || !length(targets)) {
    cli::cli_abort("{.arg targets} must be a character vector of indicator ids.", call = call)
  }
  targets <- unique(tolower(targets))
  bad <- setdiff(targets, supported)
  if (length(bad)) {
    cli::cli_abort(c(
      "Cannot calibrate {.val {bad}} in {recode} data.",
      "i" = "Supported for {recode}: {.val {supported}}."
    ), call = call)
  }
  # Keep the order of the supported list: parents before nested targets.
  targets <- supported[supported %in% targets]
  ok <- vapply(targets, function(id) {
    survey_id %in% reg[[id]]$rounds && !is.na(official_value(reg[[id]], survey_id)$value)
  }, logical(1))
  if (any(!ok) && explicit) {
    cli::cli_warn("{.val {survey_id}}: no official value for {.val {targets[!ok]}}; not calibrated.")
  }
  targets <- targets[ok]
  if (!length(targets)) {
    cli::cli_abort("No calibration target has an official value for {.val {survey_id}}.", call = call)
  }
  targets
}

# Official values for one indicator: national and the requested margins, as
# proportions (0-100), with the synthetic category labels used by
# bdhs_estimate() (division names, "urban"/"rural", wealth labels).
synth_official <- function(id, survey_id, info, margins) {
  ind <- indicator_registry()[[id]]
  rows <- synth_official_rows(ind$api_id[[1]], survey_id, ind$api_by)
  nat <- official_value(ind, survey_id)$value
  tot <- rows[rows$characteristic_category %in% c("Total", "Total 15-49"), ]
  nat_den <- if (nrow(tot)) as.numeric(tot$denom_weighted[[1]]) else NA_real_
  out <- list(data.frame(margin = "national", category = "Total", label = "Total", value = nat,
                         code = NA_real_, denom = nat_den))
  if ("division" %in% margins) {
    labs <- synth_division_labels(info$divisions)
    reg <- rows[rows$characteristic_category == "Region", ]
    clean <- explore_clean_label(reg$characteristic_label)
    found <- lapply(labs, function(cand) {
      hit <- match(cand, clean)
      hit <- hit[!is.na(hit)]
      if (length(hit)) reg[hit[[1]], ] else NULL
    })
    if (all(!vapply(found, is.null, logical(1)))) {
      out[[length(out) + 1L]] <- data.frame(
        margin = "division", category = info$divisions,
        label = vapply(found, function(r) explore_clean_label(r$characteristic_label), character(1)),
        value = vapply(found, function(r) r$value, numeric(1)),
        code = seq_along(info$divisions),
        denom = vapply(found, function(r) as.numeric(r$denom_weighted), numeric(1))
      )
    }
  }
  add_simple <- function(margin, category_name, dhs, syn) {
    r <- rows[rows$characteristic_category == category_name, ]
    hit <- match(dhs, r$characteristic_label)
    v <- r$value[hit]
    if (length(v) && !anyNA(v)) {
      out[[length(out) + 1L]] <<- data.frame(
        margin = margin, category = syn, label = dhs, value = v, code = seq_along(dhs),
        denom = as.numeric(r$denom_weighted[hit])
      )
    }
  }
  if ("residence" %in% margins) {
    add_simple("residence", "Residence", c("Urban", "Rural"), c("urban", "rural"))
  }
  if ("wealth" %in% margins) {
    add_simple("wealth", "Wealth quintile", c("Lowest", "Second", "Middle", "Fourth", "Highest"),
               c("poorest", "poorer", "middle", "richer", "richest"))
  }
  # A breakdown whose published denominators do not add up to the national
  # denominator describes a different population (e.g. the 1996-97 and
  # 1999-2000 anthropometry by residence) and cannot be reproduced together
  # with the national value: it is not calibrated.
  bad <- vapply(out, function(o) {
    tot_m <- sum(o$denom)
    o$margin[[1]] != "national" && !is.na(nat_den) && !is.na(tot_m) && abs(tot_m / nat_den - 1) > 0.02
  }, logical(1))
  dropped <- vapply(out[bad], function(o) o$margin[[1]], character(1))
  out <- do.call(rbind, out[!bad])
  out$value <- as.numeric(out$value)
  attr(out, "dropped") <- dropped
  out
}

# Rows of bdhs_indicators for one indicator and round, at the reference period
# the package uses for it.
synth_official_rows <- function(api_id, survey_id, api_by) {
  x <- bdhs::bdhs_indicators
  x <- x[x$survey_id == survey_id & x$indicator_id == api_id, ]
  by <- api_by
  if (length(by) > 1L || !is.null(names(by))) {
    by <- if (survey_id %in% names(by)) by[[survey_id]] else by[["default"]]
  }
  if (!is.na(by)) {
    x <- x[!is.na(x$by_variable_label) & x$by_variable_label == by, ]
  } else {
    x <- x[x$is_preferred, ]
  }
  x[order(x$by_variable_id), ]
}

# Candidate DHS labels for each division of a round. Older rounds publish
# Dhaka and Rajshahi only as "Dhaka before 2015" and "Rajshahi/Rangpur",
# and 1993-94 Chattogram as "Chattogram/Sylhet"; the parent grouping equals
# the division only while the split-off division does not exist yet.
synth_division_labels <- function(divisions) {
  has <- function(x) x %in% divisions
  lapply(divisions, function(d) {
    switch(d,
      barishal = "Barishal",
      chittagong = ,
      chattogram = c("Chattogram", if (!has("sylhet")) "Chattogram/Sylhet"),
      dhaka = c("Dhaka", if (!has("mymensingh")) "Dhaka before 2015"),
      khulna = "Khulna",
      mymensingh = "Mymensingh",
      rajshahi = c("Rajshahi", if (!has("rangpur")) "Rajshahi/Rangpur"),
      rangpur = "Rangpur",
      sylhet = "Sylhet"
    )
  })
}

# Sample design ---------------------------------------------------------------------

synth_design <- function(survey_id, recode, scale, info) {
  sv <- bdhs::bdhs_surveys[bdhs::bdhs_surveys$survey_id == survey_id, ]
  n_div <- length(info$divisions)
  n_strata <- 2L * n_div
  real_cl <- sv$n_clusters[[1]]
  households <- max(1L, as.integer(round(sv$n_women[[1]] / 0.92 / real_cl)))
  ref_id <- .synth_reference[[recode]]
  ref <- indicator_registry()[[ref_id]]
  rows <- synth_official_rows(ref$api_id[[1]], survey_id, ref$api_by)
  shares <- function(col) {
    labs <- synth_division_labels(info$divisions)
    reg <- rows[rows$characteristic_category == "Region", ]
    clean <- explore_clean_label(reg$characteristic_label)
    dv_ <- vapply(labs, function(cand) {
      hit <- match(cand, clean)
      hit <- hit[!is.na(hit)]
      if (length(hit)) as.numeric(reg[[col]][hit[[1]]]) else NA_real_
    }, numeric(1))
    if (anyNA(dv_)) dv_ <- rep(1, n_div)
    res <- rows[rows$characteristic_category == "Residence", ]
    rv <- as.numeric(res[[col]][match(c("Urban", "Rural"), res$characteristic_label)])
    if (length(rv) != 2L || anyNA(rv)) rv <- c(0.3, 0.7)
    # Joint table = product of the margins (strata ordered division-major).
    as.vector(t(outer(dv_ / sum(dv_), rv / sum(rv))))
  }
  weighted <- shares("denom_weighted")
  unweighted <- shares("denom_unweighted")
  n_clusters <- max(2L * n_strata, as.integer(round(real_cl * scale)))
  alloc <- pmax(2L, as.integer(round(n_clusters * unweighted)))
  list(
    n_clusters = sum(alloc), households = households,
    strata = rep(seq_len(n_strata), alloc), weighted_shares = weighted,
    reference = ref_id
  )
}

synth_prefix <- function(d) if (!is.null(d[["hv024"]])) "hv" else "v"
synth_wealth_var <- function(pre) if (pre == "hv") "hv270" else "v190"

# Official denominators of an indicator by division (divisions as surveyed,
# in code order) and residence (urban, rural); NULL where not published.
synth_denoms <- function(id, survey_id, info, col = "denom_weighted") {
  ind <- indicator_registry()[[id]]
  rows <- synth_official_rows(ind$api_id[[1]], survey_id, ind$api_by)
  reg <- rows[rows$characteristic_category == "Region", ]
  clean <- explore_clean_label(reg$characteristic_label)
  dv_ <- vapply(synth_division_labels(info$divisions), function(cand) {
    hit <- match(cand, clean)
    hit <- hit[!is.na(hit)]
    if (length(hit)) as.numeric(reg[[col]][hit[[1]]]) else NA_real_
  }, numeric(1))
  res <- rows[rows$characteristic_category == "Residence", ]
  rv <- as.numeric(res[[col]][match(c("Urban", "Rural"), res$characteristic_label)])
  list(
    division = if (!anyNA(dv_) && sum(dv_) > 0) dv_ / sum(dv_),
    residence = if (length(rv) == 2L && !anyNA(rv) && sum(rv) > 0) rv / sum(rv)
  )
}

# Post-stratification of the weights. One factor per stratum (division x
# residence) is fitted by least squares so that, for every calibration
# target (and, with a small weight, the reference population), the weighted
# synthetic universe is distributed across divisions and residence as the
# official weighted denominators. This makes the national value implied by
# the calibrated margins agree with the official national value as far as
# the shared weights allow. Weights are then normalised to the DHS
# convention (mean weight 1,000,000).
synth_poststratify <- function(d, design, survey_id, info, targets, official) {
  pre <- synth_prefix(d)
  wvar <- paste0(pre, "005")
  n_strata <- 2L * length(info$divisions)
  strat <- (dv(d, paste0(pre, "024")) - 1) * 2 + dv(d, paste0(pre, "025"))
  sf <- factor(strat, levels = seq_len(n_strata))
  div_of <- (seq_len(n_strata) - 1L) %/% 2L + 1L
  res_of <- (seq_len(n_strata) - 1L) %% 2L + 1L
  w <- dv(d, wvar)
  reg <- indicator_registry()
  terms <- list()
  for (id in unique(c(design$reference, targets))) {
    ind <- reg[[id]]
    if (!is.null(ind$all_women)) next # handled through the all-woman factors
    u <- ind$universe(d, survey_id)
    u[is.na(u)] <- FALSE
    U <- as.vector(tapply(w * u, sf, sum))
    U[is.na(U)] <- 0
    if (sum(U) <= 0) next
    den <- synth_denoms(id, survey_id, info)
    # Weight of a term: how much a share error moves the implied national
    # value, i.e. the share-weighted variance of the official values across
    # the groups (percentage points squared); the reference population only
    # breaks ties.
    term_wt <- function(margin, o) {
      off <- official[[id]]
      if (is.null(off)) return(0.01)
      v <- off$value[off$margin == margin]
      if (length(v) != length(o)) return(NA_real_)
      0.01 + sum(o * (v - sum(o * v))^2)
    }
    for (m in c("division", "residence")) {
      o <- den[[m]]
      if (is.null(o)) next
      wt <- term_wt(m, o)
      if (is.na(wt)) next
      terms[[length(terms) + 1L]] <- list(U = U, g = if (m == "division") div_of else res_of, o = o, wt = wt)
    }
  }
  ref_u <- if (length(terms)) terms[[1]]$U else rep(0, n_strata)
  fac <- ifelse(ref_u > 0, pmax(design$weighted_shares, 1e-6) * sum(ref_u) / pmax(ref_u, 1e-9), 1)
  if (length(terms)) {
    # Averaged raking: each term proposes a multiplicative adjustment per
    # stratum (official / synthetic share of its group); the strata factors
    # move by the weighted geometric mean of the proposals. This converges
    # to a compromise when the official denominators of different
    # indicators are not mutually consistent.
    wts <- vapply(terms, `[[`, numeric(1), "wt")
    for (iter in seq_len(500)) {
      prop <- vapply(terms, function(tm) {
        x <- tm$U * fac
        sh <- as.vector(tapply(x, factor(tm$g, levels = seq_along(tm$o)), sum)) / sum(x)
        adj <- ifelse(sh > 0, tm$o / sh, 1)
        log(adj[tm$g])
      }, numeric(n_strata))
      step <- as.vector(prop %*% wts) / sum(wts)
      fac <- fac * exp(step)
      if (max(abs(step)) < 1e-8) break
    }
  }
  w <- w * fac[strat]
  w <- round(w / mean(w) * 1e6)
  x <- d[[wvar]]
  x[] <- w
  d[[wvar]] <- x
  d
}

# All-women indicators are ratios sum(w y) / sum(w awfactt / 100) over
# ever-married women. The synthetic all-woman factors of the universe
# (women 20-24) are free quantities, so they are (1) raked, cell by cell, so
# that the estimated number of all women is distributed across divisions and
# residence as in the official denominators of the indicator, and (2) capped
# so that the highest official proportion stays reachable (a proportion
# above 100 / awfactt cannot be reached).
synth_aw_feasible <- function(d, off, survey_id, info) {
  ind <- indicator_registry()[["ms_married_by_18"]]
  u <- ind$universe(d, survey_id)
  u[is.na(u)] <- FALSE
  cap <- floor(100 / min(0.97, max(off$value) / 100 + 0.04))
  aw <- dv(d, "awfactt")
  aw[u] <- pmin(aw[u], cap)
  w <- dv(d, "v005")
  div <- dv(d, "v024")
  res <- dv(d, "v025")
  rows <- synth_official_rows(ind$api_id[[1]], survey_id, ind$api_by)
  dl <- synth_division_labels(info$divisions)
  reg <- rows[rows$characteristic_category == "Region", ]
  clean <- explore_clean_label(reg$characteristic_label)
  t_div <- vapply(dl, function(cand) {
    hit <- match(cand, clean)
    hit <- hit[!is.na(hit)]
    if (length(hit)) as.numeric(reg$denom_weighted[hit[[1]]]) else NA_real_
  }, numeric(1))
  rr <- rows[rows$characteristic_category == "Residence", ]
  t_res <- as.numeric(rr$denom_weighted[match(c("Urban", "Rural"), rr$characteristic_label)])
  margins <- list()
  if (!anyNA(t_div) && length(t_div) == length(info$divisions)) margins$div <- list(g = div, t = t_div / sum(t_div))
  if (length(t_res) == 2L && !anyNA(t_res)) margins$res <- list(g = res, t = t_res / sum(t_res))
  for (iter in seq_len(50)) {
    for (m in margins) {
      tot <- sum(w[u] * aw[u])
      cur <- tapply(w[u] * aw[u], factor(m$g[u], levels = seq_along(m$t)), sum)
      f <- ifelse(is.na(cur) | cur == 0, 1, m$t * tot / cur)
      aw[u] <- aw[u] * f[m$g[u]]
    }
    aw[u] <- pmin(aw[u], cap)
  }
  aw[u] <- pmax(100, round(aw[u]))
  for (v in c("awfactt", "awfactu", "awfactr", "awfacte", "awfactw")) {
    if (is.null(d[[v]])) next
    x <- d[[v]]
    x[u] <- aw[u]
    d[[v]] <- x
  }
  d
}

# Calibration of one target -----------------------------------------------------------

synth_calibrate <- function(d, id, off, survey_id, targets) {
  ind <- indicator_registry()[[id]]
  spec <- synth_specs()[[id]]
  pre <- synth_prefix(d)
  n <- nrow(d)
  u <- ind$universe(d, survey_id)
  u[is.na(u)] <- FALSE
  w <- dv(d, paste0(pre, "005")) / 1e6
  den <- if (is.null(ind$all_women)) w else w * dv(d, "awfactt") / 100
  elig <- u
  if (!is.null(spec$subset)) elig <- elig & spec$subset(d, targets)
  key <- spec$key(d)

  # Cells of every record (division, residence, wealth quintile).
  margins <- unique(off$margin)
  cell_vars <- list(
    division = dv(d, paste0(pre, "024")), residence = dv(d, paste0(pre, "025")),
    wealth = dv(d, synth_wealth_var(pre))
  )
  used <- intersect(c("division", "residence", "wealth"), margins)
  cell_key <- if (length(used)) do.call(paste, c(cell_vars[used], sep = ":")) else rep("all", n)

  rows <- which(u)
  if (!length(rows)) {
    return(d)
  }
  unit <- if (identical(spec$unit, "household")) as.character(d$hhid) else as.character(seq_len(n))
  f <- match(unit[rows], unique(unit[rows]))
  first <- rows[!duplicated(f)]
  num_u <- as.vector(rowsum(w[rows], f))
  den_u <- as.vector(rowsum(den[rows], f))
  elig_u <- as.vector(rowsum(as.numeric(elig[rows]), f)) > 0
  key_u <- key[first]
  cells <- unique(cell_key[first])
  cell_u <- match(cell_key[first], cells)
  n_cell <- length(cells)
  W <- as.vector(tapply(den_u, factor(cell_u, levels = seq_len(n_cell)), sum))
  W[is.na(W)] <- 0
  attr_of <- function(m) cell_vars[[m]][first][match(seq_len(n_cell), cell_u)]

  # Margin membership matrix (rows: margins incl. national; cols: cells).
  mm <- list()
  ov <- numeric()
  for (i in seq_len(nrow(off))) {
    m <- off$margin[[i]]
    mem <- if (m == "national") rep(TRUE, n_cell) else attr_of(m) == off$code[[i]]
    if (sum(W[mem]) <= 0) next
    mm[[length(mm) + 1L]] <- mem
    ov <- c(ov, off$value[[i]] / 100)
    names(ov)[length(ov)] <- m
  }
  M <- matrix(as.numeric(unlist(mm)), nrow = length(mm), byrow = TRUE)
  Dm <- as.vector(M %*% W)
  oc <- pmin(pmax(ov, 1e-4), 1 - 1e-4)

  # Logit raking of the additive cell model to the margins.
  fit_rows <- which(names(ov) != "national")
  if (!length(fit_rows)) fit_rows <- which(names(ov) == "national")
  eta <- rep(stats::qlogis(oc[[which(names(ov) == "national")[1]]]), n_cell)
  for (iter in seq_len(500)) {
    for (r in fit_rows) {
      mem <- M[r, ] > 0
      g <- function(s) sum(W[mem] * stats::plogis(eta[mem] + s)) / Dm[[r]] - oc[[r]]
      s <- stats::uniroot(g, c(-40, 40), tol = 1e-12)$root
      eta[mem] <- eta[mem] + s
    }
    dev <- as.vector(M[fit_rows, , drop = FALSE] %*% (W * stats::plogis(eta))) / Dm[fit_rows] - oc[fit_rows]
    if (max(abs(dev)) < 1e-10) break
  }
  target <- W * stats::plogis(eta)

  # Deterministic assignment: eligible units of a cell ordered by key.
  ord <- lapply(seq_len(n_cell), function(c) {
    i <- which(cell_u == c & elig_u)
    i[order(key_u[i])]
  })
  cs <- lapply(ord, function(i) c(0, cumsum(num_u[i])))
  k <- vapply(seq_len(n_cell), function(c) which.min(abs(cs[[c]] - target[[c]])) - 1L, integer(1))
  got <- function(k) vapply(seq_len(n_cell), function(c) cs[[c]][k[[c]] + 1L], numeric(1))

  # Greedy controlled rounding on all margins (national included). A move
  # adds the next unselected unit of a cell, drops the last selected one, or
  # swaps them; candidates are limited to a window around the cell's
  # boundary in key order, so the ordering by key is essentially kept while
  # unequal weights allow fine adjustment.
  sel <- lapply(seq_len(n_cell), function(c) seq_along(ord[[c]]) <= k[[c]])
  ach <- as.vector(M %*% got(k)) / Dm
  win <- 25L
  for (iter in seq_len(20000)) {
    r <- 100 * (ach - ov)
    best_gain <- 1e-12
    move <- NULL
    for (c in seq_len(n_cell)) {
      a <- 100 * M[, c] / Dm
      wts <- num_u[ord[[c]]]
      on <- which(sel[[c]])
      off <- which(!sel[[c]])
      on <- utils::tail(on, win)
      off <- utils::head(off, win)
      cand_add <- if (length(off)) cbind(0L, off, wts[off]) else NULL
      cand_drop <- if (length(on)) cbind(on, 0L, -wts[on]) else NULL
      cand_swap <- if (length(on) && length(off)) {
        g <- expand.grid(i = on, j = off)
        cbind(g$i, g$j, wts[g$j] - wts[g$i])
      }
      cand <- rbind(cand_add, cand_drop, cand_swap)
      if (is.null(cand) || !nrow(cand)) next
      delta <- cand[, 3]
      # Quartic loss: close to minimising the largest deviation, so that
      # rounding and inconsistencies between margins are spread evenly.
      idx <- which(a > 0)
      dev2 <- outer(delta, a[idx]) + rep(r[idx], each = length(delta))
      gain <- sum(r[idx]^4) - rowSums(dev2^4)
      b <- which.max(gain)
      if (gain[[b]] > best_gain) {
        best_gain <- gain[[b]]
        move <- list(c = c, i = cand[b, 1], j = cand[b, 2], delta = delta[[b]])
      }
    }
    if (is.null(move)) break
    if (move$i > 0) sel[[move$c]][move$i] <- FALSE
    if (move$j > 0) sel[[move$c]][move$j] <- TRUE
    ach <- ach + M[, move$c] * move$delta / Dm
  }

  y_u <- numeric(length(num_u))
  for (c in seq_len(n_cell)) {
    y_u[ord[[c]][sel[[c]]]] <- 1
  }
  y <- rep(NA_real_, n)
  y[rows] <- y_u[f]
  spec$write(d, y, cell_key, survey_id, targets)
}

# Target specifications ----------------------------------------------------------------
#
# key(d): ordering of records within cells (lowest first receive the outcome).
# subset(d, targets): records that may receive the outcome (nesting).
# write(d, y, cell, survey_id, targets): rewrite the source variables so that
# the indicator equals y (0/1) wherever y is not NA.

synth_specs <- function() {
  list(
    rh_del_facility = list(key = synth_key_up, write = synth_write_facility),
    rh_del_csection = list(
      key = synth_key_up,
      subset = function(d, t) if ("rh_del_facility" %in% t) synth_facility(d) else rep(TRUE, nrow(d)),
      write = synth_write_csection
    ),
    rh_anc_4plus = list(
      key = function(d) -(0.5 * synth_latent(d) + synth_facility(d) + stats::rnorm(nrow(d))),
      write = synth_write_anc
    ),
    ch_vac_bcg = list(key = synth_key_up, write = synth_write_vacc("h2")),
    ch_vac_dpt3 = list(key = synth_key_up, write = synth_write_vacc(c("h3", "h5", "h7"))),
    ch_vac_measles = list(key = synth_key_up, write = synth_write_vacc("h9")),
    ch_diarrhoea = list(key = synth_key_down, write = synth_write_diarrhoea),
    fp_cpr_any = list(key = synth_key_up, write = synth_write_cpr_any),
    fp_cpr_modern = list(
      key = synth_key_up,
      subset = function(d, t) if ("fp_cpr_any" %in% t) in_codes(dv(d, "v313"), 1:3) else rep(TRUE, nrow(d)),
      write = synth_write_cpr_modern
    ),
    fp_unmet_total = list(
      key = synth_key_down,
      subset = function(d, t) in_codes(dv(d, "v313"), 0),
      write = synth_write_unmet
    ),
    ms_married_by_18 = list(
      key = function(d) dv(d, "v511") + stats::runif(nrow(d)),
      write = synth_write_afm
    ),
    nt_stunting = list(key = function(d) dv(d, "hc70"), write = synth_write_shift("hc70", -200, range = c(-Inf, 9995))),
    nt_wasting = list(key = function(d) dv(d, "hc72"), write = synth_write_shift("hc72", -200, range = c(-Inf, 9995))),
    nt_underweight = list(key = function(d) dv(d, "hc71"), write = synth_write_shift("hc71", -200, range = c(-Inf, 9995))),
    nt_wm_thin = list(
      key = function(d) wm_bmi(d),
      write = function(d, y, cell, s, t) {
        var <- if (is_pr(d)) "ha40" else "v445"
        synth_write_shift(var, 1850, range = c(1200, 6000), clamp = c(1201, 5999))(d, y, cell, s, t)
      }
    ),
    hh_electricity = list(unit = "household", key = synth_key_up, write = synth_write_electricity)
  )
}

# A mild wealth gradient: standardised wealth quintile (jittered) plus noise.
synth_latent <- function(d) {
  q <- dv(d, synth_wealth_var(synth_prefix(d)))
  q[is.na(q)] <- 3
  (q + stats::runif(length(q), -0.5, 0.5) - 3) / sqrt(2)
}
synth_key_up <- function(d) -(0.5 * synth_latent(d) + stats::rnorm(nrow(d)))
synth_key_down <- function(d) 0.5 * synth_latent(d) + stats::rnorm(nrow(d))

synth_set <- function(d, var, i, values) {
  if (!length(i)) {
    return(d)
  }
  x <- d[[var]]
  x[i] <- as.numeric(values)
  d[[var]] <- x
  d
}
synth_draw <- function(n, codes, prob = NULL) {
  if (!n) return(numeric())
  if (length(codes) == 1L) rep(codes, n) else sample(codes, n, replace = TRUE, prob = prob)
}
synth_facility <- function(d) in_codes(dv(d, "m15"), 20:95)

synth_write_facility <- function(d, y, cell, s, t) {
  fac <- synth_facility(d)
  i1 <- which(y == 1 & !fac)
  i0 <- which(y == 0 & fac)
  d <- synth_set(d, "m15", i1, synth_draw(length(i1), c(21, 22, 31, 41), c(0.3, 0.1, 0.5, 0.1)))
  d <- synth_set(d, "m15", i0, synth_draw(length(i0), c(11, 12), c(0.9, 0.1)))
  fac <- synth_facility(d)
  # Delivery assistance and caesarean section follow the place of delivery.
  doctor <- as.integer(fac & stats::runif(nrow(d)) < 0.75)
  m3 <- sim_m3(s, fac, doctor)
  for (v in names(m3)) d[[v]] <- m3[[v]]
  d <- synth_set(d, "m17", which(!fac & in_codes(dv(d, "m17"), 1)), 0)
  d
}

synth_write_csection <- function(d, y, cell, s, t) {
  i1 <- which(y == 1 & !in_codes(dv(d, "m17"), 1))
  d <- synth_set(d, "m17", i1, 1)
  home <- which(y == 1 & !synth_facility(d))
  d <- synth_set(d, "m15", home, 31)
  synth_set(d, "m17", which(y == 0 & in_codes(dv(d, "m17"), 1)), 0)
}

synth_write_anc <- function(d, y, cell, s, t) {
  m14 <- dv(d, "m14")
  four <- !is.na(m14) & m14 >= 4 & m14 <= 90
  i1 <- which(y == 1 & !four)
  i0 <- which(y == 0 & four)
  d <- synth_set(d, "m14", i1, synth_draw(length(i1), 4:9))
  synth_set(d, "m14", i0, synth_draw(length(i0), 0:3))
}

synth_write_vacc <- function(vars) {
  force(vars)
  function(d, y, cell, s, t) {
    for (v in vars) {
      i1 <- which(y == 1 & !in_codes(dv(d, v), 1:3))
      d <- synth_set(d, v, i1, synth_draw(length(i1), c(1, 3, 2), c(0.5, 0.2, 0.3)))
    }
    last <- vars[[length(vars)]]
    synth_set(d, last, which(y == 0 & in_codes(dv(d, last), 1:3)), 0)
  }
}

synth_write_diarrhoea <- function(d, y, cell, s, t) {
  d <- synth_set(d, "h11", which(y == 1 & !in_codes(dv(d, "h11"), 1:2)), 2)
  synth_set(d, "h11", which(y == 0 & in_codes(dv(d, "h11"), 1:2)), 0)
}

synth_fp_set <- function(d, i, status) {
  n <- length(i)
  if (!n) return(d)
  if (status == "modern") {
    d <- synth_set(d, "v312", i, synth_draw(n, c(1, 2, 3, 5, 6, 11), c(0.45, 0.02, 0.25, 0.12, 0.10, 0.06)))
    d <- synth_set(d, "v313", i, 3)
  } else if (status == "traditional") {
    d <- synth_set(d, "v312", i, synth_draw(n, c(8, 9), c(0.5, 0.5)))
    d <- synth_set(d, "v313", i, 2)
  } else {
    d <- synth_set(d, "v312", i, 0)
    d <- synth_set(d, "v313", i, 0)
  }
  if (status == "none") {
    d <- synth_set(d, "v626a", i, ifelse(dv(d, "v012")[i] > 45, 9, 7))
  } else {
    keep <- in_codes(dv(d, "v626a")[i], 3:4)
    d <- synth_set(d, "v626a", i[!keep], synth_draw(sum(!keep), c(3, 4)))
  }
  d
}

synth_write_cpr_any <- function(d, y, cell, s, t) {
  v313 <- dv(d, "v313")
  d <- synth_fp_set(d, which(y == 1 & in_codes(v313, 0)), "traditional")
  synth_fp_set(d, which(y == 0 & in_codes(v313, 1:3)), "none")
}

synth_write_cpr_modern <- function(d, y, cell, s, t) {
  v313 <- dv(d, "v313")
  d <- synth_fp_set(d, which(y == 1 & !in_codes(v313, 3)), "modern")
  synth_fp_set(d, which(y == 0 & in_codes(v313, 3)), if ("fp_cpr_any" %in% t) "traditional" else "none")
}

synth_write_unmet <- function(d, y, cell, s, t) {
  u <- dv(d, "v626a")
  i1 <- which(y == 1 & !in_codes(u, 1:2))
  d <- synth_set(d, "v626a", i1, synth_draw(length(i1), c(1, 2)))
  synth_set(d, "v626a", which(y == 0 & in_codes(u, 1:2)), 7)
}

synth_write_afm <- function(d, y, cell, s, t) {
  afm <- dv(d, "v511")
  age <- dv(d, "v012")
  i1 <- which(y == 1 & !(afm < 18))
  i0 <- which(y == 0 & afm < 18)
  d <- synth_set(d, "v511", i1, synth_draw(length(i1), 14:17))
  synth_set(d, "v511", i0, 18 + floor(stats::runif(length(i0)) * (pmin(age[i0], 20) - 17)))
}

# Shift the distribution of a continuous measure within each cell so that
# the records assigned 1 fall below `cut` and the others at or above it.
synth_write_shift <- function(var, cut, range, clamp = NULL) {
  force(var)
  force(cut)
  force(range)
  function(d, y, cell, s, t) {
    x <- dv(d, var)
    # Only values inside the universe's valid range are shifted, so that no
    # record enters or leaves the denominator.
    valid <- !is.na(x) & x >= range[[1]] & x <= range[[2]]
    for (c in unique(cell[!is.na(y)])) {
      in_c <- cell == c
      lo <- suppressWarnings(max(x[in_c & y %in% 1]))
      hi <- suppressWarnings(min(x[in_c & y %in% 0]))
      delta <- if (is.finite(lo) && is.finite(hi)) {
        round(cut - 0.5 - (lo + hi) / 2)
      } else if (is.finite(lo)) {
        min(0, cut - 1 - lo)
      } else {
        max(0, cut - hi)
      }
      j <- in_c & valid
      x[j] <- x[j] + delta
    }
    if (!is.null(clamp)) x[valid] <- pmin(pmax(x[valid], clamp[[1]]), clamp[[2]])
    x[which(y == 1 & x >= cut)] <- cut - 1
    x[which(y == 0 & x < cut)] <- cut
    z <- d[[var]]
    z[] <- x
    d[[var]] <- z
    d
  }
}

synth_write_electricity <- function(d, y, cell, s, t) {
  i <- which(!is.na(y))
  synth_set(d, "hv206", i, y[i])
}
