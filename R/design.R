# Complex survey designs -----------------------------------------------------------

# Design variables by recode prefix. Every BDHS round uses a stratified
# two-stage cluster sample: PSU = cluster (v021), strata = sampling-error strata
# (v022), weight = v005 / 1e6.
.design_vars <- list(
  v = c(psu = "v021", strata = "v022", strata_design = "v023", region = "v024", residence = "v025", weight = "v005"),
  hv = c(psu = "hv021", strata = "hv022", strata_design = "hv023", region = "hv024", residence = "hv025", weight = "hv005"),
  mv = c(psu = "mv021", strata = "mv022", strata_design = "mv023", region = "mv024", residence = "mv025", weight = "mv005")
)

design_prefix <- function(recode) {
  switch(recode,
    HR = ,
    PR = "hv",
    MR = "mv",
    "v"
  )
}

# DHS weights stored with six implied decimals.
is_dhs_weight <- function(x) grepl("005$|^hv028$|^h[ab]69$", x)

#' Build the complex survey design for BDHS data
#'
#' Creates a [survey::svydesign()] object with the clustering, stratification
#' and weights that the DHS Program uses for the given recode, so standard
#' errors and confidence intervals are correct. The round and recode are
#' detected automatically for data read with [bdhs_read()] or
#' [bdhs_simulate()]. [bdhs_design_info()] documents the design of every
#' round.
#'
#' Pooled data (several rounds, see [bdhs_pool()]) get round-specific PSU and
#' strata identifiers, and the pooled weight `pool_wt` is used when present.
#'
#' Some strata can contain a single PSU (for example after subsetting, or in
#' small domains). The variance of such strata cannot be estimated with the
#' default `options(survey.lonely.psu = "fail")`; `bdhs_design()` tells you
#' once per session when this happens. The usual fix, used by the DHS
#' Program's own software (centering at the grand mean), is
#' `options(survey.lonely.psu = "adjust")`.
#'
#' @param data A data frame of BDHS recode data (one round, or several rounds
#'   pooled with [bdhs_pool()]).
#' @param weight Which weight to use.
#'   * `"default"`: `pool_wt` for pooled data made by [bdhs_pool()], otherwise
#'     the standard weight of the recode (`v005` for women, children and
#'     births, `hv005` for households and members, `mv005` for men).
#'   * `"dv"`: the domestic violence module weight (`d005`; `md005` for men).
#'   * `"male"`: the household weight for the men's subsample (`hv028`) in
#'     household and member files; `mv005` in the men's file.
#'   * `"household"`: the household weight `hv005`.
#'   * A column name. DHS weights with six implied decimals (`*005`,
#'     `hv028`, `ha69`, `hb69`) are divided by 1e6.
#' @param strata Stratification. `"auto"` (default) uses the DHS sampling-error
#'   strata (`v022`/`hv022`/`mv022`), which is the DHS Program's
#'   recommendation for variance estimation. `"design"` uses `v023`, and
#'   `"region_residence"` uses region x urban/rural. `"none"` ignores
#'   stratification (not recommended).
#' @param survey,recode Optional explicit survey and recode, overriding
#'   detection.
#' @param srvyr If `TRUE`, return a `tbl_svy` object for use with the srvyr
#'   package.
#'
#' @return A `survey.design2` object (or a `tbl_svy` when `srvyr = TRUE`)
#'   with attributes `bdhs_survey_id`, `bdhs_recode`, `bdhs_weight` (the
#'   weight column used) and `bdhs_singleton_strata` (number of strata with
#'   one PSU).
#' @export
#' @examples
#' ir <- bdhs_simulate(2022, "IR")
#' des <- bdhs_design(ir)
#' survey::svymean(~v201, des)
#'
#' # Pooled rounds: round-specific strata and PSUs, pooled weights
#' p <- bdhs_pool(bdhs_simulate(2017, "IR"), bdhs_simulate(2022, "IR"))
#' survey::svyby(~v201, ~survey_id, bdhs_design(p), survey::svymean)
bdhs_design <- function(data, weight = "default", strata = c("auto", "design", "region_residence", "none"),
                        survey = NULL, recode = NULL, srvyr = FALSE) {
  strata <- rlang::arg_match(strata)
  if (!is.character(weight) || length(weight) != 1L || is.na(weight)) {
    cli::cli_abort("{.arg weight} must be a single string.")
  }
  recode <- data_recode(data, recode)
  survey_id <- data_survey_id(data, survey)
  dv <- .design_vars[[design_prefix(recode)]]

  if (!dv[["psu"]] %in% names(data)) {
    cli::cli_abort("Design variable {.field {dv[['psu']]}} not found in {.arg data}.")
  }

  pooled <- "survey_id" %in% names(data) && length(unique(data$survey_id)) > 1L
  ws <- design_weight(data, weight, recode, dv, pooled)
  w <- ws$w

  group <- if (pooled) as.character(data$survey_id) else rep("", nrow(data))

  st <- switch(strata,
    auto = data[[dv[["strata"]]]],
    design = data[[dv[["strata_design"]]]],
    region_residence = {
      if (all(dv[c("region", "residence")] %in% names(data))) {
        interaction(zap_all(data[[dv[["region"]]]]), zap_all(data[[dv[["residence"]]]]), drop = TRUE)
      }
    },
    none = NULL
  )
  if (strata != "none" && is.null(st)) {
    cli::cli_abort("The strata variable for {.code strata = \"{strata}\"} is not in {.arg data}.")
  }
  if (!is.null(st) && !pooled && length(unique(stats::na.omit(as.character(zap_all(st))))) == 1L) {
    cli::cli_inform(c(
      "!" = "The strata variable for {.code strata = \"{strata}\"} is constant (e.g. {.field v023} in the 2004 BDHS); the design is effectively unstratified.",
      "i" = "Use {.code strata = \"auto\"} for the DHS sampling-error strata."
    ))
  }

  design_data <- data
  design_data$.bdhs_psu <- paste(group, as.character(zap_all(data[[dv[["psu"]]]])))
  design_data$.bdhs_wt <- w
  if (!is.null(st)) {
    design_data$.bdhs_strata <- paste(group, as.character(zap_all(st)))
  }

  keep <- !is.na(w) & w > 0
  if (!any(keep)) {
    cli::cli_abort(c(
      "Every row has a missing or zero {.field {ws$var}} weight.",
      "i" = "Is this weight collected in this round (for example the men's subsample weight {.field hv028})?"
    ))
  }
  if (any(!keep)) {
    cli::cli_inform("Dropping {sum(!keep)} row{?s} with a missing or zero weight.")
    design_data <- design_data[keep, , drop = FALSE]
  }

  n_single <- 0L
  if (!is.null(st)) {
    n_psu <- tapply(design_data$.bdhs_psu, design_data$.bdhs_strata, function(z) length(unique(z)))
    n_single <- sum(n_psu == 1L)
    lonely <- getOption("survey.lonely.psu")
    if (n_single > 0L && (is.null(lonely) || identical(lonely, "fail"))) {
      cli::cli_inform(
        c(
          "!" = "{n_single} stratum/strata ha{?s/ve} a single PSU, so {.pkg survey} cannot estimate {?its/their} variance with the default settings.",
          "i" = "Set {.code options(survey.lonely.psu = \"adjust\")} before estimating (bdhs estimation functions do this for you)."
        ),
        .frequency = "once", .frequency_id = "bdhs_lonely_psu"
      )
    }
  }

  strata_formula <- if (is.null(st)) NULL else ~.bdhs_strata
  des <- survey::svydesign(
    ids = ~.bdhs_psu,
    strata = strata_formula,
    weights = ~.bdhs_wt,
    data = design_data,
    nest = TRUE
  )
  # Print as the user's call, not the internal svydesign() call.
  des$call <- sys.call()
  attr(des, "bdhs_survey_id") <- survey_id
  attr(des, "bdhs_recode") <- recode
  attr(des, "bdhs_weight") <- ws$var
  attr(des, "bdhs_singleton_strata") <- n_single
  if (srvyr) {
    check_installed("srvyr", "to return a srvyr design.")
    des <- srvyr::as_survey_design(des)
  }
  des
}

# Resolve the weight argument to a column and the weight values.
design_weight <- function(data, weight, recode, dv, pooled, call = rlang::caller_env()) {
  if (identical(weight, "default") && "pool_wt" %in% names(data)) {
    return(list(var = "pool_wt", w = as.numeric(zap_all(data$pool_wt))))
  }
  hh <- recode %in% c("HR", "PR")
  wvar <- switch(weight,
    default = dv[["weight"]],
    dv = if (recode == "MR") "md005" else "d005",
    male = if (recode == "MR") {
      "mv005"
    } else if (hh) {
      "hv028"
    } else {
      cli::cli_abort("{.code weight = \"male\"} applies to household (HR/PR) and men's (MR) files.", call = call)
    },
    household = "hv005",
    weight
  )
  if (!wvar %in% names(data)) {
    cli::cli_abort("Weight variable {.field {wvar}} not found in {.arg data}.", call = call)
  }
  w <- as.numeric(zap_all(data[[wvar]]))
  if (is_dhs_weight(wvar)) {
    w <- w / 1e6
  }
  if (pooled && !identical(wvar, "pool_wt")) {
    cli::cli_inform(
      c("i" = "Pooled data: {.field {wvar}} is normalized within each round, so rounds are weighted by sample size. See {.fn bdhs_pool} for other choices."),
      .frequency = "once", .frequency_id = "bdhs_pooled_weight"
    )
  }
  list(var = wvar, w = w)
}

# Per-round design facts, verified against the local recode files where
# available (design_meta research) and the DHS / World Bank microdata
# catalogue metadata otherwise.
.design_rounds <- tibble::tribble(
  ~survey_id, ~n_strata, ~strata_type, ~men, ~notes,
  "BD1994DHS", 148L, "paired-PSU sampling-error strata",
  TRUE,
  "v022 groups PSUs in pairs (143 strata with 2 PSUs, 5 with 3 in the KR file); v023 is the sample domain (14 values). Ever-married women 10-49. KR covers births in the last 3 years.",
  "BD1997DHS", 154L, "paired-PSU sampling-error strata",
  TRUE,
  "Design taken from catalogue metadata (v022 1-154, v023 1-16); not verified against the data files.",
  "BD2000DHS", 168L, "paired-PSU sampling-error strata",
  TRUE,
  "163 strata with 2 PSUs and 5 with 3 (KR); v023 = division x urban/rural (12 domains). Men's weights (mv005) are reported as 0 in catalogue metadata: check before using the MR file.",
  "BD2004DHS", 177L, "paired-PSU sampling-error strata",
  TRUE,
  "v023 is constant (0), so strata = \"design\" gives an unstratified design; use v022.",
  "BD2007DHS", 179L, "paired-PSU sampling-error strata",
  TRUE,
  "176 strata with 2 PSUs and 3 with 3 (KR); v023 holds the 22 design strata (division x city corporation/other urban/rural).",
  "BD2011DHS", 20L, "division x residence (city corporation, other urban, rural)",
  TRUE,
  "v022 = v023: 20 design strata (Rangpur has no city corporation). 11 KR births to women aged 13-14 have v005 = 0.",
  "BD2014DHS", 20L, "division x residence (city corporation, other urban, rural)",
  FALSE,
  "20 sampling strata (v022 codes 1-21; v023 has 21 codes). No men's survey; hv027/hv028 are 'na'.",
  "BD2017DHS", 22L, "division x residence (city corporation, other urban, rural)",
  FALSE,
  "v022 = v023: 22 design strata. No men's survey (hv027 = 0, hv028 = 0 for all households).",
  "BD2022DHS", 16L, "division x urban/rural",
  FALSE,
  "v022 = v023 = division x urban/rural (16 strata), although the report describes 23 design strata (city corporations); use v022 as distributed. No men's survey (hv027/hv028 'na'). Anthropometry and biomarkers in a subsample."
)

#' Survey design of each BDHS round
#'
#' Documents, for every BDHS round and recode, the variables that define the
#' complex sample design (clusters, strata, weights) and round-specific
#' caveats, as used by [bdhs_design()].
#'
#' All rounds use a stratified two-stage cluster sample. The PSU (`v021`) is
#' the cluster (`v001`) in every round. The DHS sampling-error strata
#' (`v022`) are the strata to use for variance estimation in every round; in
#' 1993-94 to 2007 they are pairs of PSUs (148-179 strata), from 2011 they
#' are the design strata (division by urban/rural, with city corporations in
#' 2011-2017-18). Weights have six implied decimals and are divided by
#' 1,000,000.
#'
#' @param survey Optional survey reference(s) (see [bdhs_survey_id()]).
#'   `NULL` gives all DHS rounds.
#' @param recode Optional recode code(s) (`"IR"`, `"KR"`, `"BR"`, `"HR"`,
#'   `"PR"`, `"MR"`). `NULL` gives all of them.
#' @return A tibble with one row per round and recode: `survey_id`,
#'   `year_label`, `recode`, `psu`, `strata`, `strata_design`, `weight`,
#'   `weight_divisor`, `n_strata`, `strata_type`, `extra_weights` and
#'   `notes`.
#' @export
#' @examples
#' bdhs_design_info(recode = "IR")
#' bdhs_design_info(2004, "KR")$notes
bdhs_design_info <- function(survey = NULL, recode = NULL) {
  ids <- if (is.null(survey)) .design_rounds$survey_id else bdhs_survey_id(survey, type = "DHS")
  bad <- setdiff(ids, .design_rounds$survey_id)
  if (length(bad)) {
    cli::cli_abort("No design information for {.val {bad}}.")
  }
  recs <- if (is.null(recode)) c("IR", "KR", "BR", "HR", "PR", "MR") else vapply(recode, check_recode, character(1))
  bad <- setdiff(recs, c("IR", "KR", "BR", "HR", "PR", "MR"))
  if (length(bad)) {
    cli::cli_abort("Design information is available for IR, KR, BR, HR, PR and MR files, not {.val {bad}}.")
  }
  grid <- expand.grid(recode = recs, survey_id = ids, stringsAsFactors = FALSE)
  info <- .design_rounds[match(grid$survey_id, .design_rounds$survey_id), ]
  grid <- grid[info$men | grid$recode != "MR", , drop = FALSE]
  info <- .design_rounds[match(grid$survey_id, .design_rounds$survey_id), ]
  pre <- vapply(grid$recode, design_prefix, character(1))
  extra <- vapply(seq_len(nrow(grid)), function(i) {
    r <- grid$recode[[i]]
    w <- character()
    if (r %in% c("HR", "PR") && info$men[[i]]) w <- c(w, "hv028 (men's subsample)")
    if (r %in% c("IR", "KR", "BR")) w <- c(w, "awfact* (all-woman factors)")
    if (r == "IR" && grid$survey_id[[i]] == "BD2007DHS") w <- c(w, "d005 (domestic violence module)")
    if (length(w)) paste(w, collapse = "; ") else NA_character_
  }, character(1))
  tibble::tibble(
    survey_id = grid$survey_id,
    year_label = survey_label(grid$survey_id),
    recode = grid$recode,
    psu = paste0(pre, "021"),
    strata = paste0(pre, "022"),
    strata_design = paste0(pre, "023"),
    weight = paste0(pre, "005"),
    weight_divisor = 1e6,
    n_strata = info$n_strata,
    strata_type = info$strata_type,
    extra_weights = extra,
    notes = info$notes
  )
}
