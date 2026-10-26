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

#' Build the complex survey design for BDHS data
#'
#' Creates a [survey::svydesign()] object with the clustering, stratification
#' and weights that the DHS Program uses for the given recode, so standard
#' errors and confidence intervals are correct. The round and recode are
#' detected automatically for data read with [bdhs_read()] or
#' [bdhs_simulate()].
#'
#' @param data A data frame of BDHS recode data (one round, or several rounds
#'   pooled with [bdhs_pool()]).
#' @param weight Which weight to use. `"default"` is the standard weight of
#'   the recode (`v005` for women, children and births, `hv005` for households
#'   and members, `mv005` for men). `"dv"` uses the domestic violence module
#'   weight `d005`. A column name can also be given; it is divided by 1e6 when
#'   it is one of the DHS 6-decimal weights.
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
#'   with attributes `bdhs_survey_id` and `bdhs_recode`.
#' @export
#' @examples
#' ir <- bdhs_simulate(2022, "IR")
#' des <- bdhs_design(ir)
#' survey::svymean(~v201, des)
bdhs_design <- function(data, weight = "default", strata = c("auto", "design", "region_residence", "none"),
                        survey = NULL, recode = NULL, srvyr = FALSE) {
  strata <- rlang::arg_match(strata)
  recode <- data_recode(data, recode)
  survey_id <- data_survey_id(data, survey)
  dv <- .design_vars[[design_prefix(recode)]]

  missing_vars <- setdiff(dv[c("psu", "weight")], names(data))
  if (length(missing_vars)) {
    cli::cli_abort("Design variable{?s} {.field {missing_vars}} not found in {.arg data}.")
  }

  wvar <- if (identical(weight, "default")) {
    dv[["weight"]]
  } else if (identical(weight, "dv")) {
    "d005"
  } else {
    weight
  }
  if (!wvar %in% names(data)) {
    cli::cli_abort("Weight variable {.field {wvar}} not found in {.arg data}.")
  }
  w <- as.numeric(zap_all(data[[wvar]]))
  if (grepl("005$", wvar)) {
    w <- w / 1e6
  }

  pooled <- "survey_id" %in% names(data) && length(unique(data$survey_id)) > 1L
  group <- if (pooled) as.character(data$survey_id) else rep("", nrow(data))

  st <- switch(strata,
    auto = data[[dv[["strata"]]]],
    design = data[[dv[["strata_design"]]]],
    region_residence = interaction(zap_all(data[[dv[["region"]]]]), zap_all(data[[dv[["residence"]]]]), drop = TRUE),
    none = NULL
  )
  if (strata != "none" && is.null(st)) {
    cli::cli_abort("The strata variable for {.code strata = \"{strata}\"} is not in {.arg data}.")
  }

  design_data <- data
  design_data$.bdhs_psu <- paste(group, as.character(zap_all(data[[dv[["psu"]]]])))
  design_data$.bdhs_wt <- w
  if (!is.null(st)) {
    design_data$.bdhs_strata <- paste(group, as.character(zap_all(st)))
  }

  keep <- !is.na(w) & w > 0
  if (any(!keep)) {
    cli::cli_inform("Dropping {sum(!keep)} row{?s} with a missing or zero weight.")
    design_data <- design_data[keep, , drop = FALSE]
  }

  des <- survey::svydesign(
    ids = ~.bdhs_psu,
    strata = if (is.null(st)) NULL else ~.bdhs_strata,
    weights = ~.bdhs_wt,
    data = design_data,
    nest = TRUE
  )
  attr(des, "bdhs_survey_id") <- survey_id
  attr(des, "bdhs_recode") <- recode
  if (srvyr) {
    check_installed("srvyr", "to return a srvyr design.")
    des <- srvyr::as_survey_design(des)
  }
  des
}
