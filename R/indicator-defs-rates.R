# Indicator definitions: fertility and childhood mortality rates -------------------
#
# Rate indicators are not record-level variables; bdhs_estimate() computes
# them with bdhs_fertility() and bdhs_mortality().

.ref_rates_fe <- "Guide to DHS Statistics (DHS-7), sec. 5.1-5.3; DHS-Indicators-R Chap05_FE (FE_TFR); DHS.rates (Elkasabi 2019)"
.ref_rates_cm <- "Guide to DHS Statistics (DHS-7), sec. 8.1; DHS-Indicators-R Chap08_CM (CM_CHILD); DHS.rates (Elkasabi 2019)"

# Convert a bdhs_fertility()/bdhs_mortality() result to bdhs_estimate() columns.
rate_as_estimate <- function(r, by, n_unweighted, n_weighted) {
  out <- r[, c(by, "estimate", "se", "ci_low", "ci_high"), drop = FALSE]
  out$n_unweighted <- as.integer(r[[n_unweighted]])
  out$n_weighted <- if (is.null(n_weighted)) NA_real_ else r[[n_weighted]]
  out
}

fertility_rate_indicator <- function(id, label, type, api_id, age_group = NULL, tol_scale = 1, definition) {
  new_indicator(
    id, label, "IR", "fertility", type = "rate", api_id = api_id, window = 36L,
    rate = function(data, by, level, survey) {
      r <- bdhs_fertility(data, type = type, period = 36, by = by, se = TRUE, level = level, survey = survey)
      if (!is.null(age_group)) r <- r[r$age_group == age_group, ]
      rate_as_estimate(r, by, "n_women", "exposure")
    },
    vars = c("v005", "v008", "v011", "v021", "awfactt", sprintf("b3_%02d", 1:20)),
    definition = definition,
    notes = "Births in the 36 months before the interview; exposure adjusted with the all-woman factor (ever-married sample). Jackknife standard errors.",
    reference = .ref_rates_fe, tol_scale = tol_scale
  )
}

mortality_rate_indicator <- function(id, label, rate, api_id, definition) {
  new_indicator(
    id, label, c("BR", "IR"), "mortality", type = "rate", api_id = api_id,
    api_by = "Five years preceding the survey", window = 60L,
    rate = function(data, by, level, survey) {
      r <- bdhs_mortality(data, period = 5, rates = rate, by = by, se = TRUE, level = level, survey = survey)
      rate_as_estimate(r, by, "n_births", NULL)
    },
    vars = c("v005", "v008", "v021", "b3", "b7", sprintf("b3_%02d", 1:20), sprintf("b7_%02d", 1:20)),
    definition = definition,
    notes = "Synthetic cohort life table, 5 years before the survey, per 1,000 live births. Jackknife standard errors.",
    reference = .ref_rates_cm
  )
}

defs_rates <- function() {
  list(
    fertility_rate_indicator("fe_tfr", "Total fertility rate (births per woman)", "tfr", "FE_FRTR_W_TFR",
      tol_scale = 0.1,
      definition = "Average number of children a woman would have by age 50 at current age-specific fertility rates (3 years before the survey)."),
    fertility_rate_indicator("fe_gfr", "General fertility rate (births per 1,000 women 15-44)", "gfr", "FE_FRTR_W_GFR",
      definition = "Births in the 3 years before the survey per 1,000 woman-years lived at age 15-44."),
    fertility_rate_indicator("fe_asfr_15_19", "Adolescent birth rate (births per 1,000 women 15-19)", "asfr", "FE_FRTR_W_A15",
      age_group = "15-19",
      definition = "Births to women age 15-19 in the 3 years before the survey per 1,000 woman-years at age 15-19 (SDG 3.7.2)."),
    mortality_rate_indicator("cm_nnmr", "Neonatal mortality rate", "nnmr", "CM_ECMR_C_NNR",
      "Probability of dying in the first month of life, per 1,000 live births."),
    mortality_rate_indicator("cm_pnnmr", "Postneonatal mortality rate", "pnnmr", "CM_ECMR_C_PNR",
      "Difference between the infant and neonatal mortality rates, per 1,000 live births."),
    mortality_rate_indicator("cm_imr", "Infant mortality rate", "imr", "CM_ECMR_C_IMR",
      "Probability of dying before the first birthday, per 1,000 live births."),
    mortality_rate_indicator("cm_cmr", "Child mortality rate (4q1)", "cmr", "CM_ECMR_C_CMR",
      "Probability of dying between the first and fifth birthday, per 1,000 children surviving to age 1."),
    mortality_rate_indicator("cm_u5mr", "Under-five mortality rate", "u5mr", "CM_ECMR_C_U5M",
      "Probability of dying before the fifth birthday, per 1,000 live births (SDG 3.2.1).")
  )
}
