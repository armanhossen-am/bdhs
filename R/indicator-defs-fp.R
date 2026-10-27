# Indicator definitions: family planning, marriage and fertility (IR) -------------
#
# Logic ported from DHS-Indicators-R (Chap07_FP/FP_USE.R, FP_NEED.R;
# Chap04_MS/MS_MAR.R; Chap05_FE/FE_TFR.R, FE_CBR.R) and the Guide to DHS
# Statistics (DHS-7). Not copied: re-implemented for bdhs.

.ref_fp <- "Guide to DHS Statistics (DHS-7), ch. 7; DHS-Indicators-R Chap07_FP (FP_USE, FP_NEED)"
.ref_ms <- "Guide to DHS Statistics (DHS-7), ch. 4; DHS-Indicators-R Chap04_MS (MS_MAR)"
.ref_fe <- "Guide to DHS Statistics (DHS-7), ch. 5; DHS-Indicators-R Chap05_FE"

.note_2022_long <- paste(
  "2022: asked only in the long women's questionnaire (sqtype == 1, two thirds of households);",
  "women with the short questionnaire are excluded from the denominator."
)

# Currently married women (v502 == 1) who answered the module.
fp_universe <- function(key = "v313") {
  function(d, s) {
    in_codes(dv(d, "v502"), 1) & long_questionnaire(d, s) & !is.na(dv(d, key))
  }
}

# Unmet need variable: revised definition (v626a) when available, else v626.
unmet_var <- function(d) {
  if (has_var(d, "v626a")) dv(d, "v626a") else dv(d, "v626")
}

fp_unmet_universe <- function(d, s) {
  in_codes(dv(d, "v502"), 1) & long_questionnaire(d, s) & !is.na(unmet_var(d))
}

fp_method <- function(id, label, codes, api_id) {
  new_indicator(
    id = id, label = label, recode = "IR", topic = "family_planning",
    universe = fp_universe("v312"),
    value = function(d, s) as.numeric(in_codes(dv(d, "v312"), codes)),
    api_id = api_id, vars = c("v502", "v312", "sqtype"), requires = c("v502", "v312"),
    definition = paste0("Percentage of currently married women age 15-49 currently using ", tolower(sub("^Married women using ", "", label)), " (v312 in ", paste(codes, collapse = ", "), ")."),
    notes = .note_2022_long, reference = .ref_fp
  )
}

defs_family_planning <- function() {
  list(
    new_indicator(
      "fp_cpr_any", "Contraceptive prevalence rate: any method (married women)", "IR", "family_planning",
      requires = c("v502", "v313"),
      universe = fp_universe("v313"),
      value = function(d, s) as.numeric(in_codes(dv(d, "v313"), 1:3)),
      api_id = "FP_CUSM_W_ANY", vars = c("v502", "v313", "sqtype"),
      definition = "Percentage of currently married women age 15-49 using any contraceptive method (v313 > 0).",
      notes = .note_2022_long, reference = .ref_fp
    ),
    new_indicator(
      "fp_cpr_modern", "Contraceptive prevalence rate: modern methods (married women)", "IR", "family_planning",
      requires = c("v502", "v313"),
      universe = fp_universe("v313"),
      value = function(d, s) as.numeric(in_codes(dv(d, "v313"), 3)),
      api_id = "FP_CUSM_W_MOD", vars = c("v502", "v313", "sqtype"),
      definition = "Percentage of currently married women age 15-49 using a modern method (v313 == 3).",
      notes = .note_2022_long, reference = .ref_fp
    ),
    new_indicator(
      "fp_cpr_traditional", "Contraceptive prevalence rate: traditional methods (married women)", "IR", "family_planning",
      requires = c("v502", "v313"),
      universe = fp_universe("v313"),
      value = function(d, s) as.numeric(in_codes(dv(d, "v313"), 1:2)),
      api_id = "FP_CUSM_W_TRA", vars = c("v502", "v313", "sqtype"),
      definition = "Percentage of currently married women age 15-49 using a traditional or folkloric method (v313 in 1, 2).",
      notes = .note_2022_long, reference = .ref_fp
    ),
    fp_method("fp_method_pill", "Married women using pill", 1, "FP_CUSM_W_PIL"),
    fp_method("fp_method_iud", "Married women using IUD", 2, "FP_CUSM_W_IUD"),
    fp_method("fp_method_injectables", "Married women using injectables", 3, "FP_CUSM_W_INJ"),
    fp_method("fp_method_condom", "Married women using male condom", 5, "FP_CUSM_W_MCN"),
    fp_method("fp_method_female_sterilization", "Married women using female sterilization", 6, "FP_CUSM_W_FST"),
    fp_method("fp_method_male_sterilization", "Married women using male sterilization", 7, "FP_CUSM_W_MST"),
    fp_method("fp_method_rhythm", "Married women using periodic abstinence", 8, "FP_CUSM_W_RHY"),
    fp_method("fp_method_withdrawal", "Married women using withdrawal", 9, "FP_CUSM_W_WTH"),
    fp_method("fp_method_implants", "Married women using implants", 11, "FP_CUSM_W_IMP"),
    new_indicator(
      "fp_unmet_total", "Unmet need for family planning (married women)", "IR", "family_planning",
      requires = c("v502", "v626a|v626"),
      universe = fp_unmet_universe,
      value = function(d, s) as.numeric(in_codes(unmet_var(d), 1:2)),
      api_id = "FP_NADM_W_UNT", vars = c("v502", "v626a", "v626", "sqtype"),
      definition = "Percentage of currently married women with unmet need for spacing or limiting (revised definition, v626a in 1, 2).",
      notes = .note_2022_long, reference = .ref_fp
    ),
    new_indicator(
      "fp_unmet_spacing", "Unmet need for spacing (married women)", "IR", "family_planning",
      requires = c("v502", "v626a|v626"),
      universe = fp_unmet_universe,
      value = function(d, s) as.numeric(in_codes(unmet_var(d), 1)),
      api_id = "FP_NADM_W_UNS", vars = c("v502", "v626a", "v626", "sqtype"),
      definition = "Percentage of currently married women with unmet need for spacing (v626a == 1).",
      notes = .note_2022_long, reference = .ref_fp
    ),
    new_indicator(
      "fp_unmet_limiting", "Unmet need for limiting (married women)", "IR", "family_planning",
      requires = c("v502", "v626a|v626"),
      universe = fp_unmet_universe,
      value = function(d, s) as.numeric(in_codes(unmet_var(d), 2)),
      api_id = "FP_NADM_W_UNL", vars = c("v502", "v626a", "v626", "sqtype"),
      definition = "Percentage of currently married women with unmet need for limiting (v626a == 2).",
      notes = .note_2022_long, reference = .ref_fp
    ),
    new_indicator(
      "fp_demand_total", "Total demand for family planning (married women)", "IR", "family_planning",
      requires = c("v502", "v626a|v626"),
      universe = fp_unmet_universe,
      value = function(d, s) as.numeric(in_codes(unmet_var(d), 1:4)),
      api_id = "FP_NADM_W_TDT", vars = c("v502", "v626a", "v626", "sqtype"),
      definition = "Percentage of currently married women with unmet need or currently using (v626a in 1-4).",
      notes = .note_2022_long, reference = .ref_fp
    ),
    new_indicator(
      "fp_demand_satisfied", "Demand for family planning satisfied (any method)", "IR", "family_planning",
      requires = c("v502", "v626a|v626"),
      universe = function(d, s) fp_unmet_universe(d, s) & in_codes(unmet_var(d), 1:4),
      value = function(d, s) as.numeric(in_codes(unmet_var(d), 3:4)),
      api_id = "FP_NADM_W_PDS", vars = c("v502", "v626a", "v626", "sqtype"),
      definition = "Among currently married women with a demand for family planning, the percentage currently using any method.",
      notes = .note_2022_long, reference = .ref_fp
    ),
    new_indicator(
      "fp_demand_satisfied_modern", "Demand for family planning satisfied by modern methods", "IR", "family_planning",
      requires = c("v502", "v626a|v626", "v313"),
      universe = function(d, s) fp_unmet_universe(d, s) & in_codes(unmet_var(d), 1:4),
      value = function(d, s) as.numeric(in_codes(unmet_var(d), 3:4) & in_codes(dv(d, "v313"), 3)),
      api_id = "FP_NADM_W_PDM", vars = c("v502", "v626a", "v626", "v313", "sqtype"),
      definition = "Among currently married women with a demand for family planning, the percentage using a modern method (SDG 3.7.1).",
      notes = .note_2022_long, reference = .ref_fp
    )
  )
}

# Women in an age range, among those who answered the module (2022: long questionnaire).
age_universe <- function(lo, hi, long = TRUE) {
  function(d, s) {
    a <- dv(d, "v012")
    out <- !is.na(a) & a >= lo & a <= hi
    if (long) out <- out & long_questionnaire(d, s)
    out
  }
}

married_by <- function(age) {
  function(d, s) {
    afm <- dv(d, "v511")
    as.numeric(!is.na(afm) & afm < age)
  }
}

median_afm <- function(id, lo, api_id) {
  new_indicator(
    id, paste0("Median age at first marriage, women age ", lo, "-49"), "IR", "marriage",
    universe = age_universe(lo, 49),
    value = function(d, s) {
      afm <- dv(d, "v511")
      ifelse(is.na(afm), Inf, afm)
    },
    type = "median", all_women = "awfactt", api_id = api_id, requires = c("v012", "v511"),
    vars = c("v012", "v511", "awfactt", "sqtype"),
    params = list(min_age = lo),
    definition = paste0(
      "Age by which half of all women age ", lo, "-49 had married (interpolated from completed ages, v511),",
      " counting never-married women through the all-woman factor. Not computed if fewer than half married before age ", lo, "."
    ),
    notes = paste("Ever-married sample: never-married women are represented by the all-woman factor (awfactt).", .note_2022_long),
    reference = .ref_ms
  )
}

defs_marriage_fertility <- function() {
  list(
    new_indicator(
      "ms_married_by_15", "Women age 20-24 first married by exact age 15", "IR", "marriage",
      requires = c("v012", "v511"),
      universe = age_universe(20, 24), value = married_by(15),
      all_women = "awfactt", api_id = "MA_MBAY_W_B15", vars = c("v012", "v511", "awfactt", "sqtype"),
      definition = "Percentage of all women age 20-24 who were first married or in union before age 15 (child marriage, SDG 5.3.1).",
      notes = paste("All-women denominator via awfactt (ever-married sample).", .note_2022_long),
      reference = .ref_ms
    ),
    new_indicator(
      "ms_married_by_18", "Women age 20-24 first married by exact age 18", "IR", "marriage",
      requires = c("v012", "v511"),
      universe = age_universe(20, 24), value = married_by(18),
      all_women = "awfactt", api_id = "MA_MBAY_W_B18", vars = c("v012", "v511", "awfactt", "sqtype"),
      definition = "Percentage of all women age 20-24 who were first married or in union before age 18 (child marriage, SDG 5.3.1).",
      notes = paste("All-women denominator via awfactt (ever-married sample).", .note_2022_long),
      reference = .ref_ms
    ),
    median_afm("ms_median_afm_20_49", 20, "MA_AAFM_W_M2A"),
    median_afm("ms_median_afm_25_49", 25, "MA_AAFM_W_M2B"),
    new_indicator(
      "fe_teen_mothers", "Women age 15-19 who have had a live birth", "IR", "fertility",
      requires = c("v012", "v201"),
      universe = age_universe(15, 19, long = FALSE),
      value = function(d, s) as.numeric(in_codes(dv(d, "v201"), 1:30)),
      all_women = "awfactt", api_id = "FE_TNPG_W_LVB", vars = c("v012", "v201", "awfactt"),
      definition = "Percentage of all women age 15-19 who have had a live birth (v201 > 0).",
      notes = "All-women denominator via awfactt. 2022: birth history is in both questionnaires, so all women are included.",
      reference = .ref_fe
    ),
    new_indicator(
      "fe_teen_pregnant_first", "Women age 15-19 pregnant with their first child", "IR", "fertility",
      requires = c("v012", "v201", "v213"),
      universe = age_universe(15, 19, long = FALSE),
      value = function(d, s) as.numeric(in_codes(dv(d, "v201"), 0) & in_codes(dv(d, "v213"), 1)),
      all_women = "awfactt", api_id = "FE_TNPG_W_PRG", vars = c("v012", "v201", "v213", "awfactt"),
      definition = "Percentage of all women age 15-19 who have no live birth and are currently pregnant.",
      notes = "All-women denominator via awfactt.", reference = .ref_fe
    ),
    new_indicator(
      "fe_teen_begun_childbearing", "Women age 15-19 who have begun childbearing", "IR", "fertility",
      requires = c("v012", "v201", "v213"),
      universe = age_universe(15, 19, long = FALSE),
      value = function(d, s) as.numeric(in_codes(dv(d, "v201"), 1:30) | in_codes(dv(d, "v213"), 1)),
      all_women = "awfactt", api_id = "FE_TNPG_W_CBR", vars = c("v012", "v201", "v213", "awfactt"),
      definition = "Percentage of all women age 15-19 who have had a live birth or are pregnant with their first child.",
      notes = "All-women denominator via awfactt.", reference = .ref_fe
    ),
    new_indicator(
      "fe_ceb_mean", "Mean number of children ever born, women 15-49", "IR", "fertility",
      requires = c("v012", "v201"),
      universe = age_universe(15, 49, long = FALSE),
      value = function(d, s) dv(d, "v201"),
      type = "mean", all_women = "awfactt", api_id = "FE_CEBA_W_MNC", vars = c("v012", "v201", "awfactt"),
      definition = "Mean number of children ever born (v201) to all women age 15-49.",
      notes = "All-women denominator via awfactt (never-married women have no births).", reference = .ref_fe
    )
  )
}
