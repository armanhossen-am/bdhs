# Indicator definitions: household, education, media, empowerment, biomarkers ----
#
# Logic ported from DHS-Indicators-R (Chap02_PH/PH_WATER.R, PH_SANI.R,
# PH_HOUS.R, PH_HNDWSH.R; Chap03_RC/RC_CHAR_WM.R; Chap15_WE/WE_EMPW.R) and the
# Guide to DHS Statistics (DHS-7). Source and facility code lists per round
# are taken from the value labels of each round's recode files.

.ref_ph <- "Guide to DHS Statistics (DHS-7), ch. 2; DHS-Indicators-R Chap02_PH (PH_WATER, PH_SANI, PH_HOUS, PH_HNDWSH)"
.ref_rc <- "Guide to DHS Statistics (DHS-7), ch. 3; DHS-Indicators-R Chap03_RC (RC_CHAR_WM)"
.ref_we <- "Guide to DHS Statistics (DHS-7), ch. 15; DHS-Indicators-R Chap15_WE (WE_EMPW)"

# Improved drinking-water sources (JMP) by round, from hv201/v113 value labels.
.water_improved <- list(
  BD1994DHS = c(11, 12, 21, 41),
  BD1997DHS = c(11, 12, 21, 41, 51, 61),
  BD2000DHS = c(11, 12, 21, 41, 51, 61),
  BD2004DHS = c(11, 12, 21, 22, 23, 30, 51, 61, 71),
  default = c(11, 12, 13, 14, 21, 31, 41, 51, 61, 62, 71)
)
# Improved sanitation (JMP, ignoring sharing) by round, from hv205/v116 labels.
.sanitation_improved <- list(
  BD2000DHS = c(11, 21),
  BD2004DHS = c(11, 21),
  default = c(11, 12, 13, 15, 21, 22, 41)
)
# Solid cooking fuels by round (hv226).
.solid_fuel <- list(
  BD2004DHS = 5:9,
  default = 6:11
)

dejure <- function(d, s) in_codes(dv(d, "hv102"), 1)

.hh_note <- "De jure household population (PR, hv102 == 1); compute from PR so that households are weighted by their members."

defs_household <- function() {
  list(
    new_indicator(
      "hh_electricity", "Population with electricity", "PR", "household",
      requires = c("hv102", "hv206"),
      universe = dejure,
      value = function(d, s) {
        out <- in_codes(dv(d, "hv206"), 1)
        if (identical(s, "BD2022DHS")) out <- out | in_codes(dv(d, "sh132g"), 1)
        as.numeric(out)
      },
      api_id = "HC_ELEC_P_ELC", vars = c("hv102", "hv206", "sh132g"),
      definition = "Percentage of the de jure population living in households with electricity.",
      notes = paste(.hh_note, "2022: solar electricity (sh132g) counts, as in the official tables; grid electricity alone (hv206) gives 98.1% instead of 99.4%."),
      reference = .ref_ph
    ),
    new_indicator(
      "hh_water_improved", "Population using an improved drinking-water source", "PR", "household",
      requires = c("hv102", "hv201"),
      universe = function(d, s) {
        src <- dv(d, "hv201")
        dejure(d, s) & !is.na(src) & src != 99
      },
      value = function(d, s) as.numeric(in_codes(dv(d, "hv201"), by_round(.water_improved, s))),
      api_id = "WS_SRCE_P_IMP", vars = c("hv102", "hv201"),
      definition = "Percentage of the de jure population whose main source of drinking water is improved (piped, tube well, protected well or spring, rainwater, tanker/cart, bottled).",
      notes = paste(.hh_note, "Arsenic contamination of tube wells is not taken into account. Code lists before 2007 are from value labels and not verified against official values."),
      reference = .ref_ph
    ),
    new_indicator(
      "hh_sanitation_improved", "Population using an improved sanitation facility", "PR", "household",
      requires = c("hv102", "hv205"),
      universe = dejure,
      value = function(d, s) as.numeric(in_codes(dv(d, "hv205"), by_round(.sanitation_improved, s))),
      api_id = "WS_TLET_P_IMP", rounds = setdiff(.dhs_rounds, c("BD1994DHS", "BD1997DHS")),
      vars = c("hv102", "hv205"),
      definition = "Percentage of the de jure population using an improved toilet facility (flush to sewer/septic/pit, VIP, pit with slab, composting), shared or not.",
      notes = paste(.hh_note, "1993-94 and 1996-97 toilet codes do not identify improved facilities."),
      reference = .ref_ph
    ),
    new_indicator(
      "hh_sanitation_basic", "Population with basic sanitation service", "PR", "household",
      requires = c("hv102", "hv205", "hv225"),
      universe = dejure,
      value = function(d, s) {
        as.numeric(in_codes(dv(d, "hv205"), by_round(.sanitation_improved, s)) & in_codes(dv(d, "hv225"), 0))
      },
      api_id = "WS_TLET_P_BAS", rounds = c("BD2007DHS", "BD2011DHS", "BD2014DHS", "BD2017DHS", "BD2022DHS"),
      vars = c("hv102", "hv205", "hv225"),
      definition = "Percentage of the de jure population using an improved toilet facility that is not shared with other households.",
      notes = paste(.hh_note, "Sharing (hv225) is recorded from 2007."), reference = .ref_ph
    ),
    new_indicator(
      "hh_open_defecation", "Population practising open defecation", "PR", "household",
      requires = c("hv102", "hv205"),
      universe = dejure,
      value = function(d, s) as.numeric(in_codes(dv(d, "hv205"), c(30, 31))),
      api_id = "WS_TLET_P_NFC", vars = c("hv102", "hv205"),
      definition = "Percentage of the de jure population with no toilet facility (bush/field).",
      notes = .hh_note, reference = .ref_ph
    ),
    new_indicator(
      "hh_handwashing_basic", "Population with a basic handwashing facility", "PR", "household",
      requires = c("hv102", "hv230a", "hv230b", "hv232"),
      universe = dejure,
      value = function(d, s) {
        as.numeric(in_codes(dv(d, "hv230a"), 1:2) & in_codes(dv(d, "hv230b"), 1) & in_codes(dv(d, "hv232"), 1))
      },
      api_id = "WS_HNDW_P_BAS", rounds = c("BD2014DHS", "BD2017DHS", "BD2022DHS"),
      vars = c("hv102", "hv230a", "hv230b", "hv232"),
      definition = "Percentage of the de jure population with a handwashing place observed with water and soap.",
      notes = .hh_note, reference = .ref_ph
    ),
    new_indicator(
      "hh_cooking_solid_fuel", "Population using solid fuel for cooking", "PR", "household",
      requires = c("hv102", "hv226"),
      universe = function(d, s) dejure(d, s) & !is.na(dv(d, "hv226")),
      value = function(d, s) as.numeric(in_codes(dv(d, "hv226"), by_round(.solid_fuel, s))),
      rounds = c("BD2004DHS", "BD2007DHS", "BD2011DHS", "BD2014DHS", "BD2017DHS"),
      vars = c("hv102", "hv226"),
      definition = "Percentage of the de jure population in households using solid fuel (coal, charcoal, wood, straw/shrubs/grass, crops, dung) for cooking.",
      notes = paste(.hh_note, "No DHS API reference value for Bangladesh; cooking fuel is empty in the 2022 recode files."),
      reference = .ref_ph
    )
  )
}

all_women_universe <- function(long = FALSE) {
  function(d, s) {
    a <- dv(d, "v012")
    out <- !is.na(a) & a >= 15 & a <= 49
    if (long) out <- out & long_questionnaire(d, s, keys = c("v744a", "v313"))
    out
  }
}

weekly <- function(d, var) in_codes(dv(d, var), 2:3)

decides <- function(d, var) in_codes(dv(d, var), 1:2)

married_long <- function(d, s) {
  in_codes(dv(d, "v502"), 1) & long_questionnaire(d, s, keys = c("v743a", "v313"))
}

.edu_note <- "Interviewed (ever-married) women age 15-49; cannot be adjusted to all women. 2022: background questions are in both questionnaires, so all women are included."
.we_note <- "Currently married women. 2022: long questionnaire only."

defs_education_empowerment <- function() {
  list(
    new_indicator(
      "ed_no_education", "Women with no education", "IR", "education_media",
      requires = c("v012", "v149"),
      universe = all_women_universe(), value = function(d, s) as.numeric(in_codes(dv(d, "v149"), 0)),
      api_id = "ED_EDUC_W_NED", vars = c("v012", "v149"),
      definition = "Percentage of interviewed women age 15-49 with no education (v149 == 0).",
      notes = .edu_note, reference = .ref_rc
    ),
    new_indicator(
      "ed_secondary_plus", "Women with secondary or higher education", "IR", "education_media",
      requires = c("v012", "v149"),
      universe = all_women_universe(), value = function(d, s) as.numeric(in_codes(dv(d, "v149"), 3:5)),
      api_id = "ED_EDUC_W_SEH", vars = c("v012", "v149"),
      definition = "Percentage of interviewed women age 15-49 who completed primary and attended secondary or higher education (v149 >= 3).",
      notes = .edu_note, reference = .ref_rc
    ),
    new_indicator(
      "ed_literate", "Literate women", "IR", "education_media",
      requires = c("v012", "v106", "v155"),
      universe = all_women_universe(),
      value = function(d, s) as.numeric(in_codes(dv(d, "v106"), 3) | in_codes(dv(d, "v155"), 1:2)),
      api_id = "ED_LITR_W_LIT", vars = c("v012", "v106", "v155"),
      definition = "Percentage of interviewed women age 15-49 with higher education or who can read a whole or part of a sentence.",
      notes = .edu_note, reference = .ref_rc
    ),
    new_indicator(
      "me_newspaper_weekly", "Women who read a newspaper at least once a week", "IR", "education_media",
      requires = c("v012", "v157"),
      universe = all_women_universe(), value = function(d, s) as.numeric(weekly(d, "v157")),
      api_id = "ED_MDIA_W_NWS", vars = c("v012", "v157"),
      definition = "Percentage of interviewed women age 15-49 who read a newspaper at least once a week.",
      notes = .edu_note, reference = .ref_rc
    ),
    new_indicator(
      "me_radio_weekly", "Women who listen to the radio at least once a week", "IR", "education_media",
      requires = c("v012", "v158"),
      universe = all_women_universe(), value = function(d, s) as.numeric(weekly(d, "v158")),
      api_id = "ED_MDIA_W_RDO", vars = c("v012", "v158"),
      definition = "Percentage of interviewed women age 15-49 who listen to the radio at least once a week.",
      notes = .edu_note, reference = .ref_rc
    ),
    new_indicator(
      "me_tv_weekly", "Women who watch television at least once a week", "IR", "education_media",
      requires = c("v012", "v159"),
      universe = all_women_universe(), value = function(d, s) as.numeric(weekly(d, "v159")),
      api_id = "ED_MDIA_W_TLV", vars = c("v012", "v159"),
      definition = "Percentage of interviewed women age 15-49 who watch television at least once a week.",
      notes = .edu_note, reference = .ref_rc
    ),
    new_indicator(
      "me_no_media", "Women with no weekly exposure to newspaper, radio or television", "IR", "education_media",
      requires = c("v012", "v157", "v158", "v159"),
      universe = all_women_universe(),
      value = function(d, s) as.numeric(!weekly(d, "v157") & !weekly(d, "v158") & !weekly(d, "v159")),
      api_id = "ED_MDIA_W_N3M", vars = c("v012", "v157", "v158", "v159"),
      definition = "Percentage of interviewed women age 15-49 who are not exposed to any of the three media at least once a week.",
      notes = .edu_note, reference = .ref_rc
    ),
    new_indicator(
      "we_decide_health", "Married women who decide on their own health care", "IR", "empowerment",
      requires = c("v502", "v743a"),
      universe = married_long, value = function(d, s) as.numeric(decides(d, "v743a")),
      api_id = "WE_DMAK_W_OHC", rounds = .dhs_rounds[-(1:3)], vars = c("v502", "v743a", "sqtype"),
      definition = "Percentage of currently married women who usually decide about their own health care alone or jointly with their husband.",
      notes = .we_note, reference = .ref_we
    ),
    new_indicator(
      "we_decide_purchases", "Married women who decide on major household purchases", "IR", "empowerment",
      requires = c("v502", "v743b"),
      universe = married_long, value = function(d, s) as.numeric(decides(d, "v743b")),
      api_id = "WE_DMAK_W_MPC", rounds = .dhs_rounds[-(1:3)], vars = c("v502", "v743b", "sqtype"),
      definition = "Percentage of currently married women who usually decide about major household purchases alone or jointly with their husband.",
      notes = .we_note, reference = .ref_we
    ),
    new_indicator(
      "we_decide_visits", "Married women who decide on visits to family or relatives", "IR", "empowerment",
      requires = c("v502", "v743d"),
      universe = married_long, value = function(d, s) as.numeric(decides(d, "v743d")),
      api_id = "WE_DMAK_W_FAM", rounds = .dhs_rounds[-(1:3)], vars = c("v502", "v743d", "sqtype"),
      definition = "Percentage of currently married women who usually decide about visits to their family or relatives alone or jointly with their husband.",
      notes = .we_note, reference = .ref_we
    ),
    new_indicator(
      "we_decide_all3", "Married women who participate in all three decisions", "IR", "empowerment",
      requires = c("v502", "v743a", "v743b", "v743d"),
      universe = married_long,
      value = function(d, s) as.numeric(decides(d, "v743a") & decides(d, "v743b") & decides(d, "v743d")),
      api_id = "WE_DMAK_W_3DC", rounds = .dhs_rounds[-(1:3)], vars = c("v502", "v743a", "v743b", "v743d", "sqtype"),
      definition = "Percentage of currently married women who participate (alone or jointly) in decisions on their own health care, major purchases and family visits.",
      notes = .we_note, reference = .ref_we
    ),
    new_indicator(
      "we_decide_none", "Married women who participate in none of the three decisions", "IR", "empowerment",
      requires = c("v502", "v743a", "v743b", "v743d"),
      universe = married_long,
      value = function(d, s) as.numeric(!decides(d, "v743a") & !decides(d, "v743b") & !decides(d, "v743d")),
      api_id = "WE_DMAK_W_NON", rounds = .dhs_rounds[-(1:3)], vars = c("v502", "v743a", "v743b", "v743d", "sqtype"),
      definition = "Percentage of currently married women who participate in none of the three decisions.",
      notes = .we_note, reference = .ref_we
    ),
    new_indicator(
      "we_wifebeating_any", "Women who agree wife-beating is justified for at least one reason", "IR", "empowerment",
      requires = c("v012", "v744a"),
      universe = all_women_universe(long = TRUE),
      value = function(d, s) any_of_vars(d, paste0("v744", letters[1:5]), 1),
      api_id = "WE_AWBT_W_AGR", rounds = .dhs_rounds[-(1:3)], vars = c("v012", paste0("v744", letters[1:5]), "sqtype"),
      definition = "Percentage of interviewed women age 15-49 who agree that a husband is justified in hitting or beating his wife for at least one specified reason.",
      notes = "2022: long questionnaire only.", reference = .ref_we
    )
  )
}

# Blood pressure and glucose (PR biomarker subsample). No DHS API reference
# values exist for these, so they are not validated.
bp_values <- function(d, s) {
  if (identical(s, "BD2017DHS")) {
    list(sys = dv(d, "sb333aa"), dia = dv(d, "sb333ab"), meds = in_codes(dv(d, "sb318a"), 1))
  } else {
    pick <- function(a, b) ifelse(is.na(a), b, a)
    list(
      sys = pick(dv(d, "wbp24"), dv(d, "mbp24")),
      dia = pick(dv(d, "wbp25"), dv(d, "mbp25")),
      meds = in_codes(pick(dv(d, "wbp19"), dv(d, "mbp19")), 1)
    )
  }
}

glucose_values <- function(d, s) {
  if (identical(s, "BD2017DHS")) {
    g <- dv(d, "sb335b")
    list(glucose = ifelse(!is.na(g) & g < 994, g / 10, NA_real_), meds = in_codes(dv(d, "sb327a"), 1))
  } else {
    pick <- function(a, b) ifelse(is.na(a), b, a)
    g <- pick(dv(d, "sb267g"), dv(d, "sb367g"))
    list(glucose = ifelse(!is.na(g) & g < 99, g, NA_real_), meds = in_codes(pick(dv(d, "sb240"), dv(d, "sb340")), 1))
  }
}

defs_biomarkers <- function() {
  list(
    new_indicator(
      "ncd_hypertension", "Hypertension among adults age 18+ (measured or on medication)", "PR", "biomarkers",
      requires = c("hv105", "sb333aa|wbp24|mbp24"),
      universe = function(d, s) {
        bp <- bp_values(d, s)
        age <- dv(d, "hv105")
        !is.na(bp$sys) & bp$sys < 994 & !is.na(bp$dia) & bp$dia < 994 & !is.na(age) & age >= 18
      },
      value = function(d, s) {
        bp <- bp_values(d, s)
        as.numeric(bp$sys >= 140 | bp$dia >= 90 | bp$meds)
      },
      rounds = c("BD2017DHS", "BD2022DHS"),
      vars = c("hv105", "sb333aa", "sb333ab", "sb318a", "wbp24", "wbp25", "wbp19", "mbp24", "mbp25", "mbp19"),
      definition = "Percentage of measured household members age 18+ with systolic BP >= 140 mmHg or diastolic BP >= 90 mmHg (2017-18: mean of 2nd and 3rd readings; 2022: final reading) or taking medication to lower blood pressure.",
      notes = "Not validated: the DHS API has no Bangladesh biomarker indicators. Uses the household weight; use by = 'hv104' for women and men separately.",
      reference = "BDHS 2017-18 and 2022 final reports, chapter on non-communicable diseases"
    ),
    new_indicator(
      "ncd_diabetes", "Diabetes among adults age 18+ (plasma glucose >= 7.0 mmol/L or on medication)", "PR", "biomarkers",
      requires = c("hv105", "sb335b|sb267g|sb367g"),
      universe = function(d, s) {
        g <- glucose_values(d, s)
        age <- dv(d, "hv105")
        !is.na(g$glucose) & !is.na(age) & age >= 18
      },
      value = function(d, s) {
        g <- glucose_values(d, s)
        as.numeric(g$glucose >= 7 | g$meds)
      },
      rounds = c("BD2017DHS", "BD2022DHS"),
      vars = c("hv105", "sb335b", "sb327a", "sb267g", "sb367g", "sb240", "sb340"),
      definition = "Percentage of tested household members age 18+ with plasma glucose >= 7.0 mmol/L or taking medication for diabetes.",
      notes = "Not validated: the DHS API has no Bangladesh biomarker indicators. 2017-18 measured fasting plasma glucose; the 2022 protocol records fasting status separately and is not taken into account here.",
      reference = "BDHS 2017-18 and 2022 final reports, chapter on non-communicable diseases"
    )
  )
}
