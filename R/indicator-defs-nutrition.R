# Indicator definitions: nutrition (PR, IR, KR) -----------------------------------
#
# Logic ported from DHS-Indicators-R (Chap11_NT/NT_CH_NUT.R, NT_WM_NUT.R,
# NT_BRST_FED.R, NT_IYCF.R) and the Guide to DHS Statistics (DHS-7, ch. 11).

.ref_nt <- "Guide to DHS Statistics (DHS-7), ch. 11; DHS-Indicators-R Chap11_NT (NT_CH_NUT, NT_WM_NUT, NT_BRST_FED)"

# De facto children with a valid z-score (flagged values 9996-9999 excluded).
zscore_universe <- function(var) {
  function(d, s) {
    z <- dv(d, var)
    ok <- in_codes(dv(d, "hv103"), 1) & !is.na(z) & z < 9996
    if (!is.null(d[["hc1"]])) {
      age <- dv(d, "hc1")
      ok <- ok & (is.na(age) | age < 60)
    }
    ok
  }
}

zscore_indicator <- function(id, label, var, api_id, below = NULL, above = NULL, definition) {
  new_indicator(
    id, label, "PR", "nutrition",
    universe = zscore_universe(var),
    value = function(d, s) {
      z <- dv(d, var)
      if (!is.null(below)) as.numeric(z < below) else as.numeric(z > above)
    },
    api_id = api_id, rounds = .dhs_rounds[-1], requires = c("hv103", var),
    vars = c("hv103", "hc1", var),
    definition = definition,
    notes = paste(
      "WHO 2006 growth standards, de facto children under 5 (PR hc70-hc72).",
      "Older recodes (1996-97 to 2004) keep the WHO z-scores in separate HW files, which must be merged into PR first.",
      "2022: children were measured in the biomarker subsample of households."
    ),
    reference = .ref_nt
  )
}

# Women's anthropometry from PR (ha3/ha40, all measured women 15-49) or IR
# (v438/v445, interviewed ever-married women). Pregnant women and, in IR,
# women with a birth in the last 2 months are excluded from BMI.
is_pr <- function(d) !is.null(d[["hvidx"]]) || !is.null(d[["ha40"]])

wm_bmi <- function(d) if (is_pr(d)) dv(d, "ha40") else dv(d, "v445")
wm_height <- function(d) if (is_pr(d)) dv(d, "ha3") else dv(d, "v438")

wm_bmi_universe <- function(d, s) {
  bmi <- wm_bmi(d)
  ok <- !is.na(bmi) & bmi >= 1200 & bmi <= 6000
  if (is_pr(d)) {
    age <- dv(d, "hv105")
    ok & in_codes(dv(d, "hv104"), 2) & in_codes(dv(d, "hv103"), 1) & !is.na(age) &
      age >= 15 & age <= 49 & !in_codes(dv(d, "ha54"), 1)
  } else {
    last_birth <- dv(d, "b19_01")
    if (all(is.na(last_birth))) last_birth <- dv(d, "v008") - dv(d, "b3_01")
    ok & !in_codes(dv(d, "v213"), 1) & (is.na(last_birth) | last_birth >= 2)
  }
}

wm_height_universe <- function(d, s) {
  h <- wm_height(d)
  ok <- !is.na(h) & h >= 1300 & h <= 2200
  if (is_pr(d)) {
    age <- dv(d, "hv105")
    ok <- ok & in_codes(dv(d, "hv104"), 2) & in_codes(dv(d, "hv103"), 1) & !is.na(age) & age >= 15 & age <= 49
  }
  ok
}

.wm_note <- paste(
  "Computed from PR (ha3, ha40: all measured women, as in the DHS tables) or IR (v438, v445: interviewed ever-married women).",
  "2022 measured all women 15-49 in the biomarker subsample, so only PR reproduces the official 2022 values.",
  "PR has no date of the last birth, so women who gave birth in the last 2 months cannot be excluded there."
)

wm_bmi_indicator <- function(id, label, api_id, fun, definition, type = "proportion") {
  new_indicator(
    id, label, c("PR", "IR"), "nutrition",
    universe = wm_bmi_universe,
    value = function(d, s) fun(wm_bmi(d)),
    type = type, api_id = api_id, rounds = .dhs_rounds[-1], requires = "ha40|v445",
    vars = c("hv103", "hv104", "hv105", "ha40", "ha54", "v445", "v213", "b19_01", "b3_01", "v008"),
    definition = definition, notes = .wm_note, reference = .ref_nt
  )
}

# Youngest child under 2 living with the mother (KR).
youngest_with_mother <- function(d) {
  age <- child_age(d)
  cand <- which(in_codes(dv(d, "b9"), 0) & !is.na(age) & age < 24)
  out <- rep(FALSE, nrow(d))
  if (!length(cand)) {
    return(out)
  }
  key <- as.character(d$caseid[cand])
  ord <- order(key, dv(d, "bidx")[cand])
  first <- cand[ord][!duplicated(key[ord])]
  out[first] <- TRUE
  out
}

# Exclusively breastfed: currently breastfeeding and given no liquids or
# solids in the last 24 hours (DHS-8 adds "don't know" = 8 as not exclusive).
ebf_value <- function(d, s) {
  cur <- in_codes(dv(d, "m4"), 95)
  liquids <- intersect(
    c("v409", "v409a", "v410", "v410a", "v411", "v411a", "v412", "v412a", "v412b", "v412c",
      "v413", "v413a", "v413b", "v413c", "v413d"),
    names(d)
  )
  solids <- c(letter_vars(d, "v414", letters[1:23]), intersect("m39a", names(d)))
  hi <- if (identical(s, "BD2022DHS")) 8 else 7
  given <- rep(FALSE, nrow(d))
  for (v in c(liquids, solids)) {
    x <- dv(d, v)
    given <- given | (!is.na(x) & x >= 1 & x <= hi)
  }
  as.numeric(cur & !given)
}

defs_nutrition <- function() {
  list(
    zscore_indicator("nt_stunting", "Stunting (height-for-age < -2 SD), children under 5", "hc70", "CN_NUTS_C_HA2",
      below = -200, definition = "Percentage of de facto children under 5 with height-for-age z-score below -2 SD."),
    zscore_indicator("nt_stunting_severe", "Severe stunting (height-for-age < -3 SD)", "hc70", "CN_NUTS_C_HA3",
      below = -300, definition = "Percentage of de facto children under 5 with height-for-age z-score below -3 SD."),
    zscore_indicator("nt_wasting", "Wasting (weight-for-height < -2 SD), children under 5", "hc72", "CN_NUTS_C_WH2",
      below = -200, definition = "Percentage of de facto children under 5 with weight-for-height z-score below -2 SD."),
    zscore_indicator("nt_wasting_severe", "Severe wasting (weight-for-height < -3 SD)", "hc72", "CN_NUTS_C_WH3",
      below = -300, definition = "Percentage of de facto children under 5 with weight-for-height z-score below -3 SD."),
    zscore_indicator("nt_overweight", "Overweight (weight-for-height > +2 SD), children under 5", "hc72", "CN_NUTS_C_WHP",
      above = 200, definition = "Percentage of de facto children under 5 with weight-for-height z-score above +2 SD."),
    zscore_indicator("nt_underweight", "Underweight (weight-for-age < -2 SD), children under 5", "hc71", "CN_NUTS_C_WA2",
      below = -200, definition = "Percentage of de facto children under 5 with weight-for-age z-score below -2 SD."),
    zscore_indicator("nt_underweight_severe", "Severe underweight (weight-for-age < -3 SD)", "hc71", "CN_NUTS_C_WA3",
      below = -300, definition = "Percentage of de facto children under 5 with weight-for-age z-score below -3 SD."),
    wm_bmi_indicator("nt_wm_thin", "Women who are thin (BMI < 18.5)", "AN_NUTS_W_THN",
      function(b) as.numeric(b < 1850), "Percentage of non-pregnant women age 15-49 with BMI below 18.5."),
    wm_bmi_indicator("nt_wm_overweight_obese", "Women who are overweight or obese (BMI >= 25)", "AN_NUTS_W_OWT",
      function(b) as.numeric(b >= 2500), "Percentage of non-pregnant women age 15-49 with BMI of 25 or more."),
    wm_bmi_indicator("nt_wm_obese", "Women who are obese (BMI >= 30)", "AN_NUTS_W_OBS",
      function(b) as.numeric(b >= 3000), "Percentage of non-pregnant women age 15-49 with BMI of 30 or more."),
    wm_bmi_indicator("nt_wm_bmi_mean", "Mean body mass index of women", "AN_NUTS_W_BMI",
      function(b) b / 100, "Mean BMI (kg/m2) of non-pregnant women age 15-49.", type = "mean"),
    new_indicator(
      "nt_wm_short", "Women shorter than 145 cm", c("PR", "IR"), "nutrition",
      requires = c("ha3|v438"),
      universe = wm_height_universe,
      value = function(d, s) as.numeric(wm_height(d) < 1450),
      api_id = "AN_NUTS_W_SHT", rounds = .dhs_rounds[-1],
      vars = c("hv103", "hv104", "hv105", "ha3", "v438"),
      definition = "Percentage of women age 15-49 with height below 145 cm.",
      notes = .wm_note, reference = .ref_nt
    ),
    new_indicator(
      "nt_bf_ever", "Children ever breastfed", "KR", "nutrition",
      requires = c("midx", "m4"),
      universe = last_birth_universe(24),
      value = function(d, s) {
        m4 <- dv(d, "m4")
        ifelse(is.na(m4) | m4 == 99, NA_real_, as.numeric(m4 != 94))
      },
      api_id = "CN_BRFI_C_EVR", window = 24L,
      vars = c("midx", "b19", "b3", "v008", "m4"),
      definition = "Percentage of last-born children born in the 2 years before the survey who were ever breastfed.",
      reference = .ref_nt
    ),
    new_indicator(
      "nt_bf_early", "Breastfeeding started within one hour of birth", "KR", "nutrition",
      requires = c("midx", "m34"),
      universe = last_birth_universe(24),
      value = function(d, s) as.numeric(in_codes(dv(d, "m34"), c(0, 100))),
      api_id = "CN_BRFI_C_1HR", window = 24L,
      vars = c("midx", "b19", "b3", "v008", "m34"),
      definition = "Percentage of last-born children born in the 2 years before the survey who were put to the breast within one hour of birth (m34 0 or 100); never-breastfed children count as no.",
      reference = .ref_nt
    ),
    new_indicator(
      "nt_ebf", "Exclusive breastfeeding, children under 6 months", "KR", "nutrition",
      requires = c("caseid", "bidx", "b9", "m4"),
      universe = function(d, s) {
        a <- child_age(d)
        youngest_with_mother(d) & !is.na(a) & a < 6
      },
      value = ebf_value,
      api_id = "CN_IYCB_C_EXB", window = 6L, rounds = setdiff(.dhs_rounds, "BD2004DHS"),
      vars = c("caseid", "bidx", "b9", "b19", "b3", "v008", "m4", "m39a", "v409", "v409a", "v410", "v410a",
               "v411", "v411a", "v412", "v412a", "v412b", "v412c", "v413", "v413a", "v413b", "v413c", "v413d",
               paste0("v414", letters[1:23])),
      definition = "Percentage of youngest children under 6 months living with their mother who were exclusively breastfed (24-hour recall).",
      notes = "The 2004 KR lacks the 24-hour feeding recall variables; the 2007 value differs from the published one (unresolved).",
      reference = .ref_nt
    )
  )
}
