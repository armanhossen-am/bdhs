# Indicator definitions: maternal and child health (KR) ---------------------------
#
# Logic ported from DHS-Indicators-R (Chap09_RH/RH_ANC.R, RH_DEL.R, RH_PNC.R;
# Chap10_CH/CH_VAC.R, CH_DIAR.R, CH_ARI_FV.R) and the Guide to DHS Statistics
# (DHS-7), with Bangladesh-specific provider and source lists that were
# checked round by round against DHS Program API values.

.ref_rh <- "Guide to DHS Statistics (DHS-7), ch. 9; DHS-Indicators-R Chap09_RH (RH_ANC, RH_DEL, RH_PNC)"
.ref_ch <- "Guide to DHS Statistics (DHS-7), ch. 10; DHS-Indicators-R Chap10_CH (CH_VAC, CH_DIAR, CH_ARI_FV)"

# "Medically trained provider" letters of m2* (ANC) and m3* (delivery) per
# round, read from the KR value labels and verified against the DHS API
# (RH_ANCP_W_SKP, RH_DELA_C_SKP). 1993-94 to 2004: the API counts only
# doctors (a) and nurses/midwives (b) as skilled. 1996-97 is assumed to
# follow 1993-94 (no local file to verify).
.anc_skilled_letters <- list(
  BD1994DHS = c("a", "b"), BD1997DHS = c("a", "b"), BD2000DHS = c("a", "b"),
  BD2004DHS = c("a", "b"), BD2007DHS = c("a", "b", "d", "e", "f"),
  BD2011DHS = c("a", "b", "c", "d", "e"), BD2014DHS = c("a", "b", "c", "d", "e"),
  BD2017DHS = c("a", "b", "c", "e", "j"), BD2022DHS = c("a", "b", "c", "d", "e")
)
.del_skilled_letters <- list(
  BD1994DHS = c("a", "b"), BD1997DHS = c("a", "b"), BD2000DHS = c("a", "b"),
  BD2004DHS = c("a", "b"), BD2007DHS = c("a", "b", "d", "e", "f"),
  BD2011DHS = c("a", "b", "c", "e"), BD2014DHS = c("a", "b", "c", "d", "e"),
  BD2017DHS = c("a", "b", "c", "d", "e"), BD2022DHS = c("a", "b", "c", "d", "e")
)

# Most recent birth (midx == 1) in the last `months` months.
last_birth_universe <- function(months) {
  function(d, s) in_codes(dv(d, "midx"), 1) & child_age(d) < months
}

# All births in the last `months` months.
births_universe <- function(months) {
  function(d, s) {
    a <- child_age(d)
    !is.na(a) & a < months
  }
}

# Living children aged lo..hi months.
living_universe <- function(lo, hi) {
  function(d, s) {
    a <- child_age(d)
    in_codes(dv(d, "b5"), 1) & !is.na(a) & a >= lo & a <= hi
  }
}

.by_3y <- "Three years preceding the survey"
.by_5y <- c(BD1994DHS = "Three years preceding the survey", default = "Five years preceding the survey")

defs_maternal <- function() {
  list(
    new_indicator(
      "rh_anc_skilled", "Antenatal care from a skilled provider", "KR", "maternal_health",
      requires = c("midx", "m2a"),
      universe = last_birth_universe(36),
      value = function(d, s) any_of_vars(d, paste0("m2", .anc_skilled_letters[[s]]), 1),
      api_id = "RH_ANCP_W_SKP", api_by = .by_3y, window = 36L,
      vars = c("midx", "b19", "b3", "v008", paste0("m2", letters[1:24])),
      definition = "Percentage of women with a live birth in the 3 years before the survey who received antenatal care for the most recent birth from a medically trained provider.",
      notes = "Bangladesh definition of medically trained provider; the m2* letters differ by round (doctor and nurse/midwife only up to 2004; FWV, CSBA, SACMO etc. added later).",
      reference = .ref_rh
    ),
    new_indicator(
      "rh_anc_doctor", "Antenatal care from a doctor", "KR", "maternal_health",
      requires = c("midx", "m2a"),
      universe = last_birth_universe(36),
      value = function(d, s) any_of_vars(d, "m2a", 1),
      api_id = "RH_ANCP_W_DOC", api_by = .by_3y, window = 36L,
      vars = c("midx", "b19", "b3", "v008", "m2a"),
      definition = "Percentage of women with a live birth in the 3 years before the survey who received antenatal care for the most recent birth from a qualified doctor (m2a).",
      reference = .ref_rh
    ),
    new_indicator(
      "rh_anc_4plus", "Four or more antenatal care visits", "KR", "maternal_health",
      requires = c("midx", "m14"),
      universe = last_birth_universe(36),
      value = function(d, s) {
        m14 <- dv(d, "m14")
        ifelse(is.na(m14), NA_real_, as.numeric(m14 >= 4 & m14 <= 90))
      },
      api_id = "RH_ANCN_W_N4P", api_by = .by_3y, window = 36L,
      vars = c("midx", "b19", "b3", "v008", "m14"),
      definition = "Percentage of women with a live birth in the 3 years before the survey who had 4+ antenatal visits for the most recent birth (m14 in 4-90; don't know counted as no).",
      reference = .ref_rh
    ),
    new_indicator(
      "rh_anc_first_trimester", "First antenatal visit before the fourth month of pregnancy", "KR", "maternal_health",
      requires = c("midx", "m13"),
      universe = last_birth_universe(36),
      value = function(d, s) as.numeric(in_codes(dv(d, "m13"), 0:3)),
      api_id = "RH_ANCT_W_TL4", api_by = .by_3y, window = 36L,
      rounds = c("BD1994DHS", "BD1997DHS", "BD2000DHS", "BD2004DHS", "BD2007DHS", "BD2017DHS", "BD2022DHS"),
      vars = c("midx", "b19", "b3", "v008", "m13"),
      definition = "Percentage of women with a live birth in the 3 years before the survey whose first antenatal visit for the most recent birth was before the fourth month of pregnancy (m13 < 4); women without antenatal care count as no.",
      notes = "Timing of the first visit (m13) was not published for 2011 and 2014.",
      reference = .ref_rh
    ),
    new_indicator(
      "rh_del_facility", "Delivery in a health facility", "KR", "maternal_health",
      requires = c("m15"),
      universe = births_universe(36),
      value = function(d, s) {
        m15 <- dv(d, "m15")
        ifelse(is.na(m15), NA_real_, as.numeric(m15 >= 20 & m15 < 96))
      },
      api_id = "RH_DELP_C_DHF", api_by = .by_3y, window = 36L,
      vars = c("b19", "b3", "v008", "m15"),
      definition = "Percentage of live births in the 3 years before the survey delivered in a health facility (m15 20-95: public, private and NGO facilities).",
      notes = "Bangladesh codes NGO facilities as 4x; the generic DHS rule (codes 20-39) undercounts facility births by up to 3.6 points.",
      reference = .ref_rh
    ),
    new_indicator(
      "rh_del_home", "Delivery at home", "KR", "maternal_health",
      requires = c("m15"),
      universe = births_universe(36),
      value = function(d, s) {
        m15 <- dv(d, "m15")
        ifelse(is.na(m15), NA_real_, as.numeric(m15 >= 10 & m15 < 20))
      },
      api_id = "RH_DELP_C_HOM", api_by = .by_3y, window = 36L,
      vars = c("b19", "b3", "v008", "m15"),
      definition = "Percentage of live births in the 3 years before the survey delivered at the respondent's or another home (m15 10-19).",
      reference = .ref_rh
    ),
    new_indicator(
      "rh_del_skilled", "Delivery assisted by a skilled provider", "KR", "maternal_health",
      requires = c("m3a"),
      universe = births_universe(36),
      value = function(d, s) any_of_vars(d, paste0("m3", .del_skilled_letters[[s]]), 1),
      api_id = "RH_DELA_C_SKP", api_by = .by_3y, window = 36L,
      vars = c("b19", "b3", "v008", paste0("m3", letters[1:24])),
      definition = "Percentage of live births in the 3 years before the survey assisted by a medically trained provider (SDG 3.1.2).",
      notes = "Bangladesh definition of medically trained provider; the m3* letters differ by round (doctor and nurse/midwife only up to 2004).",
      reference = .ref_rh
    ),
    new_indicator(
      "rh_del_doctor", "Delivery assisted by a doctor", "KR", "maternal_health",
      requires = c("m3a"),
      universe = births_universe(36),
      value = function(d, s) any_of_vars(d, "m3a", 1),
      api_id = "RH_DELA_C_DOC", api_by = .by_3y, window = 36L,
      vars = c("b19", "b3", "v008", "m3a"),
      definition = "Percentage of live births in the 3 years before the survey assisted by a qualified doctor (m3a).",
      reference = .ref_rh
    ),
    new_indicator(
      "rh_del_csection", "Delivery by caesarean section", "KR", "maternal_health",
      requires = c("m17"),
      universe = births_universe(36),
      value = function(d, s) as.numeric(in_codes(dv(d, "m17"), 1)),
      api_id = "RH_DELA_C_CSC", api_by = .by_3y, window = 36L,
      rounds = setdiff(.dhs_rounds, "BD1994DHS"),
      vars = c("b19", "b3", "v008", "m17"),
      definition = "Percentage of live births in the 3 years before the survey delivered by caesarean section (m17 == 1).",
      notes = "Not collected in 1993-94.",
      reference = .ref_rh
    ),
    new_indicator(
      "rh_pnc_mother_2days", "Postnatal check of the mother within two days of birth", "KR", "maternal_health",
      requires = c("midx", "m62|m50|m66"),
      universe = last_birth_universe(24),
      value = pnc_mother_2days,
      api_id = c("RH_PCMT_W_L4H", "RH_PCMT_W_23H", "RH_PCMT_W_D12"), window = 24L,
      rounds = c("BD2007DHS", "BD2011DHS", "BD2014DHS", "BD2017DHS", "BD2022DHS"),
      vars = c("midx", "b19", "b3", "v008", "m50", "m51", "m51a", "m52", "m62", "m63", "m64", "m66", "m67", "m68"),
      definition = "Percentage of women with a live birth in the 2 years before the survey whose first postnatal check for the most recent birth took place within two days of delivery.",
      notes = paste(
        "Round-specific, as in the published tables: 2007 any check after delivery (m66, m51a);",
        "2011-2014 any provider (m50, m51, m52); 2017-18 and 2022 DHS-7 algorithm with checks before discharge (m62-m64)",
        "or after discharge/at home (m66-m68) by health personnel only (codes 11-18).",
        "Official value = sum of the <4 hours, 4-23 hours and 1-2 days categories."
      ),
      reference = .ref_rh
    )
  )
}

# Timing codes: hours 1xx, days 2xx, weeks 3xx; within two days = 100-171 or 200-202.
within_two_days <- function(t) !is.na(t) & ((t >= 100 & t <= 171) | (t >= 200 & t <= 202))

# Mother's first postnatal check within two days of birth (1/0).
pnc_mother_2days <- function(d, s) {
  if (identical(s, "BD2007DHS")) {
    return(as.numeric(in_codes(dv(d, "m66"), 1) & within_two_days(dv(d, "m51a"))))
  }
  if (s %in% c("BD2011DHS", "BD2014DHS")) {
    return(as.numeric(in_codes(dv(d, "m50"), 1) & in_codes(dv(d, "m52"), 11:31) & within_two_days(dv(d, "m51"))))
  }
  # DHS-7 algorithm (DHS-Indicators-R RH_PNC.R), Bangladesh health personnel codes.
  health_codes <- 11:18
  m64 <- dv(d, "m64")
  m67 <- dv(d, "m67")
  m68 <- dv(d, "m68")
  check <- in_codes(dv(d, "m62"), 1) | in_codes(dv(d, "m66"), 1)
  health <- function(p) in_codes(p, health_codes)
  other <- function(p) !is.na(p) & !p %in% health_codes & p < 99
  time <- ifelse(health(m64), dv(d, "m63"), ifelse(check, 999, NA_real_))
  time[!is.na(time) & time < 1000 & other(m64)] <- 0
  after <- !is.na(time) & time == 999 & health(m68)
  time[after] <- m67[after]
  time[!is.na(time) & time == 999 & !is.na(m67) & m67 < 1000 & other(m68)] <- 0
  time[!check] <- 0
  as.numeric(within_two_days(time))
}

# Vaccination: received according to card or mother (codes 1-3).
vacc <- function(d, var) in_codes(dv(d, var), 1:3)

vacc_counts <- function(d) {
  list(
    bcg = vacc(d, "h2"),
    dpt1 = vacc(d, "h3"),
    polio1 = vacc(d, "h4"),
    dpt = vacc(d, "h3") + vacc(d, "h5") + vacc(d, "h7"),
    polio = vacc(d, "h4") + vacc(d, "h6") + vacc(d, "h8"),
    measles = vacc(d, "h9") | vacc(d, "h9a"),
    polio0 = vacc(d, "h0")
  )
}

.vacc_rounds <- c("BD1994DHS", "BD1997DHS", "BD2000DHS", "BD2004DHS", "BD2007DHS", "BD2011DHS", "BD2014DHS", "BD2017DHS")
.vacc_note <- "Children 12-23 months, card or mother's report. The 2022 recode files contain no vaccination data (all h-variables empty)."

vacc_indicator <- function(id, label, api_id, fun, definition, requires) {
  new_indicator(
    id, label, "KR", "child_health",
    universe = living_universe(12, 23),
    value = function(d, s) as.numeric(fun(vacc_counts(d))),
    api_id = api_id, rounds = .vacc_rounds, requires = c("b5", requires),
    vars = c("b5", "b19", "b3", "v008", "h0", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h9a"),
    definition = definition, notes = .vacc_note, reference = .ref_ch
  )
}

# Care seeking: any source except those excluded in the round's DHS tables
# (1993-94 to 2007: traditional practitioner; 2022: "other"), verified
# against the DHS API.
care_sources <- function(d, s, prefix) {
  excl <- switch(s,
    BD1994DHS = , BD1997DHS = , BD2000DHS = , BD2004DHS = , BD2007DHS = "t",
    BD2022DHS = "x",
    character()
  )
  setdiff(letter_vars(d, prefix), paste0(prefix, excl))
}

ari_symptoms <- function(d, s) {
  h31 <- dv(d, "h31")
  h31b <- dv(d, "h31b")
  h31c <- dv(d, "h31c")
  out <- switch(s,
    BD1994DHS = , BD1997DHS = , BD2000DHS = , BD2004DHS = in_codes(h31, 1:2) & in_codes(h31b, 1),
    BD2007DHS = , BD2011DHS = , BD2014DHS = in_codes(h31, 1:2) & in_codes(h31b, 1) & in_codes(h31c, c(1, 3)),
    in_codes(h31b, 1) & in_codes(h31c, c(1, 3))
  )
  as.numeric(out)
}

diarrhoea <- function(d) in_codes(dv(d, "h11"), 1:2)

defs_child_health <- function() {
  under5 <- living_universe(0, 59)
  list(
    vacc_indicator("ch_vac_bcg", "BCG vaccination (children 12-23 months)", "CH_VACC_C_BCG",
      function(v) v$bcg, "Percentage of living children 12-23 months who received BCG.", "h2"),
    vacc_indicator("ch_vac_dpt1", "DPT/pentavalent 1 vaccination (children 12-23 months)", "CH_VACC_C_DP1",
      function(v) v$dpt1, "Percentage of living children 12-23 months who received the first dose of DPT/pentavalent (h3).", "h3"),
    vacc_indicator("ch_vac_dpt3", "DPT/pentavalent 3 vaccination (children 12-23 months)", "CH_VACC_C_DP3",
      function(v) v$dpt >= 3, "Percentage of living children 12-23 months who received three doses of DPT/pentavalent.", c("h3", "h5", "h7")),
    vacc_indicator("ch_vac_polio1", "Polio 1 vaccination (children 12-23 months)", "CH_VACC_C_OP1",
      function(v) v$polio1, "Percentage of living children 12-23 months who received the first dose of polio vaccine (h4, excluding polio 0).", "h4"),
    vacc_indicator("ch_vac_polio3", "Polio 3 vaccination (children 12-23 months)", "CH_VACC_C_OP3",
      function(v) v$polio >= 3, "Percentage of living children 12-23 months who received three doses of polio vaccine (excluding polio 0).", c("h4", "h6", "h8")),
    vacc_indicator("ch_vac_measles", "Measles/MR vaccination (children 12-23 months)", "CH_VACC_C_MSL",
      function(v) v$measles, "Percentage of living children 12-23 months who received measles or MR vaccine (h9, h9a).", "h9"),
    vacc_indicator("ch_vac_basic", "All basic vaccinations (children 12-23 months)", "CH_VACC_C_BAS",
      function(v) v$bcg & v$dpt >= 3 & v$polio >= 3 & v$measles,
      "Percentage of living children 12-23 months who received BCG, three doses of DPT and polio, and measles.", c("h2", "h3", "h4", "h9")),
    vacc_indicator("ch_vac_none", "No vaccinations (children 12-23 months)", "CH_VACC_C_NON",
      function(v) !v$bcg & v$dpt == 0 & v$polio == 0 & !v$measles & !v$polio0,
      "Percentage of living children 12-23 months who received no vaccination.", c("h2", "h3", "h4", "h9")),
    new_indicator(
      "ch_diarrhoea", "Diarrhoea in the two weeks before the survey", "KR", "child_health",
      requires = c("b5", "h11"),
      universe = under5, value = function(d, s) as.numeric(diarrhoea(d)),
      api_id = "CH_DIAR_C_DIA", api_by = .by_5y, window = 60L,
      vars = c("b5", "b19", "b3", "v008", "h11"),
      definition = "Percentage of living children under 5 who had diarrhoea in the two weeks before the survey (h11 in 1, 2).",
      notes = "1993-94 KR covers births in the last 3 years only.", reference = .ref_ch
    ),
    new_indicator(
      "ch_diarrhoea_ors", "ORS for children with diarrhoea", "KR", "child_health",
      requires = c("b5", "h11", "h13|h13b"),
      universe = function(d, s) under5(d, s) & diarrhoea(d),
      value = function(d, s) as.numeric(in_codes(dv(d, "h13"), 1:2) | in_codes(dv(d, "h13b"), 1)),
      api_id = "CH_DIAT_C_ORS", api_by = .by_5y, window = 60L,
      vars = c("b5", "b19", "b3", "v008", "h11", "h13", "h13b"),
      definition = "Among living children under 5 with diarrhoea in the last two weeks, the percentage given oral rehydration salts (fluid from ORS packet or pre-packaged ORS).",
      reference = .ref_ch
    ),
    new_indicator(
      "ch_diarrhoea_zinc", "Zinc for children with diarrhoea", "KR", "child_health",
      requires = c("b5", "h11", "h15e|h15j"),
      universe = function(d, s) under5(d, s) & diarrhoea(d),
      value = function(d, s) {
        if (identical(s, "BD2014DHS")) any_of_vars(d, c("h15j", "h15k"), 1) else any_of_vars(d, "h15e", 1)
      },
      api_id = "CH_DIAT_C_ZNC", api_by = .by_5y, window = 60L,
      rounds = c("BD2014DHS", "BD2017DHS", "BD2022DHS"),
      vars = c("b5", "b19", "b3", "v008", "h11", "h15e", "h15j", "h15k"),
      definition = "Among living children under 5 with diarrhoea in the last two weeks, the percentage given zinc.",
      notes = "2014 records zinc in h15j/h15k (tablet/syrup), later rounds in h15e.", reference = .ref_ch
    ),
    new_indicator(
      "ch_diarrhoea_care", "Advice or treatment sought for diarrhoea", "KR", "child_health",
      requires = c("b5", "h11", "h12a"),
      universe = function(d, s) under5(d, s) & diarrhoea(d),
      value = function(d, s) any_of_vars(d, care_sources(d, s, "h12"), 1),
      api_id = "CH_DIAT_C_ADV", api_by = .by_5y, window = 60L,
      vars = c("b5", "b19", "b3", "v008", "h11", paste0("h12", letters[1:24])),
      definition = "Among living children under 5 with diarrhoea, the percentage for whom advice or treatment was sought from a health facility or provider.",
      notes = "Sources follow the DHS tables of each round (traditional practitioners excluded up to 2007, 'other' excluded in 2022).",
      reference = .ref_ch
    ),
    new_indicator(
      "ch_ari", "Symptoms of acute respiratory infection", "KR", "child_health",
      requires = c("b5", "h31b"),
      universe = under5, value = ari_symptoms,
      api_id = "CH_ARIS_C_ARI", api_by = .by_5y, window = 60L,
      vars = c("b5", "b19", "b3", "v008", "h31", "h31b", "h31c"),
      definition = "Percentage of living children under 5 with symptoms of ARI (cough with short, rapid breathing that was chest-related) in the two weeks before the survey.",
      notes = "Definition break: the chest-related condition (h31c) was added in 2007, explaining the drop from 19% to 5%; from 2017 rapid breathing is asked of all children.",
      reference = .ref_ch
    ),
    new_indicator(
      "ch_ari_care", "Advice or treatment sought for ARI", "KR", "child_health",
      requires = c("b5", "h31b", "h32a"),
      universe = function(d, s) under5(d, s) & ari_symptoms(d, s) == 1,
      value = function(d, s) any_of_vars(d, care_sources(d, s, "h32"), 1),
      api_id = "CH_ARIS_C_ADV", api_by = .by_5y, window = 60L,
      vars = c("b5", "b19", "b3", "v008", "h31", "h31b", "h31c", paste0("h32", letters[1:24])),
      definition = "Among living children under 5 with ARI symptoms, the percentage for whom advice or treatment was sought from a health facility or provider.",
      reference = .ref_ch
    ),
    new_indicator(
      "ch_fever", "Fever in the two weeks before the survey", "KR", "child_health",
      requires = c("b5", "h22"),
      universe = under5, value = function(d, s) as.numeric(in_codes(dv(d, "h22"), 1)),
      api_id = "CH_FEVR_C_FEV", api_by = .by_5y, window = 60L,
      rounds = setdiff(.dhs_rounds, "BD1994DHS"),
      vars = c("b5", "b19", "b3", "v008", "h22"),
      definition = "Percentage of living children under 5 who had fever in the two weeks before the survey.",
      notes = "Not collected in 1993-94.", reference = .ref_ch
    ),
    new_indicator(
      "ch_fever_care", "Advice or treatment sought for fever", "KR", "child_health",
      requires = c("b5", "h22", "h32a"),
      universe = function(d, s) under5(d, s) & in_codes(dv(d, "h22"), 1),
      value = function(d, s) any_of_vars(d, care_sources(d, s, "h32"), 1),
      api_id = "CH_FEVT_C_ADV", api_by = .by_5y, window = 60L,
      rounds = setdiff(.dhs_rounds, "BD1994DHS"),
      vars = c("b5", "b19", "b3", "v008", "h22", paste0("h32", letters[1:24])),
      definition = "Among living children under 5 with fever, the percentage for whom advice or treatment was sought from a health facility or provider.",
      reference = .ref_ch
    )
  )
}
