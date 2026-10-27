# Quantifies common pitfalls in BDHS analysis: for each one, the estimate a
# naive analysis gives next to the correct estimate and the official DHS value.
# Reads the author's licensed BDHS files (BDHS_LOCAL_DIR) and writes national
# or subgroup aggregates only to paper/results/pitfalls.csv.
#
#   BDHS_LOCAL_DIR=~/Documents/MU/Research Rscript paper/analysis/01-pitfalls.R

devtools::load_all(quiet = TRUE)
source("paper/analysis/helpers.R")
options(survey.lonely.psu = "adjust")

files <- local_bdhs_files()
out <- list()
add <- function(pitfall, survey, indicator, group, naive, correct, official, note) {
  out[[length(out) + 1L]] <<- data.frame(
    pitfall = pitfall, survey_id = survey, indicator = indicator, group = group,
    naive = round(naive, 2), correct = round(correct, 2), official = official, note = note
  )
}
wmean <- function(des, f) 100 * as.numeric(coef(survey::svymean(f, des, na.rm = TRUE)))

# 1. 2022 short questionnaire: women without the family planning module are
#    counted as having no unmet need.
ir22 <- read_local(files, "BD2022DHS", "IR", c("fp_unmet_total", "fp_cpr_traditional"))
correct <- est(ir22, "fp_unmet_total")
married <- as_codes(ir22$v502) == 1
un <- as_codes(ir22$v626a)
ir22$naive_unmet <- ifelse(married, as.numeric(un %in% 1:2), NA)
naive <- wmean(bdhs_design(ir22), ~naive_unmet)
add("2022 short questionnaire ignored", "BD2022DHS", "Unmet need for family planning (married women)",
    "Total", naive, correct, official("FP_NADM_W_UNT", 2022),
    "Women with the short questionnaire (sqtype = 2) have no family planning module.")

# 2. Ever-married sample treated as all women (no all-woman factor).
for (s in c("BD2017DHS", "BD2022DHS")) {
  ir <- if (s == "BD2022DHS") read_local(files, s, "IR", "ms_married_by_18") else read_local(files, s, "IR", "ms_married_by_18")
  ind <- bdhs_add_indicators(ir, "ms_married_by_18")
  naive <- wmean(bdhs_design(ind), ~ms_married_by_18)
  add("Ever-married sample treated as all women", s, "Women 20-24 married before 18", "Total",
      naive, est(ir, "ms_married_by_18"), official("MA_MBAY_W_B18", s),
      "BDHS interviews ever-married women only; all-woman factors restore the all-women denominator.")
}

# 3. Subgroup all-woman factor in the child-marriage tables (residence, 2017-18).
ir17 <- read_local(files, "BD2017DHS", "IR", "ms_married_by_18", extra = "v025")
ind <- bdhs_add_indicators(ir17, "ms_married_by_18")
ind$y <- ind$ms_married_by_18
ind$awu <- ind$awfactu / 100
des <- bdhs_design(ind)
naive_r <- survey::svyby(~y, ~v025, subset(des, !is.na(y)), survey::svyratio, denominator = ~awu)
cor_r <- est(ir17, "ms_married_by_18", by = "v025", all = TRUE)
off <- bdhs_official("MA_MBAY_W_B18", survey = 2017, characteristic = "Residence")
for (k in 1:2) {
  grp <- c("urban", "rural")[k]
  add("Subgroup all-woman factor in child-marriage tables", "BD2017DHS", "Women 20-24 married before 18",
      grp, 100 * coef(naive_r)[k], cor_r$estimate[k], off$value[k],
      "The DHS tables use the total factor (awfactt) for every subgroup.")
}

# 4. GFR without births to mothers under 15.
fd <- as.data.frame(haven::zap_labels(read_local(files, "BD2017DHS", "IR", "fe_tfr")))
naive <- utils::capture.output(g <- DHS.rates::fert(fd, Indicator = "gfr", EverMW = "Yes", AWFact = "awfactt"))
add("Births to mothers under 15 left out of the GFR", "BD2017DHS", "General fertility rate", "Total",
    g[1, "GFR"], bdhs_fertility(read_local(files, "BD2017DHS", "IR", "fe_tfr"), type = "gfr", se = FALSE)$estimate,
    official("FE_FRTR_W_GFR", 2017), "The official GFR counts all births in the period.")

# 5. Exclusive breastfeeding counting country-specific liquids (2007).
kr07 <- read_local(files, "BD2007DHS", "KR", "nt_ebf", extra = c("v413a", "v413b", "v413c", "v413d"))
u <- resolve_indicators("nt_ebf")[[1]]$universe(kr07, "BD2007DHS")
correct_y <- bdhs_add_indicators(kr07, "nt_ebf")$nt_ebf
extra <- Reduce(`|`, lapply(intersect(c("v413a", "v413b", "v413c", "v413d"), names(kr07)), function(v) {
  x <- dv(kr07, v); !is.na(x) & x >= 1 & x <= 7
}))
kr07$naive_ebf <- ifelse(u, as.numeric(correct_y == 1 & !extra), NA)
add("Country-specific liquids counted in exclusive breastfeeding", "BD2007DHS",
    "Exclusive breastfeeding under 6 months", "Total", wmean(bdhs_design(kr07), ~naive_ebf),
    est(kr07, "nt_ebf"), official("CN_IYCB_C_EXB", 2007),
    "The DHS standard definition uses the standard liquids only (not v413a-d).")

# 6. Measles vaccination without the MR vaccine (2017-18).
kr17 <- read_local(files, "BD2017DHS", "KR", "ch_vac_measles")
um <- resolve_indicators("ch_vac_measles")[[1]]$universe(kr17, "BD2017DHS")
kr17$naive_msl <- ifelse(um, as.numeric(dv(kr17, "h9") %in% 1:3), NA)
add("MR vaccine (h9a) ignored", "BD2017DHS", "Measles vaccination, children 12-23 months", "Total",
    wmean(bdhs_design(kr17), ~naive_msl), est(kr17, "ch_vac_measles"), official("CH_VACC_C_MSL", 2017),
    "Some children received the measles-rubella vaccine, recorded in h9a.")

# 7. Unweighted analysis.
kr22 <- read_local(files, "BD2022DHS", "KR", "rh_del_csection")
y <- bdhs_add_indicators(kr22, "rh_del_csection")$rh_del_csection
add("Sampling weights ignored", "BD2022DHS", "Caesarean delivery (births, last 3 years)", "Total",
    100 * mean(y, na.rm = TRUE), est(kr22, "rh_del_csection"), official("RH_DELA_C_CSC", 2022, period = "Three"),
    "Unweighted mean of the sample.")

# 8. Same division code, different division (code 5 in 2014 vs 2017-18).
for (s in c("BD2014DHS", "BD2017DHS")) {
  kr <- read_local(files, s, "KR", "rh_del_csection")
  e <- est(kr, "rh_del_csection", by = "v024", all = TRUE)
  lab <- as.character(e$v024[5])
  add("Division codes compared across rounds", s, "Caesarean delivery", paste("v024 = 5:", lab),
      NA, e$estimate[5], NA, "Code 5 is Rajshahi in 2014 but Mymensingh in 2017-18.")
}

res <- do.call(rbind, out)
dir.create("paper/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, "paper/results/pitfalls.csv", row.names = FALSE)
print(res[, c("pitfall", "survey_id", "group", "naive", "correct", "official")])
