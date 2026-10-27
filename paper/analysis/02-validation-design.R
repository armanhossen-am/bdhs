# Validation summary (from the record shipped with bdhs) and design effects:
# how much a simple-random-sample analysis understates standard errors.
# Writes aggregates only to paper/results/.
#
#   BDHS_LOCAL_DIR=~/Documents/MU/Research Rscript paper/analysis/02-validation-design.R

devtools::load_all(quiet = TRUE)
source("paper/analysis/helpers.R")
options(survey.lonely.psu = "adjust")
dir.create("paper/results", showWarnings = FALSE, recursive = TRUE)

# Validation: every computed national value against the official one.
rec <- bdhs_validation_record()
reg <- bdhs_indicator_list()
rec$topic <- reg$topic[match(rec$indicator, reg$id)]
rec$abs_diff <- abs(rec$computed - rec$official)
utils::write.csv(rec, "paper/results/validation.csv", row.names = FALSE)
by_round <- do.call(rbind, lapply(split(rec, rec$survey_id), function(d) data.frame(
  survey_id = d$survey_id[1], recodes = paste(sort(unique(d$recode)), collapse = ", "),
  n = nrow(d), passed = sum(d$pass), median_abs_diff = round(stats::median(d$abs_diff), 3),
  max_abs_diff_passed = round(max(d$abs_diff[d$pass]), 2)
)))
utils::write.csv(by_round, "paper/results/validation_by_round.csv", row.names = FALSE)
print(by_round)

# Design effects for headline indicators: design-based SE vs the SE a
# simple random sample of the same size would give.
files <- local_bdhs_files()
targets <- list(
  list("BD2022DHS", "KR", "rh_del_csection"), list("BD2022DHS", "KR", "rh_del_facility"),
  list("BD2022DHS", "KR", "rh_anc_4plus"), list("BD2022DHS", "PR", "nt_stunting"),
  list("BD2022DHS", "IR", "fp_cpr_modern"), list("BD2017DHS", "KR", "ch_vac_basic"),
  list("BD2017DHS", "PR", "nt_stunting"), list("BD2017DHS", "IR", "fp_cpr_modern")
)
deff <- do.call(rbind, lapply(targets, function(t) {
  d <- read_local(files, t[[1]], t[[2]], t[[3]])
  e <- est(d, t[[3]], all = TRUE)
  y <- bdhs_add_indicators(d, t[[3]])[[t[[3]]]]
  n <- sum(!is.na(y))
  p <- e$estimate / 100
  se_srs <- 100 * sqrt(p * (1 - p) / n)
  data.frame(survey_id = t[[1]], indicator = t[[3]], estimate = round(e$estimate, 1),
             se_design = round(e$se, 2), se_srs = round(se_srs, 2),
             deff = round((e$se / se_srs)^2, 2), n = n)
}))
utils::write.csv(deff, "paper/results/design_effects.csv", row.names = FALSE)
print(deff)
