# Data-quality diagnostics for every local BDHS file (aggregates only).
#   BDHS_LOCAL_DIR=~/Documents/MU/Research Rscript paper/analysis/03-quality.R
devtools::load_all(quiet = TRUE)
source("paper/analysis/helpers.R")
files <- local_bdhs_files()
files <- files[files$recode %in% c("KR", "BR", "PR", "IR"), ]
q <- do.call(rbind, lapply(seq_len(nrow(files)), function(i) {
  d <- suppressMessages(suppressWarnings(bdhs_read_file(files$path[i])))
  out <- suppressMessages(suppressWarnings(bdhs_quality(d)))
  rm(d); gc(verbose = FALSE)
  as.data.frame(out)[, c("survey_id", "recode", "check", "statistic", "group", "value", "benchmark", "interpretation", "n")]
}))
q$value <- round(q$value, 2)
utils::write.csv(q, "paper/results/quality_full.csv", row.names = FALSE)
key <- q[(q$check == "age_heaping" & q$statistic == "whipple") |
           (q$check == "age_displacement") |
           (q$check == "age_at_death" & grepl("heaping", q$statistic)) |
           (q$check == "anthropometry" & grepl("sd", q$statistic)) |
           (q$check == "birth_date_completeness" & grepl("month_year|complete", q$statistic)), ]
utils::write.csv(key, "paper/results/quality.csv", row.names = FALSE)
print(utils::head(key, 60))

# Compact per-round summary for the manuscript.
pick <- function(s, rec, chk, stat, grp = NULL) {
  r <- q[q$survey_id == s & q$recode %in% rec & q$check == chk & q$statistic == stat, ]
  if (!is.null(grp)) r <- r[r$group %in% grp, ]
  if (nrow(r)) r$value[[1]] else NA_real_
}
rounds <- sort(unique(q$survey_id))
summ <- do.call(rbind, lapply(rounds, function(s) data.frame(
  survey_id = s,
  pct_birth_dates_complete = pick(s, "KR", "birth_date_completeness", "pct_month_and_year"),
  child_haz_sd = pick(s, c("PR", "KR"), "anthropometry", "haz_sd"),
  child_haz_pct_flagged = pick(s, c("PR", "KR"), "anthropometry", "haz_pct_flagged"),
  child_height_pct_digit_0_or_5 = pick(s, c("PR", "KR"), "anthropometry", "height_terminal_digit_pct_0_or_5"),
  whipple_men = pick(s, "PR", "age_heaping", "whipple", "male"),
  whipple_women = pick(s, "PR", "age_heaping", "whipple", "female"),
  women_ratio_50_to_49 = pick(s, "PR", "age_displacement", "ratio_50_to_49", c("female", "women")),
  heaping_12_months = pick(s, c("BR", "IR"), "age_at_death", "heaping_12_months")
)))
utils::write.csv(summ, "paper/results/quality.csv", row.names = FALSE)
print(summ)
