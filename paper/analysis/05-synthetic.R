# Fidelity of calibrated synthetic BDHS data to the official estimates, for
# every round and the KR, IR and PR recodes. Uses only the bundled official
# estimates (no microdata). Writes paper/results/synthetic_fidelity*.csv.
#
#   Rscript paper/analysis/05-synthetic.R

devtools::load_all(quiet = TRUE)
rounds <- bdhs_survey_id(type = "DHS")
recodes <- c("KR", "IR", "PR")
detail <- list()
summ <- list()
for (s in rounds) {
  for (r in recodes) {
    t0 <- proc.time()[["elapsed"]]
    x <- suppressMessages(suppressWarnings(bdhs_synthesize(s, r, seed = 1)))
    secs <- proc.time()[["elapsed"]] - t0
    f <- as.data.frame(suppressMessages(suppressWarnings(bdhs_synthetic_fidelity(x))))
    f$recode <- r
    detail[[length(detail) + 1L]] <- f
    summ[[length(summ) + 1L]] <- data.frame(
      survey_id = s, recode = r, targets = length(unique(f$indicator)), margins = nrow(f),
      n = nrow(x), mean_abs_diff = round(mean(abs(f$diff), na.rm = TRUE), 3),
      max_abs_diff = round(max(abs(f$diff), na.rm = TRUE), 2), seconds = round(secs, 1)
    )
    message(s, " ", r, ": max |diff| ", round(max(abs(f$diff), na.rm = TRUE), 2))
  }
}
detail <- do.call(rbind, detail)
summ <- do.call(rbind, summ)
utils::write.csv(detail, "paper/results/synthetic_fidelity_detail.csv", row.names = FALSE)
utils::write.csv(summ, "paper/results/synthetic_fidelity.csv", row.names = FALSE)
print(summ)
