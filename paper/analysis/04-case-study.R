# Case study: caesarean delivery and facility delivery, 2000-2022 — trends,
# between-round tests, wealth inequality and Kitagawa decomposition — plus the
# SDG tracker. Aggregates only, written to paper/results/.
#
#   BDHS_LOCAL_DIR=~/Documents/MU/Research Rscript paper/analysis/04-case-study.R

devtools::load_all(quiet = TRUE)
source("paper/analysis/helpers.R")
options(survey.lonely.psu = "adjust")
q <- function(x) suppressWarnings(suppressMessages(x))

files <- local_bdhs_files()
inds <- c("rh_del_csection", "rh_del_facility")
extra <- c("v190", "v106", "v149", "v025", "v024")
kr_ids <- sort(files$survey_id[files$recode == "KR" & files$survey_id >= "BD2000DHS"])
kr <- lapply(kr_ids, function(s) read_local(files, s, "KR", inds, extra))
names(kr) <- kr_ids

# 1. Trends and between-round changes.
trend <- dplyr::bind_rows(lapply(kr, function(d) q(bdhs_estimate(d, inds))))
trend <- trend[, c("survey_id", "indicator", "estimate", "se", "ci_low", "ci_high", "n_unweighted")]
utils::write.csv(trend, "paper/results/case_csection_trend.csv", row.names = FALSE)

consec <- q(do.call(bdhs_compare, c(unname(kr), list(indicators = inds))))
k04 <- kr[names(kr) >= "BD2004DHS"]
overall <- q(do.call(bdhs_compare, c(unname(k04), list(indicators = inds, rounds = "first_last"))))
cols <- c("indicator", "survey_1", "survey_2", "difference", "se", "p_value", "annual_change")
changes <- rbind(as.data.frame(consec)[, cols], as.data.frame(overall)[, cols])
utils::write.csv(changes, "paper/results/case_csection_changes.csv", row.names = FALSE)

# 2. Wealth inequality in caesarean delivery, with the official rich-poor gap.
ineq <- dplyr::bind_rows(lapply(k04, function(d) q(bdhs_inequality(d, "rh_del_csection", by = "v190"))))
ineq <- as.data.frame(ineq)[, c("survey_id", "measure", "estimate", "se", "ci_low", "ci_high")]
off <- as.data.frame(bdhs_inequality_official("rh_del_csection"))[, c("survey_id", "measure", "estimate")]
names(off)[3] <- "official"
ineq <- merge(ineq, off, by = c("survey_id", "measure"), all.x = TRUE)
utils::write.csv(ineq, "paper/results/case_csection_inequality.csv", row.names = FALSE)

# 3. Kitagawa decomposition of the change in caesarean delivery.
h <- lapply(k04, function(d) q(bdhs_harmonize(d, vars = c("mother_edu_level", "wealth_quintile"), warn = FALSE)))
dec <- list()
for (pr in list(c("BD2004DHS", "BD2022DHS"), c("BD2017DHS", "BD2022DHS"))) {
  for (by in c("mother_edu_level", "wealth_quintile")) {
    x <- as.data.frame(q(bdhs_decompose(h[[pr[1]]], h[[pr[2]]], "rh_del_csection", by = by)))
    x <- x[x$group == "All", ]
    x$from <- pr[1]; x$to <- pr[2]; x$by <- by
    dec[[length(dec) + 1L]] <- x
  }
}
dec <- do.call(rbind, dec)
keep <- intersect(c("from", "to", "by", "component", "estimate", "se", "ci_low", "ci_high", "percent"), names(dec))
utils::write.csv(dec[, keep], "paper/results/case_csection_decomposition.csv", row.names = FALSE)

# 4. SDG tracker from the official estimates (no microdata needed).
sdg <- as.data.frame(bdhs_sdg())
utils::write.csv(sdg, "paper/results/sdg.csv", row.names = FALSE)

print(trend[trend$indicator == "rh_del_csection", ])
print(ineq[ineq$measure %in% c("sii", "rii", "eci", "difference"), ])
print(dec[, keep])
print(utils::head(sdg))
