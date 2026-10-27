test_that("grouped inequality measures match hand computations", {
  m <- grouped_inequality(c(10, 20, 30, 40, 50), rep(1, 5))
  # ridits 0.1, 0.3, ..., 0.9; mean 30; cov(y, R) = 4; var(R) = 0.08
  expect_equal(m[["difference"]], 40)
  expect_equal(m[["ratio"]], 5)
  expect_equal(m[["sii"]], 50)
  expect_equal(m[["ci"]], 2 * 0.2 * (1 + 6 + 15 + 28 + 45) / 30 - 1)
  expect_equal(m[["eci"]], 8 * 4 / 100)
  expect_equal(m[["eci"]], 4 * 0.3 * m[["ci"]])
  # Equal values: no inequality
  flat <- grouped_inequality(rep(25, 5), c(3, 1, 2, 5, 4))
  expect_equal(unname(flat[c("difference", "sii", "ci", "eci")]), c(0, 0, 0, 0))
  expect_equal(unname(flat[c("ratio", "rii")]), c(1, 1))
})

test_that("microdata measures equal the grouped formulas on the same data", {
  kr <- bdhs_simulate(2022, "KR", n_clusters = 40)
  ineq <- bdhs_inequality(kr, "rh_del_csection")
  expect_s3_class(ineq, "bdhs_inequality")
  expect_equal(ineq$measure, c("difference", "ratio", "sii", "rii", "ci", "eci"))
  est <- bdhs_estimate(kr, "rh_del_csection", by = "v190")
  est <- est[order(as.integer(est$v190)), ]
  g <- grouped_inequality(est$estimate, est$n_weighted)
  for (m in names(g)) {
    expect_equal(ineq$estimate[ineq$measure == m], g[[m]], tolerance = 1e-6, info = m)
  }
  expect_true(all(ineq$se > 0))
  expect_true(all(ineq$ci_low < ineq$estimate & ineq$estimate < ineq$ci_high))
  expect_equal(ineq$lowest[1], as.character(est$v190[1]))
})

test_that("difference SE uses the joint covariance", {
  kr <- bdhs_simulate(2022, "KR", n_clusters = 40)
  ineq <- bdhs_inequality(kr, "rh_del_facility", measures = "difference")
  cmp <- bdhs_compare(kr, indicators = "rh_del_facility", by = "v190")
  g <- cmp[cmp$comparison == "groups" & cmp$group_2 == ineq$highest, ]
  expect_equal(ineq$estimate, g$difference)
  expect_equal(ineq$se, g$se)
})

test_that("harmonized wealth quintile is used when present", {
  kr <- bdhs_simulate(2022, "KR", n_clusters = 40)
  h <- suppressMessages(bdhs_harmonize(kr, vars = "wealth_quintile"))
  a <- bdhs_inequality(kr, "rh_del_csection", measures = c("difference", "ci"))
  b <- bdhs_inequality(h, "rh_del_csection", measures = c("difference", "ci"))
  expect_equal(unique(b$by), "wealth_quintile")
  expect_equal(a$estimate, b$estimate)
})

test_that("all-women indicators get the gap measures only", {
  ir <- bdhs_simulate(2022, "IR", n_clusters = 30)
  expect_message(x <- bdhs_inequality(ir, "ms_married_by_18"), "all-women")
  expect_equal(x$measure, c("difference", "ratio"))
})

test_that("bdhs_inequality checks its inputs", {
  kr <- bdhs_simulate(2022, "KR")
  expect_error(bdhs_inequality(kr, c("rh_del_csection", "rh_del_facility")), "single")
  expect_error(bdhs_inequality(kr, "rh_del_csection", measures = "gini"))
  expect_error(bdhs_inequality(kr[, setdiff(names(kr), "v190")], "rh_del_csection"), "wealth")
})

test_that("official gap measures", {
  x <- bdhs_inequality_official("RH_DELA_C_CSC", survey = 2022)
  q <- bdhs_official("RH_DELA_C_CSC", survey = 2022, characteristic = "Wealth quintile")
  lo <- q$value[q$characteristic_label == "Lowest"]
  hi <- q$value[q$characteristic_label == "Highest"]
  expect_equal(x$estimate[x$measure == "difference"], hi - lo)
  expect_equal(x$estimate[x$measure == "ratio"], hi / lo)
  expect_true(all(is.na(x$se)))
  y <- bdhs_inequality_official("rh_del_csection", measures = "difference")
  expect_true(all(c("BD2004DHS", "BD2022DHS") %in% y$survey_id))
})

test_that("inequality plots", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot(bdhs_inequality_official("RH_DELA_C_CSC")), "ggplot")
})

# Real data (skipped by default); aggregates only -----------------------------------------

# The maintainer's recode files, found with bdhs_identify_file() (first file
# per round).
local_recode_files <- function(dir, recode) {
  f <- list.files(dir, pattern = "[.]dta$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
  id <- suppressWarnings(suppressMessages(bdhs_identify_file(f)))
  id <- id[!is.na(id$survey_id) & id$recode %in% recode, , drop = FALSE]
  id <- id[!duplicated(id$survey_id), c("survey_id", "path"), drop = FALSE]
  id[order(id$survey_id), , drop = FALSE]
}

# Read only the columns the indicators and grouping variables need.
read_local_kr <- function(path, survey_id, indicators, extra = c("v190", "v106", "v025")) {
  nm <- names(haven::read_dta(path, n_max = 0))
  cols <- unique(c(indicator_columns(indicators, tolower(nm)), intersect(extra, tolower(nm))))
  d <- haven::read_dta(path, col_select = dplyr::all_of(nm[tolower(nm) %in% cols]))
  names(d) <- tolower(names(d))
  attr(d, "bdhs_survey_id") <- survey_id
  attr(d, "bdhs_recode") <- "KR"
  d
}

test_that("rich-poor gaps from microdata match the official values", {
  dir <- skip_if_no_local_data()
  files <- local_recode_files(dir, "KR")
  files <- files[files$survey_id %in% c("BD2004DHS", "BD2007DHS", "BD2011DHS", "BD2014DHS", "BD2017DHS", "BD2022DHS"), ]
  skip_if(nrow(files) == 0L, "no local KR files")
  for (i in seq_len(nrow(files))) {
    kr <- read_local_kr(files$path[i], files$survey_id[i], c("rh_del_csection", "rh_del_facility"))
    for (ind in c("rh_del_csection", "rh_del_facility")) {
      mine <- suppressMessages(bdhs_inequality(kr, ind, by = "v190"))
      off <- bdhs_inequality_official(ind, survey = files$survey_id[i])
      info <- paste(files$survey_id[i], ind)
      d_m <- mine$estimate[mine$measure == "difference"]
      d_o <- off$estimate[off$measure == "difference"]
      # Within rounding (C-section). rh_del_facility differs from the official
      # value by 0.2 points nationally in 2014 and 2017 (already within the
      # validation tolerance), and by up to 0.7 points in the richest quintile.
      tol <- if (ind == "rh_del_facility" && files$survey_id[i] %in% c("BD2014DHS", "BD2017DHS")) 1 else 0.25
      expect_lt(abs(d_m - d_o), tol, label = info)
      # Concentration index from microdata vs grouped official values
      expect_lt(abs(mine$estimate[mine$measure == "ci"] - off$estimate[off$measure == "ci"]), 0.01, label = info)
      # Sanity: richer mothers use more facility / C-section care
      expect_gt(mine$estimate[mine$measure == "sii"], 0, label = info)
      ci <- mine$estimate[mine$measure == "ci"]
      expect_true(ci > 0 && ci < 1, label = info)
    }
  }
})
