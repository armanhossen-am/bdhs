test_that("national proportions match survey::svymean on the design", {
  kr <- bdhs_simulate(2017, "KR", n_clusters = 20, households = 10)
  est <- bdhs_estimate(kr, "rh_del_facility")
  expect_s3_class(est, "bdhs_estimate")
  expect_named(est, c("survey_id", "indicator", "label", "estimate", "se", "ci_low", "ci_high",
                      "n_unweighted", "n_weighted", "api_id"))
  y <- bdhs_add_indicators(kr, "rh_del_facility")$rh_del_facility
  w <- kr$v005 / 1e6
  ok <- !is.na(y)
  expect_equal(est$estimate, 100 * sum(w[ok] * y[ok]) / sum(w[ok]))
  expect_equal(est$n_unweighted, sum(ok))
  expect_equal(est$n_weighted, sum(w[ok]))
  kr$.y <- y
  des <- bdhs_design(kr)
  ref <- survey::svymean(~.y, subset(des, !is.na(.y)))
  expect_equal(est$se, 100 * as.numeric(survey::SE(ref)))
  expect_true(est$ci_low < est$estimate && est$estimate < est$ci_high)
  expect_equal(est$api_id, "RH_DELP_C_DHF")
  expect_equal(est$survey_id, "BD2017DHS")
})

test_that("CI methods, scale and by-groups work", {
  kr <- bdhs_simulate(2017, "KR", n_clusters = 20, households = 10)
  lg <- bdhs_estimate(kr, "rh_del_csection", ci_method = "logit")
  wd <- bdhs_estimate(kr, "rh_del_csection", ci_method = "wald", scale = 1)
  expect_equal(lg$estimate / 100, wd$estimate)
  expect_equal(wd$estimate - wd$ci_low, wd$ci_high - wd$estimate)
  expect_true(lg$ci_low >= 0 && lg$ci_high <= 100)

  by <- bdhs_estimate(kr, c("rh_del_facility", "ch_diarrhoea"), by = "v025")
  expect_equal(nrow(by), 4)
  expect_true(is.factor(by$v025))
  expect_setequal(as.character(by$v025), c("urban", "rural"))
  nat <- bdhs_estimate(kr, "rh_del_facility")
  sub <- by[by$indicator == "rh_del_facility", ]
  expect_equal(sum(sub$n_unweighted), nat$n_unweighted)
  expect_equal(sum(sub$n_weighted), nat$n_weighted)
  two <- bdhs_estimate(kr, "rh_del_facility", by = c("v025", "v190"))
  expect_true(all(c("v025", "v190") %in% names(two)))
  expect_lte(nrow(two), 10)
  expect_error(bdhs_estimate(kr, "rh_del_facility", by = "nope"), "not found")
})

test_that("all-women indicators use the ratio estimator", {
  ir <- bdhs_simulate(2017, "IR", n_clusters = 20, households = 10)
  est <- bdhs_estimate(ir, "ms_married_by_18")
  y <- bdhs_add_indicators(ir, "ms_married_by_18")$ms_married_by_18
  w <- ir$v005 / 1e6
  aw <- ir$awfactt / 100
  ok <- !is.na(y)
  expect_equal(est$estimate, 100 * sum(w[ok] * y[ok]) / sum(w[ok] * aw[ok]))
  expect_equal(est$n_weighted, sum(w[ok] * aw[ok]))
  # Subgroups use the matching all-woman factor ...
  ir$awfactu <- ir$awfactt * 1.1
  t <- bdhs_add_indicators(ir, "fe_teen_mothers")$fe_teen_mothers
  okt <- !is.na(t) & ir$v025 == 1
  teen <- bdhs_estimate(ir, "fe_teen_mothers", by = "v025")
  expect_equal(teen$estimate[teen$v025 == "urban"], 100 * sum(w[okt] * t[okt]) / sum(w[okt] * ir$awfactu[okt] / 100))
  expect_true(teen$se[1] > 0)
  # ... except child marriage, which the DHS tables compute with awfactt for
  # every subgroup (reproduces the official 2017-18 values exactly).
  byu <- bdhs_estimate(ir, "ms_married_by_18", by = "v025")
  u <- ok & ir$v025 == 1
  expect_equal(byu$estimate[byu$v025 == "urban"], 100 * sum(w[u] * y[u]) / sum(w[u] * aw[u]))
  # Harmonized grouping variables get the same factor as the raw ones.
  expect_equal(awfact_for_by("residence"), "awfactu")
  expect_equal(awfact_for_by("division_trend6"), "awfactr")
  expect_equal(awfact_for_by("wealth_quintile"), "awfactw")
  # Means: CEB to all women.
  ceb <- bdhs_estimate(ir, "fe_ceb_mean")
  expect_equal(ceb$estimate, sum(w * ir$v201) / sum(w * aw))
})

test_that("medians use the DHS interpolation", {
  ir <- bdhs_simulate(2017, "IR", n_clusters = 20, households = 10)
  est <- bdhs_estimate(ir, "ms_median_afm_20_49")
  s <- ir$v012 >= 20
  w <- (ir$v005 / 1e6)[s]
  afm <- ir$v511[s]
  den <- sum(w * ir$awfactt[s] / 100)
  p <- vapply(1:60, function(a) sum(w[afm < a]) / den, numeric(1))
  i <- which(p >= 0.5)[1]
  expect_equal(est$estimate, (i - 1) + (0.5 - p[i - 1]) / (p[i] - p[i - 1]))
  expect_true(est$se > 0)
  expect_equal(dhs_median(c(0.1, 0.3, 0.6, 0.9), 1:4, NULL), 2 + 0.2 / 0.3)
  expect_true(is.na(dhs_median(c(0.1, 0.3, 0.4, 0.6), 1:4, 3)))
})

test_that("rates and pooled rounds are estimated", {
  ir <- bdhs_simulate(2017, "IR", n_clusters = 12, households = 8)
  est <- bdhs_estimate(ir, c("fe_tfr", "fp_cpr_modern"))
  expect_equal(est$indicator, c("fe_tfr", "fp_cpr_modern"))
  expect_equal(est$estimate[1], bdhs_fertility(ir, se = FALSE)$estimate)
  br <- bdhs_simulate(2017, "BR", n_clusters = 12, households = 8)
  m <- bdhs_estimate(br, c("cm_imr", "cm_u5mr"), by = "v025")
  expect_equal(nrow(m), 4)

  kr1 <- bdhs_simulate(2014, "KR", n_clusters = 10, households = 8)
  kr2 <- bdhs_simulate(2022, "KR", n_clusters = 10, households = 8, seed = 2)
  kr1$survey_id <- "BD2014DHS"
  kr2$survey_id <- "BD2022DHS"
  pooled <- suppressWarnings(dplyr::bind_rows(kr1, kr2))
  attr(pooled, "bdhs_recode") <- "KR"
  # The survey_id column wins over a single-round attribute left by binding.
  expect_equal(attr(pooled, "bdhs_survey_id"), "BD2014DHS")
  est <- bdhs_estimate(pooled, "rh_del_facility")
  expect_equal(est$survey_id, c("BD2014DHS", "BD2022DHS"))
  expect_equal(est$estimate[1], bdhs_estimate(kr1, "rh_del_facility", survey = 2014)$estimate)
  expect_warning(v <- bdhs_estimate(pooled, "ch_vac_bcg"), "not available")
  expect_equal(v$survey_id, "BD2014DHS")
})

test_that("bdhs_estimate() validates inputs and handles empty denominators", {
  kr <- bdhs_simulate(2017, "KR", n_clusters = 6, households = 6)
  expect_error(bdhs_estimate(kr, "fp_cpr_any"), "not computed from")
  expect_error(bdhs_estimate(kr, "rh_del_facility", design_args = "x"), "must be a list")
  # The simulated KR has no m13 (timing of first antenatal visit).
  expect_warning(e <- bdhs_estimate(kr, c("rh_del_facility", "rh_anc_first_trimester")), "not found")
  expect_true(is.na(e$estimate[e$indicator == "rh_anc_first_trimester"]))
  expect_false(is.na(e$estimate[e$indicator == "rh_del_facility"]))
  all <- suppressWarnings(bdhs_estimate(kr, NULL))
  expect_gt(nrow(all), 10)
  expect_true(all(all$indicator %in% bdhs_indicator_list(recode = "KR")$id))
  est <- bdhs_estimate(kr, "rh_del_facility")
  expect_match(estimate_header(est), "BDHS estimates: 1 indicator \\| BD2017DHS \\| 95% CI")
  expect_output(print(est), "BDHS estimates")
})

test_that("design_args are passed to bdhs_design()", {
  kr <- bdhs_simulate(2017, "KR", n_clusters = 20, households = 10)
  a <- bdhs_estimate(kr, "rh_del_facility")
  b <- bdhs_estimate(kr, "rh_del_facility", design_args = list(strata = "none"))
  expect_equal(a$estimate, b$estimate)
  expect_false(isTRUE(all.equal(a$se, b$se)))
})
