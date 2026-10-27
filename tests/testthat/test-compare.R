test_that("between-round differences combine independent standard errors", {
  kr17 <- bdhs_simulate(2017, "KR")
  kr22 <- bdhs_simulate(2022, "KR")
  e1 <- bdhs_estimate(kr17, "rh_del_facility")
  e2 <- bdhs_estimate(kr22, "rh_del_facility")
  cmp <- bdhs_compare(e1, e2)
  expect_s3_class(cmp, "bdhs_comparison")
  expect_equal(nrow(cmp), 1L)
  expect_equal(cmp$difference, e2$estimate - e1$estimate)
  expect_equal(cmp$se, sqrt(e1$se^2 + e2$se^2))
  expect_equal(cmp$z, cmp$difference / cmp$se)
  expect_equal(cmp$p_value, 2 * pnorm(-abs(cmp$z)))
  expect_equal(cmp$ci_high - cmp$difference, qnorm(0.975) * cmp$se)
  yrs <- survey_mid_year("BD2022DHS") - survey_mid_year("BD2017DHS")
  expect_equal(cmp$years, yrs)
  expect_equal(cmp$annual_change, cmp$difference / yrs)

  # Same answer from the microdata, in any order of the inputs
  cmp2 <- bdhs_compare(kr22, kr17, indicators = "rh_del_facility")
  expect_equal(cmp2$difference, cmp$difference)
  expect_equal(cmp2$survey_1, "BD2017DHS")
})

test_that("hand-computed difference statistics", {
  st <- independent_difference(40, 43, 3, 4, 0.95)
  expect_equal(st$difference, 3)
  expect_equal(st$se, 5)
  expect_equal(st$z, 0.6)
  expect_equal(st$p_value, 2 * pnorm(-0.6))
  # 2017-18 fieldwork Oct 2017 - Mar 2018: mid-point around the new year
  expect_true(abs(survey_mid_year("BD2017DHS") - 2018) < 0.1)
  expect_equal(division_layout(c("BD1994DHS", "BD2004DHS", "BD2011DHS", "BD2022DHS")), c(5L, 6L, 7L, 8L))
})

test_that("round pairs", {
  expect_equal(nrow(round_pairs(4, "consecutive")), 3L)
  expect_equal(nrow(round_pairs(4, "all")), 6L)
  expect_equal(round_pairs(4, "first_last"), matrix(c(1L, 4L), ncol = 2))
})

test_that("group contrasts within a round respect the covariance", {
  kr <- bdhs_simulate(2022, "KR")
  cmp <- bdhs_compare(kr, indicators = "rh_del_csection", by = "v025")
  g <- cmp[cmp$comparison == "groups", ]
  expect_equal(nrow(g), 1L)
  est <- bdhs_estimate(kr, "rh_del_csection", by = "v025")
  expect_equal(sort(c(g$estimate_1, g$estimate_2)), sort(est$estimate))
  expect_equal(g$difference, g$estimate_2 - g$estimate_1)

  # Hand computation from the joint covariance matrix
  withr::local_options(survey.lonely.psu = "adjust")
  r <- group_estimates(kr, resolve_indicators("rh_del_csection")[[1]], "BD2022DHS", "KR", "v025", list())
  V <- vcov(r)
  expect_equal(g$se, 100 * sqrt(V[1, 1] + V[2, 2] - 2 * V[1, 2]))
})

test_that("all-women indicators use the ratio estimator in group contrasts", {
  ir <- bdhs_simulate(2022, "IR")
  cmp <- bdhs_compare(ir, indicators = "ms_married_by_18", by = "v025")
  est <- bdhs_estimate(ir, "ms_married_by_18", by = "v025")
  expect_equal(sort(c(cmp$estimate_1, cmp$estimate_2)), sort(est$estimate))
})

test_that("pairwise contrasts and pooled data", {
  p <- suppressMessages(bdhs_pool(bdhs_simulate(2014, "KR"), bdhs_simulate(2022, "KR")))
  expect_warning(
    cmp <- bdhs_compare(p, indicators = "rh_del_csection", by = "v024", groups = "pairwise"),
    "division_trend6"
  )
  g <- cmp[cmp$comparison == "groups" & cmp$survey_1 == "BD2022DHS", ]
  expect_equal(nrow(g), choose(8, 2))
  expect_gt(sum(cmp$comparison == "rounds"), 0)
})

test_that("bdhs_compare checks its inputs", {
  kr <- bdhs_simulate(2022, "KR")
  e <- bdhs_estimate(kr, "rh_del_facility")
  expect_error(bdhs_compare(), "Supply")
  expect_error(bdhs_compare(kr, e, indicators = "rh_del_facility"), "not a mix")
  expect_error(bdhs_compare(kr), "indicators")
  expect_error(bdhs_compare(kr, kr, indicators = "rh_del_facility"), "more than once")
  expect_error(bdhs_compare(kr, indicators = "rh_del_facility", by = c("v025", "v024")), "single")
})
