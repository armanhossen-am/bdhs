test_that("AARC matches the UNICEF formula", {
  expect_equal(aarc_rate(100, 50, 10), 100 * (1 - 0.5^(1 / 10)))
  expect_equal(aarc_rate(40, 40, 5), 0)
  expect_true(aarc_rate(40, 50, 5) < 0)
})

test_that("SDG tracker from the official values", {
  x <- bdhs_sdg()
  expect_s3_class(x, "bdhs_sdg")
  expect_true(all(c("stunting", "wasting", "u5mr", "nmr", "sba", "demand_satisfied", "abr",
                    "married_by_15", "married_by_18") %in% x$id))
  u <- x[x$id == "u5mr", ]
  off <- bdhs_official("CM_ECMR_C_U5M")
  v14 <- off$value[off$survey_id == "BD2014DHS"]
  v22 <- off$value[off$survey_id == "BD2022DHS"]
  t14 <- survey_mid_year("BD2014DHS")
  t22 <- survey_mid_year("BD2022DHS")
  expect_equal(u$latest_value, v22)
  expect_equal(u$aarc, aarc_rate(v14, v22, t22 - t14))
  expect_equal(u$target, 25)
  expect_equal(u$required_aarc, aarc_rate(v22, 25, 2030 - t22))
  expect_equal(u$projected, v22 * (1 - u$aarc / 100)^(2030 - t22))
  expect_equal(u$on_track, round(u$projected) <= 25)

  # Coverage indicators use the shortfall from 100
  s <- x[x$id == "sba", ]
  expect_equal(s$aarc, aarc_rate(100 - s$baseline_value, 100 - s$latest_value, t22 - t14))
  expect_equal(s$required_aarc, aarc_rate(100 - s$latest_value, 10, 2030 - t22))

  # Elimination / universal targets have no required AARC
  expect_true(is.na(x$required_aarc[x$id == "married_by_18"]))
  expect_true(is.na(x$required_aarc[x$id == "improved_water"]))
  # No target, no verdict
  expect_true(is.na(x$on_track[x$id == "abr"]))
  # Stunting: half of the 2011 prevalence
  st <- bdhs_official("CN_NUTS_C_HA2", survey = 2011)$value
  expect_equal(x$target[x$id == "stunting"], round(0.5 * st, 1))
})

test_that("bdhs_sdg selects indicators", {
  expect_equal(bdhs_sdg(c("stunting", "3.2.1"))$id, c("stunting", "u5mr"))
  expect_equal(bdhs_sdg("CM_ECMR_C_NNR")$id, "nmr")
  expect_error(bdhs_sdg("happiness"), "Unknown")
  b <- bdhs_sdg("u5mr", baseline = 2017)
  expect_equal(b$baseline_survey, "BD2017DHS")
})

test_that("SDG plot", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot(bdhs_sdg()), "ggplot")
})
