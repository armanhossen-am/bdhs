test_that("official values are looked up by round and reference period", {
  reg <- indicator_registry()
  v <- official_value(reg$rh_del_skilled, "BD2017DHS")
  expect_equal(v$value, 52.7) # three years preceding the survey
  expect_equal(v$precision, 1L)
  expect_equal(official_value(reg$fp_cpr_modern, "BD2022DHS")$value, 54.7)
  # 1993-94 child illness uses the 3-year window (KR covers 3 years).
  expect_equal(official_value(reg$ch_diarrhoea, "BD1994DHS")$value, 12.3)
  expect_equal(official_value(reg$ch_diarrhoea, "BD2017DHS")$value, 4.7)
  # Composite indicators sum their API categories.
  expect_equal(official_value(reg$rh_pnc_mother_2days, "BD2017DHS")$value, 49 + 2.5 + 1)
  # Education and media use the "Total 15-49" category.
  expect_equal(official_value(reg$ed_no_education, "BD2017DHS")$value, 16.6)
  expect_equal(official_value(reg$cm_imr, "BD2022DHS")$value, 27)
  expect_true(is.na(official_value(reg$ch_vac_bcg, "BD2022DHS")$value))
})

test_that("bdhs_validate() compares computed and official values", {
  kr <- bdhs_simulate(2017, "KR", n_clusters = 20, households = 10)
  v <- bdhs_validate(kr, c("rh_del_facility", "rh_del_csection"))
  expect_named(v, c("survey_id", "indicator", "label", "api_id", "computed", "official", "diff",
                    "tolerance", "pass", "n_unweighted", "official_n_unweighted"))
  expect_equal(v$official, c(49.4, 32.7))
  expect_equal(v$diff, v$computed - v$official)
  expect_equal(v$tolerance, c(0.55, 0.55))
  expect_type(v$pass, "logical")
  big <- bdhs_validate(kr, c("rh_del_facility", "rh_del_csection"), tolerance = 100)
  expect_true(all(big$pass))
  all <- suppressWarnings(bdhs_validate(kr))
  expect_true(all(!is.na(all$official)))
  expect_true(all(all$indicator %in% bdhs_indicator_list(recode = "KR")$id))
  expect_error(bdhs_validate(kr, tolerance = -1), "non-negative")
  expect_message(bdhs_validate(bdhs_simulate(2017, "PR", n_clusters = 4, households = 4), "hh_cooking_solid_fuel"), "No official")
})

test_that("tolerances are scaled for rates", {
  ir <- bdhs_simulate(2017, "IR", n_clusters = 10, households = 8)
  v <- bdhs_validate(ir, c("fe_tfr", "fe_gfr"))
  expect_equal(v$tolerance, c(0.5 * 0.1 + 0.05, 0.5 + 0.5))
})

test_that("the validation record only lists registry indicators", {
  rec <- indicator_validation_status()
  expect_true(all(rec$indicator %in% names(indicator_registry())))
  expect_true(all(rec$survey_id %in% bdhs_survey_id(type = "DHS")))
})
