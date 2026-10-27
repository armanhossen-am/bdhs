sim <- function(survey, recode, ...) {
  bdhs_simulate(survey, recode, n_clusters = 12, households = 5, ...)
}
quiet <- function(expr) suppressMessages(expr)

test_that("bdhs_design() uses DHS clusters, strata and weights", {
  ir <- sim(2022, "IR")
  des <- bdhs_design(ir)
  expect_s3_class(des, "survey.design2")
  expect_equal(attr(des, "bdhs_survey_id"), "BD2022DHS")
  expect_equal(attr(des, "bdhs_recode"), "IR")
  expect_equal(attr(des, "bdhs_weight"), "v005")
  expect_equal(attr(des, "bdhs_singleton_strata"), 0L)
  expect_equal(unname(weights(des)), as.numeric(ir$v005) / 1e6)
  expect_equal(length(unique(des$cluster[[1]])), length(unique(ir$v021)))
  m <- survey::svymean(~v201, des)
  expect_equal(unname(coef(m)), stats::weighted.mean(ir$v201, ir$v005))

  # backward-compatible arguments
  expect_equal(attr(bdhs_design(ir, weight = "v005", strata = "none"), "bdhs_weight"), "v005")
  expect_s3_class(bdhs_design(ir, strata = "region_residence"), "survey.design2")
  expect_error(bdhs_design(ir, weight = "dv"), "d005")
  ir$d005 <- ir$v005 * 2
  expect_equal(unname(weights(bdhs_design(ir, weight = "dv"))), as.numeric(ir$v005) * 2 / 1e6)
})

test_that("household files support household and men's subsample weights", {
  hr <- sim(2004, "HR")
  expect_equal(attr(bdhs_design(hr), "bdhs_weight"), "hv005")
  expect_error(bdhs_design(hr, weight = "male"), "hv028")
  hr$hv028 <- ifelse(as.numeric(hr$hv001) %% 2 == 0, as.numeric(hr$hv005), 0)
  des <- quiet(bdhs_design(hr, weight = "male"))
  expect_equal(attr(des, "bdhs_weight"), "hv028")
  expect_equal(nrow(des$variables), sum(hr$hv028 > 0))
  hr$hv028 <- 0
  expect_error(quiet(bdhs_design(hr, weight = "male")), "zero")
  expect_error(bdhs_design(sim(2004, "IR"), weight = "male"), "household")
})

test_that("pooled designs use pool_wt and round-specific PSUs", {
  p <- quiet(bdhs_pool(sim(2014, "IR"), sim(2022, "IR"), weights = "equal"))
  des <- bdhs_design(p)
  expect_equal(attr(des, "bdhs_weight"), "pool_wt")
  expect_equal(unname(weights(des)), p$pool_wt, ignore_attr = TRUE)
  expect_equal(length(unique(des$cluster[[1]])), 24L)
  expect_true(all(grepl("^BD20(14|22)DHS ", des$variables$.bdhs_strata)))
  # Other weights on pooled data still work (normalized within rounds)
  rlang::local_options(rlib_message_verbosity = "verbose")
  expect_message(d2 <- bdhs_design(p, weight = "v005"), "normalized within each round")
  expect_equal(unname(weights(d2)), as.numeric(p$v005) / 1e6)
})

test_that("singleton-PSU strata are reported once with the fix", {
  ir <- sim(2022, "IR")
  one <- ir[!(as.numeric(ir$v022) == 1 & ir$v001 == min(ir$v001[as.numeric(ir$v022) == 1])), ]
  one <- one[!(as.numeric(one$v022) == 2 & one$v001 != min(one$v001[as.numeric(one$v022) == 2])), ]
  rlang::local_options(rlib_message_verbosity = "verbose")
  withr::local_options(survey.lonely.psu = NULL)
  expect_message(des <- bdhs_design(one), "survey.lonely.psu")
  expect_gte(attr(des, "bdhs_singleton_strata"), 1L)
  withr::local_options(survey.lonely.psu = "adjust")
  expect_no_message(bdhs_design(one))
})

test_that("a constant strata variable is flagged", {
  kr <- sim(2004, "KR")
  kr$v023 <- 0
  expect_message(bdhs_design(kr, strata = "design"), "constant")
  expect_error(bdhs_design(kr[, setdiff(names(kr), "v021")]), "v021")
})

test_that("bdhs_design_info() documents every round", {
  info <- bdhs_design_info()
  expect_s3_class(info, "tbl_df")
  expect_true(all(c(
    "survey_id", "year_label", "recode", "psu", "strata", "weight", "weight_divisor",
    "n_strata", "strata_type", "notes"
  ) %in% names(info)))
  expect_setequal(unique(info$survey_id), bdhs_survey_id(type = "DHS"))
  mr <- info[info$recode == "MR", ]
  expect_setequal(mr$survey_id, c("BD1994DHS", "BD1997DHS", "BD2000DHS", "BD2004DHS", "BD2007DHS", "BD2011DHS"))
  expect_equal(info$psu[info$recode == "PR"][1], "hv021")
  expect_match(bdhs_design_info(2004, "KR")$notes, "v023")
  expect_equal(bdhs_design_info("2017-18", "IR")$n_strata, 22L)
  expect_equal(bdhs_design_info(1994, "IR")$n_strata, 148L)
  expect_equal(nrow(bdhs_design_info(2022, c("IR", "HR"))), 2L)
  expect_error(bdhs_design_info(2022, "GE"), "Design information")
})
