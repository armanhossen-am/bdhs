sim <- function(survey, recode, ...) {
  bdhs_simulate(survey, recode, n_clusters = 12, households = 5, ...)
}
quiet <- function(expr) suppressMessages(expr)

test_that("bdhs_pool() stacks rounds with survey_id and normalized weights", {
  a <- sim(2014, "KR")
  b <- sim(2022, "KR")
  expect_message(p <- bdhs_pool(a, b), "Value labels differ")
  expect_s3_class(p, "tbl_df")
  expect_equal(names(p)[1], "survey_id")
  expect_equal(nrow(p), nrow(a) + nrow(b))
  expect_equal(as.vector(table(p$survey_id)), c(nrow(a), nrow(b)))
  expect_equal(p$pool_wt, c(as.numeric(a$v005), as.numeric(b$v005)) / 1e6, ignore_attr = TRUE)
  expect_equal(attr(p, "bdhs_recode"), "KR")
  expect_true(attr(p, "bdhs_pooled"))
  expect_equal(attr(p, "bdhs_pool_weights"), "normalized")
  # Rounds detected from the data, also when given as a list
  p2 <- quiet(bdhs_pool(list(a, b)))
  expect_identical(p, p2)
  expect_equal(data_survey_id(p), c("BD2014DHS", "BD2022DHS"))
})

test_that("labels are reconciled across rounds", {
  a <- sim(2014, "KR")
  b <- sim(2022, "KR")
  p <- quiet(bdhs_pool(a, b))
  # v024 codes mean different divisions: labels dropped, codes kept
  expect_false(inherits(p$v024, "haven_labelled"))
  expect_equal(p$v024, c(as.numeric(a$v024), as.numeric(b$v024)), ignore_attr = TRUE)
  expect_equal(attr(p$v024, "label"), "region")
  # identical labels survive
  expect_s3_class(p$v025, "haven_labelled")
  # mixed types become character
  b$v000 <- 8
  expect_message(p3 <- bdhs_pool(a, b), "mixed types")
  expect_type(p3$v000, "character")
})

test_that("equal and denormalized weights rescale whole rounds", {
  a <- sim(2014, "KR")
  b <- sim(2022, "KR", seed = 2)
  pe <- quiet(bdhs_pool(a, b, weights = "equal"))
  tot <- tapply(pe$pool_wt, pe$survey_id, sum)
  expect_equal(unname(tot[1]), unname(tot[2]))
  expect_equal(sum(pe$pool_wt), nrow(pe))

  pd <- quiet(bdhs_pool(a, b, weights = "denormalized"))
  pop <- pool_population()
  f <- pop$female_15_49 * pop$p_ever_married / pop$n_women_sample
  names(f) <- pop$survey_id
  expect_equal(pd$pool_wt[pd$survey_id == "BD2022DHS"], as.numeric(b$v005) / 1e6 * f[["BD2022DHS"]])
  ratio <- pd$pool_wt / (as.numeric(c(a$v005, b$v005)) / 1e6)
  expect_equal(length(unique(round(ratio, 6))), 2L)
  expect_equal(attr(pd, "bdhs_pool_weights"), "denormalized")
})

test_that("the population table has every DHS round", {
  pop <- pool_population()
  expect_true(all(c("survey_id", "female_15_49", "n_women_sample", "p_ever_married") %in% names(pop)))
  expect_true(all(bdhs_survey_id(type = "DHS") %in% pop$survey_id))
  expect_true(all(pop$female_15_49 > 2e7 & pop$n_women_sample > 5000))
})

test_that("bdhs_pool() can harmonize and select columns", {
  vars <- c("division_trend6", "residence", "child_age_months", "vacc_bcg", "b5")
  expect_message(
    p <- bdhs_pool(sim(2017, "KR"), sim(2022, "KR"), harmonize = TRUE, vars = vars),
    "Missing \\(all"
  )
  expect_true(all(c("division_trend6", "residence", "child_age_months", "vacc_bcg", "b5") %in% names(p)))
  expect_true(all(c("v001", "v005", "v021", "v022", "pool_wt") %in% names(p)))
  expect_false("m14" %in% names(p))
  expect_s3_class(p$division_trend6, "factor")
  expect_false(anyNA(p$division_trend6))
  expect_true(all(is.na(p$vacc_bcg[p$survey_id == "BD2022DHS"])))
  expect_false(all(is.na(p$vacc_bcg[p$survey_id == "BD2017DHS"])))
  expect_true(all(c("division_trend6", "vacc_bcg") %in% attr(p, "bdhs_harmonized")))

  q <- quiet(bdhs_pool(sim(2017, "KR"), sim(2022, "KR"), vars = c("b5", "m14")))
  expect_true(all(c("b5", "m14", "v005", "pool_wt") %in% names(q)))
  expect_false("h2" %in% names(q))
  expect_error(bdhs_pool(sim(2017, "KR"), sim(2022, "KR"), harmonize = TRUE, vars = "m14"), "harmonized")
})

test_that("bdhs_pool() refuses mixed recodes and repeated rounds", {
  expect_error(bdhs_pool(sim(2017, "KR"), sim(2022, "IR")), "same recode")
  expect_error(bdhs_pool(sim(2017, "KR"), sim(2017, "KR")), "more than once")
  expect_error(bdhs_pool(1, 2), "data frames")
  expect_error(bdhs_pool(sim(2017, "KR"), weights = "nope"))
})

test_that("pooled household data and nested pooling work", {
  p <- quiet(bdhs_pool(sim(2014, "HR"), sim(2022, "HR")))
  expect_equal(p$pool_wt, as.numeric(p$hv005) / 1e6, ignore_attr = TRUE)
  p3 <- quiet(bdhs_pool(p, sim(2007, "HR")))
  expect_equal(sort(unique(p3$survey_id)), c("BD2007DHS", "BD2014DHS", "BD2022DHS"))
})
