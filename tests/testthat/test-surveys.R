test_that("bdhs_survey_id() resolves years, ranges, ids and file codes", {
  expect_equal(bdhs_survey_id(2022), "BD2022DHS")
  expect_equal(
    bdhs_survey_id(c("2017-18", 2017, 2018, "7R", "bd7r", "BD2017DHS")),
    rep("BD2017DHS", 6)
  )
  expect_equal(bdhs_survey_id(c("1999-2000", "1999-00", 1999, 2000)), rep("BD2000DHS", 4))
  expect_equal(bdhs_survey_id(c(1993, 1994, "1993-94", "31")), rep("BD1994DHS", 4))
  expect_equal(bdhs_survey_id(c(1996, "3A")), c("BD1997DHS", "BD1997DHS"))
})

test_that("bdhs_survey_id() handles SPA and BMMS aliases", {
  expect_equal(bdhs_survey_id(c("spa2014", "SPA 2017", "BD1999SPA")), c("BD2014SPA", "BD2017SPA", "BD1999SPA"))
  expect_equal(bdhs_survey_id("bmms2001"), "BD2001OTH")
  # A bare year prefers the DHS round over an SPA of the same year.
  expect_equal(bdhs_survey_id(2014), "BD2014DHS")
})

test_that("bdhs_survey_id() errors or returns NA for unknown surveys", {
  expect_error(bdhs_survey_id(2030), "Can't resolve")
  expect_equal(bdhs_survey_id(c("nope", 2022), error = FALSE), c(NA, "BD2022DHS"))
  expect_equal(bdhs_survey_id(character()), character())
  expect_length(bdhs_survey_id(), 13)
  expect_length(bdhs_survey_id(type = "DHS"), 9)
})

test_that("survey_from_v000() breaks phase ties with the interview year", {
  expect_equal(survey_from_v000("BD7", c(2017, 2018)), "BD2017DHS")
  expect_equal(survey_from_v000("BD7", 2014), "BD2014DHS")
  expect_equal(survey_from_v000("BD3", 93), "BD1994DHS")
  expect_equal(survey_from_v000("BD8", NULL), "BD2022DHS")
  expect_true(is.na(survey_from_v000("BD4", NULL)))
})

test_that("bdhs_rounds() and bdhs_recodes() return tidy tables", {
  r <- bdhs_rounds()
  expect_s3_class(r, "tbl_df")
  expect_equal(nrow(r), 9)
  expect_false("aliases" %in% names(r))
  expect_true(all(c("IR", "KR", "BR", "HR", "PR", "MR") %in% bdhs_recodes()$recode))
  expect_equal(check_recode("ir"), "IR")
  expect_error(check_recode("ZZ"), "must be one DHS recode code")
})

test_that("data_survey_id() and data_recode() detect simulated data", {
  for (r in c("IR", "KR", "BR", "HR", "PR")) {
    x <- bdhs_simulate(2017, r, n_clusters = 4, households = 3)
    expect_equal(data_survey_id(x), "BD2017DHS")
    expect_equal(data_recode(x), r)
    attr(x, "bdhs_survey_id") <- NULL
    attr(x, "bdhs_recode") <- NULL
    expect_equal(data_survey_id(x), "BD2017DHS")
    expect_equal(data_recode(x), r)
  }
  x <- bdhs_simulate(1994, "IR", n_clusters = 4, households = 3)
  attr(x, "bdhs_survey_id") <- NULL
  expect_equal(data_survey_id(x), "BD1994DHS")
  expect_equal(data_survey_id(x, survey = "2004"), "BD2004DHS")
  expect_error(data_survey_id(data.frame(a = 1)), "Can't tell which BDHS round")
  expect_error(data_recode(data.frame(a = 1)), "Can't tell which DHS recode")
})

test_that("data_recode() tells KR from a BR that also carries midx", {
  br <- bdhs_simulate(2017, "BR", n_clusters = 4, households = 3)
  attr(br, "bdhs_recode") <- NULL
  br$midx <- br$bidx
  expect_equal(data_recode(br), "BR")
  kr <- bdhs_simulate(2017, "KR", n_clusters = 4, households = 3)
  attr(kr, "bdhs_recode") <- NULL
  expect_equal(data_recode(kr), "KR")
})
