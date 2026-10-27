# Hand-built household members: `ages` (vector) with sex, all de facto.
fake_pr <- function(age, sex = rep(2, length(age))) {
  tibble::tibble(
    hhid = seq_along(age), hvidx = 1L,
    hv103 = 1, hv104 = sex, hv105 = age
  )
}

test_that("Whipple and Myers indices have their textbook extremes", {
  smooth <- rep(0:99, each = 10)
  expect_equal(whipple_index(smooth), 100)
  expect_equal(myers_index(smooth), 0)
  heaped <- rep(seq(25, 60, by = 5), 10)
  expect_equal(whipple_index(heaped), 500)
  all_zero <- rep(seq(10, 80, by = 10), 10)
  expect_equal(myers_index(all_zero), 90)
  expect_true(is.na(whipple_index(c(5, 70))))
})

test_that("age heaping and displacement come from PR ages", {
  ages <- c(rep(0:94, each = 4), rep(c(14, 50), 4))
  pr <- fake_pr(ages, sex = rep(2, length(ages)))
  pr$hv103[1] <- 0 # not de facto: dropped by default
  q <- bdhs_quality(pr, checks = c("age_heaping", "age_displacement"))
  expect_s3_class(q, "bdhs_quality")
  expect_named(q, c("survey_id", "recode", "check", "statistic", "group", "value", "benchmark", "interpretation", "n"))
  w <- q$value[q$statistic == "whipple" & q$group == "female"]
  expect_equal(w, 100 * (4 * 8 + 4) / ((40 * 4 + 4) / 5)) # 50 has 4 extra
  r14 <- q[q$statistic == "ratio_14_to_15" & q$group == "female", ]
  expect_equal(r14$value, 2)
  expect_match(r14$interpretation, "displacement")
  expect_equal(q$value[q$statistic == "ratio_50_to_49" & q$group == "female"], 2)
  prof <- attr(q, "profiles")$age
  expect_equal(sum(prof$n), length(ages) - 1)
  q_all <- bdhs_quality(pr, checks = "age_heaping", population = "all")
  expect_equal(sum(attr(q_all, "profiles")$age$n), length(ages))
})

test_that("birth dates, calendar-year ratios and age at death", {
  v008 <- 1300 # December 2008 (cmc)
  # Births: 100 per calendar year 1990-2007, but 2003 has 70 and 2002 has 130.
  yrs <- 1990:2007
  n <- ifelse(yrs == 2003, 70, ifelse(yrs == 2002, 130, 100))
  b_year <- rep(yrs, n)
  b3 <- (b_year - 1900) * 12 + 6
  br <- tibble::tibble(
    caseid = "1", bidx = seq_along(b3), v008 = v008, b3 = b3,
    b5 = 1, b10 = rep(c(1, 1, 1, 3), length.out = length(b3)), b6 = NA_real_, b7 = NA_real_
  )
  dead <- 1:40
  br$b5[dead] <- 0
  br$b6[dead] <- c(rep(211, 5), rep(212, 10), rep(301, 10), rep(213, 5), rep(100, 5), rep(998, 5))
  br$b7[dead] <- c(rep(11, 5), rep(12, 20), rep(13, 5), rep(0, 5), rep(NA, 5))
  attr(br, "bdhs_recode") <- "BR"
  q <- bdhs_quality(br, checks = c("birth_date_completeness", "births_by_year", "age_at_death"))
  expect_equal(unique(q$recode), "BR")
  expect_equal(q$value[q$statistic == "pct_month_and_year" & q$group == "all"], 75)
  cyr <- q[q$statistic == "calendar_year_ratio", ]
  expect_equal(cyr$value[cyr$group == "2003"], 200 * 70 / (130 + 100))
  expect_equal(cyr$value[cyr$group == "2002"], 200 * 130 / (100 + 70))
  expect_match(cyr$interpretation[cyr$group == "2003"], "first year of the 5-year window")
  expect_false("2007" %in% cyr$group) # next to the interview year
  expect_equal(cyr$value[cyr$group == "1995"], 100)
  births <- attr(q, "profiles")$births
  expect_equal(sum(births$n), nrow(br))
  expect_equal(unique(births$cutoff_year), 2003)
  h <- q[q$statistic == "heaping_12_months", ]
  expect_equal(h$value[h$group == "reported (12 months or 1 year)"], 400)
  expect_equal(h$value[h$group == "imputed months (b7)"], 400)
  expect_equal(q$value[q$statistic == "pct_missing" & q$check == "age_at_death"], 12.5)
  expect_equal(q$value[q$statistic == "pct_reported_years"], 25)
})

test_that("births from an IR wide birth history and KR window checks", {
  ir <- tibble::tibble(
    caseid = c("1", "2"), v001 = 1, v008 = 1300,
    b3_01 = c(1290, 1250), b3_02 = c(1200, NA), b10_01 = c(1, 1), b10_02 = c(5, NA)
  )
  q <- bdhs_quality(ir, checks = "birth_date_completeness")
  expect_equal(q$value[q$group == "all"], 100 * 2 / 3)
  kr <- tibble::tibble(midx = 1, caseid = "1", v008 = 1300, b3 = 1300 - c(rep(5, 10), rep(40, 10), rep(50, 5)))
  q <- bdhs_quality(kr, checks = "births_by_year")
  expect_equal(q$value[q$statistic == "births_year4_vs_year3"], 50)
  expect_false(any(q$statistic == "years_before_ratio" & q$group == "4"))
})

test_that("anthropometry flags, SDs and digit preference", {
  pr <- tibble::tibble(
    hhid = 1:10, hvidx = 1, hv103 = 1, hv104 = 1, hv105 = 2, hc1 = 24,
    hc2 = c(rep(120, 5), 125, 121, 122, 9994, NA),
    hc3 = 850,
    hc70 = c(-100, 100, -100, 100, -100, 100, 9998, 9996, NA, -700)
  )
  q <- bdhs_quality(pr, checks = "anthropometry")
  get <- function(s) q$value[q$statistic == s]
  expect_equal(get("pct_missing_weight"), 20)
  expect_equal(get("weight_terminal_digit_pct_0"), 62.5)
  expect_equal(get("weight_terminal_digit_pct_0_or_5"), 75)
  expect_equal(get("height_terminal_digit_dissimilarity"), 90)
  expect_equal(get("haz_pct_missing"), 10)
  expect_equal(get("haz_pct_flagged"), 100 * 2 / 9)
  expect_equal(get("haz_pct_out_of_who_range"), 100 / 7)
  expect_equal(get("haz_sd"), stats::sd(c(-1, 1, -1, 1, -1, 1, -7)))
})

test_that("completeness reads don't-know codes from labels; missing checks are skipped", {
  d <- tibble::tibble(
    caseid = as.character(1:4), v001 = 1,
    v106 = haven::labelled(c(0, 1, 8, NA), c(none = 0, primary = 1, "don't know" = 8))
  )
  expect_message(q <- bdhs_quality(d, survey = 2017), "Skipped")
  expect_equal(unique(q$survey_id), "BD2017DHS")
  expect_equal(q$value[q$statistic == "pct_na"], 25)
  expect_equal(q$value[q$statistic == "pct_dk_or_missing_code"], 100 / 3)
  expect_warning(bdhs_quality(d, checks = "completeness", vars = c("v106", "zz")), "zz")
  expect_error(bdhs_quality(d, checks = "nope"))
  expect_error(bdhs_quality(1:3), "data frame")
})

test_that("bdhs_quality() runs on simulated recodes and plots profiles", {
  pr <- bdhs_simulate(2022, "PR")
  q <- suppressMessages(bdhs_quality(pr))
  expect_true(all(c("age_heaping", "anthropometry") %in% q$check))
  expect_equal(unique(q$survey_id), "BD2022DHS")
  br <- bdhs_simulate(2022, "BR")
  qb <- suppressMessages(bdhs_quality(br))
  expect_true("births_by_year" %in% qb$check)
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot.bdhs_quality(q), "ggplot")
  expect_s3_class(autoplot.bdhs_quality(qb, type = "births"), "ggplot")
  expect_error(plot.bdhs_quality(q, type = "births"), "births profile")
})

test_that("bdhs_quality() on local files gives plausible aggregates", {
  dir <- skip_if_no_local_data()
  path <- file.path(dir, "BDHS NPD", "BDPR7RDT", "BDPR7RFL.DTA")
  skip_if_not(file.exists(path))
  pr <- bdhs_read_file(path, vars = c("hv103", "hv104", "hv105", "hc1", "hc2", "hc3", "hc70", "hc71", "hc72"))
  q <- suppressMessages(bdhs_quality(pr, checks = c("age_heaping", "anthropometry")))
  w <- q$value[q$statistic == "whipple" & q$group == "total"]
  expect_true(w > 100 && w < 500)
  sd_haz <- q$value[q$statistic == "haz_sd"]
  expect_true(sd_haz > 0.9 && sd_haz < 1.6)
})
