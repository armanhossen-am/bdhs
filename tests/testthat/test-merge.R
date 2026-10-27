sim <- function(survey, recode, ...) {
  bdhs_simulate(survey, recode, n_clusters = 12, households = 5, ...)
}
quiet <- function(expr) suppressMessages(expr)
codes_of <- function(x) as.numeric(unclass(x))

test_that("children and women get household variables (v001, v002 = hv001, hv002)", {
  kr <- sim(2022, "KR")
  hr <- sim(2022, "HR")
  expect_message(m <- bdhs_merge(kr, hr), "100%")
  expect_equal(nrow(m), nrow(kr))
  expect_true(all(c("hv201", "hv270", "hhid") %in% names(m)))
  expect_false(any(c("hv001", "hv002") %in% names(m)))
  expect_equal(m[names(kr)], kr, ignore_attr = TRUE)
  expect_equal(attr(m, "bdhs_recode"), "KR")
  expect_equal(attr(m, "bdhs_survey_id"), "BD2022DHS")
  i <- match(paste(kr$v001, kr$v002), paste(hr$hv001, hr$hv002))
  expect_equal(as.numeric(m$hv201), as.numeric(hr$hv201[i]))
})

test_that("births get mothers' variables on caseid and duplicates are dropped", {
  br <- sim(2022, "BR")
  ir <- sim(2022, "IR")
  m <- quiet(bdhs_merge(br, ir))
  expect_equal(nrow(m), nrow(br))
  expect_true(all(c("v201", "v312", "b3_01") %in% names(m)))
  expect_false(any(grepl("_ir$", names(m))))
  expect_equal(sum(names(m) == "v012"), 1L)

  kr <- sim(2022, "KR")
  mk <- quiet(bdhs_merge(kr, br))
  expect_equal(nrow(mk), nrow(kr))
  expect_true("b7" %in% names(mk))
})

test_that("women and members link on line numbers", {
  ir <- sim(2022, "IR")
  pr <- sim(2022, "PR")
  m <- quiet(bdhs_merge(ir, pr))
  expect_equal(nrow(m), nrow(ir))
  expect_true(all(m$hv104 == 2))
  expect_equal(as.numeric(m$hv105), as.numeric(ir$v012))

  hr <- sim(2022, "HR")
  mp <- quiet(bdhs_merge(pr, hr))
  expect_equal(nrow(mp), nrow(pr))
  # hv270 is identical in PR and HR: not duplicated
  expect_false("hv270_hr" %in% names(mp))
  expect_true("hv201" %in% names(mp))
})

test_that("children link to the household roster through b16", {
  kr <- sim(2022, "KR")
  pr <- sim(2022, "PR")
  # Without b16 there is no child-level key.
  expect_error(bdhs_merge(kr[, setdiff(names(kr), "b16")], pr), "b16")
  # The simulated b16 is the child's line in the roster (NA for dead children).
  alive <- codes_of(kr$b5) == 1
  m <- quiet(bdhs_merge(kr, pr))
  expect_equal(nrow(m), nrow(kr))
  expect_true(all(is.na(m$hv105[!alive])))
  expect_equal(sum(!is.na(m$hv105)), sum(!is.na(kr$b16)))
  expect_true(all(m$hv112[!is.na(kr$b16)] == 2))
  # Ages agree: completed years in the roster from months in KR.
  ok <- !is.na(m$hv105)
  expect_equal(as.numeric(m$hv105[ok]), as.numeric(m$b19[ok]) %/% 12)
})

test_that("mismatched surveys are refused and duplicate columns suffixed", {
  kr <- sim(2022, "KR")
  hr <- sim(2017, "HR")
  expect_error(bdhs_merge(kr, hr), "different surveys")

  hr22 <- sim(2022, "HR")
  hr22$v012 <- 99 # same name, different values
  m <- quiet(bdhs_merge(kr, hr22))
  expect_true("v012_hr" %in% names(m))
  expect_equal(m$v012, kr$v012)
  m2 <- quiet(bdhs_merge(kr, hr22, suffix = ".hh"))
  expect_true("v012.hh" %in% names(m2))
})

test_that("explicit keys, inner joins and one-to-many joins", {
  kr <- sim(2022, "KR")
  hr <- sim(2022, "HR")
  hr_half <- hr[hr$hv001 <= 6, ]
  m <- quiet(bdhs_merge(kr, hr_half, join = "inner"))
  expect_true(all(m$v001 <= 6))
  m2 <- quiet(bdhs_merge(kr, hr, by = c(v001 = "hv001", v002 = "hv002")))
  expect_equal(m2$hv201, quiet(bdhs_merge(kr, hr))$hv201)
  expect_error(bdhs_merge(kr, hr, by = c("v001")), "named")

  expect_message(m3 <- bdhs_merge(hr, kr), "repeated")
  expect_equal(nrow(m3), nrow(hr) + sum(duplicated(paste(kr$v001, kr$v002))))
})

test_that("WI and HW files link on household ids", {
  kr <- sim(1994, "KR")
  hr <- sim(1994, "HR")
  wi <- tibble::tibble(whhid = hr$hhid, wlthind5 = as.numeric(hr$hv270), wlthindf = as.numeric(hr$hv271))
  m <- quiet(bdhs_merge(kr, wi))
  expect_equal(nrow(m), nrow(kr))
  expect_false(anyNA(m$wlthind5))
  h <- quiet(bdhs_harmonize(m, vars = c("wealth_quintile", "wealth_score")))
  expect_equal(as.integer(h$wealth_quintile), as.integer(m$wlthind5))
  expect_equal(h$wealth_score, m$wlthindf / 1e5, ignore_attr = TRUE)

  pr <- sim(2004, "PR")
  hw <- tibble::tibble(
    hwhhid = pr$hhid[!is.na(pr$hc1)], hwline = as.numeric(pr$hvidx[!is.na(pr$hc1)]),
    hc70 = 100
  )
  mp <- quiet(bdhs_merge(pr, hw))
  expect_equal(sum(mp$hc70_hw == 100, na.rm = TRUE), nrow(hw))
})

test_that("pooled data merge within rounds", {
  kr <- quiet(bdhs_pool(sim(2017, "KR"), sim(2022, "KR")))
  hr <- quiet(bdhs_pool(sim(2017, "HR"), sim(2022, "HR")))
  m <- quiet(bdhs_merge(kr, hr))
  expect_equal(nrow(m), nrow(kr))
  ref <- quiet(bdhs_merge(sim(2017, "KR"), sim(2017, "HR")))
  expect_equal(as.numeric(m$hv201[m$survey_id == "BD2017DHS"]), as.numeric(ref$hv201))
})
