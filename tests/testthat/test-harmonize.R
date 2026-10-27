sim <- function(survey, recode, ...) {
  bdhs_simulate(survey, recode, n_clusters = 16, households = 6, ...)
}
codes <- function(x) as.numeric(unclass(x))
quiet <- function(expr) suppressMessages(expr)

test_that("the bundled crosswalk is complete and consistent", {
  cw <- crosswalk()
  expect_length(cw$vars, 112)
  expect_identical(cw_check(cw), character())
  expect_s3_class(cw$lookups$district_bbs, "data.frame")
  expect_true(all(c("code", "name", "division_2022") %in% names(cw$lookups$district_bbs)))
  # Raw structure kept for other readers of crosswalk.rds.
  raw <- readRDS(system.file("extdata", "crosswalk.rds", package = "bdhs"))
  expect_named(raw, c("meta", "lookups", "variables"))
  expect_equal(raw$variables[[1]]$name, "survey_id")
})

test_that("bdhs_harmonized_vars() lists variables and per-round rules", {
  v <- bdhs_harmonized_vars()
  expect_s3_class(v, "tbl_df")
  expect_equal(nrow(v), 112)
  expect_true(all(c(
    "name", "label", "type", "unit", "comparability", "levels", "rounds",
    "external_rounds", "sources", "household_var"
  ) %in% names(v)))
  d6 <- v[v$name == "division_trend6", ]
  expect_equal(d6$levels[[1]]$value, c("barishal", "chattogram", "dhaka", "khulna", "rajshahi", "sylhet"))
  expect_true("BD2022DHS" %in% d6$rounds[[1]])
  expect_match(d6$sources, "2022: v024", fixed = TRUE)
  wq <- v[v$name == "wealth_quintile", ]
  expect_equal(wq$external_rounds[[1]], c("BD1994DHS", "BD1997DHS", "BD2000DHS"))
  # Every DHS round has a rule for every variable (possibly "unavailable").
  expect_true(all(lengths(lapply(crosswalk()$vars, function(x) x$rounds)) == 9L))
  expect_equal(wq$household_var, "hv270")

  r <- bdhs_harmonized_vars(2022)
  expect_equal(nrow(r), 112)
  expect_false(r$available[r$name == "vacc_bcg"])
  expect_equal(r$source[r$name == "division_surveyed"], "v024")
  expect_equal(unique(r$survey_id), "BD2022DHS")
})

test_that("division codes are harmonized with each round's rules", {
  kr14 <- sim(2014, "KR")
  h14 <- bdhs_harmonize(kr14, vars = c("division_surveyed", "division_trend6"))
  code <- codes(kr14$v024)
  expect_true(all(h14$division_trend6[code %in% c(5, 6)] == "rajshahi"))
  expect_true(all(h14$division_surveyed[code == 6] == "rangpur"))
  expect_true(all(h14$division_trend6[code == 7] == "sylhet"))

  kr22 <- sim(2022, "KR")
  h22 <- quiet(bdhs_harmonize(kr22, vars = c("division_surveyed", "division_trend6", "division_trend5")))
  code <- codes(kr22$v024)
  expect_true(all(h22$division_trend6[code == 5] == "dhaka"))
  expect_true(all(h22$division_trend6[code == 7] == "rajshahi"))
  expect_true(all(h22$division_surveyed[code == 6] == "rajshahi"))
  expect_true(all(h22$division_trend5[code == 8] == "chattogram"))
  expect_identical(levels(h22$division_surveyed), c(
    "barishal", "chattogram", "dhaka", "khulna", "mymensingh", "rajshahi", "rangpur", "sylhet"
  ))
  expect_false(anyNA(h22$division_trend6))
  expect_match(attr(h22$division_trend6, "label"), "trend")
})

test_that("1996-97 rules come from the DHS-III codebook", {
  kr97 <- sim(1997, "KR")
  h <- quiet(bdhs_harmonize(kr97, vars = c("division_surveyed", "division_trend6", "child_sex", "csection")))
  code <- codes(kr97$v024)
  expect_true(all(h$division_surveyed[code == 6] == "sylhet"))
  expect_true(all(h$division_trend6[code == 5] == "rajshahi"))
  expect_equal(as.integer(h$child_sex), as.integer(codes(kr97$b4)))
  expect_true(all(is.na(h$csection)))
  r <- bdhs_harmonized_vars(1997)
  expect_match(r$note[r$name == "religion"], "codebook")
})

test_that("household files only use true household equivalents", {
  pr <- sim(2022, "PR")
  h <- quiet(bdhs_harmonize(pr))
  expect_false(any(c("mother_age", "mother_agegroup", "religion") %in% names(h)))
  expect_true(all(c("residence", "division_trend6", "haz_who", "stunted_who") %in% names(h)))
})

test_that("pooled data are harmonized row by row", {
  p <- quiet(bdhs_pool(sim(2011, "KR"), sim(2022, "KR")))
  h <- bdhs_harmonize(p, vars = c("division_trend6", "child_age_months", "survey_id"))
  code <- codes(p$v024)
  s11 <- p$survey_id == "BD2011DHS"
  s22 <- p$survey_id == "BD2022DHS"
  expect_true(all(h$division_trend6[s11 & code == 6] == "rajshahi"))
  expect_true(all(h$division_trend6[s11 & code == 7] == "sylhet"))
  expect_true(all(h$division_trend6[s22 & code == 6] == "rajshahi"))
  expect_true(all(h$division_trend6[s22 & code == 7] == "rajshahi"))
  expect_true(all(h$division_trend6[s22 & code == 8] == "sylhet"))
  # 2011 uses v008 - b3; 2017-18 and 2022 use the day-precise b19.
  expect_equal(h$child_age_months[s11], as.integer(codes(p$v008) - codes(p$b3))[s11])
  expect_equal(h$child_age_months[s22], as.integer(codes(p$b19))[s22])
  expect_equal(h$survey_id, p$survey_id, ignore_attr = TRUE)
})

test_that("types follow the crosswalk", {
  ir <- sim(2017, "IR")
  h <- quiet(bdhs_harmonize(ir, vars = c("psu", "wt", "currently_married", "mother_agegroup", "interview_year")))
  expect_type(h$psu, "integer")
  expect_type(h$wt, "double")
  expect_equal(h$wt, codes(ir$v005) / 1e6, ignore_attr = TRUE)
  expect_type(h$currently_married, "integer")
  expect_true(all(h$currently_married %in% c(0L, 1L)))
  expect_equal(h$currently_married, as.integer(codes(ir$v502) == 1), ignore_attr = TRUE)
  expect_s3_class(h$mother_agegroup, "factor")
  expect_equal(levels(h$mother_agegroup)[2], "15_19")
  expect_true(all(h$interview_year %in% c(2017L, 2018L)))
})

test_that("unexpected codes become NA with a warning", {
  kr <- sim(2022, "KR")
  kr$v025[1:3] <- 7
  expect_warning(h <- bdhs_harmonize(kr, vars = "residence"), "unexpected code")
  expect_true(all(is.na(h$residence[1:3])))
  expect_false(anyNA(h$residence[-(1:3)]))
  expect_no_warning(bdhs_harmonize(kr, vars = "residence", warn = FALSE))
})

test_that("codes labelled as missing become NA silently", {
  kr <- sim(2022, "KR")
  x <- codes(kr$v025)
  x[1] <- 9
  kr$v025 <- haven::labelled(x, c(urban = 1, rural = 2, missing = 9), label = "type of place of residence")
  expect_no_warning(h <- bdhs_harmonize(kr, vars = "residence"))
  expect_true(is.na(h$residence[1]))
})

test_that("value-label drift is reported", {
  kr <- sim(2022, "KR")
  labs <- attr(kr$v024, "labels")
  names(labs)[labs == 5] <- "rajshahi"
  attr(kr$v024, "labels") <- labs
  expect_warning(bdhs_harmonize(kr, vars = "division_surveyed"), "labelled")
  # Variable-label drift on any_of sources (the m3* letters).
  attr(kr$m3a, "label") <- "assistance: traditional birth attendant"
  expect_warning(bdhs_harmonize(kr, vars = "assist_doctor"), "m3a")
})

test_that("rounds without the variable give NA with a message", {
  kr <- sim(2022, "KR")
  expect_message(h <- bdhs_harmonize(kr, vars = "vacc_bcg"), "not available")
  expect_true(all(is.na(h$vacc_bcg)))
  expect_type(h$vacc_bcg, "integer")

  kr17 <- sim(2017, "KR")
  h17 <- bdhs_harmonize(kr17, vars = "vacc_bcg")
  expect_equal(h17$vacc_bcg, as.integer(codes(kr17$h2) %in% 1:3) + 0L * codes(kr17$h2), ignore_attr = TRUE)
})

test_that("external WI variables are used once merged", {
  kr <- sim(1994, "KR")
  expect_message(h <- bdhs_harmonize(kr, vars = "wealth_quintile"), "WI")
  expect_true(all(is.na(h$wealth_quintile)))
  kr$wlthind5 <- codes(kr$v190)
  h <- bdhs_harmonize(kr, vars = "wealth_quintile")
  expect_equal(as.integer(h$wealth_quintile), as.integer(kr$wlthind5))
})

test_that("expression rules see only their variables and the lookups", {
  kr <- sim(1994, "KR")
  kr$sdistr <- rep(c(26, 91, 27, 6), length.out = nrow(kr))
  h <- bdhs_harmonize(kr, vars = c("division_backcast8", "division_trend6", "district_bbs"))
  expect_equal(as.character(h$division_backcast8[1:4]), c("dhaka", "sylhet", "rangpur", "barishal"))
  expect_true(all(h$division_trend6[kr$sdistr == 91] == "sylhet"))
  expect_true(all(h$division_trend6[kr$sdistr != 91] != "sylhet"))
  expect_equal(h$district_bbs, as.integer(kr$sdistr), ignore_attr = TRUE)
})

test_that("any_of, derive and household-file rules work", {
  kr <- sim(2022, "KR")
  h <- quiet(bdhs_harmonize(kr, vars = c("assist_doctor", "assist_none", "anc_4plus", "csection")))
  expect_equal(h$assist_doctor, as.integer(codes(kr$m3a) == 1), ignore_attr = TRUE)
  expect_equal(h$csection, as.integer(codes(kr$m17)), ignore_attr = TRUE)
  m14 <- codes(kr$m14)
  expect_equal(h$anc_4plus, ifelse(m14 >= 4, 1L, ifelse(is.na(m14), NA, 0L)), ignore_attr = TRUE)

  hr <- sim(2022, "HR")
  hh <- quiet(bdhs_harmonize(hr, vars = c("water_improved", "residence", "division_trend6")))
  expect_equal(hh$water_improved, as.integer(codes(hr$hv201) != 43), ignore_attr = TRUE)
  expect_true(all(hh$division_trend6[codes(hr$hv024) == 5] == "dhaka"))
  expect_false("water_source_cat" %in% names(hh))

  pr <- sim(2022, "PR")
  hp <- quiet(bdhs_harmonize(pr, vars = c("haz_who", "stunted_who")))
  raw <- codes(pr$hc70)
  expect_true(all(is.na(hp$haz_who[raw >= 9996 & !is.na(raw)])))
  ok <- !is.na(raw) & raw < 9996
  expect_equal(hp$haz_who[ok], raw[ok] / 100)
  expect_equal(hp$stunted_who[ok], as.integer(raw[ok] / 100 < -2))
})

test_that("absent sources, factor sources and unknown rounds are reported", {
  kr <- sim(2022, "KR")
  expect_message(h <- bdhs_harmonize(kr, vars = c("residence", "religion")), "Skipped 1 variable")
  expect_true("residence" %in% names(h))
  expect_false("religion" %in% names(h))

  kf <- kr
  kf$v025 <- haven::as_factor(kf$v025)
  expect_message(h <- bdhs_harmonize(kf, vars = "residence"), "factor")
  expect_false("residence" %in% names(h))

  expect_message(h <- bdhs_harmonize(kr, vars = "residence", survey = "spa2014"), "No verified harmonization rules")
  expect_false("residence" %in% names(h))
  expect_error(bdhs_harmonize(kr, vars = "not_a_variable"), "Unknown harmonized variable")
  expect_error(bdhs_harmonize(list(a = 1)), "data frame")
})

test_that("bdhs_harmonize() keeps data, attributes and adds a prefix", {
  kr <- sim(2022, "KR")
  h <- bdhs_harmonize(kr, vars = c("residence", "wt"), prefix = "h_")
  expect_s3_class(h, "tbl_df")
  expect_equal(ncol(h), ncol(kr) + 2)
  expect_equal(h[names(kr)], kr, ignore_attr = TRUE)
  expect_equal(attr(h, "bdhs_survey_id"), "BD2022DHS")
  expect_equal(attr(h, "bdhs_recode"), "KR")
  expect_equal(attr(h, "bdhs_harmonized"), c("h_residence", "h_wt"))
  h2 <- bdhs_harmonize(h, vars = "residence", prefix = "h_")
  expect_equal(ncol(h2), ncol(h))

  # Every applicable variable for a whole file.
  ir <- sim(2022, "IR")
  hi <- quiet(bdhs_harmonize(ir))
  expect_true(all(c("division_trend6", "religion", "currently_married", "wealth_quintile") %in% names(hi)))
  expect_false(any(c("child_sex", "csection") %in% names(hi)))
})

# Local data (aggregates only) ---------------------------------------------------

local_kr_files <- c(
  BD1994DHS = "BDHS Caeserean/Data/BDHS1993KR.DTA",
  BD2000DHS = "BDHS Caeserean/Data/BDHS1999KR.DTA",
  BD2004DHS = "BDHS Caeserean/Data/BDHS2004KR.DTA",
  BD2007DHS = "BDHS Caeserean/Data/BDHS2007KR.DTA",
  BD2011DHS = "BDHS Caeserean/Data/BDHS2011KR.DTA",
  BD2014DHS = "BDHS Caeserean/Data/BDHS2014KR.DTA",
  BD2017DHS = "BDHS NPD/BDKR7RDT/BDKR7RFL.DTA",
  BD2022DHS = "BDHS Caeserean/Data/BDHS2022KR.DTA"
)

read_local <- function(rel, survey_id, recode) {
  path <- file.path(local_data_dir(), rel)
  testthat::skip_if_not(file.exists(path), paste("missing", rel))
  d <- lower_names(haven::read_dta(path))
  attr(d, "bdhs_survey_id") <- survey_id
  attr(d, "bdhs_recode") <- recode
  d
}

wpct <- function(x, w) 100 * sum(w[!is.na(x)] * x[!is.na(x)]) / sum(w[!is.na(x)])

test_that("local KR files harmonize and reproduce official aggregates", {
  skip_if_no_local_data()
  # Official DHS API values (CH_DIAR_C_DIA, CH_VACC_C_DP3, urban share of living children).
  diarrhea <- c(
    BD1994DHS = 12.4, BD2000DHS = 6.1, BD2004DHS = 7.5, BD2007DHS = 9.8,
    BD2011DHS = 4.6, BD2014DHS = 5.7, BD2017DHS = 4.7, BD2022DHS = 4.8
  )
  dpt3 <- c(
    BD1994DHS = 66.0, BD2000DHS = 72.1, BD2004DHS = 81.0, BD2007DHS = 91.1,
    BD2011DHS = 93.4, BD2014DHS = 91.3, BD2017DHS = 95.9
  )
  urban <- c(
    BD2000DHS = 16.5, BD2004DHS = 19.7, BD2007DHS = 20.9, BD2011DHS = 22.3,
    BD2014DHS = 25.6, BD2017DHS = 27.4, BD2022DHS = 27.0
  )
  for (sid in names(local_kr_files)) {
    kr <- read_local(local_kr_files[[sid]], sid, "KR")
    expect_no_warning(h <- suppressMessages(bdhs_harmonize(kr)))
    expect_true(all(c("division_trend6", "residence", "child_sex") %in% names(h)), info = sid)
    expect_false(anyNA(h$division_trend6), info = sid)
    alive <- h$child_alive %in% 1
    age <- h$child_age_months
    expect_lt(abs(wpct(h$diarrhea_2wk[alive], h$wt[alive]) - diarrhea[[sid]]), 0.15)
    if (sid %in% names(dpt3)) {
      ok <- alive & age >= 12 & age <= 23
      expect_lt(abs(wpct(h$vacc_dpt3[ok] %in% 1, h$wt[ok]) - dpt3[[sid]]), 0.15)
    }
    if (sid %in% names(urban)) {
      expect_lt(abs(wpct(h$residence[alive] == "urban", h$wt[alive]) - urban[[sid]]), 0.15)
    }
  }
})

test_that("local IR and PR files harmonize without unexpected codes", {
  skip_if_no_local_data()
  files <- list(
    list("BDHS NPD/BDIR7RDT/BDIR7RFL.DTA", "BD2017DHS", "IR"),
    list("BDHS Data/BDIR81FL.DTA", "BD2022DHS", "IR"),
    list("BDHS NPD/BDPR7RDT/BDPR7RFL.DTA", "BD2017DHS", "PR", 28.3),
    list("BDHS Pre-hypertension/BDPR81FL.DTA", "BD2022DHS", "PR", 28.4)
  )
  for (f in files) {
    d <- read_local(f[[1]], f[[2]], f[[3]])
    expect_no_warning(h <- suppressMessages(bdhs_harmonize(d)))
    expect_false(anyNA(h$division_surveyed))
    if (f[[3]] == "PR") {
      first <- !duplicated(d$hhid)
      # Official urban share of households (HC_ELEC_H_ELC denominators)
      expect_lt(abs(wpct(h$residence[first] == "urban", h$wt[first]) - f[[4]]), 0.15)
    }
  }
})
