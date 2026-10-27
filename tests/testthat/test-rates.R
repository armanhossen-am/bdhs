quietly <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(expr))
  out
}

test_that("fertility rates reproduce DHS.rates on simulated data", {
  skip_if_not_installed("DHS.rates")
  skip_if_not_installed("reshape")
  ir <- bdhs_simulate(2017, "IR", n_clusters = 30, households = 12)
  fd <- as.data.frame(haven::zap_labels(ir))
  tfr <- quietly(DHS.rates::fert(fd, Indicator = "tfr", EverMW = "Yes", AWFact = "awfactt", JK = "Yes"))
  ours <- bdhs_fertility(ir)
  expect_equal(ours$estimate, unname(tfr[1, "TFR"]), tolerance = 1e-3)
  expect_equal(ours$se, unname(tfr[1, "SE"]), tolerance = 1e-2)
  asfr <- quietly(DHS.rates::fert(fd, Indicator = "asfr", EverMW = "Yes", AWFact = "awfactt"))
  expect_equal(bdhs_fertility(ir, "asfr", se = FALSE)$estimate, asfr$ASFR, tolerance = 1e-4)
  gfr <- quietly(DHS.rates::fert(fd, Indicator = "gfr", EverMW = "Yes", AWFact = "awfactt"))
  expect_equal(bdhs_fertility(ir, "gfr", se = FALSE)$estimate, unname(gfr[1, "GFR"]), tolerance = 1e-4)
  # Subgroups with the matching all-woman factor.
  ir$awfactu <- ir$awfactt * c(1, 1.2)[as.numeric(ir$v025)]
  fd <- as.data.frame(haven::zap_labels(ir))
  cls <- quietly(DHS.rates::fert(fd, Indicator = "tfr", EverMW = "Yes", AWFact = "awfactu", Class = "v025"))
  by <- bdhs_fertility(ir, by = "v025", se = FALSE)
  expect_equal(by$estimate, as.numeric(cls$TFR), tolerance = 1e-3)
})

test_that("mortality rates reproduce DHS.rates on simulated data", {
  skip_if_not_installed("DHS.rates")
  skip_if_not_installed("matrixStats")
  br <- bdhs_simulate(2017, "BR", n_clusters = 30, households = 12)
  bd <- as.data.frame(haven::zap_labels(br))
  ref <- quietly(DHS.rates::chmort(bd, JK = "Yes"))
  ours <- bdhs_mortality(br)
  expect_equal(ours$indicator, c("nnmr", "pnnmr", "imr", "cmr", "u5mr"))
  expect_equal(ours$estimate, unname(ref[, "R"]), tolerance = 1e-3)
  expect_equal(ours$se, unname(ref[, "SE"]), tolerance = 1e-2)
  ref10 <- quietly(DHS.rates::chmort(bd, Period = 120))
  expect_equal(bdhs_mortality(br, period = 10, se = FALSE)$estimate, unname(ref10[, "R"]), tolerance = 1e-3)
})

test_that("mortality from an IR birth history equals mortality from BR", {
  br <- bdhs_simulate(2017, "BR", n_clusters = 15, households = 10)
  # Rebuild a wide birth history (b3_01, b7_01, ...) from the births recode.
  br$k <- sprintf("%02d", as.integer(br$bidx))
  women <- unique(as.data.frame(haven::zap_labels(br[, c("caseid", "v005", "v008", "v021", "v022", "v025")])))
  wide <- women
  for (k in sort(unique(br$k))) {
    b <- br[br$k == k, ]
    i <- match(wide$caseid, b$caseid)
    wide[[paste0("b3_", k)]] <- as.numeric(b$b3)[i]
    wide[[paste0("b7_", k)]] <- as.numeric(b$b7)[i]
  }
  attr(wide, "bdhs_survey_id") <- "BD2017DHS"
  attr(wide, "bdhs_recode") <- "IR"
  a <- bdhs_mortality(br, se = FALSE)
  b <- bdhs_mortality(wide, se = FALSE)
  expect_equal(a$estimate, b$estimate)
  expect_equal(a$n_births, b$n_births)
  g <- bdhs_mortality(wide, rates = c("imr", "u5mr"), by = "v025")
  expect_equal(nrow(g), 4)
  expect_true(all(g$se > 0))
})

test_that("rate functions validate their inputs", {
  ir <- bdhs_simulate(2017, "IR", n_clusters = 6, households = 6)
  expect_error(bdhs_fertility(ir, period = -1), "positive")
  expect_error(bdhs_fertility(ir, by = "nope"), "not found")
  expect_error(bdhs_fertility(ir[, c("v001", "v005")], survey = 2017), "birth history")
  expect_error(bdhs_mortality(ir), "ages at death")
  expect_error(bdhs_mortality(ir, rates = "nope"))
  expect_error(bdhs_fertility(ir, awfact = "nope"), "not found")
  noaw <- bdhs_fertility(ir, awfact = FALSE, se = FALSE)$estimate
  aw <- bdhs_fertility(ir, se = FALSE)$estimate
  expect_gt(noaw, aw)
  expect_equal(awfact_for_by("v024"), "awfactr")
  expect_equal(awfact_for_by(c("v024", "v025")), "awfactt")
})

test_that("fertility output is tidy", {
  ir <- bdhs_simulate(2017, "IR", n_clusters = 10, households = 8)
  a <- bdhs_fertility(ir, "asfr", se = FALSE)
  expect_equal(a$age_group, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))
  expect_true(all(is.na(a$se)))
  expect_named(a, c("survey_id", "indicator", "age_group", "estimate", "se", "ci_low", "ci_high",
                    "n_births", "exposure", "n_women", "n_psu", "period"))
  # TFR is five times the sum of the ASFRs (per 1,000).
  expect_equal(bdhs_fertility(ir, se = FALSE)$estimate, 5 * sum(a$estimate) / 1000)
  short <- bdhs_fertility(ir, period = 12, se = FALSE)
  expect_equal(short$period, 12L)
  g <- bdhs_fertility(ir, "gfr", by = "v025")
  expect_equal(as.character(g$v025), c("urban", "rural"))
})
