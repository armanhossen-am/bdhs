# Calibrated synthetic data: offline, built from the bundled official values.

synth_cache <- new.env(parent = emptyenv())
synth <- function(survey, recode, ...) {
  key <- paste(survey, recode, ...)
  if (is.null(synth_cache[[key]])) {
    synth_cache[[key]] <- bdhs_synthesize(survey, recode, scale = 0.4, ...)
  }
  synth_cache[[key]]
}

test_that("calibrated margins reproduce the official values (2017-18, 2022, 2004)", {
  skip_on_cran()
  for (s in c(2017, 2022, 2004)) {
    for (r in c("KR", "IR", "PR")) {
      x <- synth(s, r)
      f <- bdhs_synthetic_fidelity(x)
      expect_true(all(c("national", "division", "residence") %in% f$margin), info = paste(s, r))
      expect_false(anyNA(f$synthetic), info = paste(s, r))
      expect_lt(max(abs(f$diff)), 0.5, label = paste(s, r, "max |synthetic - official|"))
      # The stored fidelity table matches a fresh computation.
      expect_equal(attr(x, "bdhs_fidelity")$synthetic, f$synthetic)
    }
  }
})

test_that("default targets follow the round and recode", {
  skip_on_cran()
  ids <- function(s, r) unique(attr(synth(s, r), "bdhs_targets")$indicator)
  expect_setequal(ids(2022, "KR"), c("rh_del_facility", "rh_del_csection", "rh_anc_4plus", "ch_diarrhoea"))
  expect_true(all(c("ch_vac_bcg", "ch_vac_dpt3", "ch_vac_measles") %in% ids(2017, "KR")))
  expect_setequal(ids(2017, "IR"), c("fp_cpr_any", "fp_cpr_modern", "fp_unmet_total", "ms_married_by_18"))
  expect_true(all(c("nt_stunting", "hh_electricity", "nt_wm_thin") %in% ids(2022, "PR")))
  # 2022 divisions are matched to the eight divisions as surveyed.
  tg <- attr(synth(2022, "KR"), "bdhs_targets")
  expect_setequal(tg$category[tg$margin == "division"],
                  c("barishal", "chattogram", "dhaka", "khulna", "mymensingh", "rajshahi", "rangpur", "sylhet"))
  expect_false(any(grepl("/| before ", tg$official_label)))
})

test_that("an older round with parent region labels is calibrated", {
  skip_on_cran()
  x <- bdhs_synthesize(1994, "IR", scale = 0.4)
  f <- bdhs_synthetic_fidelity(x)
  expect_equal(sum(f$margin == "division") / length(unique(f$indicator)), 5)
  expect_lt(max(abs(f$diff)), 0.5)
  kr <- bdhs_synthesize(1994, "KR", targets = "rh_del_facility", scale = 0.4)
  expect_true(all(kr$b19 < 36)) # 1993-94 KR: births in the last 3 years
  expect_lt(max(abs(bdhs_synthetic_fidelity(kr)$diff)), 0.5)
})

test_that("synthetic source variables stay coherent", {
  skip_on_cran()
  kr <- synth(2017, "KR")
  cs <- kr$m17 == 1 & kr$b19 < 36
  expect_true(all(kr$m15[which(cs)] >= 20))
  ir <- synth(2017, "IR")
  married <- ir$v502 == 1
  expect_true(all(ir$v626a[married & ir$v313 > 0] %in% 3:4))
  expect_true(all(ir$v313[married & ir$v626a %in% 1:2] == 0))
  expect_true(all((ir$v313 == 3) == (ir$v312 %in% c(1, 2, 3, 5, 6, 7, 11))))
  pr <- synth(2017, "PR")
  by_hh <- tapply(as.numeric(pr$hv206), pr$hhid, function(v) length(unique(v)))
  expect_true(all(by_hh == 1))
  expect_true(isTRUE(attr(pr, "bdhs_synthetic")))
  expect_true(isTRUE(attr(pr, "bdhs_simulated")))
  expect_equal(mean(pr$hv005), 1e6, tolerance = 1e-3)
})

test_that("bdhs_synthesize() is reproducible and leaves the RNG alone", {
  skip_on_cran()
  a <- bdhs_synthesize(2022, "IR", targets = "fp_cpr_modern", scale = 0.1, seed = 3)
  b <- bdhs_synthesize(2022, "IR", targets = "fp_cpr_modern", scale = 0.1, seed = 3)
  expect_identical(a, b)
  c <- bdhs_synthesize(2022, "IR", targets = "fp_cpr_modern", scale = 0.1, seed = 4)
  expect_false(identical(a$v313, c$v313))
  set.seed(42)
  u1 <- stats::runif(1)
  set.seed(42)
  invisible(bdhs_synthesize(2022, "KR", targets = "ch_diarrhoea", scale = 0.1))
  expect_identical(stats::runif(1), u1)
})

test_that("national-only and wealth margins work", {
  skip_on_cran()
  x <- bdhs_synthesize(2017, "KR", targets = "rh_del_csection", margins = NULL, scale = 0.2)
  f <- bdhs_synthetic_fidelity(x)
  expect_equal(f$margin, "national")
  expect_lt(abs(f$diff), 0.1)
  w <- bdhs_synthesize(2017, "IR", targets = "fp_cpr_modern", margins = c("residence", "wealth"), scale = 0.5)
  fw <- bdhs_synthetic_fidelity(w)
  expect_equal(sum(fw$margin == "wealth"), 5)
  expect_lt(max(abs(fw$diff)), 0.5)
})

test_that("bdhs_synthesize() validates its arguments", {
  expect_error(bdhs_synthesize(2022, "BR"))
  expect_error(bdhs_synthesize(2022, "KR", targets = "fp_cpr_modern"), "Cannot calibrate")
  expect_error(bdhs_synthesize(2022, "KR", margins = "district"), "margins")
  expect_error(bdhs_synthesize(2022, "KR", scale = 0), "scale")
  expect_error(bdhs_synthesize(c(2017, 2022), "KR"), "single")
  expect_error(bdhs_synthetic_fidelity(bdhs_simulate(2022, "KR")), "not calibrated")
  expect_error(
    expect_warning(bdhs_synthesize(2022, "KR", targets = "ch_vac_bcg", scale = 0.1), "no official value"),
    "No calibration target"
  )
})
