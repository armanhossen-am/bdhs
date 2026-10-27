test_that("the registry is well formed", {
  reg <- indicator_registry()
  expect_gte(length(reg), 60)
  ids <- names(reg)
  expect_false(anyDuplicated(ids) > 0)
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", ids)))
  for (ind in reg) {
    expect_s3_class(ind, "bdhs_indicator")
    expect_true(all(ind$rounds %in% bdhs_survey_id(type = "DHS")), info = ind$id)
    expect_true(ind$type %in% c("proportion", "mean", "median", "rate"), info = ind$id)
    if (ind$type != "rate") {
      expect_true(is.function(ind$universe) && is.function(ind$value), info = ind$id)
    }
  }
})

test_that("every DHS API id in the registry exists in the bundled official data", {
  api <- unique(unlist(lapply(indicator_registry(), `[[`, "api_id")))
  api <- api[!is.na(api)]
  expect_gt(length(api), 50)
  expect_true(all(api %in% bdhs_indicators$indicator_id), info = paste(setdiff(api, bdhs_indicators$indicator_id), collapse = ", "))
})

test_that("bdhs_indicator_list() describes and filters the registry", {
  l <- bdhs_indicator_list()
  expect_s3_class(l, "tbl_df")
  expect_gte(nrow(l), 60)
  expect_true(all(c("id", "label", "topic", "recode", "type", "api_id", "all_women", "window",
                    "rounds", "validated", "definition", "notes", "reference") %in% names(l)))
  expect_type(l$rounds, "list")
  fp <- bdhs_indicator_list(topic = "family_planning")
  expect_true(all(fp$topic == "family_planning"))
  expect_true("fp_cpr_modern" %in% fp$id)
  kr <- bdhs_indicator_list(recode = "kr")
  expect_true(all(grepl("KR", kr$recode)))
  expect_true(all(c("rh_del_skilled", "ch_vac_basic") %in% kr$id))
  # Women's anthropometry can be computed from PR or IR.
  expect_true("nt_wm_thin" %in% bdhs_indicator_list(recode = "IR")$id)
  expect_true("nt_wm_thin" %in% bdhs_indicator_list(recode = "PR")$id)
  expect_equal(l$all_women[l$id == "ms_married_by_18"], "awfactt")
  expect_true(is.na(l$all_women[l$id == "fp_cpr_any"]))
  expect_output(print(indicator_registry()[["fe_tfr"]]), "fe_tfr")
})

test_that("bdhs_add_indicators() adds 0/1 columns, NA outside the universe", {
  kr <- bdhs_simulate(2017, "KR", n_clusters = 10, households = 10)
  out <- bdhs_add_indicators(kr, c("rh_del_facility", "rh_anc_4plus", "ch_diarrhoea"))
  expect_equal(nrow(out), nrow(kr))
  age <- kr$b19
  m15 <- as.numeric(kr$m15)
  expect_true(all(is.na(out$rh_del_facility[age >= 36])))
  in_u <- age < 36
  expect_equal(out$rh_del_facility[in_u], as.numeric(m15[in_u] >= 20 & m15[in_u] < 96))
  expect_true(all(is.na(out$rh_anc_4plus[kr$midx != 1])))
  expect_true(all(out$ch_diarrhoea[kr$b5 == 1] %in% c(0, 1)))
  expect_true(all(is.na(out$ch_diarrhoea[kr$b5 == 0])))
  expect_equal(attr(out$rh_del_facility, "label"), "Delivery in a health facility")

  ir <- bdhs_simulate(2017, "IR", n_clusters = 6, households = 6)
  out <- bdhs_add_indicators(ir, "ms_married_by_18")
  expect_equal(attr(out$ms_married_by_18, "bdhs_all_women"), "awfactt")
  expect_message(bdhs_add_indicators(ir, c("fe_tfr", "fp_cpr_any")), "Skipping rate")
})

test_that("bdhs_add_indicators() checks ids, recodes and rounds", {
  kr <- bdhs_simulate(2022, "KR", n_clusters = 4, households = 4)
  expect_error(bdhs_add_indicators(kr, "nope"), "Unknown indicator")
  expect_error(bdhs_add_indicators(kr, "fp_cpr_any"), "not computed from")
  expect_warning(out <- bdhs_add_indicators(kr, "ch_vac_bcg"), "not available")
  expect_true(all(is.na(out$ch_vac_bcg)))
  kr$m15 <- haven::as_factor(kr$m15)
  expect_error(bdhs_add_indicators(kr, "rh_del_facility"), "numeric DHS codes")
})

test_that("skilled-provider letters follow the round", {
  kr <- bdhs_simulate(2017, "KR", n_clusters = 6, households = 6)
  kr$m3a[] <- 0
  kr$m3b[] <- 0
  kr$m3c[] <- 1 # FWV: skilled from 2011
  v17 <- bdhs_add_indicators(kr, "rh_del_skilled")$rh_del_skilled
  expect_true(all(v17[!is.na(v17)] == 1))
  attr(kr, "bdhs_survey_id") <- "BD2004DHS"
  v04 <- bdhs_add_indicators(kr, "rh_del_skilled")$rh_del_skilled
  expect_true(all(v04[!is.na(v04)] == 0))
})

test_that("2022 long/short questionnaire is respected", {
  ir <- bdhs_simulate(2022, "IR", n_clusters = 10, households = 10)
  short <- seq_len(nrow(ir)) %% 3 == 0
  ir$sqtype <- haven::labelled(ifelse(short, 2, 1), c("long questionnaire" = 1, "short questionnaire" = 2))
  ir$v313[short] <- NA
  ir$v511[short] <- NA
  out <- bdhs_add_indicators(ir, c("fp_cpr_any", "ms_married_by_18", "fe_teen_mothers"))
  expect_true(all(is.na(out$fp_cpr_any[short])))
  expect_true(all(is.na(out$ms_married_by_18[short])))
  # Birth history is in both questionnaires.
  teen <- ir$v012 <= 19
  expect_true(all(!is.na(out$fe_teen_mothers[short & teen])))
  # Without sqtype, missing module variables identify short-questionnaire women.
  ir$sqtype <- NULL
  out2 <- bdhs_add_indicators(ir, "fp_cpr_any")
  expect_true(all(is.na(out2$fp_cpr_any[short])))
  expect_equal(long_questionnaire(ir, "BD2017DHS"), rep(TRUE, nrow(ir)))
})

test_that("round-specific definitions are applied", {
  kr <- bdhs_simulate(2014, "KR", n_clusters = 6, households = 6)
  n <- nrow(kr)
  kr$h11 <- haven::labelled(rep(2, n), c(no = 0, yes = 2))
  kr$h15j <- rep(1, n)
  kr$h15e <- rep(0, n)
  z14 <- bdhs_add_indicators(kr, "ch_diarrhoea_zinc")$ch_diarrhoea_zinc
  expect_true(all(z14[!is.na(z14)] == 1))
  attr(kr, "bdhs_survey_id") <- "BD2017DHS"
  z17 <- bdhs_add_indicators(kr, "ch_diarrhoea_zinc")$ch_diarrhoea_zinc
  expect_true(all(z17[!is.na(z17)] == 0))
  # ARI: from 2017 the cough condition is not required.
  kr$h31 <- rep(0, n)
  kr$h31b <- rep(1, n)
  kr$h31c <- rep(1, n)
  expect_true(all(ari_symptoms(kr, "BD2017DHS") == 1))
  expect_true(all(ari_symptoms(kr, "BD2011DHS") == 0))
  # Electricity 2022 counts solar.
  pr <- data.frame(hv102 = c(1, 1, 1), hv206 = c(1, 0, 0), sh132g = c(0, 1, 0))
  e <- indicator_registry()$hh_electricity
  expect_equal(indicator_values(e, pr, "BD2022DHS"), c(1, 1, 0))
  expect_equal(indicator_values(e, pr, "BD2017DHS"), c(1, 0, 0))
})

test_that("postnatal care timing follows the DHS-7 algorithm", {
  d <- data.frame(
    m62 = c(1, 1, 0, 0, 0), m63 = c(102, 305, NA, NA, NA), m64 = c(11, 11, NA, NA, NA),
    m66 = c(0, 0, 1, 1, 0), m67 = c(NA, NA, 201, 101, NA), m68 = c(NA, NA, 12, 22, NA)
  )
  expect_equal(pnc_mother_2days(d, "BD2017DHS"), c(1, 0, 1, 0, 0))
  old <- data.frame(m50 = c(1, 1, 0), m51 = c(110, 230, NA), m52 = c(21, 11, NA))
  expect_equal(pnc_mother_2days(old, "BD2014DHS"), c(1, 0, 0))
})

test_that("indicator_columns() lists the variables to read", {
  cols <- indicator_columns(c("rh_del_facility", "fe_tfr"), c("v001", "v005", "m15", "b3_01", "b7_01", "zzz", "awfactt"))
  expect_true(all(c("v005", "m15", "b3_01", "awfactt") %in% cols))
  expect_false("zzz" %in% cols)
})
