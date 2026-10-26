official_cols <- c(
  "survey_id", "year_label", "indicator_id", "indicator", "characteristic_category",
  "characteristic_label", "value", "ci_low", "ci_high", "denom_weighted",
  "denom_unweighted", "region_id", "breakdown"
)

test_that("bdhs_official() returns tidy national estimates ordered by survey year", {
  tfr <- bdhs_official("FE_FRTR_W_TFR")
  expect_s3_class(tfr, "tbl_df")
  expect_named(tfr, official_cols)
  expect_equal(nrow(tfr), 9)
  expect_equal(tfr$survey_id, bdhs_survey_id(type = "DHS"))
  expect_equal(tfr$year_label[c(1, 3, 8)], c("1993-94", "1999-2000", "2017-18"))
  expect_true(all(tfr$breakdown == "national"))
  expect_equal(tfr$value[tfr$survey_id == "BD2022DHS"], 2.3)
  expect_equal(tfr$value[tfr$survey_id == "BD2017DHS"], 2.3)
})

test_that("bdhs_official() filters surveys, breakdowns and characteristics", {
  st <- bdhs_official("CN_NUTS_C_HA2", survey = c("2017-18", 2022), characteristic = "Wealth quintile")
  expect_equal(unique(st$survey_id), c("BD2017DHS", "BD2022DHS"))
  expect_equal(unique(st$characteristic_category), "Wealth quintile")
  expect_equal(st$characteristic_label[1:5], c("Lowest", "Second", "Middle", "Fourth", "Highest"))

  # Case-insensitive, with friendly synonyms.
  expect_equal(
    bdhs_official("CN_NUTS_C_HA2", survey = 2022, characteristic = "wealth"),
    bdhs_official("CN_NUTS_C_HA2", survey = 2022, characteristic = "WEALTH QUINTILE")
  )

  sub <- bdhs_official("FE_FRTR_W_TFR", survey = 2022, characteristic = "division")
  expect_true(all(sub$breakdown == "subnational"))
  expect_false(any(startsWith(sub$characteristic_label, ".")))
  expect_true(all(c("Mymensingh", "Rangpur", "Dhaka before 2015") %in% sub$characteristic_label))
  expect_true(all(grepl("^BDDHS2022", sub$region_id)))

  bg <- bdhs_official("FE_FRTR_W_TFR", survey = 2022, breakdown = "background")
  expect_true(all(bg$breakdown == "background"))
  expect_true("Residence" %in% bg$characteristic_category)

  all_rows <- bdhs_official("FE_FRTR_W_TFR", survey = 2022, breakdown = "all")
  expect_equal(nrow(all_rows), nrow(bg) + nrow(sub) + 1)
  # National first, then background, then subnational rows.
  expect_equal(all_rows$breakdown[[1]], "national")
})

test_that("bdhs_official() can return non-preferred reference periods", {
  pref <- bdhs_official("CH_DIFP_C_FAL", survey = 2022)
  all <- bdhs_official("CH_DIFP_C_FAL", survey = 2022, preferred = FALSE)
  expect_false("by_variable_label" %in% names(pref))
  expect_true("by_variable_label" %in% names(all))
  expect_gt(nrow(all), nrow(pref))
  expect_true(all(c("Three years preceding the survey", "Five years preceding the survey") %in% all$by_variable_label))
})

test_that("bdhs_official() searches labels and definitions", {
  st <- bdhs_official(search = "stunt", survey = 2022)
  expect_true(all(c("CN_NUTS_C_HA2", "CN_NUTS_C_HA3") %in% st$indicator_id))
  # Case-insensitive.
  expect_equal(bdhs_official(search = "STUNT", survey = 2022), st)
  # indicator and search intersect.
  both <- bdhs_official("CN_NUTS_C_HA2", search = "stunt", survey = 2022)
  expect_equal(unique(both$indicator_id), "CN_NUTS_C_HA2")
  anemia <- bdhs_official(search = "anemia", survey = 2011, breakdown = "subnational")
  expect_true(all(anemia$breakdown == "subnational"))
  catalog <- bdhs_indicator_catalog[bdhs_indicator_catalog$indicator_id %in% anemia$indicator_id, ]
  expect_true(all(grepl("anemia", paste(catalog$label, catalog$definition), ignore.case = TRUE)))
})

test_that("bdhs_official() gives helpful errors", {
  expect_error(bdhs_official("FE_FRTR_W_TRF"), "Unknown DHS indicator")
  expect_error(bdhs_official("FE_FRTR_W_TRF"), "FE_FRTR_W_TFR")
  expect_error(bdhs_official(search = "zzzqqqxx"), "bdhs_search")
  expect_error(bdhs_official("FE_FRTR_W_TFR", survey = "spa2014"), "No official estimates are bundled")
  expect_error(bdhs_official("FE_FRTR_W_TFR", characteristic = "Sex"), "Available")
  expect_error(bdhs_official("FE_FRTR_W_TFR", breakdown = "regional"), "breakdown")
  expect_error(bdhs_official(1), "character vector")
  expect_error(bdhs_official("CN_NUTS_C_HA2", survey = 1994), "No official estimates match")
})
