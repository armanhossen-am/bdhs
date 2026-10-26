test_that("bdhs_search() finds indicators and harmonized variables", {
  res <- bdhs_search("stunt")
  expect_s3_class(res, "bdhs_search")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("source", "id", "label", "recode", "surveys"))
  expect_true(all(c("CN_NUTS_C_HA2", "stunted_who") %in% res$id))
  expect_setequal(unique(res$source), c("indicator", "harmonized"))
  # Case-insensitive.
  expect_equal(nrow(bdhs_search("STUNT")), nrow(res))
})

test_that("bdhs_search() searches variable names and labels with filters", {
  res <- bdhs_search("v024", where = "variables")
  expect_equal(unique(res$source), "variable")
  expect_true(all(c("v024", "hv024") %in% res$id))
  # Exact name matches come first.
  expect_equal(res$id[[1]], "v024")

  ir <- bdhs_search("region|division", where = "variables", recode = "IR")
  expect_true(all(ir$recode == "IR"))
  expect_true("v024" %in% ir$id)

  old <- bdhs_search("v024", where = "variables", survey = 1994)
  expect_equal(old$recode, "KR")
  expect_match(old$surveys, "BD1994DHS")
})

test_that("bdhs_search() filters indicators by survey", {
  res <- bdhs_search("anemia", where = "indicators", survey = 2011)
  expect_gt(nrow(res), 0)
  expect_true(all(res$source == "indicator"))
  expect_true(all(grepl("BD2011DHS", res$surveys)))
  expect_true(all(is.na(res$recode)))
})

test_that("bdhs_search() can search value labels", {
  expect_equal(nrow(bdhs_search("rajshani", where = "variables")), 0)
  res <- bdhs_search("rajshani", where = "variables", value_labels = TRUE)
  expect_true("v024" %in% res$id)
})

test_that("bdhs_search() handles invalid regular expressions and empty results", {
  expect_no_error(res <- bdhs_search("(", where = "harmonized"))
  expect_gt(nrow(res), 0)
  none <- bdhs_search("zzzqqqxx")
  expect_equal(nrow(none), 0)
  expect_named(none, c("source", "id", "label", "recode", "surveys"))
  expect_error(bdhs_search(""), "non-empty")
  expect_error(bdhs_search("x", where = "everything"), "where")
})

test_that("bdhs_search() prints compactly", {
  res <- bdhs_search("stunt")
  expect_output(print(res), "bdhs search for \"stunt\"")
  expect_output(print(res), "Indicators \\(2\\)")
  expect_output(print(res), "CN_NUTS_C_HA2")
  many <- bdhs_search("vaccin", where = "indicators")
  expect_output(print(many, n = 2), "more")
  expect_output(print(bdhs_search("zzzqqqxx")), "No matches")
})

test_that("bdhs_variable() shows a variable across rounds and flags label drift", {
  v <- bdhs_variable("V024")
  expect_s3_class(v, "tbl_df")
  expect_named(v, c("survey_id", "recode", "label", "n_valid", "value_labels", "label_drift"))
  expect_true(all(c("BD1994DHS", "BD2022DHS") %in% v$survey_id))
  # Chronological order.
  expect_false(is.unsorted(match(v$survey_id, bdhs_survey_id(type = "DHS"))))
  expect_false(anyDuplicated(v[, c("survey_id", "recode")]) > 0)
  latest <- v[v$survey_id == "BD2022DHS", ]
  expect_true(all(grepl("2=chattogram", latest$value_labels)))
  expect_false(any(latest$label_drift))
  # 1993-94 has five divisions and the "rajshani" typo.
  expect_true(v$label_drift[v$survey_id == "BD1994DHS"][[1]])
  expect_match(v$value_labels[v$survey_id == "BD1994DHS"][[1]], "5=rajshani")

  kr <- bdhs_variable("v024", recode = "KR")
  expect_true(all(kr$recode == "KR"))
})

test_that("bdhs_variable() errors helpfully", {
  expect_error(bdhs_variable("zz999"), "bdhs_search")
  expect_error(bdhs_variable("v024", recode = "SQ"), "appears in")
  expect_error(bdhs_variable(c("v024", "v025")), "single")
})
