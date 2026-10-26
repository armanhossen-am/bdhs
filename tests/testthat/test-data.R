# Bundled datasets: schemas, keys and cross-dataset consistency.

has_dups <- function(df, cols = names(df)) {
  nrow(dplyr::distinct(df[cols])) != nrow(df)
}

all_ids <- function() {
  unique(c(
    bdhs_surveys$survey_id, bdhs_datasets$survey_id, bdhs_indicators$survey_id,
    bdhs_codebook$survey_id, bdhs_value_labels$survey_id, bdhs_regions$survey_id,
    bdhs_pool_population$survey_id
  ))
}

test_that("bdhs_surveys has one row per survey and the contract columns", {
  expect_s3_class(bdhs_surveys, "tbl_df")
  expect_equal(nrow(bdhs_surveys), 13L)
  expect_false(anyDuplicated(bdhs_surveys$survey_id) > 0)
  expect_true(all(c(
    "survey_id", "survey_type", "survey_year", "survey_label", "survey_num",
    "recode_version", "dhs_phase", "fieldwork_start", "fieldwork_end",
    "implementing_org", "n_clusters", "n_households", "women_universe",
    "n_women", "n_men"
  ) %in% names(bdhs_surveys)))
  expect_setequal(bdhs_surveys$survey_id, bdhs_survey_id(type = c("DHS", "SPA", "OTH")))
  dhs <- bdhs_surveys[bdhs_surveys$survey_type == "DHS", ]
  expect_true(all(dhs$women_universe == "Ever Married Women"))
  expect_true(all(dhs$has_indicator_data))
  expect_s3_class(bdhs_surveys$fieldwork_start, "Date")
})

test_that("bdhs_datasets lists unique files of known surveys", {
  expect_true(all(c(
    "survey_id", "survey_type", "survey_year", "file_name", "file_stem",
    "file_type_code", "file_type", "recode_version", "format_code",
    "file_format", "dataset_type", "file_size", "last_modified"
  ) %in% names(bdhs_datasets)))
  expect_equal(nrow(bdhs_datasets), 441L)
  expect_false(has_dups(bdhs_datasets, "file_name"))
  expect_true(all(bdhs_datasets$survey_id %in% bdhs_surveys$survey_id))
  expect_true(all(bdhs_datasets$format_code %in% c("DT", "FL", "SD", "SV", NA)))
  expect_true(all(substr(bdhs_datasets$file_stem, 1, 2) == "BD"))
})

test_that("bdhs_indicators has the contract columns, a unique key and no duplicates", {
  expect_true(all(c(
    "survey_id", "survey_year", "survey_label", "indicator_id", "indicator",
    "indicator_type", "breakdown", "characteristic_id", "characteristic_category",
    "characteristic_label", "by_variable_label", "region_id", "value", "precision",
    "denom_weighted", "denom_unweighted", "ci_low", "ci_high", "is_total",
    "is_preferred", "default_view"
  ) %in% names(bdhs_indicators)))
  expect_gt(nrow(bdhs_indicators), 300000)
  # the API duplicated every 2022 region row; none may remain
  expect_false(has_dups(bdhs_indicators))
  expect_false(has_dups(bdhs_indicators, c(
    "survey_id", "indicator_id", "indicator_order", "characteristic_id", "by_variable_id"
  )))
  expect_true(all(bdhs_indicators$survey_id %in% bdhs_surveys$survey_id[bdhs_surveys$has_indicator_data]))
  expect_setequal(unique(bdhs_indicators$breakdown), c("national", "subnational", "background"))
  expect_equal(bdhs_indicators$is_total, bdhs_indicators$breakdown == "national")
  expect_true(is.numeric(bdhs_indicators$value) && !anyNA(bdhs_indicators$value))
})

test_that("default_view keeps preferred rows and covers every cell", {
  ind <- bdhs_indicators
  expect_true(all(ind$default_view[ind$is_preferred]))
  cells <- dplyr::summarise(
    dplyr::group_by(ind, .data$survey_id, .data$indicator_id, .data$indicator_order, .data$characteristic_id),
    n_default = sum(.data$default_view), n_pref = sum(.data$is_preferred),
    .groups = "drop"
  )
  expect_true(all(cells$n_default >= 1))
  expect_true(all(cells$n_pref <= 1 | cells$n_pref == cells$n_default))
})

test_that("bdhs_indicator_catalog describes every indicator in bdhs_indicators", {
  cat_ <- bdhs_indicator_catalog
  expect_true(all(c(
    "indicator_id", "label", "short_name", "definition", "denominator",
    "measurement_type", "topic", "subtopic", "population", "precision",
    "surveys", "first_year", "last_year", "has_subnational", "has_ci"
  ) %in% names(cat_)))
  expect_equal(nrow(cat_), 2607L)
  expect_false(has_dups(cat_, c("indicator_id", "indicator_order")))
  key_ind <- unique(paste(bdhs_indicators$indicator_id, bdhs_indicators$indicator_order))
  expect_setequal(paste(cat_$indicator_id, cat_$indicator_order), key_ind)
  expect_equal(cat_$id_is_shared, duplicated(cat_$indicator_id) | duplicated(cat_$indicator_id, fromLast = TRUE))
})

test_that("codebooks contain metadata columns only", {
  expect_named(bdhs_codebook, c(
    "survey_id", "recode", "file_stem", "position", "variable", "label", "type",
    "n_value_labels", "n_valid"
  ))
  expect_named(bdhs_value_labels, c("survey_id", "recode", "variable", "value", "label"))
  expect_false(has_dups(bdhs_codebook, c("survey_id", "recode", "variable")))
  expect_false(has_dups(bdhs_codebook, c("survey_id", "recode", "position")))
  expect_false(has_dups(bdhs_value_labels, c("survey_id", "recode", "variable", "value")))
  expect_equal(bdhs_codebook$variable, tolower(bdhs_codebook$variable))
  expect_true(all(bdhs_codebook$recode %in% bdhs_recodes()$recode))
  expect_true(all(bdhs_codebook$file_stem %in% bdhs_datasets$file_stem))
  expect_true(all(bdhs_codebook$type %in% c("haven_labelled", "numeric", "character")))
  expect_true(all(bdhs_codebook$n_valid >= 0))
})

test_that("codebooks cover the documented files and agree with each other", {
  pairs <- unique(paste(bdhs_codebook$survey_id, bdhs_codebook$recode))
  expect_setequal(pairs, c(
    paste(c("BD1994DHS", "BD2000DHS", "BD2004DHS", "BD2007DHS", "BD2011DHS", "BD2014DHS"), "KR"),
    paste("BD2017DHS", c("BR", "FW", "HR", "IR", "KR", "PR", "SQ", "VA")),
    paste("BD2022DHS", c("IR", "KR", "PR"))
  ))
  # every value label belongs to a labelled variable, with matching counts
  n_vl <- table(paste(bdhs_value_labels$survey_id, bdhs_value_labels$recode, bdhs_value_labels$variable))
  labelled <- bdhs_codebook[bdhs_codebook$n_value_labels > 0, ]
  key <- paste(labelled$survey_id, labelled$recode, labelled$variable)
  expect_setequal(names(n_vl), key)
  expect_equal(as.integer(n_vl[key]), labelled$n_value_labels)
})

test_that("bdhs_regions has one row per survey x region code", {
  r <- bdhs_regions
  expect_named(r, c(
    "survey_id", "region_var", "region_code", "region_label", "division",
    "division_label", "includes", "unit_id", "reg_id", "trend6", "trend5",
    "code_verified", "label_source", "note"
  ))
  expect_equal(nrow(r), 86L)
  expect_setequal(unique(r$survey_id), bdhs_surveys$survey_id)
  expect_false(has_dups(r, c("survey_id", "region_code")))
  expect_false(has_dups(r, c("survey_id", "unit_id")))
  divisions <- c("barishal", "chattogram", "dhaka", "khulna", "mymensingh", "rajshahi", "rangpur", "sylhet")
  expect_true(all(r$division %in% divisions))
  expect_equal(r$division_label, paste0(toupper(substr(r$division, 1, 1)), substring(r$division, 2)))
  expect_true(all(r$trend6 %in% c(setdiff(divisions, c("mymensingh", "rangpur")), NA)))
  expect_true(all(r$trend5 %in% c("barishal", "chattogram", "dhaka", "khulna", "rajshahi")))
  expect_equal(which(is.na(r$trend6)), which(r$survey_id == "BD1994DHS" & r$region_code == 2))
  expect_true(all(r$code_verified[r$survey_id %in% c(
    "BD1994DHS", "BD2000DHS", "BD2004DHS", "BD2007DHS", "BD2011DHS",
    "BD2014DHS", "BD2017DHS", "BD2022DHS"
  )]))
  expect_false(any(r$code_verified[r$survey_id %in% c(
    "BD1997DHS", "BD1999SPA", "BD2001OTH", "BD2014SPA", "BD2017SPA"
  )]))
})

test_that("bdhs_regions codes match the microdata coding", {
  r <- bdhs_regions
  code <- function(id) setNames(r$division[r$survey_id == id], r$region_code[r$survey_id == id])
  expect_equal(
    unname(code("BD2017DHS")),
    c("barishal", "chattogram", "dhaka", "khulna", "mymensingh", "rajshahi", "rangpur", "sylhet")
  )
  expect_equal(unname(code("BD2014DHS")), c("barishal", "chattogram", "dhaka", "khulna", "rajshahi", "rangpur", "sylhet"))
  expect_equal(unname(code("BD1994DHS")), c("barishal", "chattogram", "dhaka", "khulna", "rajshahi"))
  expect_equal(r$region_label[r$survey_id == "BD1994DHS" & r$region_code == 5], "rajshani")
  # verified labels are the value labels of the files
  v <- r[r$code_verified, ]
  vl <- bdhs_value_labels[bdhs_value_labels$variable == "v024", ]
  hit <- match(paste(v$survey_id, v$region_code), paste(vl$survey_id, vl$value))
  expect_false(anyNA(hit))
  expect_equal(v$region_label, vl$label[hit])
  # DHS region ids join to the official regional estimates
  dhs_ids <- r$reg_id[grepl("DHS$", r$survey_id)]
  expect_true(all(dhs_ids %in% bdhs_indicators$region_id))
})

test_that("bdhs_regions is consistent with the bundled boundaries", {
  path <- system.file("extdata", "bdhs_boundaries.rds", package = "bdhs")
  expect_true(nzchar(path))
  skip_if_not_installed("sf")
  b <- readRDS(path)
  expect_equal(nrow(b), 86L)
  kb <- paste(b[["survey_id"]], b[["region_code"]])
  kr <- paste(bdhs_regions$survey_id, bdhs_regions$region_code)
  expect_setequal(kb, kr)
  j <- match(kb, kr)
  expect_equal(b[["unit_id"]], bdhs_regions$unit_id[j])
  expect_equal(b[["reg_id"]], bdhs_regions$reg_id[j])
  expect_equal(b[["code_verified"]], bdhs_regions$code_verified[j])
  expect_equal(b[["region_var"]], bdhs_regions$region_var[j])
})

test_that("bdhs_pool_population has the columns bdhs_pool() needs", {
  p <- bdhs_pool_population
  expect_true(all(c("survey_id", "ref_year", "female_15_49", "n_women_sample", "source") %in% names(p)))
  expect_equal(nrow(p), 9L)
  expect_setequal(p$survey_id, bdhs_survey_id(type = "DHS"))
  expect_false(anyDuplicated(p$survey_id) > 0)
  expect_true(all(p$female_15_49 > 2e7 & p$female_15_49 < 6e7))
  expect_true(all(diff(p$female_15_49) > 0))
  expect_equal(p$n_women_sample, bdhs_surveys$n_women[match(p$survey_id, bdhs_surveys$survey_id)])
  year <- bdhs_surveys$survey_year[match(p$survey_id, bdhs_surveys$survey_id)]
  expect_true(all(abs(p$ref_year - year) < 1))
  expect_true(all(p$p_ever_married > 0.7 & p$p_ever_married < 1))
})

test_that("every survey_id in the bundled data resolves with bdhs_survey_id()", {
  ids <- all_ids()
  expect_equal(bdhs_survey_id(ids, type = c("DHS", "SPA", "OTH")), ids)
})

test_that("bundled text is ASCII", {
  for (nm in c(
    "bdhs_surveys", "bdhs_datasets", "bdhs_indicators", "bdhs_indicator_catalog",
    "bdhs_codebook", "bdhs_value_labels", "bdhs_regions", "bdhs_pool_population"
  )) {
    x <- get(nm)
    chr <- vapply(x, is.character, logical(1))
    bad <- vapply(x[chr], function(col) any(grepl("[^ -~]", col, useBytes = TRUE)), logical(1))
    expect_false(any(bad), label = paste(nm, "non-ASCII columns:", paste(names(bad)[bad], collapse = ", ")))
  }
})
