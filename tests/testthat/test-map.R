test_that("bdhs_boundaries() returns the divisions of a round", {
  skip_if_not_installed("sf")
  b <- bdhs_boundaries(2022)
  expect_s3_class(b, "sf")
  expect_named(b, c("survey_id", "region_code", "region_name", "unit_id", "reg_id", "geometry"))
  expect_equal(nrow(b), 8)
  expect_equal(b$region_code, 1:8)
  expect_equal(sf::st_crs(b)$epsg, 4326L)
  expect_true(all(b$survey_id == "BD2022DHS"))

  two <- bdhs_boundaries(c("1993-94", 2022))
  expect_equal(nrow(two), 5 + 8)
  expect_equal(unique(two$survey_id), c("BD1994DHS", "BD2022DHS"))
  expect_equal(nrow(bdhs_boundaries(NULL)), 86)
  expect_equal(nrow(bdhs_boundaries("spa2017")), 8)
  expect_match(attr(b, "source"), "Spatial Data Repository")
})

test_that("bdhs_map() maps official subnational estimates", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  expect_no_warning(p <- bdhs_map("CN_NUTS_C_HA2"))
  expect_s3_class(p, "ggplot")
  expect_equal(unname(vapply(p$layers, function(l) class(l$geom)[[1]], character(1))), c("GeomSf", "GeomLabel"))
  expect_s3_class(p$data, "sf")
  expect_equal(nrow(p$data), 8)
  expect_false(anyNA(p$data$value))
  # Values are the official 2022 division estimates, joined on reg_id.
  off <- bdhs_official("CN_NUTS_C_HA2", survey = 2022, breakdown = "subnational")
  expect_equal(p$data$value, off$value[match(p$data$reg_id, off$region_id)])
  expect_equal(p$labels$title, "Children stunted")
  expect_match(p$labels$caption, "DHS Program API")
  expect_match(p$labels$caption, "ODbL")
  expect_equal(nrow(ggplot2::layer_data(p, 2)), 8)
  expect_no_warning(ggplot2::ggplot_build(p))

  multi <- bdhs_map("FE_FRTR_W_TFR", survey = c(1994, 2014, 2022))
  expect_s3_class(multi$facet, "FacetWrap")
  expect_equal(nrow(multi$data), 5 + 7 + 8)
  expect_false(anyNA(multi$data$value))
})

test_that("bdhs_map() maps user estimates by region code or name", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  est <- data.frame(v024 = 1:8, estimate = c(25, 30, 22, 21, 31, 23, 24, 34))
  p <- bdhs_map(est, survey = 2022, title = "My indicator")
  expect_equal(p$labels$title, "My indicator")
  expect_equal(p$data$value[p$data$region_name == "Sylhet"], 34)
  expect_match(p$labels$caption, "user-supplied")

  # Old spellings and labelled codes; survey from a survey_id column.
  by_name <- data.frame(
    survey_id = "BD2014DHS",
    region = c("Barisal", "Chittagong", "Dhaka", "Khulna", "Rajshahi", "Rangpur", "Sylhet"),
    value = 1:7
  )
  q <- bdhs_map(by_name)
  expect_equal(q$data$value[q$data$region_name == "Chattogram"], 2)
  expect_false(anyNA(q$data$value))

  lab <- tibble::tibble(
    survey_id = "BD2022DHS",
    v024 = haven::labelled(c(1, 8), c(barishal = 1, sylhet = 8)),
    estimate = c(10, 20), ci_low = c(8, 18)
  )
  r <- bdhs_map(lab)
  expect_equal(sum(!is.na(r$data$value)), 2)
  expect_match(r$labels$caption, "computed with bdhs")

  fac <- data.frame(division = factor(c("Barishal", "Sylhet")), value = c(1, 2))
  attr(fac, "bdhs_survey_id") <- "BD2022DHS"
  expect_equal(sum(!is.na(bdhs_map(fac)$data$value)), 2)

  # Several rounds in one data frame give panels.
  pooled <- data.frame(survey_id = rep(c("BD2017DHS", "BD2022DHS"), each = 8), v024 = rep(1:8, 2), estimate = 1:16)
  expect_s3_class(bdhs_map(pooled)$facet, "FacetWrap")
})

test_that("bdhs_map() errors and warns helpfully", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  est <- data.frame(v024 = 1:8, estimate = 1:8)
  expect_error(bdhs_map(est), "survey")
  expect_error(bdhs_map(data.frame(x = 1, estimate = 1), survey = 2022), "region column")
  expect_error(bdhs_map(data.frame(v024 = 1, y = "a"), survey = 2022), "value column")
  expect_error(bdhs_map(data.frame(v024 = c(1, 1), estimate = 1:2), survey = 2022), "more than one row")
  expect_error(
    bdhs_map(data.frame(v024 = 1:2, estimate = 1:2, indicator = c("a", "b")), survey = 2022),
    "several indicators"
  )
  expect_warning(
    bdhs_map(data.frame(region_name = c("Barishal", "Atlantis"), value = 1:2), survey = 2022),
    "Atlantis"
  )
  expect_error(bdhs_map("FE_FRTR_W_TFR", survey = "spa2014"), "No official estimates")
  expect_error(bdhs_map(42), "indicator id or a data frame")
  expect_error(bdhs_boundaries(2030), "Can't resolve")
})

test_that("bdhs_map() draws harmonized trend layouts with matching boundaries", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  est <- tibble::tibble(
    survey_id = rep(c("BD2004DHS", "BD2022DHS"), each = 6),
    division_trend6 = factor(rep(c("barishal", "chattogram", "dhaka", "khulna", "rajshahi", "sylhet"), 2)),
    estimate = 1:12
  )
  p <- bdhs_map(est)
  expect_s3_class(p, "ggplot")
  d <- p$data
  expect_equal(nrow(d), 12)
  expect_false(anyNA(d$value))
  expect_setequal(unique(d$survey_id), c("BD2004DHS", "BD2022DHS"))
  expect_match(ggplot2::get_labs(p)$subtitle, "six-division trend layout")
})
