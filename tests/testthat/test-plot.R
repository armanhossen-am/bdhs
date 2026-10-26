layer_geoms <- function(p) {
  vapply(p$layers, function(l) class(l$geom)[[1]], character(1), USE.NAMES = FALSE)
}

fake_estimate <- function(surveys = "BD2022DHS") {
  base <- tibble::tibble(
    indicator = c("anc4", "anc4", "sba", "sba"),
    label = c("ANC 4+ visits", "ANC 4+ visits", "Skilled birth attendance", "Skilled birth attendance"),
    v025 = haven::labelled(c(1, 2, 1, 2), c(urban = 1, rural = 2), label = "type of place of residence"),
    estimate = c(54, 38, 74, 52),
    se = 2,
    ci_low = c(50, 35, 70, 49),
    ci_high = c(58, 41, 78, 55),
    n_unweighted = 100L,
    n_weighted = 100,
    api_id = NA_character_
  )
  out <- do.call(rbind, lapply(surveys, function(s) {
    b <- base
    b$survey_id <- s
    b
  }))
  out <- out[, c("survey_id", setdiff(names(out), "survey_id"))]
  class(out) <- c("bdhs_estimate", class(out))
  out
}

test_that("bdhs_plot_trend() draws a national trend", {
  skip_if_not_installed("ggplot2")
  p <- bdhs_plot_trend("FE_FRTR_W_TFR")
  expect_s3_class(p, "ggplot")
  expect_equal(layer_geoms(p), c("GeomLine", "GeomPoint"))
  expect_equal(nrow(p$data), 9)
  expect_equal(p$labels$title, "Total fertility rate 15-49")
  expect_match(p$labels$caption, "Source: DHS Program API")
  expect_match(p$labels$subtitle, "1993-94 to 2022")
  x <- p$scales$get_scales("x")
  expect_equal(x$labels[c(1, 3, 8)], c("1993-94", "1999-00", "2017-18"))
  expect_equal(nrow(ggplot2::layer_data(p, 2)), 9)
})

test_that("bdhs_plot_trend() draws groups, regions, divisions and CIs", {
  skip_if_not_installed("ggplot2")
  res <- bdhs_plot_trend("CN_NUTS_C_HA2", by = "Residence")
  expect_setequal(levels(res$data$group), c("Urban", "Rural"))
  expect_match(res$labels$subtitle, "By residence")
  expect_equal(length(unique(ggplot2::layer_data(res, 2)$colour)), 2)

  reg <- bdhs_plot_trend("CM_ECMR_C_U5M", by = "Region")
  expect_true("GeomErrorbar" %in% layer_geoms(reg))
  expect_equal(nlevels(reg$data$group), 5)
  expect_true("Dhaka before 2015" %in% levels(reg$data$group))
  expect_match(reg$labels$caption, "95% confidence intervals")

  no_ci <- bdhs_plot_trend("CM_ECMR_C_U5M", ci = FALSE)
  expect_false("GeomErrorbar" %in% layer_geoms(no_ci))

  div <- bdhs_plot_trend("CM_ECMR_C_U5M", by = "Division")
  expect_s3_class(div$facet, "FacetWrap")
  expect_true(all(c("Mymensingh", "Rangpur", "Sylhet") %in% levels(div$data$group)))
  expect_no_error(ggplot2::ggplot_build(div))

  recent <- bdhs_plot_trend("FE_FRTR_W_TFR", by = "wealth", survey = c(2014, 2022))
  expect_equal(sort(unique(recent$data$survey_id)), c("BD2014DHS", "BD2022DHS"))
  expect_match(recent$labels$subtitle, "wealth quintile")
})

test_that("bdhs_plot_trend() facets several indicators", {
  skip_if_not_installed("ggplot2")
  p <- bdhs_plot_trend(c("CN_NUTS_C_HA2", "CN_NUTS_C_WH2"))
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(nlevels(p$data$indicator), 2)
})

test_that("bdhs_plot_trend() errors helpfully", {
  skip_if_not_installed("ggplot2")
  expect_error(bdhs_plot_trend("FE_FRTR_W_TFR", by = "Sex"), "Available")
  expect_error(bdhs_plot_trend("NOT_AN_ID"), "Unknown DHS indicator")
  expect_error(bdhs_plot_trend(), "indicator")
  expect_error(bdhs_plot_trend("FE_FRTR_W_TFR", by = c("a", "b")), "single")
})

test_that("plot() of bdhs_estimate results gives point and interval panels", {
  skip_if_not_installed("ggplot2")
  est <- fake_estimate()
  p <- plot(est)
  expect_s3_class(p, "ggplot")
  expect_equal(layer_geoms(p), c("GeomErrorbar", "GeomPoint"))
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(levels(p$data$.facet), c("ANC 4+ visits", "Skilled birth attendance"))
  expect_setequal(levels(p$data$.group), c("urban", "rural"))
  expect_match(p$labels$subtitle, "2022")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(nrow(ggplot2::layer_data(p, 2)), 4)

  # Several rounds: one colour per round.
  two <- plot(fake_estimate(c("BD2017DHS", "BD2022DHS")))
  expect_equal(levels(two$data$.survey), c("2017-18", "2022"))
  expect_equal(length(unique(ggplot2::layer_data(two, 2)$colour)), 2)

  # No by-groups, one indicator.
  single <- fake_estimate()[1, ]
  single$v025 <- NULL
  p1 <- plot(single)
  expect_equal(p1$labels$title, "ANC 4+ visits")
  expect_equal(levels(p1$data$.group), "Total")

  expect_s3_class(ggplot2::autoplot(est), "ggplot")
  bad <- est
  bad$ci_low <- NULL
  expect_error(plot(bad), "ci_low")
})
