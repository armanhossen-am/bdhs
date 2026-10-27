test_that("bdhs_na() turns labelled special codes into NA", {
  x <- haven::labelled(
    c(1, 2, 8, 9, 97, 1),
    c(yes = 1, no = 2, "don't know" = 8, missing = 9, "not a dejure resident" = 97),
    label = "ever had vaccination"
  )
  y <- bdhs_na(x)
  expect_s3_class(y, "haven_labelled")
  expect_equal(as.numeric(unclass(y)), c(1, 2, NA, NA, NA, 1))
  expect_equal(unname(attr(y, "labels")), c(1, 2))
  expect_equal(attr(y, "label"), "ever had vaccination")

  k <- bdhs_na(x, dont_know = "keep")
  expect_equal(as.numeric(unclass(k)), c(1, 2, 8, NA, NA, 1))
  expect_error(bdhs_na(x, dont_know = "maybe"))
})

test_that("substantive categories are kept", {
  toilet <- haven::labelled(
    c(11, 15, 31, 96, 99),
    c(
      "flush to piped sewer system" = 11, "flush, don't know where" = 15,
      "no facility/bush/field" = 31, other = 96, missing = 99
    )
  )
  expect_equal(as.numeric(unclass(bdhs_na(toilet))), c(11, 15, 31, 96, NA))
  dk <- haven::labelled(c(1, 8, 98), c("dk" = 8, "don't know" = 98, "flagged cases" = 1))
  expect_true(all(is.na(bdhs_na(dk))))
  odd <- haven::labelled(c(1, 2, 3, 4), c(
    inconsistent = 1, "height out of plausible limits" = 2,
    "not applicable" = 3, "days: number missing" = 4
  ))
  expect_true(all(is.na(bdhs_na(odd))))
})

test_that("anthropometry codes and tagged NAs are cleaned in data frames", {
  d <- tibble::tibble(
    hw70 = c(-150, 9996, 9998, 120),
    hc71 = c(9999, 10, NA, -20),
    v445 = c(2100, 9998, 2500, 9999),
    v012 = c(9998, 20, 30, 40),
    tag = haven::labelled(c(1, haven::tagged_na("a"), 2, 3), c(yes = 1, no = 2, three = 3)),
    fac = factor(c("yes", "don't know", "no", "yes"))
  )
  expect_message(out <- bdhs_na(d), "special code")
  expect_equal(out$hw70, c(-150, NA, NA, 120))
  expect_equal(out$hc71, c(NA, 10, NA, -20))
  expect_equal(out$v445, c(2100, NA, 2500, NA))
  expect_equal(out$v012, d$v012) # not an anthropometry variable
  expect_false(any(haven::is_tagged_na(unclass(out$tag))))
  expect_true(is.na(out$tag[2]))
  expect_equal(levels(out$fac), c("no", "yes"))
  expect_true(is.na(out$fac[2]))

  sel <- bdhs_na(d, cols = c(hw70, fac))
  expect_equal(sel$hc71, d$hc71)
  expect_equal(sel$hw70, c(-150, NA, NA, 120))
  sel2 <- suppressMessages(bdhs_na(d, cols = dplyr::starts_with("h")))
  expect_equal(sel2$v445, d$v445)
  expect_equal(sel2$hc71, c(NA, 10, NA, -20))
})

test_that("SPSS user-defined missing values become NA", {
  x <- haven::labelled_spss(c(1, 2, 9), c(yes = 1, no = 2, missing = 9), na_values = 9)
  expect_equal(as.numeric(unclass(bdhs_na(x))), c(1, 2, NA))
})

test_that("bdhs_na() works on simulated recodes", {
  pr <- bdhs_simulate(2022, "PR", n_clusters = 10, households = 5)
  out <- suppressMessages(bdhs_na(pr))
  expect_true(all(out$hc70 < 9994, na.rm = TRUE))
  expect_equal(nrow(out), nrow(pr))
  expect_equal(attributes(out)[c("bdhs_survey_id", "bdhs_recode")], attributes(pr)[c("bdhs_survey_id", "bdhs_recode")])
  expect_equal(bdhs_na(c("a", "b")), c("a", "b"))
})
