test_that("Kitagawa decomposition is exact and matches hand computation", {
  kr14 <- bdhs_simulate(2014, "KR")
  kr22 <- bdhs_simulate(2022, "KR")
  dec <- bdhs_decompose(kr14, kr22, "rh_del_csection", by = "v025")
  expect_s3_class(dec, "bdhs_decomposition")
  g <- attr(dec, "groups")
  c1 <- g$share_1 / 100
  c2 <- g$share_2 / 100
  comp <- (c2 - c1) * (g$rate_1 + g$rate_2) / 2
  within <- (g$rate_2 - g$rate_1) * (c1 + c2) / 2
  expect_equal(dec$estimate[dec$component == "composition" & dec$group != "All"], comp)
  expect_equal(dec$estimate[dec$component == "within" & dec$group != "All"], within)
  tot <- dec$estimate[dec$component == "total"]
  expect_equal(sum(comp) + sum(within), tot)
  # The total change is the change of the overall estimate
  e1 <- bdhs_estimate(kr14, "rh_del_csection")
  e2 <- bdhs_estimate(kr22, "rh_del_csection")
  expect_equal(tot, e2$estimate - e1$estimate)
  expect_equal(sum(dec$percent[dec$group == "All" & dec$component != "total"]), 100)
  # Jackknife SE of the total change is close to the linearization SE
  se_lin <- sqrt(e1$se^2 + e2$se^2)
  se_jk <- dec$se[dec$component == "total"]
  expect_lt(abs(se_jk / se_lin - 1), 0.3)
  expect_true(all(dec$se > 0))
})

test_that("toy example: pure composition and pure rate change", {
  # Group A rate 0.2, group B rate 0.6; shares move from 50/50 to 25/75.
  f <- function(Y1, W1, Y2, W2) {
    c1 <- W1 / sum(W1)
    c2 <- W2 / sum(W2)
    r1 <- Y1 / W1
    r2 <- Y2 / W2
    c(sum((c2 - c1) * (r1 + r2) / 2), sum((r2 - r1) * (c1 + c2) / 2))
  }
  expect_equal(f(c(10, 30), c(50, 50), c(5, 45), c(25, 75)), c(0.1, 0))
  expect_equal(f(c(10, 30), c(50, 50), c(20, 30), c(50, 50)), c(0, 0.1))
  # Delete-one jackknife of a mean equals the usual SE of the mean
  t <- list(Y = 10, W = 4, Yp = matrix(c(1, 2, 3, 4), ncol = 1), Wp = matrix(1, 4, 1))
  v <- jackknife_var(t, function(Y, W) Y / W, 2.5)
  expect_equal(v, var(1:4) / 4)
})

test_that("harmonized groups are created on the fly", {
  kr14 <- bdhs_simulate(2014, "KR")
  kr22 <- bdhs_simulate(2022, "KR")
  dec <- bdhs_decompose(kr14, kr22, "rh_del_csection", by = "mother_edu_level", se = "none")
  expect_setequal(setdiff(dec$group, "All"), c("none", "primary", "secondary", "higher"))
  expect_true(all(is.na(dec$se)))
})

test_that("bdhs_decompose checks its inputs", {
  kr <- bdhs_simulate(2022, "KR")
  ir <- bdhs_simulate(2022, "IR")
  expect_error(bdhs_decompose(kr, kr, "rh_del_csection", by = "v025"), "same round")
  expect_error(bdhs_decompose(ir, ir, "ms_married_by_18", by = "v025"), "all-women")
  expect_error(bdhs_decompose(kr, kr, "rh_del_csection", by = c("v025", "v024")), "single")
})

test_that("decomposition plot", {
  skip_if_not_installed("ggplot2")
  dec <- bdhs_decompose(bdhs_simulate(2014, "KR"), bdhs_simulate(2022, "KR"), "rh_del_csection", by = "v025")
  expect_s3_class(plot(dec), "ggplot")
})
