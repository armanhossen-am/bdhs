test_that("bdhs_describe() returns a codebook of any data frame", {
  ir <- bdhs_simulate(2022, "IR", n_clusters = 4, households = 5)
  cb <- bdhs_describe(ir)
  expect_s3_class(cb, "tbl_df")
  expect_named(cb, c("variable", "label", "type", "n_valid", "n_missing", "n_value_labels", "value_labels"))
  expect_equal(nrow(cb), ncol(ir))
  expect_equal(cb$variable, names(ir))
  expect_equal(cb$n_valid + cb$n_missing, rep(nrow(ir), ncol(ir)))
  expect_equal(attr(cb, "bdhs_survey_id"), "BD2022DHS")
  expect_equal(attr(cb, "bdhs_recode"), "IR")

  v024 <- cb[cb$variable == "v024", ]
  expect_equal(v024$type, "haven_labelled")
  expect_equal(v024$n_value_labels, length(attr(ir$v024, "labels")))
  expect_match(v024$value_labels, "^1=")
  expect_equal(cb$type[cb$variable == "caseid"], "character")
})

test_that("bdhs_describe() counts missing values and handles factors", {
  df <- data.frame(
    x = c(1, NA, 3),
    f = factor(c("a", "b", NA), levels = c("a", "b", "c")),
    s = c("secret-1", "secret-2", "secret-3"),
    stringsAsFactors = FALSE
  )
  df$y <- haven::labelled(c(0, 1, haven::tagged_na("a")), c(no = 0, yes = 1, "not asked" = haven::tagged_na("a")))
  attr(df$x, "label") <- "a number"
  cb <- bdhs_describe(df)
  expect_equal(cb$n_missing, c(1L, 1L, 0L, 1L))
  expect_equal(cb$label[[1]], "a number")
  expect_equal(cb$type, c("numeric", "factor", "character", "haven_labelled"))
  expect_equal(cb$value_labels[[2]], "a; b; c")
  expect_equal(cb$n_value_labels[[2]], 3L)
  expect_equal(cb$value_labels[[4]], "0=no; 1=yes; NA(a)=not asked")
  # Data values never appear in the codebook.
  expect_false(any(grepl("secret", unlist(cb))))
})

test_that("bdhs_describe() rejects non data frames", {
  expect_error(bdhs_describe(1:3), "data frame")
})
