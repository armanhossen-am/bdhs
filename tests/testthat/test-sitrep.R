test_that("bdhs_sitrep() reports the store and the conditions of use", {
  store <- local_bdhs_store()
  msgs <- character()
  out <- withCallingHandlers(
    bdhs_sitrep(),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  text <- paste(msgs, collapse = "\n")
  expect_match(text, "niportru@citechco.net", fixed = TRUE)
  expect_match(text, "redistribute")
  expect_match(text, "None yet")
  expect_equal(out$data_dir, normalize_path(store))
  expect_true(out$writable)
  expect_null(out$git_root)
  expect_equal(nrow(out$registry), 0)
  expect_true("haven" %in% names(out$packages))

  dir <- withr::local_tempdir()
  f <- write_fixture_dta(dir, "BDKR81FL.DTA", 2022, "KR")
  bdhs_import(f, copy = FALSE, quiet = TRUE)
  out <- suppressMessages(bdhs_sitrep())
  expect_equal(out$registry$survey_id, "BD2022DHS")
  expect_equal(out$registry$recodes, "KR*")
  file.remove(f)
  expect_equal(suppressMessages(bdhs_sitrep())$registry$recodes, "KR*!")
})

test_that("bdhs_sitrep() flags a store inside a git repository", {
  repo <- withr::local_tempdir()
  dir.create(file.path(repo, ".git"))
  withr::local_options(bdhs.data_dir = file.path(repo, "dhs"))
  out <- suppressMessages(bdhs_sitrep())
  expect_equal(out$git_root, normalize_path(repo))
  expect_false(out$exists)
  expect_true(out$writable)
})
