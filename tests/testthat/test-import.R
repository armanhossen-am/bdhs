test_that("bdhs_data_dir() resolves option > env var > default", {
  withr::local_options(bdhs.data_dir = NULL)
  withr::local_envvar(BDHS_DATA_DIR = NA)
  expect_equal(bdhs_data_dir(), normalize_path(tools::R_user_dir("bdhs", "data")))
  env_dir <- withr::local_tempdir()
  withr::local_envvar(BDHS_DATA_DIR = env_dir)
  expect_equal(bdhs_data_dir(), normalize_path(env_dir))
  opt_dir <- file.path(withr::local_tempdir(), "store")
  expect_message(out <- bdhs_data_dir(opt_dir), "BDHS_DATA_DIR=")
  expect_equal(out, normalize_path(opt_dir))
  expect_equal(getOption("bdhs.data_dir"), normalize_path(opt_dir))
  expect_equal(bdhs_data_dir(), normalize_path(opt_dir))
  # Setting the directory does not create it.
  expect_false(dir.exists(opt_dir))
  expect_error(bdhs_data_dir(c("a", "b")), "single directory")
})

test_that("bdhs_import() imports a download folder (zip, renamed, SPSS, pooled)", {
  store <- local_bdhs_store()
  dl <- fixture_downloads()
  expect_warning(out <- capture_msgs(bdhs_import(dl, format = "dta")), "derived or pooled")
  expect_match(out$text, "Imported 2 datasets")
  expect_match(out$text, "duplicate")
  reg <- out$value
  expect_s3_class(reg, "tbl_df")
  expect_named(reg, names(registry_template()))
  reg <- reg[order(reg$recode), ]
  expect_equal(reg$survey_id, c("BD2022DHS", "BD2022DHS"))
  expect_equal(reg$recode, c("IR", "KR"))
  expect_equal(reg$file_stem, c("BDIR81FL", "BDKR81FL"))
  # The Stata file inside the zip wins over the loose SPSS file.
  expect_equal(reg$format, c("dta", "dta"))
  expect_match(reg$source_path[[1]], "BDIR81DT.ZIP > BDIR81FL.DTA", fixed = TRUE)
  expect_equal(reg$path, c("BD2022DHS/BDIR81FL.dta", "BD2022DHS/BDKR81FL.dta"))
  expect_false(any(reg$linked))
  expect_true(all(file.exists(file.path(store, reg$path))))
  expect_equal(reg$n_rows[[1]], nrow(sim_recode(2022, "IR")))
  expect_equal(reg$n_cols[[2]], ncol(sim_recode(2022, "KR")))
  expect_true(file.exists(file.path(store, "registry.rds")))
  expect_equal(nrow(registry_read(store)), 2)

  # Importing again skips everything by checksum.
  expect_warning(again <- capture_msgs(bdhs_import(dl, format = "dta")), "derived")
  expect_match(again$text, "already imported")
  again <- again$value
  expect_equal(nrow(again), 0)
  expect_equal(nrow(registry_read(store)), 2)
})

test_that("copy = FALSE registers files in place and never copies them", {
  store <- local_bdhs_store()
  dir <- withr::local_tempdir()
  f <- write_fixture_dta(dir, "BDHS2022KR.DTA", 2022, "KR")
  reg <- bdhs_import(f, copy = FALSE, quiet = TRUE)
  expect_true(reg$linked)
  expect_equal(reg$path, normalize_path(f))
  expect_equal(reg$format, "dta")
  expect_false(dir.exists(file.path(store, "BD2022DHS")))
  expect_equal(unname(tools::md5sum(f)), reg$md5)
})

test_that("files in zips are copied even with copy = FALSE", {
  skip_if_no_zip()
  local_bdhs_store()
  stage <- withr::local_tempdir()
  dir <- withr::local_tempdir()
  write_fixture_dta(stage, "BDHR81FL.DTA", 2022, "HR")
  z <- write_fixture_zip(file.path(dir, "BDHR81DT.ZIP"), stage, "BDHR81FL.DTA")
  reg <- bdhs_import(z, copy = FALSE, format = "dta", quiet = TRUE)
  expect_false(reg$linked)
  expect_equal(reg$recode, "HR")
})

test_that("bulk downloads with nested folders and zips are unpacked", {
  skip_if_no_zip()
  local_bdhs_store()
  stage <- withr::local_tempdir()
  dir <- withr::local_tempdir()
  dir.create(file.path(stage, "BDPR7RDT"))
  write_fixture_dta(file.path(stage, "BDPR7RDT"), "BDPR7RFL.DTA", 2017, "PR")
  writeLines("x", file.path(stage, "BDPR7RDT", "BDPR7RFL.DO"))
  stage2 <- withr::local_tempdir()
  write_fixture_dta(stage2, "BDHR7RFL.DTA", 2017, "HR")
  write_fixture_zip(file.path(stage, "BDHR7RDT.ZIP"), stage2, "BDHR7RFL.DTA")
  bulk <- write_fixture_zip(
    file.path(dir, "STATA_BD_2017-18_DHS_01012025.zip"), stage,
    c("BDPR7RDT/BDPR7RFL.DTA", "BDPR7RDT/BDPR7RFL.DO", "BDHR7RDT.ZIP")
  )
  reg <- bdhs_import(bulk, format = "dta", quiet = TRUE)
  expect_equal(sort(reg$recode), c("HR", "PR"))
  expect_equal(unique(reg$survey_id), "BD2017DHS")
  expect_true(any(grepl("BDHR7RDT.ZIP > BDHR7RFL.DTA", reg$source_path, fixed = TRUE)))
})

test_that("changed files are not replaced without overwrite = TRUE", {
  local_bdhs_store()
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  f1 <- write_fixture_dta(dir1, "BDKR81FL.DTA", 2022, "KR", seed = 1)
  f2 <- write_fixture_dta(dir2, "BDKR81FL.DTA", 2022, "KR", seed = 2)
  first <- bdhs_import(f1, format = "dta", quiet = TRUE)
  expect_warning(none <- bdhs_import(f2, format = "dta", quiet = TRUE), "overwrite = TRUE")
  expect_equal(nrow(none), 0)
  expect_equal(registry_read()$md5, first$md5)
  second <- bdhs_import(f2, format = "dta", overwrite = TRUE, quiet = TRUE)
  expect_equal(registry_read()$md5, second$md5)
  expect_equal(nrow(registry_read()), 1)
})

test_that("files inside the data directory are refused", {
  store <- local_bdhs_store()
  dir <- withr::local_tempdir()
  f <- write_fixture_dta(dir, "BDKR81FL.DTA", 2022, "KR")
  bdhs_import(f, format = "dta", quiet = TRUE)
  out <- capture_msgs(bdhs_import(store))
  expect_match(out$text, "inside the bdhs data directory")
  expect_equal(nrow(out$value), 0)
})

test_that("unrecognised files are skipped with a message", {
  local_bdhs_store()
  dir <- withr::local_tempdir()
  haven::write_dta(data.frame(x = 1:2), file.path(dir, "auto.dta"))
  out <- capture_msgs(bdhs_import(dir))
  expect_match(out$text, "not recognised")
  expect_equal(nrow(out$value), 0)
  empty <- withr::local_tempdir()
  expect_message(bdhs_import(empty), "No Stata")
})

test_that("insecure data directories trigger a warning", {
  repo <- withr::local_tempdir()
  dir.create(file.path(repo, ".git"))
  withr::local_options(bdhs.data_dir = file.path(repo, "data"))
  src <- withr::local_tempdir()
  f <- write_fixture_dta(src, "BDKR81FL.DTA", 2022, "KR")
  expect_warning(bdhs_import(f, format = "dta", quiet = TRUE), "git repository")
  expect_equal(path_sync_service("/Users/me/Dropbox/data"), "Dropbox")
  expect_equal(path_sync_service("C:/Users/me/OneDrive - Uni/bdhs"), "OneDrive")
  expect_equal(path_sync_service("/Users/me/Library/CloudStorage/GoogleDrive-me/My Drive/x"), "Google Drive")
  expect_equal(path_sync_service("/Users/me/Library/Mobile Documents/com~apple~CloudDocs/x"), "iCloud Drive")
  expect_null(path_sync_service("/Users/me/secure/bdhs"))
  expect_null(path_git_root(withr::local_tempdir()))
})

test_that("Parquet storage keeps labels and tagged missing values", {
  skip_if_not_installed("arrow")
  store <- local_bdhs_store()
  dir <- withr::local_tempdir()
  kr <- sim_recode(2022, "KR")
  kr$hw70 <- haven::labelled(kr$hw70, c("flagged cases" = 9996), label = "height for age sd")
  kr$hw70[1:2] <- haven::tagged_na("a")
  f <- write_fixture_dta(dir, "BDKR81FL.DTA", data = kr)
  reg <- bdhs_import(f, format = "auto", quiet = TRUE)
  expect_equal(reg$format, "parquet")
  expect_equal(reg$path, "BD2022DHS/BDKR81FL.parquet")
  expect_equal(reg$n_cols, ncol(kr))
  original <- haven::read_dta(f)
  back <- bdhs_read(2022, "KR")
  expect_equal(names(back), names(original))
  for (nm in names(original)) {
    expect_identical(attributes(back[[nm]]), attributes(original[[nm]]), label = nm)
  }
  expect_equal(haven::na_tag(back$hw70[1:3]), c("a", "a", NA))
  expect_equal(as.data.frame(haven::zap_missing(back)), as.data.frame(haven::zap_missing(original)), ignore_attr = TRUE)
  # Column selection reads the tag column along with its variable only.
  sel <- bdhs_read(2022, "KR", vars = "hw70", keep_design = FALSE)
  expect_named(sel, "hw70")
  expect_equal(haven::na_tag(sel$hw70[1:2]), c("a", "a"))
  expect_equal(nrow(bdhs_read(2022, "KR", vars = "hw70", n_max = 3)), 3)
  # Labels live in a sidecar file, deleted together with the Parquet copy.
  files <- file.path(store, "BD2022DHS", c("BDKR81FL.parquet", "BDKR81FL.attrs.rds"))
  expect_true(all(file.exists(files)))
  rlang::local_interactive(FALSE)
  suppressMessages(bdhs_forget(2022, "KR", delete = TRUE))
  expect_false(any(file.exists(files)))
})

test_that("format = 'parquet' falls back to a copy when the conversion fails", {
  skip_if_not_installed("arrow")
  local_bdhs_store()
  dir <- withr::local_tempdir()
  f <- write_fixture_dta(dir, "BDHR81FL.DTA", 2022, "HR")
  local_mocked_bindings(parquet_round_trips = function(df, path) FALSE)
  expect_warning(reg <- bdhs_import(f, format = "parquet", quiet = TRUE), "original file was copied")
  expect_equal(reg$format, "dta")
})

test_that("bdhs_inventory() lists the catalogue with availability", {
  local_bdhs_store()
  inv <- bdhs_inventory()
  expect_true(all(inv$survey_id %in% bdhs_survey_id(type = "DHS")))
  expect_false(any(duplicated(paste(inv$survey_id, inv$recode))))
  expect_false(any(inv$available))
  expect_false("MR" %in% inv$recode[inv$survey_id == "BD2014DHS"])
  dir <- withr::local_tempdir()
  f <- write_fixture_dta(dir, "BDKR81FL.DTA", 2022, "KR")
  bdhs_import(f, copy = FALSE, quiet = TRUE)
  inv <- bdhs_inventory(2022)
  expect_equal(inv$available, inv$recode == "KR")
  expect_equal(inv$path[inv$recode == "KR"], normalize_path(f))
  expect_equal(nrow(bdhs_inventory(2022, "KR")), 1)
  expect_gt(nrow(bdhs_inventory(type = c("DHS", "SPA"))), nrow(bdhs_inventory()))
})

test_that("bdhs_forget() unregisters, deletes stored copies only on request", {
  store <- local_bdhs_store()
  dir <- withr::local_tempdir()
  f_kr <- write_fixture_dta(dir, "BDKR81FL.DTA", 2022, "KR")
  f_hr <- write_fixture_dta(dir, "BDHR81FL.DTA", 2022, "HR")
  f_ir <- write_fixture_dta(dir, "BDIR81FL.DTA", 2022, "IR")
  bdhs_import(c(f_kr, f_hr), format = "dta", quiet = TRUE)
  bdhs_import(f_ir, copy = FALSE, quiet = TRUE)
  rlang::local_interactive(FALSE)

  gone <- capture_msgs(bdhs_forget(2022, "KR"))
  expect_match(gone$text, "kept")
  gone <- gone$value
  expect_equal(gone$recode, "KR")
  expect_true(file.exists(file.path(store, "BD2022DHS", "BDKR81FL.dta")))
  expect_false("KR" %in% registry_read()$recode)

  expect_match(capture_msgs(bdhs_forget(2022, "HR", delete = TRUE))$text, "Deleted")
  expect_false(file.exists(file.path(store, "BD2022DHS", "BDHR81FL.dta")))

  expect_match(capture_msgs(bdhs_forget(2022, "IR", delete = TRUE))$text, "not touched")
  expect_true(file.exists(f_ir))
  expect_equal(nrow(registry_read()), 0)
  expect_message(bdhs_forget(2022, "IR"), "Nothing registered")
})

test_that("files whose name and content disagree are not imported", {
  store <- local_bdhs_store()
  dir <- withr::local_tempdir()
  write_fixture_dta(dir, "BDIR81FL.DTA", 2022, "KR")
  haven::write_dta(data.frame(id = 1:5, income = c(10, 20, 30, 40, 50), district = 1:5),
                   file.path(dir, "BDHS2014HR.dta"))
  warnings <- character()
  out <- withCallingHandlers(
    capture_msgs(bdhs_import(dir, format = "dta")),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(nrow(out$value), 0)
  expect_equal(nrow(registry_read(store)), 0)
  expect_match(paste(warnings, collapse = "\n"), "name and the content of 1 file disagree")
  expect_match(paste(warnings, collapse = "\n"), "BD2022DHS IR but the variables are those of a KR file")
  expect_match(paste(warnings, collapse = "\n"), "could not be confirmed")
  expect_match(out$text, "file name and content disagree, not imported")
  expect_match(out$text, "not confirmed as a BDHS recode file")
})

test_that("the import summary shows the notes of imported files", {
  local_bdhs_store()
  dir <- withr::local_tempdir()
  write_fixture_dta(dir, "children.dta", 2022, "KR")
  out <- capture_msgs(bdhs_import(dir, format = "dta"))
  expect_match(out$text, "BD2022DHS KR (BDKR81FL): copied (.dta)", fixed = TRUE)
  expect_match(out$text, "identified by content")
})

test_that("a registered file that was moved or deleted is registered again", {
  store <- local_bdhs_store()
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  f <- write_fixture_dta(dir1, "BDHR81FL.DTA", 2022, "HR")
  bdhs_import(f, copy = FALSE, quiet = TRUE)
  moved <- file.path(dir2, "BDHR81FL.DTA")
  file.rename(f, moved)
  out <- capture_msgs(bdhs_import(moved, copy = FALSE))
  expect_match(out$text, "registered again")
  expect_equal(nrow(out$value), 1)
  reg <- registry_read(store)
  expect_equal(nrow(reg), 1)
  expect_equal(reg$path, normalize_path(moved))
  expect_equal(nrow(bdhs_read(2022, "HR", vars = "hv001")), nrow(sim_recode(2022, "HR")))

  # A deleted stored copy is stored again.
  f_kr <- write_fixture_dta(dir1, "BDKR81FL.DTA", 2022, "KR")
  bdhs_import(f_kr, format = "dta", quiet = TRUE)
  unlink(file.path(store, "BD2022DHS", "BDKR81FL.dta"))
  out <- capture_msgs(bdhs_import(f_kr, format = "dta"))
  expect_match(out$text, "registered again")
  expect_true(file.exists(file.path(store, "BD2022DHS", "BDKR81FL.dta")))
  expect_equal(nrow(registry_read(store)), 2)
  # Files that still exist are skipped as before.
  expect_match(capture_msgs(bdhs_import(f_kr, format = "dta"))$text, "already imported")
})

test_that("zips kept by bdhs_download(import = FALSE) can be imported", {
  skip_if_no_zip()
  store <- local_bdhs_store()
  stage <- withr::local_tempdir()
  write_fixture_dta(stage, "BDPR81FL.DTA", 2022, "PR")
  downloads <- file.path(store, "downloads")
  dir.create(downloads, recursive = TRUE)
  write_fixture_zip(file.path(downloads, "BDPR81DT.ZIP"), stage, "BDPR81FL.DTA")
  reg <- bdhs_import(downloads, format = "dta", quiet = TRUE)
  expect_equal(reg$recode, "PR")
  expect_equal(reg$path, "BD2022DHS/BDPR81FL.dta")
  # Stored copies are still refused.
  out <- capture_msgs(bdhs_import(file.path(store, "BD2022DHS")))
  expect_match(out$text, "inside the bdhs data directory")
})

test_that("the store lock is exclusive, re-entrant and released", {
  store <- local_bdhs_store()
  key <- store_lock(store)
  expect_true(dir.exists(file.path(store, ".lock")))
  expect_equal(store_lock(store), key)
  store_unlock(key)
  expect_true(dir.exists(file.path(store, ".lock")))
  store_unlock(key)
  expect_false(dir.exists(file.path(store, ".lock")))

  # A lock held by another session: imports wait, then stop clearly.
  lock <- file.path(store, ".lock")
  dir.create(lock)
  writeLines(c("pid: 1", "host: another-computer", "user: someone", "time: now"), file.path(lock, "owner"))
  withr::local_options(bdhs.lock_timeout = 0.3)
  src <- withr::local_tempdir()
  f <- write_fixture_dta(src, "BDKR81FL.DTA", 2022, "KR")
  expect_error(bdhs_import(f, format = "dta", quiet = TRUE), class = "bdhs_store_locked")
  expect_error(bdhs_import(f, format = "dta", quiet = TRUE), "in use by another R session")
  expect_equal(nrow(registry_read(store)), 0)

  # ... unless the lock is stale.
  Sys.setFileTime(file.path(lock, "owner"), Sys.time() - 7200)
  reg <- bdhs_import(f, format = "dta", quiet = TRUE)
  expect_equal(reg$recode, "KR")
  expect_false(dir.exists(lock))
})

test_that("a lock left by a process that has ended is taken over", {
  skip_on_os("windows")
  store <- local_bdhs_store()
  # The id of a process that has ended.
  pid <- as.integer(system2("sh", c("-c", shQuote("echo $$")), stdout = TRUE))
  lock <- file.path(store, ".lock")
  dir.create(lock)
  writeLines(c(paste("pid:", pid), paste("host:", Sys.info()[["nodename"]]),
               paste("user:", Sys.info()[["user"]]), "time: earlier"), file.path(lock, "owner"))
  withr::local_options(bdhs.lock_timeout = 5)
  key <- store_lock(store)
  expect_equal(store_lock_owner(lock)[["pid"]], as.character(Sys.getpid()))
  store_unlock(key)
})

test_that("rows registered by another session during an import are kept", {
  store <- local_bdhs_store()
  src <- withr::local_tempdir()
  f_hr <- write_fixture_dta(src, "BDHR81FL.DTA", 2022, "HR")
  f_kr <- write_fixture_dta(src, "BDKR81FL.DTA", 2022, "KR")
  real_store_one <- store_one
  # Another session (not using the lock, e.g. an older bdhs) registers the HR
  # file while this import is storing the KR file.
  local_mocked_bindings(store_one = function(cand, dir, linked, store_format) {
    other <- registry_template()
    other[1, "survey_id"] <- "BD2022DHS"
    other$recode <- "HR"
    other$path <- normalize_path(f_hr)
    other$linked <- TRUE
    other$md5 <- unname(tools::md5sum(f_hr))
    registry_write(other, dir)
    real_store_one(cand, dir, linked, store_format)
  })
  bdhs_import(f_kr, format = "dta", quiet = TRUE)
  reg <- registry_read(store)
  expect_equal(sort(reg$recode), c("HR", "KR"))
})

test_that("a data directory that is not writable gives a clear error", {
  skip_on_os("windows")
  skip_if(identical(unname(Sys.info()[["effective_user"]]), "root"), "root can write anywhere")
  ro <- withr::local_tempdir()
  Sys.chmod(ro, "0555")
  withr::defer(Sys.chmod(ro, "0755"))
  withr::local_options(bdhs.data_dir = file.path(ro, "store"))
  src <- withr::local_tempdir()
  f <- write_fixture_dta(src, "BDKR81FL.DTA", 2022, "KR")
  expect_error(bdhs_import(f, format = "dta", quiet = TRUE), class = "bdhs_store_not_writable")
  expect_error(bdhs_import(f, copy = FALSE, quiet = TRUE), "Can't write to the bdhs data directory")
  expect_error(registry_write(registry_template(), file.path(ro, "store")), class = "bdhs_store_not_writable")
})
