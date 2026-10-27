import_fixture <- function(survey = 2022, recode = "IR", name = NULL, format = "dta", copy = TRUE,
                           env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  name <- name %||% paste0("BD", recode, survey_file_code(bdhs_survey_id(survey)), "FL.DTA")
  f <- write_fixture_dta(dir, name, survey, recode)
  bdhs_import(f, format = format, copy = copy, quiet = TRUE)
  f
}

for (fmt in c("dta", "parquet")) {
  test_that(paste("bdhs_read() selects variables and keeps the design (", fmt, ")"), {
    if (fmt == "parquet") skip_if_not_installed("arrow")
    local_bdhs_store()
    import_fixture(2022, "IR", format = fmt)
    full <- sim_recode(2022, "IR")

    ir <- bdhs_read(2022, "IR")
    expect_s3_class(ir, "tbl_df")
    expect_equal(dim(ir), dim(full))
    expect_equal(attr(ir, "bdhs_survey_id"), "BD2022DHS")
    expect_equal(attr(ir, "bdhs_recode"), "IR")
    expect_equal(attr(ir, "bdhs_file"), "BDIR81FL")

    sel <- bdhs_read("2022", "ir", vars = c("V106", "v012"))
    design <- c("caseid", "v001", "v002", "v003", "v005", "v021", "v022", "v023", "v024", "v025",
                "awfactt", "awfactu", "awfactr", "awfacte", "awfactw")
    expect_setequal(names(sel), c(design, "v106", "v012"))
    # File order is kept.
    expect_equal(names(sel), names(full)[names(full) %in% names(sel)])
    expect_s3_class(sel$v106, "haven_labelled")
    expect_equal(attr(sel$v106, "label"), attr(full$v106, "label"))
    expect_equal(attr(sel, "bdhs_file"), "BDIR81FL")

    expect_named(bdhs_read(2022, "IR", vars = "v012", keep_design = FALSE), "v012")
    rx <- bdhs_read(2022, "IR", vars_regex = "^b3_0[1-3]$", keep_design = FALSE)
    expect_named(rx, c("b3_01", "b3_02", "b3_03"))
    expect_equal(nrow(bdhs_read(2022, "IR", vars = "v012", n_max = 5)), 5)
    expect_warning(w <- bdhs_read(2022, "IR", vars = c("v012", "zz999"), keep_design = FALSE), "zz999")
    expect_named(w, "v012")
    expect_error(
      suppressWarnings(bdhs_read(2022, "IR", vars = "zz999", keep_design = FALSE)),
      "None of the requested"
    )
  })
}

test_that("bdhs_read() label modes", {
  local_bdhs_store()
  import_fixture(2022, "KR")
  kr <- bdhs_read(2022, "KR", vars = c("b4", "v106", "hw70"), labels = "factor")
  expect_s3_class(kr$b4, "factor")
  expect_equal(levels(kr$b4), c("male", "female"))
  expect_equal(attr(kr$b4, "label"), "sex of child")
  expect_type(kr$hw70, "double")
  codes <- bdhs_read(2022, "KR", vars = "b4", labels = "codes", keep_design = FALSE)
  expect_false(inherits(codes$b4, "haven_labelled"))
  expect_null(attr(codes$b4, "labels"))
  expect_equal(attr(codes$b4, "label"), "sex of child")
  expect_error(bdhs_read(2022, "KR", labels = "nope"), "must be one of")
})

test_that("partially labelled variables stay numeric with labels = 'factor'", {
  x <- haven::labelled(c(-150, 30, 9996), c("flagged cases" = 9996))
  df <- apply_labels(tibble::tibble(x = x, y = haven::labelled(c(1, 2, 1), c(a = 1, b = 2))), "factor")
  expect_s3_class(df$x, "haven_labelled")
  expect_s3_class(df$y, "factor")
})

test_that("bdhs_read() explains what to do when data are missing", {
  local_bdhs_store()
  expect_error(bdhs_read(2022, "IR"), "not in your bdhs data store")
  expect_error(bdhs_read(2022, "IR"), "new-user-registration")
  expect_error(bdhs_read(2022, "IR"), "bdhs_import")
  expect_error(bdhs_read(2014, "MR"), "never released")
  expect_error(bdhs_read(2014, "MR"), "BD2011DHS")
  expect_error(bdhs_read(c(2014, 2022), "IR"), "single survey")
  expect_error(bdhs_read(2022, "XX"), "recode")
  expect_error(bdhs_read(2022, "IR", n_max = -1), "n_max")

  dir <- withr::local_tempdir()
  f <- write_fixture_dta(dir, "BDKR81FL.DTA", 2022, "KR")
  bdhs_import(f, copy = FALSE, quiet = TRUE)
  file.remove(f)
  expect_error(bdhs_read(2022, "KR"), "moved or deleted")
})

test_that("bdhs_read() finds linked files and reads SPSS files", {
  local_bdhs_store()
  dir <- withr::local_tempdir()
  f <- write_fixture_sav(dir, "BDHR81FL.SAV", 2022, "HR")
  reg <- bdhs_import(f, copy = FALSE, quiet = TRUE)
  expect_equal(reg$format, "sav")
  hr <- bdhs_read(2022, "HR", vars = "HV270")
  expect_true(all(names(hr) == tolower(names(hr))))
  expect_true(all(c("hhid", "hv001", "hv005", "hv022", "hv270") %in% names(hr)))
  expect_s3_class(hr$hv270, "haven_labelled")
})

test_that("bdhs_read_file() reads and identifies any DHS file", {
  dir <- withr::local_tempdir()
  f <- write_fixture_dta(dir, "BDHS2022KR.dta", 2022, "KR")
  kr <- bdhs_read_file(f, vars = "hw70")
  expect_equal(attr(kr, "bdhs_survey_id"), "BD2022DHS")
  expect_equal(attr(kr, "bdhs_recode"), "KR")
  expect_equal(attr(kr, "bdhs_file"), "BDKR81FL")
  expect_true(all(c("caseid", "midx", "bidx", "v005", "hw70") %in% names(kr)))

  s <- write_fixture_sav(dir, "BDPR81FL.SAV", 2022, "PR")
  pr <- bdhs_read_file(s, vars = c("hv104", "hv105"), keep_design = FALSE, n_max = 4)
  expect_named(pr, c("hv104", "hv105"))
  expect_equal(nrow(pr), 4)
  expect_equal(attr(pr, "bdhs_recode"), "PR")

  other <- file.path(dir, "auto.dta")
  haven::write_dta(data.frame(x = 1:2), other)
  expect_warning(o <- bdhs_read_file(other), "Could not identify")
  expect_null(attr(o, "bdhs_survey_id"))

  pooled <- dplyr::bind_rows(haven::zap_labels(sim_recode(2004, "KR")), haven::zap_labels(sim_recode(2022, "KR")))
  p <- write_fixture_dta(dir, "pooled.dta", data = pooled)
  expect_warning(bdhs_read_file(p, vars = "hw70"), "derived or pooled")
  expect_error(bdhs_read_file(file.path(dir, "none.dta")), "existing file")
})

test_that("bdhs_read_file() reads zips with one dataset", {
  skip_if_no_zip()
  stage <- withr::local_tempdir()
  dir <- withr::local_tempdir()
  write_fixture_dta(stage, "BDBR81FL.DTA", 2022, "BR")
  z <- write_fixture_zip(file.path(dir, "BDBR81DT.ZIP"), stage, "BDBR81FL.DTA")
  br <- bdhs_read_file(z, vars = "b4")
  expect_equal(attr(br, "bdhs_recode"), "BR")
  expect_true("b4" %in% names(br))
  write_fixture_dta(stage, "BDKR81FL.DTA", 2022, "KR")
  z2 <- write_fixture_zip(file.path(dir, "two.zip"), stage, c("BDBR81FL.DTA", "BDKR81FL.DTA"))
  expect_error(bdhs_read_file(z2), "several datasets")
})

test_that("local files can be registered in place and read", {
  skip_if_no_local_data()
  base <- local_data_dir()
  files <- file.path(base, c(
    "BDHS Caeserean/Data/BDHS1993KR.DTA",
    "BDHS Caeserean/Data/BDHS1999KR.DTA",
    "BDHS Caeserean/Data/BDHS2018KR.DTA",
    "BDHS NPD/Data/2022BDPR81FL.DTA",
    "BDHS Caeserean/Data/BDHS_2004_2022_KR_appended.dta"
  ))
  skip_if_not(all(file.exists(files)), "some local files are missing")
  md5_before <- unname(tools::md5sum(files))
  store <- local_bdhs_store()
  expect_warning(reg <- bdhs_import(files, copy = FALSE, quiet = TRUE), "derived or pooled")
  expect_true(all(reg$linked))
  expect_equal(
    paste(reg$survey_id, reg$recode),
    c("BD1994DHS KR", "BD2000DHS KR", "BD2017DHS KR", "BD2022DHS PR")
  )
  expect_equal(reg$file_stem, c("BDKR31FL", "BDKR41FL", "BDKR7RFL", "BDPR81FL"))
  # Nothing was copied into the store, and the originals are untouched.
  expect_equal(list.files(store), "registry.rds")
  expect_equal(unname(tools::md5sum(files)), md5_before)

  kr <- bdhs_read(2017, "KR", vars = c("hw70", "m14"))
  expect_equal(nrow(kr), reg$n_rows[reg$survey_id == "BD2017DHS"])
  expect_true(all(c("caseid", "v005", "v021", "v022", "awfactt", "hw70", "m14") %in% names(kr)))
  expect_s3_class(kr$m14, "haven_labelled")
  expect_equal(attr(kr, "bdhs_file"), "BDKR7RFL")
  pr <- bdhs_read(2022, "PR", vars = "hv104", n_max = 100)
  expect_equal(nrow(pr), 100)
  expect_true(all(c("hhid", "hvidx", "hv005", "hv022", "hv104") %in% names(pr)))
  old <- bdhs_read(1994, "KR", vars = "v024", labels = "factor")
  expect_equal(nlevels(old$v024), 5)
})
