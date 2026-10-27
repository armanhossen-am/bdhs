test_that("standard, lower-case and prefixed DHS names are parsed", {
  p <- function(x) {
    r <- parse_dhs_name(x)
    c(r$survey_id, r$recode, r$file_stem, r$kind)
  }
  expect_equal(p("BDIR81FL.DTA"), c("BD2022DHS", "IR", "BDIR81FL", "standard"))
  expect_equal(p("bdkr7rfl.dta"), c("BD2017DHS", "KR", "BDKR7RFL", "standard"))
  expect_equal(p("BDIR7RDT.ZIP"), c("BD2017DHS", "IR", "BDIR7RFL", "standard"))
  expect_equal(p("BDIR7RFL.ZIP"), c("BD2017DHS", "IR", "BDIR7RFL", "standard"))
  expect_equal(p("2022BDPR81FL.DTA"), c("BD2022DHS", "PR", "BDPR81FL", "standard"))
  expect_equal(p("2017BDPR7RFL.DTA"), c("BD2017DHS", "PR", "BDPR7RFL", "standard"))
  expect_equal(p("BDKR41FL.DTA"), c("BD2000DHS", "KR", "BDKR41FL", "standard"))
  # Recode versions that differ from the round's file code come from the catalogue.
  expect_equal(p("BDSQ35FL.DTA"), c("BD1994DHS", "SQ", "BDSQ35FL", "standard"))
  expect_equal(p("BDHW4JFL.DTA")[1:2], c("BD2004DHS", "HW"))
})

test_that("renamed files are parsed from the year and recode in the name", {
  p <- function(x) {
    r <- parse_dhs_name(x)
    c(r$survey_id, r$recode, r$file_stem)
  }
  expect_equal(p("BDHS2018KR.DTA"), c("BD2017DHS", "KR", "BDKR7RFL"))
  expect_equal(p("BDHS1993KR.DTA"), c("BD1994DHS", "KR", "BDKR31FL"))
  expect_equal(p("BDHS1999KR.DTA"), c("BD2000DHS", "KR", "BDKR41FL"))
  expect_equal(p("bdhs_2014_ir.dta"), c("BD2014DHS", "IR", "BDIR72FL"))
  pooled <- parse_dhs_name("BDHS_2004_2022_KR_appended.dta")
  expect_true(pooled$derived)
  expect_true(is.na(pooled$survey_id))
  expect_match(pooled$note, "several rounds")
  expect_true(parse_dhs_name("BDHS_2004_2022_KR_FINAL.dta")$derived)
  expect_equal(parse_dhs_name("mydata.dta")$kind, "none")
})

test_that("bdhs_identify_file() uses names, then content", {
  dir <- withr::local_tempdir()
  write_fixture_dta(dir, "BDIR81FL.DTA", 2022, "IR")
  write_fixture_dta(dir, "BDHS2018KR.DTA", 2017, "KR")
  write_fixture_dta(dir, "children.dta", 2022, "KR")
  write_fixture_dta(dir, "hh members.dta", 2014, "PR")
  id <- bdhs_identify_file(dir)
  expect_s3_class(id, "tbl_df")
  expect_named(id, c("path", "file_stem", "survey_id", "recode", "format", "method", "derived", "note"))
  id <- id[order(basename(id$path)), ]
  expect_equal(basename(id$path), c("BDHS2018KR.DTA", "BDIR81FL.DTA", "children.dta", "hh members.dta"))
  expect_equal(id$survey_id, c("BD2017DHS", "BD2022DHS", "BD2022DHS", "BD2014DHS"))
  expect_equal(id$recode, c("KR", "IR", "KR", "PR"))
  expect_equal(id$file_stem, c("BDKR7RFL", "BDIR81FL", "BDKR81FL", "BDPR72FL"))
  expect_equal(id$method, c("filename", "filename", "content", "content"))
  expect_equal(id$format, rep("dta", 4))
  expect_false(any(id$derived))
})

test_that("content identification copes with 2-digit years and shared phase codes", {
  dir <- withr::local_tempdir()
  kr <- sim_recode(2000, "KR")
  kr$v000 <- "BD3" # the 1999-2000 KR shares v000 with 1993-94
  kr$v007 <- ifelse(seq_len(nrow(kr)) %% 2 == 0, 99, 0)
  f <- write_fixture_dta(dir, "kids.dta", data = kr)
  id <- bdhs_identify_file(f)
  expect_equal(id$survey_id, "BD2000DHS")
  expect_equal(id$recode, "KR")
  expect_equal(id$file_stem, "BDKR41FL")
  kr$v007 <- 93
  f <- write_fixture_dta(dir, "kids93.dta", data = kr)
  expect_equal(bdhs_identify_file(f)$survey_id, "BD1994DHS")
})

test_that("KR and BR are told apart by the children's ages", {
  dir <- withr::local_tempdir()
  br <- sim_recode(2017, "BR")
  skip_if_not(max(br$v008 - br$b3) > 60, "simulated births are all recent")
  br$midx <- br$bidx # the 2017-18 BR carries midx, like the KR
  write_fixture_dta(dir, "births.dta", data = br)
  write_fixture_dta(dir, "kids.dta", 2017, "KR")
  id <- bdhs_identify_file(file.path(dir, c("births.dta", "kids.dta")))
  expect_equal(id$recode, c("BR", "KR"))
  expect_equal(id$file_stem, c("BDBR7RFL", "BDKR7RFL"))
})

test_that("pooled and derived files are flagged", {
  dir <- withr::local_tempdir()
  pooled <- dplyr::bind_rows(haven::zap_labels(sim_recode(2004, "KR")), haven::zap_labels(sim_recode(2022, "KR")))
  f1 <- write_fixture_dta(dir, "kr_all.dta", data = pooled)
  extra <- sim_recode(2022, "KR")
  extra$csection <- 1
  extra$age_cat <- 1
  extra$year <- 2022
  f2 <- write_fixture_dta(dir, "BDHS2022KR_clean.dta", data = extra)
  f3 <- write_fixture_dta(dir, "BDHS_2004_2022_KR_appended.dta", data = pooled)
  id <- bdhs_identify_file(c(f1, f2, f3))
  expect_equal(id$derived, c(TRUE, TRUE, TRUE))
  expect_match(id$note[[1]], "pooled file")
  expect_match(id$note[[2]], "not in DHS recodes")
  expect_equal(id$survey_id[[2]], "BD2022DHS")
  expect_equal(id$method, rep("content", 3))
})

test_that("zip members are listed without extracting", {
  skip_if_no_zip()
  stage <- withr::local_tempdir()
  dir <- withr::local_tempdir()
  write_fixture_dta(stage, "BDKR81FL.DTA", 2022, "KR")
  writeLines("dictionary", file.path(stage, "BDKR81FL.DCT"))
  z <- write_fixture_zip(file.path(dir, "BDKR81DT.ZIP"), stage, c("BDKR81FL.DTA", "BDKR81FL.DCT"))
  id <- bdhs_identify_file(z)
  expect_equal(nrow(id), 1)
  expect_equal(id$survey_id, "BD2022DHS")
  expect_equal(id$recode, "KR")
  expect_match(id$path, "BDKR81DT.ZIP", fixed = TRUE)

  flat <- file.path(dir, "BDKR81FL.ZIP")
  write_fixture_zip(flat, stage, "BDKR81FL.DCT")
  id <- bdhs_identify_file(flat)
  expect_equal(id$format, "zip")
  expect_match(id$note, "flat ASCII")
})

test_that("non-data and unknown files are reported, not identified", {
  dir <- withr::local_tempdir()
  other <- data.frame(x = 1:3, y = c("a", "b", "c"))
  f <- file.path(dir, "auto.dta")
  haven::write_dta(other, f)
  id <- bdhs_identify_file(f)
  expect_true(is.na(id$survey_id))
  expect_true(is.na(id$method))
  expect_match(id$note, "could not identify")
  expect_error(bdhs_identify_file(file.path(dir, "nope.dta")), "Can't find")
})

test_that("a name/content conflict is refused, naming both", {
  dir <- withr::local_tempdir()
  # Children's recode content saved under the women's recode name.
  f_rec <- write_fixture_dta(dir, "BDIR81FL.DTA", 2022, "KR")
  # A 2014 file under a 2022 name (renamed and standard).
  f_round <- write_fixture_dta(dir, "BDHS2022IR.dta", 2014, "IR")
  f_std <- write_fixture_dta(dir, "BDKR81FL.DTA", 2014, "KR")
  # A household-member file of another country.
  pr <- sim_recode(2014, "PR")
  pr$hv000 <- "NP7"
  f_ctry <- write_fixture_dta(dir, "BDHS2014PR.dta", data = pr)

  expect_warning(id <- bdhs_identify_file(c(f_rec, f_round, f_ctry)), "disagree")
  expect_true(all(is.na(id$survey_id)))
  expect_true(all(is.na(id$recode)))
  expect_true(all(is.na(id$method)))
  expect_match(id$note[[1]], "the name says BD2022DHS IR but the variables are those of a KR file")
  expect_match(id$note[[2]], "the name says BD2022DHS IR but the interview dates are from BD2014DHS")
  expect_match(id$note[[3]], "the country code is NP")

  # A standard name is checked against the interview dates when the content is
  # read (as bdhs_import() does).
  r <- identify_one(f_std, content = "always")
  expect_equal(r$refused, "conflict")
  expect_true(is.na(r$survey_id))
  expect_match(r$note, "BD2022DHS KR but the interview dates are from BD2014DHS")
})

test_that("files whose name and content agree are not refused", {
  dir <- withr::local_tempdir()
  br <- sim_recode(2017, "BR")
  br$midx <- br$bidx # the 2017-18 BR carries midx, like the KR
  f_br <- write_fixture_dta(dir, "BDBR7RFL.DTA", data = br)
  f_kr <- write_fixture_dta(dir, "BDHS2018KR.DTA", 2017, "KR")
  # Without b3 the content cannot tell KR from BR: not a conflict.
  br$b3 <- NULL
  f_nob3 <- write_fixture_dta(withr::local_tempdir(), "BDBR7RFL.DTA", data = br)
  ids <- lapply(c(f_br, f_kr, f_nob3), identify_one, content = "always")
  expect_equal(vapply(ids, function(x) x$survey_id, ""), rep("BD2017DHS", 3))
  expect_equal(vapply(ids, function(x) x$recode, ""), c("BR", "KR", "BR"))
  expect_true(all(is.na(vapply(ids, function(x) x$refused, ""))))
  expect_no_warning(bdhs_identify_file(c(f_br, f_kr, f_nob3)))
})

test_that("renamed files need positive confirmation from their content", {
  dir <- withr::local_tempdir()
  # Not a DHS file at all, but named like one.
  f_fake <- file.path(dir, "BDHS2014HR.dta")
  haven::write_dta(data.frame(id = 1:5, income = c(10, 20, 30, 40, 50), district = 1:5), f_fake)
  # DHS-like variables but no country code.
  ir <- sim_recode(2014, "IR")
  ir$v000 <- NULL
  f_nocc <- write_fixture_dta(dir, "BDHS2014IR.dta", data = ir)
  expect_warning(id <- bdhs_identify_file(c(f_fake, f_nocc)), "could not be confirmed")
  expect_true(all(is.na(id$survey_id)))
  expect_match(id$note[[1]], "the name suggests BD2014DHS HR but the content does not confirm it")
  expect_match(id$note[[1]], "no country code")
  expect_match(id$note[[1]], "hhid")
  expect_match(id$note[[2]], "no country code")

  # Without a name, unconfirmed content is not identified either.
  f_kids <- write_fixture_dta(dir, "kids.dta", data = ir)
  kr <- sim_recode(2014, "KR")
  kr$v000 <- "IA7"
  f_ia <- write_fixture_dta(dir, "india.dta", data = kr)
  id <- bdhs_identify_file(c(f_kids, f_ia))
  expect_true(all(is.na(id$survey_id)))
  expect_true(all(is.na(id$method)))
  expect_match(id$note[[1]], "does not confirm")
  expect_match(id$note[[2]], "country code IA")

  # Genuine renamed files are still identified.
  f_ok <- write_fixture_dta(dir, "bdhs_2014_hr.dta", 2014, "HR")
  id <- bdhs_identify_file(f_ok)
  expect_equal(c(id$survey_id, id$recode, id$method), c("BD2014DHS", "HR", "filename"))
})

test_that("SPA and BMMS names map to their own surveys", {
  p <- function(x) {
    r <- parse_dhs_name(x)
    c(r$survey_id, r$recode, r$file_stem, r$kind)
  }
  expect_equal(p("SPA2014_FC.dta"), c("BD2014SPA", "FC", "BDFC7AFL", "renamed"))
  expect_equal(p("BDHS2017FC.dta"), c("BD2017SPA", "FC", "BDFC7IFL", "renamed"))
  expect_equal(p("spa_1999_pv.sav"), c("BD1999SPA", "PV", "BDPV4RFL", "renamed"))
  expect_equal(p("BMMS2001_HH.dta"), c("BD2001OTH", "HH", "BDHH4AFL", "renamed"))
  # Standard SPA names end in SP.
  expect_equal(p("BDFC7AFLSP.DTA"), c("BD2014SPA", "FC", "BDFC7AFL", "standard"))
  expect_equal(p("BDKR81FLSP.DTA")[[4]], "none")
  # An SPA name with a DHS recode is ambiguous.
  amb <- parse_dhs_name("SPA2014_KR.dta")
  expect_true(amb$ambiguous)
  expect_true(is.na(amb$survey_id))

  dir <- withr::local_tempdir()
  f_spa <- file.path(dir, "SPA2014_FC.dta")
  haven::write_dta(data.frame(facil = 1:3, q100 = 1:3), f_spa)
  f_amb <- write_fixture_dta(dir, "SPA2014_KR.dta", 2014, "KR")
  expect_warning(id <- bdhs_identify_file(c(f_spa, f_amb)), "could not be confirmed")
  expect_true(all(is.na(id$survey_id)))
  expect_match(id$note[[1]], "BD2014SPA FC")
  expect_match(id$note[[1]], "BDFC7AFL.DTA", fixed = TRUE)
  expect_match(id$note[[2]], "ambiguous")
})

test_that("normalize_years() handles 2-digit years", {
  expect_equal(normalize_years(c(93, 99, 0, 2017, NA)), c(1993, 1999, 2000, 2017, NA))
})

test_that("local files are identified correctly", {
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
  id <- bdhs_identify_file(files)
  expect_equal(id$survey_id[1:4], c("BD1994DHS", "BD2000DHS", "BD2017DHS", "BD2022DHS"))
  expect_equal(id$recode[1:4], c("KR", "KR", "KR", "PR"))
  expect_equal(id$derived, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  # Every genuine local file passes the derived-file checks when read.
  genuine <- c(
    list.files(file.path(base, "BDHS Caeserean/Data"), "^BDHS[0-9]{4}KR\\.DTA$", full.names = TRUE),
    list.files(file.path(base, "BDHS NPD"), "FL\\.DTA$", recursive = TRUE, full.names = TRUE)
  )
  checked <- lapply(genuine, identify_one, content = "always")
  expect_false(any(vapply(checked, function(x) isTRUE(x$derived), logical(1))))
  expect_false(anyNA(vapply(checked, function(x) x$survey_id, character(1))))
  # Content agrees with the names (incl. the 2017-18 BR, which carries midx).
  notes <- vapply(checked, function(x) x$note %|NA|% "", character(1))
  expect_false(any(grepl("name suggests|disagree", notes)))
  expect_true(all(is.na(vapply(checked, function(x) x$refused, character(1)))))
  br <- checked[[grep("BDBR7RFL", genuine)]]
  expect_equal(br$recode, "BR")
})
