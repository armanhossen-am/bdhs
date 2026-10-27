# All tests here are offline: the HTTP layer (dhs_http) and the password lookup
# are mocked, and no real credentials are used.

test_that("dhs_files_for() maps surveys and recodes to DHS download files", {
  f <- dhs_files_for(2022, c("IR", "KR"))
  expect_equal(f$file_name, c("BDIR81DT.ZIP", "BDKR81DT.ZIP"))
  expect_equal(f$survey_id, c("BD2022DHS", "BD2022DHS"))
  expect_true(all(f$file_size > 0))
  expect_equal(dhs_files_for("2017-18", "PR", "spss")$file_name, "BDPR7RSV.ZIP")
  expect_equal(dhs_files_for(1994, "KR", "sas")$recode, "KR")
  expect_error(dhs_files_for(2014, "MR"), "Never released")
  expect_error(dhs_files_for(2022, "GE"), "Not available in stata format")
})

test_that("download URLs are selected by file name and country", {
  urls <- c(
    "https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=BDIR81DT.ZIP&Tp=1&Ctry_Code=BD&surv_id=599&dm=1&dmode=nm",
    "https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=BDKR81DT.ZIP&Tp=1&Ctry_Code=BD&surv_id=599&dm=1&dmode=nm",
    "https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NPIR81DT.ZIP&Tp=1&Ctry_Code=NP&surv_id=600&dm=1&dmode=nm",
    "https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=BDIR81DT.ZIP&Tp=1&Ctry_Code=IA&surv_id=1&dm=1&dmode=nm"
  )
  expect_equal(dhs_select_urls(urls, c("bdkr81dt.zip", "BDIR81DT.ZIP")), urls[c(2, 1)])
  expect_error(dhs_select_urls(urls, "BDPR81DT.ZIP"), "cannot download")
})

test_that("login and download-manager pages are parsed", {
  login <- paste0(
    "<form action='/search'><input type='hidden' name='q' value='x'></form>",
    "<form action=\"login_main.cfm\" METHOD=\"post\">",
    "<input type=\"hidden\" name=\"Submitted\" value=\"1\" />",
    "<input type=\"hidden\" name=\"CSRFTOKEN\" value=\"abc&amp;123\" />",
    "<input type=\"text\" name=\"UserName\" value=\"\"/><input type=\"password\" name=\"UserPass\"/>",
    "</form><form action=\"login_main.cfm\"><input type=\"hidden\" name=\"forgotpass\" value=\"yes\" /></form>"
  )
  expect_equal(html_form_hidden(login, "UserPass"), list(Submitted = "1", CSRFTOKEN = "abc&123"))

  projects <- paste0(
    "<select name='proj_id'><option value=\"\">Select a project</option>",
    "<option value=\"111\">Child health in Bangladesh: trends across all rounds</option>",
    "<option value=\"222\">Maternal care</option></select>"
  )
  expect_equal(dhs_project_id(projects, "Child health in Bangladesh: trends across all rounds"), "111")
  expect_equal(dhs_project_id(projects, "maternal care"), "222")
  expect_error(dhs_project_id(projects, "Other"), "None of your DHS projects")
  expect_error(dhs_project_id("<html>Invalid login</html>", "x"), "Signing in")

  manager <- paste0(
    "<input type=\"checkbox\" name=\"ctrycodelist\" value=\"BD\">",
    "<input type=\"checkbox\" name=\"ctrycodelist\" value=\"NP\">",
    "<input type=\"hidden\" name=\"filedatatypelist_DHS\" value=\"1\">",
    "<input type=\"hidden\" name=\"fformatlist\" value=\"DT\">",
    "<input type=\"hidden\" name=\"other\" value=\"z\">"
  )
  f <- dhs_manager_fields(manager)
  expect_equal(f$ctrycodelist, c("BD", "NP"))
  expect_equal(f$filedatatypelist_DHS, "1")
  expect_null(f$other)

  expect_equal(
    dhs_urlslist_link("<a href=\"/data/download/urlslist_123.cfm?Proj_ID=111&amp;x=1\">list</a>"),
    "https://dhsprogram.com/data/download/urlslist_123.cfm?Proj_ID=111&x=1"
  )
  expect_error(dhs_urlslist_link("<a href='/other'>x</a>"), "list of download links")
})

test_that("bdhs_download() stops with manual instructions without credentials", {
  rlang::local_interactive(FALSE)
  withr::local_envvar(DHS_EMAIL = NA, DHS_PROJECT = NA, DHS_PASSWORD = NA)
  local_mocked_bindings(dhs_http = function(...) stop("network must not be used"))
  expect_error(bdhs_download(2022, "IR"), "not set")
  expect_error(bdhs_download(2022, "IR"), "bdhs_import")
  local_mocked_bindings(dhs_password = function(email) NULL)
  expect_error(
    bdhs_download(2022, "IR", email = "someone@example.org", project = "Test project"),
    "No DHS password"
  )
})

test_that("bdhs_download() follows the download-manager flow (mocked)", {
  skip_if_not_installed("httr2")
  skip_if_no_zip()
  local_bdhs_store()
  stage <- withr::local_tempdir()
  zips <- withr::local_tempdir()
  write_fixture_dta(stage, "BDKR81FL.DTA", 2022, "KR")
  fixture <- write_fixture_zip(file.path(zips, "BDKR81DT.ZIP"), stage, "BDKR81FL.DTA")

  calls <- list()
  fake_http <- function(session, url, body = NULL, path = NULL) {
    calls[[length(calls) + 1L]] <<- list(url = url, body = body)
    if (!is.null(path)) {
      file.copy(fixture, path, overwrite = TRUE)
      return(path)
    }
    if (grepl("login_main", url) && is.null(body)) {
      return("<form action='login_main.cfm'><input type='hidden' name='Submitted' value='1'><input name='UserPass' type='password'></form>")
    }
    if (grepl("login_main", url)) {
      return("<select><option value='4242'>Test project</option></select>")
    }
    if (identical(body$subaction, "Build URL File List")) {
      return("<a href='/data/download/urlslist_4242.cfm?Proj_ID=4242'>urls</a>")
    }
    if (identical(body$action, "downloadmanager")) {
      return("<input name='ctrycodelist' value='BD'><input name='ctrycodelist' value='NP'><input name='fformatlist' value='DT'>")
    }
    if (grepl("urlslist", url)) {
      return(paste0(
        "https://dhsprogram.com/data/download.cfm?Filename=BDKR81DT.ZIP&Tp=1&Ctry_Code=BD&surv_id=599\n",
        "https://dhsprogram.com/data/download.cfm?Filename=BDIR81DT.ZIP&Tp=1&Ctry_Code=BD&surv_id=599\n"
      ))
    }
    ""
  }
  local_mocked_bindings(
    dhs_http = fake_http,
    dhs_password = function(email) "not-a-real-password",
    dhs_files_for = function(survey, recode, format = "stata", call = NULL) {
      tibble::tibble(survey_id = "BD2022DHS", recode = "KR", file_name = "BDKR81DT.ZIP",
                     file_size = file.size(fixture))
    }
  )
  out <- capture_msgs(bdhs_download(2022, "KR", email = "someone@example.org", project = "Test project"))
  expect_match(out$text, "experimental")
  expect_match(out$text, "Imported 1 dataset")
  reg <- out$value
  expect_equal(reg$survey_id, "BD2022DHS")
  expect_equal(reg$recode, "KR")
  expect_equal(nrow(bdhs_read(2022, "KR", vars = "hw70")), nrow(sim_recode(2022, "KR")))

  urls <- vapply(calls, function(x) x$url, character(1))
  expect_match(urls[[2]], "login_main.cfm")
  login_body <- calls[[2]]$body
  expect_equal(login_body$UserName, "someone@example.org")
  expect_equal(login_body$UserType, "2")
  build <- Filter(function(x) identical(x$body$subaction, "Build URL File List"), calls)[[1]]$body
  expect_equal(build$Proj_ID, "4242")
  expect_equal(build$ctrycodelist, "BD")
  expect_match(urls[[length(urls)]], "Filename=BDKR81DT.ZIP", fixed = TRUE)
  # The password never ends up in the store or the registry.
  expect_false(any(grepl("not-a-real-password", unlist(registry_read()))))
})

test_that("dhs_fetch() signs in again when the file size is wrong", {
  skip_if_no_zip()
  stage <- withr::local_tempdir()
  dir <- withr::local_tempdir()
  write_fixture_dta(stage, "BDKR81FL.DTA", 2022, "KR")
  fixture <- write_fixture_zip(file.path(dir, "fixture.zip"), stage, "BDKR81FL.DTA")
  n <- 0
  local_mocked_bindings(dhs_http = function(session, url, body = NULL, path = NULL) {
    n <<- n + 1
    if (n == 1) writeLines("<html>session expired</html>", path) else file.copy(fixture, path, overwrite = TRUE)
    path
  })
  relogins <- 0
  dest <- file.path(dir, "BDKR81DT.ZIP")
  out <- dhs_fetch(list(), "u", dest, file.size(fixture), function() relogins <<- relogins + 1)
  expect_equal(out, dest)
  expect_equal(relogins, 1)
  expect_equal(n, 2)

  # A valid zip of another size is accepted with a warning; HTML is not.
  expect_warning(dhs_fetch(list(), "u", dest, 1, function() NULL), "catalogue")
  local_mocked_bindings(dhs_http = function(session, url, body = NULL, path = NULL) {
    writeLines("<html>login</html>", path)
    path
  })
  expect_error(dhs_fetch(list(), "u", dest, 10, function() NULL), "did not send the data file")
  expect_false(file.exists(dest))
})

test_that("form posts never follow redirects off dhsprogram.com", {
  skip_if_not_installed("httr2")
  session <- dhs_session()
  form <- list(UserName = "someone@example.org", UserPass = "not-a-real-password")
  seen <- list()
  record <- function(req) {
    seen[[length(seen) + 1L]] <<- list(
      url = req$url, has_body = !is.null(req$body), follow = req$options$followlocation
    )
  }
  # 307 to another host: the form (with the password) is not sent there.
  httr2::local_mocked_responses(function(req) {
    record(req)
    httr2::response(307, headers = list(Location = "https://collector.example/steal"))
  })
  expect_error(dhs_http(session, "https://dhsprogram.com/data/dataset_admin/login_main.cfm", body = form),
               class = "bdhs_unsafe_url")
  expect_length(seen, 1)
  expect_equal(seen[[1]]$follow, 0L)
  # Look-alike hosts, plain http and user-info tricks are refused too.
  for (loc in c("https://dhsprogram.com.example.net/x", "http://dhsprogram.com/x",
                "https://dhsprogram.com@collector.example/x", "https://collector.example/dhsprogram.com")) {
    seen <- list()
    httr2::local_mocked_responses(function(req) {
      record(req)
      httr2::response(302, headers = list(Location = loc))
    })
    expect_error(dhs_http(session, "https://dhsprogram.com/a", body = form), class = "bdhs_unsafe_url")
    expect_length(seen, 1)
  }

  # Same-site redirects: 302 continues with a GET, 307 re-sends the form.
  for (status in c(302L, 307L)) {
    seen <- list()
    httr2::local_mocked_responses(function(req) {
      record(req)
      if (length(seen) == 1L) {
        httr2::response(status, headers = list(Location = "/data/dataset_admin/index.cfm"))
      } else {
        httr2::response(200, body = charToRaw("<html>ok</html>"))
      }
    })
    expect_equal(dhs_http(session, "https://dhsprogram.com/data/dataset_admin/login_main.cfm", body = form), "<html>ok</html>")
    expect_length(seen, 2)
    expect_equal(seen[[2]]$url, "https://dhsprogram.com/data/dataset_admin/index.cfm")
    expect_equal(seen[[2]]$has_body, status == 307L)
  }
})

test_that("only https links to dhsprogram.com are followed", {
  expect_equal(
    dhs_urlslist_link("<a href=\"https://www.dhsprogram.com/data/download/urlslist_1.cfm\">list</a>"),
    "https://www.dhsprogram.com/data/download/urlslist_1.cfm"
  )
  for (href in c("https://collector.example/data/download/urlslist_1.cfm",
                 "http://dhsprogram.com/data/download/urlslist_1.cfm",
                 "https://dhsprogram.com.collector.example/data/download/urlslist_1.cfm",
                 "https://user@collector.example/data/download/urlslist_1.cfm",
                 "//collector.example/data/download/urlslist_1.cfm")) {
    expect_error(dhs_urlslist_link(paste0("<a href='", href, "'>x</a>")), class = "bdhs_unsafe_url")
  }
  good <- "https://dhsprogram.com/data/download.cfm?Filename=BDKR81DT.ZIP&Ctry_Code=BD"
  bad <- "https://collector.example/download.cfm?Filename=BDKR81DT.ZIP&Ctry_Code=BD"
  expect_error(dhs_select_urls(bad, "BDKR81DT.ZIP"), class = "bdhs_unsafe_url")
  expect_error(dhs_select_urls(sub("https", "http", good), "BDKR81DT.ZIP"), "not on")
  expect_equal(dhs_select_urls(c(bad, good), "BDKR81DT.ZIP"), good)
  expect_equal(dhs_safe_url(c(good, bad, NA)), c(good, NA, NA))
})

test_that("a failed sign-in leaves the password nowhere", {
  skip_if_not_installed("httr2")
  # A random fake password, so that it does not occur in this file's source
  # (which error backtraces reference).
  password <- paste0("fake-", paste(sample(c(letters, 0:9), 24, replace = TRUE), collapse = ""))
  creds <- list(email = "someone@example.org", password = password, project = "Test project")
  contains_password <- function(x) {
    any(grepl(password, rawToChar(serialize(x, NULL, ascii = TRUE)), fixed = TRUE))
  }
  login_page <- "<form action='login_main.cfm'><input type='hidden' name='Submitted' value='1'><input name='UserPass' type='password'></form>"

  # HTTP error: httr2's error carries the request, form included.
  local_mocked_bindings(dhs_http = function(session, url, body = NULL, path = NULL, call = NULL) {
    if (is.null(body)) {
      return(login_page)
    }
    req <- httr2::req_body_form(httr2::request(url), !!!body)
    httr2::with_mocked_responses(function(req) httr2::response(500), httr2::req_perform(req))
  })
  err <- tryCatch(dhs_login(dhs_session(), creds), error = function(e) e)
  expect_s3_class(err, "bdhs_login_failed")
  expect_match(conditionMessage(err), "HTTP status 500")
  expect_null(err$parent)
  expect_false(contains_password(err))

  # Connection failure on a real request (to a closed local port): httr2 also
  # records the request for httr2::last_request().
  skip_on_cran()
  local_mocked_bindings(dhs_http = function(session, url, body = NULL, path = NULL, call = NULL) {
    if (is.null(body)) {
      return(login_page)
    }
    req <- httr2::req_body_form(httr2::request("http://127.0.0.1:9/login"), !!!body)
    httr2::req_perform(httr2::req_options(req, connecttimeout = 2))
  })
  err <- tryCatch(dhs_login(dhs_session(), creds), error = function(e) e)
  expect_s3_class(err, "bdhs_login_failed")
  expect_null(err$parent)
  expect_false(contains_password(err))
  expect_false(contains_password(httr2::last_request()))
  expect_false(contains_password(httr2::last_response()))
})

test_that("the password is never read with a visible prompt", {
  rlang::local_interactive(TRUE)
  withr::local_envvar(DHS_PASSWORD = NA)
  asked <- character()
  local_mocked_bindings(
    dhs_keyring_password = function(email) NULL,
    has_askpass = function() FALSE,
    dhs_ask_password = function(prompt) {
      asked <<- c(asked, prompt)
      "typed"
    }
  )
  expect_null(dhs_password("someone@example.org"))
  expect_length(asked, 0)
  expect_error(dhs_credentials("someone@example.org", "Test project"), "askpass")
  expect_error(dhs_credentials("someone@example.org", "Test project"), "DHS_PASSWORD")
  expect_false(any(grepl("readline", deparse(dhs_password), fixed = TRUE)))

  local_mocked_bindings(has_askpass = function() TRUE)
  expect_equal(dhs_password("someone@example.org"), "typed")
  expect_length(asked, 1)
  withr::local_envvar(DHS_PASSWORD = "from-env")
  expect_equal(dhs_password("someone@example.org"), "from-env")
})

test_that("the rdhs engine leaves rdhs's cache alone", {
  skip_if_not_installed("rdhs")
  skip_if_no_zip()
  local_bdhs_store()
  stage <- withr::local_tempdir()
  cache <- withr::local_tempdir("rdhs-cache-")
  write_fixture_dta(stage, "BDKR81FL.DTA", 2022, "KR")
  zip <- write_fixture_zip(file.path(cache, "BDKR81DT.ZIP"), stage, "BDKR81FL.DTA")
  local_mocked_bindings(get_datasets = function(dataset_filenames, ...) list(BDKR81DT = zip), .package = "rdhs")
  out <- capture_msgs(bdhs_download(2022, "KR", engine = "rdhs"))
  expect_match(out$text, "rdhs keeps the downloaded zip in its cache")
  expect_equal(out$value$recode, "KR")
  expect_true(file.exists(zip))
})

test_that("zips downloaded with import = FALSE can be imported afterwards", {
  skip_if_not_installed("httr2")
  skip_if_no_zip()
  store <- local_bdhs_store()
  stage <- withr::local_tempdir()
  zips <- withr::local_tempdir()
  write_fixture_dta(stage, "BDHR81FL.DTA", 2022, "HR")
  fixture <- write_fixture_zip(file.path(zips, "BDHR81DT.ZIP"), stage, "BDHR81FL.DTA")
  local_mocked_bindings(
    dhs_http = function(session, url, body = NULL, path = NULL) {
      if (!is.null(path)) {
        file.copy(fixture, path, overwrite = TRUE)
        return(path)
      }
      if (grepl("login_main", url) && is.null(body)) {
        return("<form><input type='hidden' name='Submitted' value='1'><input name='UserPass' type='password'></form>")
      }
      if (grepl("login_main", url)) {
        return("<select><option value='4242'>Test project</option></select>")
      }
      if (identical(body$subaction, "Build URL File List")) {
        return("<a href='/data/download/urlslist_4242.cfm?Proj_ID=4242'>urls</a>")
      }
      if (grepl("urlslist", url)) {
        return("https://dhsprogram.com/data/download.cfm?Filename=BDHR81DT.ZIP&Tp=1&Ctry_Code=BD\n")
      }
      ""
    },
    dhs_password = function(email) "not-a-real-password",
    dhs_files_for = function(survey, recode, format = "stata", call = NULL) {
      tibble::tibble(survey_id = "BD2022DHS", recode = "HR", file_name = "BDHR81DT.ZIP",
                     file_size = file.size(fixture))
    }
  )
  dl <- capture_msgs(bdhs_download(2022, "HR", email = "someone@example.org", project = "Test project", import = FALSE))$value
  expect_equal(dirname(dl$path), normalize_path(file.path(store, "downloads")))
  reg <- bdhs_import(file.path(bdhs_data_dir(), "downloads"), format = "dta", quiet = TRUE)
  expect_equal(reg$recode, "HR")
  expect_equal(reg$survey_id, "BD2022DHS")
})
