# Downloading datasets from the DHS Program website (experimental) ------------------------

.dhs_base <- "https://dhsprogram.com"
.dhs_admin <- "https://dhsprogram.com/data/dataset_admin/"
.dhs_keyring_service <- "bdhs-dhs"

#' Download BDHS datasets with your DHS Program account (experimental)
#'
#' @description
#' **Experimental.** The DHS Program has no download API, so this function
#' signs in to <https://dhsprogram.com> with *your* account and uses the
#' website's download manager, as a browser would (the approach of the rdhs
#' package). Website changes can break it at any time; the reliable route is to
#' download the files yourself and run [bdhs_import()].
#'
#' You can only download datasets that the DHS Program has approved for your
#' project. Register at <https://dhsprogram.com/data/new-user-registration.cfm>
#' and request access to the Bangladesh surveys first.
#'
#' @section Credentials:
#' * `email` and `project` default to the `DHS_EMAIL` and `DHS_PROJECT`
#'   environment variables (for example set in your `.Renviron`). `project`
#'   is the name of your approved DHS project (its first 30 characters are
#'   enough).
#' * The password is taken from the system keyring when the keyring package
#'   is installed (service `"bdhs-dhs"`, username = your email; store it once
#'   with `keyring::key_set("bdhs-dhs", username = "you@example.org")`), then
#'   from the `DHS_PASSWORD` environment variable, and is otherwise asked for
#'   interactively. bdhs never stores, prints or logs it, and does not keep it
#'   in options.
#'
#' In non-interactive sessions without credentials the function stops with
#' instructions for downloading the files manually.
#'
#' @param survey Survey references (see [bdhs_survey_id()]).
#' @param recode Recode codes (see [bdhs_recodes()]).
#' @param format File format to download: `"stata"` (recommended), `"spss"`
#'   or `"sas"`.
#' @param email Email address of your DHS Program account.
#' @param project Name of your approved DHS project.
#' @param import If `TRUE` (default), import the downloaded files with
#'   [bdhs_import()] and delete the downloaded zips. If `FALSE`, keep the zips
#'   in a `downloads` folder of [bdhs_data_dir()].
#' @param engine `"bdhs"` (default) uses bdhs's own downloader (needs the
#'   httr2 package). `"rdhs"` delegates to [rdhs::get_datasets()]; configure
#'   rdhs first with `rdhs::set_rdhs_config()`.
#' @return If `import = TRUE`, the registry rows of the imported datasets
#'   (see [bdhs_import()]); otherwise a tibble with the downloaded `file_name`
#'   and `path`. Invisibly.
#' @family data store
#' @export
#' @examplesIf interactive() && nzchar(Sys.getenv("DHS_EMAIL"))
#' bdhs_download(2022, c("IR", "KR"))
#' bdhs_read(2022, "KR", vars = "hw70")
bdhs_download <- function(survey, recode, format = "stata",
                          email = Sys.getenv("DHS_EMAIL"),
                          project = Sys.getenv("DHS_PROJECT"),
                          import = TRUE, engine = c("bdhs", "rdhs")) {
  engine <- rlang::arg_match(engine)
  format <- rlang::arg_match(format, c("stata", "spss", "sas"))
  files <- dhs_files_for(survey, recode, format)
  if (engine == "rdhs") {
    return(download_with_rdhs(files, import))
  }
  check_installed("httr2", "to download datasets from the DHS Program website")
  creds <- dhs_credentials(email, project, files)
  session <- dhs_session()
  on.exit(unlink(session$cookies), add = TRUE)

  cli::cli_inform(c("i" = "Signing in to the DHS Program website as {.email {creds$email}} (experimental)..."))
  proj_id <- dhs_login(session, creds)
  urls <- dhs_url_list(session, proj_id)
  urls <- dhs_select_urls(urls, files$file_name)

  dest_dir <- if (import) tempfile("bdhs-download-") else file.path(store_dir(), "downloads")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  if (import) {
    on.exit(unlink(dest_dir, recursive = TRUE), add = TRUE)
  } else {
    warn_insecure_store(store_dir())
  }
  relogin <- function() {
    id <- dhs_login(session, creds)
    dhs_open_download_manager(session, id)
    invisible(id)
  }
  paths <- character(nrow(files))
  for (i in seq_len(nrow(files))) {
    cli::cli_inform(c("i" = "Downloading {.file {files$file_name[[i]]}} ({format_bytes(files$file_size[[i]])})..."))
    paths[[i]] <- dhs_fetch(session, urls[[i]], file.path(dest_dir, toupper(files$file_name[[i]])),
                            files$file_size[[i]], relogin)
  }
  rm(creds, relogin)
  if (import) {
    return(bdhs_import(paths, copy = TRUE))
  }
  cli::cli_inform(c("v" = "Downloaded {length(paths)} file{?s} to {.path {dest_dir}}."))
  invisible(tibble::tibble(file_name = basename(paths), path = paths))
}

# Datasets to download --------------------------------------------------------------------

# Catalogue rows (file_name, file_size) for survey x recode in a format.
dhs_files_for <- function(survey, recode, format = "stata", call = rlang::caller_env()) {
  code <- c(stata = "DT", spss = "SV", sas = "SD")[[format]]
  sids <- bdhs_survey_id(survey, type = c("DHS", "SPA", "OTH"))
  recs <- unique(vapply(recode, check_recode, character(1), USE.NAMES = FALSE))
  cat <- catalogue()
  grid <- expand.grid(survey_id = unique(sids), recode = recs, stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    hit <- cat[cat$survey_id == grid$survey_id[[i]] & cat$file_type_code == grid$recode[[i]] &
                 cat$format_code %in% code, ]
    hit[seq_len(min(1L, nrow(hit))), ]
  })
  found <- vapply(rows, nrow, integer(1)) > 0
  if (!all(found)) {
    miss <- grid[!found, ]
    released <- paste(cat$survey_id, cat$file_type_code)
    never <- !paste(miss$survey_id, miss$recode) %in% released
    cli::cli_abort(c(
      "Can't download {.val {paste(miss$survey_id, miss$recode)}} in {format} format.",
      "i" = if (any(never)) "Never released by the DHS Program: {.val {paste(miss$survey_id, miss$recode)[never]}}." else NULL,
      "i" = if (any(!never)) "Not available in {format} format: {.val {paste(miss$survey_id, miss$recode)[!never]}}." else NULL,
      "i" = "See {.fn bdhs_inventory} for the datasets of each round."
    ), call = call)
  }
  hit <- dplyr::bind_rows(rows)
  tibble::tibble(
    survey_id = hit$survey_id,
    recode = hit$file_type_code,
    file_name = hit$file_name,
    file_size = hit$file_size
  )
}

# Credentials ---------------------------------------------------------------------------------

dhs_credentials <- function(email, project, files = NULL, call = rlang::caller_env()) {
  email <- if (is.null(email) || is.na(email[1])) "" else as.character(email[1])
  project <- if (is.null(project) || is.na(project[1])) "" else as.character(project[1])
  if (!nzchar(email) || !nzchar(project)) {
    abort_manual_download(files, "Your DHS account email and project name are not set.", call = call)
  }
  password <- dhs_password(email)
  if (is.null(password) || !nzchar(password)) {
    abort_manual_download(files, "No DHS password found.", call = call)
  }
  list(email = email, project = project, password = password)
}

# Password from the keyring, then DHS_PASSWORD, then an interactive prompt.
dhs_password <- function(email) {
  if (rlang::is_installed("keyring")) {
    pw <- tryCatch(keyring::key_get(.dhs_keyring_service, username = email), error = function(e) "")
    if (rlang::is_string(pw) && nzchar(pw)) {
      return(pw)
    }
  }
  pw <- Sys.getenv("DHS_PASSWORD")
  if (nzchar(pw)) {
    return(pw)
  }
  if (rlang::is_interactive()) {
    prompt <- paste0("DHS Program password for ", email, ": ")
    pw <- if (rlang::is_installed("askpass")) askpass::askpass(prompt) else readline(prompt)
    if (rlang::is_string(pw) && nzchar(pw)) {
      return(pw)
    }
  }
  NULL
}

abort_manual_download <- function(files, problem, call = rlang::caller_env()) {
  example <- if (!is.null(files) && nrow(files)) toupper(files$file_name[[1]]) else "BDIR81DT.ZIP"
  cli::cli_abort(c(
    problem,
    "i" = "Download the files yourself: sign in at {.url https://dhsprogram.com/data/dataset_admin/login_main.cfm}, open your Bangladesh project and download the Stata files{if (!is.null(files)) paste0(' (', paste(toupper(files$file_name), collapse = ', '), ')') else ''}.",
    "i" = "Then run {.code bdhs_import(\"path/to/{example}\")}.",
    "i" = "To download from R instead, set {.envvar DHS_EMAIL} and {.envvar DHS_PROJECT} and store your password with {.code keyring::key_set(\"bdhs-dhs\", username = \"<your email>\")} (or set {.envvar DHS_PASSWORD})."
  ), call = call)
}

# HTTP ---------------------------------------------------------------------------------------

dhs_session <- function() {
  list(
    cookies = tempfile("bdhs-cookies-"),
    user_agent = paste0("bdhs R package ", bdhs_version(), " (https://github.com/armanhossen-am/bdhs)")
  )
}

# The only function that talks to the network: GET (body = NULL) or POST a
# form; returns the response body, or `path` when the body is written to disk.
dhs_http <- function(session, url, body = NULL, path = NULL) {
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, session$user_agent)
  req <- httr2::req_cookie_preserve(req, session$cookies)
  req <- httr2::req_retry(req, max_tries = 3)
  if (!is.null(body)) {
    req <- httr2::req_body_form(req, !!!body, .multi = "explode")
  }
  resp <- httr2::req_perform(req, path = path)
  if (is.null(path)) httr2::resp_body_string(resp) else path
}

# Sign in and return the id of the user's project.
dhs_login <- function(session, creds) {
  url <- paste0(.dhs_admin, "login_main.cfm")
  page <- dhs_http(session, url)
  body <- html_form_hidden(page, "UserPass")
  body[c("UserName", "UserPass", "Submitted", "UserType")] <- list(creds$email, creds$password, "1", "2")
  page <- dhs_http(session, url, body = body)
  dhs_project_id(page, creds$project)
}

dhs_open_download_manager <- function(session, proj_id) {
  dhs_http(session, paste0(.dhs_admin, "download-datasets.cfm"), body = list(proj_id = proj_id))
  dhs_http(session, paste0(.dhs_admin, "index.cfm"), body = list(Proj_ID = proj_id, action = "getdatasets"))
  dhs_http(session, paste0(.dhs_admin, "index.cfm"), body = list(Proj_ID = proj_id, action = "downloadmanager"))
}

# Download URLs of every file the project can download.
dhs_url_list <- function(session, proj_id) {
  page <- dhs_open_download_manager(session, proj_id)
  fields <- dhs_manager_fields(page)
  if ("BD" %in% fields$ctrycodelist) {
    fields$ctrycodelist <- "BD"
  }
  body <- c(
    list(
      surveymode = "all", Proj_ID = proj_id, action = "downloadmanager",
      subaction = "Build URL File List", sub = "submit", submit = "Build URL File List",
      FileDataTypeCode = "", ctrycode = ""
    ),
    fields
  )
  page <- dhs_http(session, paste0(.dhs_admin, "index.cfm"), body = body)
  link <- dhs_urlslist_link(page)
  txt <- dhs_http(session, link)
  urls <- trimws(strsplit(txt, "\r?\n")[[1]])
  urls[nzchar(urls)]
}

# Download one file, checking its size against the catalogue. A wrong size
# usually means the session expired: sign in again and retry.
dhs_fetch <- function(session, url, dest, expected_size, relogin, attempts = 3L) {
  size <- NA_real_
  for (attempt in seq_len(attempts)) {
    dhs_http(session, url, path = dest)
    size <- file.size(dest)
    if (is.na(expected_size) || isTRUE(size == expected_size)) {
      return(dest)
    }
    if (attempt < attempts) {
      relogin()
    }
  }
  if (is_zip(dest)) {
    cli::cli_warn("{.file {basename(dest)}} is {format_bytes(size)}, not the {format_bytes(expected_size)} listed in the DHS catalogue; the file may have been updated.")
    return(dest)
  }
  unlink(dest)
  cli::cli_abort(c(
    "Downloading {.file {basename(dest)}} failed: the DHS website did not send the data file.",
    "i" = "Check that your project is approved for this dataset, or download it manually and use {.fn bdhs_import}."
  ))
}

download_with_rdhs <- function(files, import) {
  check_installed("rdhs", "to download datasets with the rdhs engine")
  paths <- rdhs::get_datasets(files$file_name, download_option = "zip")
  paths <- unlist(paths, use.names = FALSE)
  if (import) {
    return(bdhs_import(paths, copy = TRUE))
  }
  invisible(tibble::tibble(file_name = basename(paths), path = paths))
}

# HTML parsing helpers (regular expressions; the pages are simple forms) ----------------------

html_attr <- function(tags, attr) {
  m <- regmatches(tags, regexec(paste0("\\b", attr, "\\s*=\\s*[\"']([^\"']*)[\"']"), tags, ignore.case = TRUE))
  vapply(m, function(x) if (length(x) > 1L) html_unescape(x[[2]]) else NA_character_, character(1))
}

html_unescape <- function(x) {
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  gsub("&gt;", ">", x, fixed = TRUE)
}

# Hidden inputs of the form containing `marker` (e.g. CSRF tokens).
html_form_hidden <- function(page, marker) {
  forms <- regmatches(page, gregexpr("<form\\b.*?</form>", page, ignore.case = TRUE, perl = TRUE))[[1]]
  form <- forms[grepl(marker, forms, fixed = TRUE)]
  if (!length(form)) {
    return(list())
  }
  inputs <- regmatches(form[[1]], gregexpr("<input\\b[^>]*>", form[[1]], ignore.case = TRUE))[[1]]
  hidden <- inputs[grepl("type\\s*=\\s*[\"']hidden[\"']", inputs, ignore.case = TRUE)]
  nm <- html_attr(hidden, "name")
  val <- html_attr(hidden, "value")
  val[is.na(val)] <- ""
  stats::setNames(as.list(val[!is.na(nm)]), nm[!is.na(nm)])
}

dhs_project_id <- function(page, project, call = rlang::caller_env()) {
  opts <- regmatches(page, gregexpr("<option\\b[^>]*>[^<]*", page, ignore.case = TRUE))[[1]]
  ids <- html_attr(opts, "value")
  labels <- trimws(html_unescape(sub("^<option\\b[^>]*>", "", opts, ignore.case = TRUE)))
  keep <- !is.na(ids) & grepl("^[0-9]+$", ids)
  ids <- ids[keep]
  labels <- labels[keep]
  if (!length(ids)) {
    cli::cli_abort(c(
      "Signing in to the DHS Program website failed.",
      "i" = "Check your email and password, and that your account has an approved project."
    ), call = call)
  }
  key <- tolower(substr(trimws(project), 1, 30))
  hit <- which(grepl(key, tolower(labels), fixed = TRUE))
  if (length(hit) > 1L) {
    exact <- hit[tolower(labels[hit]) == tolower(trimws(project))]
    hit <- if (length(exact) == 1L) exact else hit
  }
  if (length(hit) != 1L) {
    cli::cli_abort(c(
      if (length(hit)) "Several of your DHS projects match {.val {project}}." else "None of your DHS projects matches {.val {project}}.",
      "i" = "Your projects: {.val {labels}}.",
      "i" = "Set {.arg project} (or {.envvar DHS_PROJECT}) to the full project name."
    ), call = call)
  }
  ids[[hit]]
}

# Hidden form fields of the download manager that select countries, file
# types and formats.
dhs_manager_fields <- function(page) {
  inputs <- regmatches(page, gregexpr("<input\\b[^>]*>", page, ignore.case = TRUE))[[1]]
  nm <- html_attr(inputs, "name")
  val <- html_attr(inputs, "value")
  keep <- !is.na(nm) & !is.na(val) & grepl("^(ctrycodelist|filedatatypelist_.+|fformatlist)$", nm)
  split(val[keep], nm[keep])
}

dhs_urlslist_link <- function(page, call = rlang::caller_env()) {
  hrefs <- html_attr(regmatches(page, gregexpr("<a\\b[^>]*>", page, ignore.case = TRUE))[[1]], "href")
  link <- hrefs[!is.na(hrefs) & grepl("/data/download/urlslist", hrefs, fixed = TRUE)]
  if (!length(link)) {
    cli::cli_abort(c(
      "The DHS website did not return the list of download links.",
      "i" = "The website may have changed; download the files manually and use {.fn bdhs_import}."
    ), call = call)
  }
  link <- link[[1]]
  if (startsWith(link, "/")) paste0(.dhs_base, link) else link
}

# Pick the download URL of each file (matched on the Filename= parameter).
dhs_select_urls <- function(urls, file_names, call = rlang::caller_env()) {
  param <- function(u, p) {
    m <- regmatches(u, regexec(paste0("[?&]", p, "=([^&]+)"), u, ignore.case = TRUE))
    vapply(m, function(x) if (length(x) > 1L) utils::URLdecode(x[[2]]) else NA_character_, character(1))
  }
  fname <- toupper(param(urls, "Filename"))
  ctry <- toupper(param(urls, "Ctry_Code"))
  fname[!is.na(ctry) & ctry != "BD"] <- NA_character_
  idx <- match(toupper(file_names), fname)
  if (anyNA(idx)) {
    cli::cli_abort(c(
      "Your DHS project cannot download {.file {toupper(file_names[is.na(idx)])}}.",
      "i" = "Request access to the Bangladesh surveys for your project at {.url https://dhsprogram.com/data/dataset_admin/}."
    ), call = call)
  }
  urls[idx]
}

is_zip <- function(path) {
  !is.null(tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL, warning = function(w) NULL))
}

format_bytes <- function(x) {
  if (is.na(x)) {
    return("unknown size")
  }
  format(structure(as.numeric(x), class = "object_size"), units = "auto", standard = "SI")
}
