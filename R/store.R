# The local microdata store ------------------------------------------------------
#
# Layout: <data_dir>/registry.rds plus stored copies under <data_dir>/<survey_id>/.
# The registry is a tibble with one row per (survey_id, recode). Paths of stored
# copies are relative to the data directory so the store can be moved; linked
# files (registered in place with `copy = FALSE`) keep absolute paths.

#' Where bdhs keeps your DHS microdata
#'
#' `bdhs_data_dir()` returns the directory of the local data store used by
#' [bdhs_import()] and [bdhs_read()]. It is resolved in this order:
#'
#' 1. the `bdhs.data_dir` option,
#' 2. the `BDHS_DATA_DIR` environment variable,
#' 3. `tools::R_user_dir("bdhs", "data")`.
#'
#' The directory is created only when you first import data. DHS microdata
#' are licensed to you personally: keep the store on an encrypted disk, and
#' outside git repositories and cloud-synchronised folders (Dropbox, OneDrive,
#' Google Drive, iCloud). [bdhs_import()] and [bdhs_sitrep()] warn when it is
#' not.
#'
#' @param path Optional new data directory. It is set for the current R
#'   session only (as the `bdhs.data_dir` option); the message tells you the
#'   line to add to your `.Renviron` to make it permanent. Nothing is written
#'   to your configuration files.
#' @return The data directory, as a normalised path (invisibly when `path` is
#'   given).
#' @family data store
#' @export
#' @examples
#' bdhs_data_dir()
#'
#' # Use another location for this session
#' old <- options(bdhs.data_dir = NULL)
#' bdhs_data_dir(file.path(tempdir(), "bdhs-data"))
#' bdhs_data_dir()
#' options(old)
bdhs_data_dir <- function(path = NULL) {
  if (is.null(path)) {
    return(store_dir())
  }
  if (!rlang::is_string(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single directory path.")
  }
  path <- normalize_path(path)
  if (file.exists(path) && !dir.exists(path)) {
    cli::cli_abort("{.path {path}} is a file, not a directory.")
  }
  options(bdhs.data_dir = path)
  line <- if (grepl("[[:space:]#\"']", path)) {
    paste0("BDHS_DATA_DIR=\"", path, "\"")
  } else {
    paste0("BDHS_DATA_DIR=", path)
  }
  cli::cli_inform(c(
    "v" = "bdhs data directory set to {.path {path}} for this R session.",
    "i" = "To keep it in future sessions, add this line to your {.file .Renviron} (for example with {.code usethis::edit_r_environ()}):",
    " " = "{.code {line}}"
  ))
  invisible(path)
}

# Current data directory (not created).
store_dir <- function() {
  path <- bdhs_opt("data_dir")
  if (is.null(path) || !nzchar(path)) {
    path <- Sys.getenv("BDHS_DATA_DIR")
  }
  if (!nzchar(path)) {
    path <- tools::R_user_dir("bdhs", "data")
  }
  normalize_path(path)
}

# Where the data directory setting comes from (for bdhs_sitrep()).
store_dir_source <- function() {
  path <- bdhs_opt("data_dir")
  if (!is.null(path) && nzchar(path)) {
    return("option bdhs.data_dir")
  }
  if (nzchar(Sys.getenv("BDHS_DATA_DIR"))) {
    return("environment variable BDHS_DATA_DIR")
  }
  "default (tools::R_user_dir)"
}

# Absolute path with symbolic links resolved, also for paths that do not
# exist yet (their deepest existing parent is resolved).
normalize_path <- function(path) {
  vapply(path.expand(path), function(p) {
    parent <- dirname(p)
    if (file.exists(p) || identical(parent, p)) {
      return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
    file.path(normalize_path(parent), basename(p))
  }, character(1), USE.NAMES = FALSE)
}

# Registry -----------------------------------------------------------------------

registry_template <- function() {
  tibble::tibble(
    survey_id = character(),
    recode = character(),
    file_stem = character(),
    format = character(),
    path = character(),
    linked = logical(),
    source_path = character(),
    md5 = character(),
    n_rows = integer(),
    n_cols = integer(),
    imported_at = .POSIXct(numeric(), tz = "UTC"),
    bdhs_version = character()
  )
}

registry_file <- function(dir = store_dir()) {
  file.path(dir, "registry.rds")
}

registry_read <- function(dir = store_dir()) {
  file <- registry_file(dir)
  tmpl <- registry_template()
  if (!file.exists(file)) {
    return(tmpl)
  }
  reg <- tryCatch(readRDS(file), error = function(e) {
    cli::cli_abort(c(
      "The bdhs registry {.path {file}} is unreadable.",
      "i" = "Delete it and import your files again with {.fn bdhs_import}."
    ), parent = e)
  })
  reg <- tibble::as_tibble(reg)
  for (nm in setdiff(names(tmpl), names(reg))) {
    reg[[nm]] <- rep(tmpl[[nm]][NA_integer_], nrow(reg))
  }
  reg[names(tmpl)]
}

# Write the registry atomically (temporary file + rename). Callers that read,
# change and write it hold the store lock (store_lock()) meanwhile.
registry_write <- function(reg, dir = store_dir(), call = rlang::caller_env()) {
  check_store_writable(dir, call = call)
  reg <- reg[order(reg$survey_id, reg$recode), names(registry_template())]
  tmp <- tempfile("registry-", tmpdir = dir, fileext = ".rds")
  ok <- tryCatch({
    saveRDS(reg, tmp)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) {
    unlink(tmp)
    abort_store_not_writable(dir, call = call)
  }
  if (!file.rename(tmp, registry_file(dir))) {
    copied <- file.copy(tmp, registry_file(dir), overwrite = TRUE)
    unlink(tmp)
    if (!copied) {
      abort_store_not_writable(dir, call = call)
    }
  }
  invisible(reg)
}

# Write the registry after an import. It is read again first (a writer that
# does not use the lock may have changed it); the rows this import replaced
# and older rows of the same datasets are dropped, the new rows added.
registry_merge_write <- function(new_rows, replaced, dir = store_dir(), call = rlang::caller_env()) {
  reg <- registry_read(dir)
  id <- function(x) paste(x$survey_id, x$recode, x$md5, x$path, sep = "\r")
  drop <- paste(reg$survey_id, reg$recode) %in% paste(new_rows$survey_id, new_rows$recode) |
    id(reg) %in% id(replaced)
  registry_write(dplyr::bind_rows(reg[!drop, ], new_rows), dir, call = call)
}

# Create the data directory if needed and check that it is writable.
check_store_writable <- function(dir, call = rlang::caller_env()) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(dir) || file.access(dir, 2L) != 0L) {
    abort_store_not_writable(dir, call = call)
  }
  invisible(dir)
}

abort_store_not_writable <- function(dir, call = rlang::caller_env()) {
  cli::cli_abort(c(
    "Can't write to the bdhs data directory {.path {dir}}.",
    "i" = "Check that the folder can be created and that you have permission to write to it.",
    "i" = "Or choose another location with {.code bdhs_data_dir(\"path\")} or the {.envvar BDHS_DATA_DIR} environment variable."
  ), class = "bdhs_store_not_writable", call = call)
}

# Store lock -----------------------------------------------------------------------
#
# Imports and bdhs_forget() change the registry and the stored copies; two R
# sessions doing so at once would lose registry rows and orphan stored files.
# The lock is a directory (<data_dir>/.lock) because creating a directory is
# atomic. It holds an "owner" file (process id, host, user, time) that is
# touched while the lock is in use. A lock is stale, and taken over, when its
# owner file has not been touched for `stale` seconds or when its process on
# this computer has ended. Locks are re-entrant within an R session.

.store_locks <- new.env(parent = emptyenv())

store_lock_path <- function(dir) {
  file.path(dir, ".lock")
}

# Acquire the lock of the store in `dir`; returns a key for store_unlock().
store_lock <- function(dir = store_dir(), timeout = bdhs_opt("lock_timeout", 60),
                       stale = bdhs_opt("lock_stale", 1800), call = rlang::caller_env()) {
  key <- normalize_path(dir)
  held <- .store_locks[[key]] %||% 0L
  if (held > 0L) {
    .store_locks[[key]] <- held + 1L
    return(invisible(key))
  }
  check_store_writable(key, call = call)
  lock <- store_lock_path(key)
  start <- Sys.time()
  wait <- 0.05
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) {
      written <- tryCatch({
        writeLines(c(
          paste0("pid: ", Sys.getpid()),
          paste0("host: ", Sys.info()[["nodename"]]),
          paste0("user: ", Sys.info()[["user"]]),
          paste0("time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
        ), file.path(lock, "owner"))
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (!written) {
        unlink(lock, recursive = TRUE)
        abort_store_not_writable(key, call = call)
      }
      .store_locks[[key]] <- 1L
      return(invisible(key))
    }
    if (!dir.exists(lock)) {
      abort_store_not_writable(key, call = call)
    }
    if (store_lock_is_stale(lock, stale)) {
      # Move it aside first so that only one session removes it.
      aside <- paste0(lock, "-", basename(tempfile("stale-")))
      if (file.rename(lock, aside)) {
        unlink(aside, recursive = TRUE)
      }
      next
    }
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout) {
      owner <- store_lock_owner(lock)
      cli::cli_abort(c(
        "The bdhs data directory {.path {key}} is in use by another R session.",
        "i" = if (all(c("pid", "host", "time") %in% names(owner))) "Lock held by process {owner[['pid']]} on {owner[['host']]} since {owner[['time']]}." else NULL,
        "i" = "Try again when the other import has finished.",
        "i" = "If no other R session is using the store, delete {.path {lock}}."
      ), class = "bdhs_store_locked", call = call)
    }
    Sys.sleep(wait)
    wait <- min(wait * 2, 1)
  }
}

store_unlock <- function(key) {
  held <- .store_locks[[key]] %||% 0L
  if (held > 1L) {
    .store_locks[[key]] <- held - 1L
    return(invisible(NULL))
  }
  if (exists(key, envir = .store_locks, inherits = FALSE)) {
    rm(list = key, envir = .store_locks)
  }
  lock <- store_lock_path(key)
  # Leave a lock that another session took over (after ours went stale).
  pid <- store_lock_owner(lock)["pid"]
  if (is.na(pid) || identical(unname(pid), as.character(Sys.getpid()))) {
    unlink(lock, recursive = TRUE)
  }
  invisible(NULL)
}

# Mark the lock as in use (for long imports).
store_lock_touch <- function(key) {
  owner <- file.path(store_lock_path(key), "owner")
  if (file.exists(owner)) {
    Sys.setFileTime(owner, Sys.time())
  }
  invisible(NULL)
}

# Owner fields of a lock (named character vector, empty if unreadable).
store_lock_owner <- function(lock) {
  lines <- tryCatch(readLines(file.path(lock, "owner"), warn = FALSE), error = function(e) character(), warning = function(w) character())
  lines <- lines[grepl(": ", lines, fixed = TRUE)]
  stats::setNames(sub("^[^:]*: ", "", lines), sub(": .*$", "", lines))
}

store_lock_is_stale <- function(lock, stale) {
  owner_file <- file.path(lock, "owner")
  # Without an owner file the lock was just created: judge by its own age.
  stamp <- if (file.exists(owner_file)) file.mtime(owner_file) else file.mtime(lock)
  if (is.na(stamp)) {
    return(FALSE)
  }
  if (as.numeric(difftime(Sys.time(), stamp, units = "secs")) > stale) {
    return(TRUE)
  }
  owner <- store_lock_owner(lock)
  pid <- suppressWarnings(as.integer(unname(owner["pid"])))
  same_owner <- identical(unname(owner["host"]), Sys.info()[["nodename"]]) &&
    identical(unname(owner["user"]), Sys.info()[["user"]])
  if (!same_owner || is.na(pid)) {
    return(FALSE)
  }
  if (pid == Sys.getpid()) {
    # This session holds no lock on the store (store_lock() is re-entrant), so
    # the lock is a leftover of this process.
    return(TRUE)
  }
  # Signal 0 only checks whether the process exists (Unix only).
  .Platform$OS.type == "unix" && !isTRUE(tools::pskill(pid, 0L))
}

# Absolute paths of registry rows.
registry_abs_path <- function(reg, dir = store_dir()) {
  as.character(ifelse(reg$linked, reg$path, file.path(dir, reg$path)))
}

# Secure-storage checks -----------------------------------------------------------

# Root of the git work tree containing `path`, or NULL.
path_git_root <- function(path) {
  d <- normalize_path(path)
  repeat {
    if (file.exists(file.path(d, ".git"))) {
      return(d)
    }
    parent <- dirname(d)
    if (identical(parent, d)) {
      return(NULL)
    }
    d <- parent
  }
}

# Name of the cloud-sync service whose folder contains `path`, or NULL.
path_sync_service <- function(path) {
  p <- tolower(normalize_path(path))
  services <- c(
    "Dropbox" = "dropbox",
    "OneDrive" = "onedrive",
    "Google Drive" = "google ?drive|googledrive|/my drive(/|$)",
    "iCloud Drive" = "icloud|mobile documents|clouddocs",
    "Box" = "box sync|cloudstorage/box",
    "pCloud" = "pcloud",
    "a cloud-storage provider" = "/library/cloudstorage/"
  )
  hit <- names(services)[vapply(services, grepl, logical(1), x = p)]
  if (length(hit)) hit[[1]] else NULL
}

# Warn when the store is somewhere DHS data should not be kept.
warn_insecure_store <- function(dir = store_dir()) {
  git <- path_git_root(dir)
  sync <- path_sync_service(dir)
  if (is.null(git) && is.null(sync)) {
    return(invisible(FALSE))
  }
  msg <- c("The bdhs data directory {.path {dir}} is not a secure place for DHS microdata.")
  if (!is.null(git)) {
    msg <- c(msg, "!" = "It is inside the git repository {.path {git}}: a commit or push would redistribute the data.")
  }
  if (!is.null(sync)) {
    msg <- c(msg, "!" = "It is inside a {sync} folder, which copies the data to third-party servers.")
  }
  msg <- c(msg,
    "i" = "The DHS Program requires that you store the data securely and never share them.",
    "i" = "Choose another location with {.code bdhs_data_dir(\"path\")} or the {.envvar BDHS_DATA_DIR} environment variable."
  )
  cli::cli_warn(msg)
  invisible(TRUE)
}

# Catalogue helpers ----------------------------------------------------------------

catalogue <- function() {
  bdhs::bdhs_datasets
}

# One row per survey x recode in the DHS catalogue, with the DHS data-file stem
# (e.g. "BDIR81FL") and the name of the Stata download (e.g. "BDIR81DT.ZIP").
catalogue_recodes <- function(type = c("DHS", "SPA", "OTH")) {
  cat <- catalogue()
  cat <- cat[cat$survey_type %in% toupper(type), ]
  key <- paste(cat$survey_id, cat$file_type_code)
  core <- substr(cat$file_stem, 1, 6)
  first <- !duplicated(key)
  out <- tibble::tibble(
    survey_id = cat$survey_id[first],
    survey_type = cat$survey_type[first],
    recode = cat$file_type_code[first],
    file_stem = paste0(core[first], "FL"),
    stata_file = vapply(key[first], function(k) {
      f <- cat$file_name[key == k & cat$format_code %in% "DT"]
      if (length(f)) toupper(f[[1]]) else NA_character_
    }, character(1), USE.NAMES = FALSE)
  )
  out[order(match(out$survey_id, .rounds$survey_id), out$recode), ]
}

# DHS data-file stem for a survey x recode, or NA if never released.
catalogue_stem <- function(survey_id, recode) {
  cr <- catalogue_recodes()
  cr$file_stem[match(paste(survey_id, recode), paste(cr$survey_id, cr$recode))]
}

# Inventory --------------------------------------------------------------------------

#' What DHS datasets exist, and which ones you have
#'
#' Lists every dataset in the DHS Program catalogue for Bangladesh (one row per
#' survey and recode file type, from [bdhs_datasets]) and whether it is
#' available in your local store.
#'
#' @param survey Optional survey references (see [bdhs_survey_id()]).
#' @param recode Optional recode codes (see [bdhs_recodes()]).
#' @param type Survey types to list: any of `"DHS"`, `"SPA"`, `"OTH"`.
#' @return A tibble with columns `survey_id`, `year_label`, `recode`,
#'   `recode_name`, `file_stem`, `available`, `format`, `linked`, `path`,
#'   `n_rows`, `n_cols` and `imported_at`.
#' @family data store
#' @export
#' @examples
#' bdhs_inventory(2022)
#' bdhs_inventory(recode = "KR")
bdhs_inventory <- function(survey = NULL, recode = NULL, type = "DHS") {
  type <- toupper(type)
  cr <- catalogue_recodes(type)
  if (!is.null(survey)) {
    cr <- cr[cr$survey_id %in% bdhs_survey_id(survey, type = c("DHS", "SPA", "OTH")), ]
  }
  if (!is.null(recode)) {
    recode <- vapply(recode, check_recode, character(1), USE.NAMES = FALSE)
    cr <- cr[cr$recode %in% recode, ]
  }
  dir <- store_dir()
  reg <- registry_read(dir)
  i <- match(paste(cr$survey_id, cr$recode), paste(reg$survey_id, reg$recode))
  path <- rep(NA_character_, nrow(cr))
  path[!is.na(i)] <- registry_abs_path(reg[i[!is.na(i)], ], dir)
  out <- tibble::tibble(
    survey_id = cr$survey_id,
    year_label = survey_label(cr$survey_id),
    recode = cr$recode,
    recode_name = .recodes$name[match(cr$recode, .recodes$recode)],
    file_stem = cr$file_stem,
    available = !is.na(path) & file.exists(path %|NA|% ""),
    format = reg$format[i],
    linked = reg$linked[i],
    path = path,
    n_rows = reg$n_rows[i],
    n_cols = reg$n_cols[i],
    imported_at = reg$imported_at[i]
  )
  out
}

# Forget --------------------------------------------------------------------------------

#' Remove datasets from the local store
#'
#' Unregisters datasets so that [bdhs_read()] no longer finds them. Stored
#' copies are deleted only with `delete = TRUE` (after confirmation in
#' interactive sessions). Original files that were registered in place with
#' `bdhs_import(copy = FALSE)` are never deleted.
#'
#' @param survey A survey reference (see [bdhs_survey_id()]).
#' @param recode Recode codes to forget; `NULL` forgets every recode of the
#'   survey.
#' @param delete If `TRUE`, also delete the copies kept in the store.
#' @return The removed registry rows, invisibly.
#' @family data store
#' @export
#' @examples
#' \dontshow{old <- options(bdhs.data_dir = file.path(tempdir(), "bdhs-forget-example"))}
#' f <- file.path(tempdir(), "BDKR81FL.DTA")
#' haven::write_dta(bdhs_simulate(2022, "KR", n_clusters = 4, households = 3), f)
#' bdhs_import(f, format = "dta", quiet = TRUE)
#' bdhs_forget(2022, "KR", delete = TRUE)
#' \dontshow{options(old)}
bdhs_forget <- function(survey, recode = NULL, delete = FALSE) {
  survey_id <- bdhs_survey_id(survey, type = c("DHS", "SPA", "OTH"))
  if (length(survey_id) != 1L) {
    cli::cli_abort("{.arg survey} must refer to a single survey.")
  }
  if (!is.null(recode)) {
    recode <- vapply(recode, check_recode, character(1), USE.NAMES = FALSE)
  }
  dir <- store_dir()
  reg <- registry_read(dir)
  hit <- reg$survey_id == survey_id & (is.null(recode) | reg$recode %in% recode)
  if (!any(hit)) {
    cli::cli_inform("Nothing registered for {survey_id}{if (!is.null(recode)) paste0(' ', paste(recode, collapse = ', ')) else ''}.")
    return(invisible(reg[0, ]))
  }
  rows <- reg[hit, ]
  labels <- paste(rows$survey_id, rows$recode)
  stored <- !rows$linked
  if (delete && any(stored) && interactive()) {
    ok <- utils::askYesNo(paste0(
      "Delete the stored ", if (sum(stored) == 1) "copy" else "copies", " of ",
      paste(labels[stored], collapse = ", "), " from ", dir, "?"
    ), default = FALSE)
    if (!isTRUE(ok)) {
      cli::cli_inform("Nothing was unregistered or deleted.")
      return(invisible(reg[0, ]))
    }
  }
  # Change the registry as it is now, under the store lock: another session
  # may have imported meanwhile.
  lock <- store_lock(dir)
  on.exit(store_unlock(lock), add = TRUE)
  reg <- registry_read(dir)
  hit <- reg$survey_id == survey_id & (is.null(recode) | reg$recode %in% recode)
  rows <- reg[hit, ]
  labels <- paste(rows$survey_id, rows$recode)
  stored <- !rows$linked
  if (!nrow(rows)) {
    cli::cli_inform("Nothing registered for {survey_id}{if (!is.null(recode)) paste0(' ', paste(recode, collapse = ', ')) else ''}.")
    return(invisible(rows))
  }
  if (delete && any(stored)) {
    paths <- registry_abs_path(rows[stored, ], dir)
    inside <- startsWith(normalize_path(paths), paste0(normalize_path(dir), "/"))
    unlink(store_files(paths[inside], rows$format[stored][inside]))
    for (d in unique(dirname(paths[inside]))) {
      if (!length(list.files(d, all.files = TRUE, no.. = TRUE))) unlink(d, recursive = TRUE)
    }
    cli::cli_inform(c("v" = "Deleted the stored {cli::qty(sum(inside))}cop{?y/ies} of {.val {labels[stored][inside]}}."))
  } else if (any(stored)) {
    cli::cli_inform(c("i" = "The stored {cli::qty(sum(stored))}cop{?y/ies} of {.val {labels[stored]}} {?was/were} kept in {.path {dir}}; use {.code delete = TRUE} to remove {?it/them}."))
  }
  if (any(!stored)) {
    cli::cli_inform(c("i" = "Original {cli::qty(sum(!stored))}file{?s} registered in place {?was/were} not touched: {.path {rows$path[!stored]}}."))
  }
  registry_write(reg[!hit, ], dir)
  cli::cli_inform(c("v" = "Unregistered {.val {labels}}."))
  invisible(rows)
}
