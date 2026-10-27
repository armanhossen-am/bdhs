# Importing downloaded DHS files into the local store --------------------------------------

#' Import DHS microdata you have downloaded
#'
#' Registers DHS data files in the local bdhs data store so that
#' [bdhs_read()] can load any round and recode in one line. Point it at the
#' files, zip archives or folders you downloaded from the DHS Program: files
#' are identified with [bdhs_identify_file()] whatever they are called,
#' folders are searched recursively and zip archives (also nested ones, such
#' as the bulk "STATA_BD_..." downloads) are unpacked to a temporary folder.
#'
#' When the same dataset is found in several formats, the Stata version is
#' preferred, then SPSS, then SAS. Files that were already imported (same MD5
#' checksum) are skipped, as are derived or pooled files (several rounds
#' appended together, or with variables that are not in the DHS recode).
#'
#' With `copy = TRUE` (default) a copy is kept in [bdhs_data_dir()]: with
#' `format = "auto"` it is converted to Parquet when the arrow package is
#' installed (much faster to read selected columns; variable labels, value
#' labels and Stata extended missing values are kept), otherwise the original
#' file is copied. With `copy = FALSE` files are registered where they are
#' and nothing is copied; files inside zip archives are always copied.
#'
#' DHS microdata are licensed to you personally and must not be shared. bdhs
#' never uploads or shares them, and warns when the data directory is inside a
#' git repository or a cloud-synchronised folder.
#'
#' @param path Paths to data files (`.dta`, `.sav`, `.sas7bdat`), zip archives
#'   or folders.
#' @param copy Keep a copy in the data store (`TRUE`, default) or register the
#'   files in place (`FALSE`).
#' @param format Storage format for copies: `"auto"` (Parquet if arrow is
#'   installed, otherwise the original file), `"parquet"` or `"dta"` (a
#'   byte-for-byte copy of the original Stata, SPSS or SAS file).
#' @param overwrite Replace datasets that are already registered from a
#'   different file (or re-import identical ones).
#' @param quiet If `TRUE`, do not print the import summary. Warnings are
#'   still shown.
#' @return The registry rows of the imported datasets, invisibly: columns
#'   `survey_id`, `recode`, `file_stem`, `format`, `path`, `linked`,
#'   `source_path`, `md5`, `n_rows`, `n_cols`, `imported_at` and
#'   `bdhs_version`.
#' @family data store
#' @export
#' @examples
#' \dontshow{old <- options(bdhs.data_dir = file.path(tempdir(), "bdhs-import-example"))}
#' # Synthetic files with DHS names, for illustration only
#' dl <- file.path(tempdir(), "dhs-downloads")
#' dir.create(dl, showWarnings = FALSE)
#' haven::write_dta(bdhs_simulate(2022, "KR", n_clusters = 4, households = 3),
#'                  file.path(dl, "BDKR81FL.DTA"))
#' haven::write_dta(bdhs_simulate(2022, "HR", n_clusters = 4, households = 3),
#'                  file.path(dl, "BDHS2022HR.dta"))
#' bdhs_import(dl, format = "dta")
#' bdhs_inventory(2022)
#' \dontshow{bdhs_forget(2022, delete = TRUE); options(old)}
bdhs_import <- function(path, copy = TRUE, format = c("auto", "parquet", "dta"),
                        overwrite = FALSE, quiet = FALSE) {
  format <- rlang::arg_match(format)
  if (!is.character(path) || !length(path) || anyNA(path)) {
    cli::cli_abort("{.arg path} must be a character vector of file or folder paths.")
  }
  missing <- path[!file.exists(path)]
  if (length(missing)) {
    cli::cli_abort("Can't find {.path {missing}}.")
  }
  if (format == "parquet") {
    check_installed("arrow", "to store datasets in Parquet format")
  }
  store_format <- if (format == "auto") {
    if (rlang::is_installed("arrow")) "parquet" else "original"
  } else if (format == "parquet") {
    "parquet"
  } else {
    "original"
  }

  dir <- store_dir()
  tmp <- tempfile("bdhs-import-")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  res <- collect_candidates(path, tmp, dir)
  if (!nrow(res)) {
    cli::cli_inform(c("i" = "No Stata, SPSS, SAS or zip files found in {.path {path}}."))
    return(invisible(registry_template()))
  }

  # Identify and checksum.
  for (i in which(res$status == "pending")) {
    id <- tryCatch(identify_one(res$path[[i]], content = "always", display = res$origin[[i]]),
                   error = function(e) list(note = conditionMessage(e), derived = FALSE))
    res$survey_id[[i]] <- id$survey_id %||% NA_character_
    res$recode[[i]] <- id$recode %||% NA_character_
    res$file_stem[[i]] <- id$file_stem %||% NA_character_
    res$n_rows[[i]] <- id$n_rows %||% NA_integer_
    res$n_cols[[i]] <- id$n_cols %||% NA_integer_
    res$note[[i]] <- id$note %||% NA_character_
    if (isTRUE(id$derived)) {
      res$status[[i]] <- "derived"
    } else if (is.na(res$survey_id[[i]]) || is.na(res$recode[[i]])) {
      res$status[[i]] <- "unidentified"
    } else {
      res$md5[[i]] <- unname(tools::md5sum(res$path[[i]]))
      if (is.na(res$file_stem[[i]])) {
        res$file_stem[[i]] <- toupper(tools::file_path_sans_ext(basename(res$path[[i]])))
      }
    }
  }

  # One file per survey x recode: Stata first, then loose files before zipped ones.
  pend <- which(res$status == "pending")
  pend <- pend[order(res$survey_id[pend], res$recode[pend], format_priority(res$format[pend]), res$in_zip[pend])]
  key <- paste(res$survey_id, res$recode)
  for (k in unique(key[pend])) {
    rows <- pend[key[pend] == k]
    for (j in rows[-1]) {
      res$status[[j]] <- "duplicate"
      res$note[[j]] <- if (identical(res$md5[[j]], res$md5[[rows[[1]]]])) {
        paste0("identical to ", res$origin[[rows[[1]]]])
      } else {
        paste0(res$origin[[rows[[1]]]], " was preferred for ", k)
      }
    }
  }

  # Compare with what is already registered.
  reg <- registry_read(dir)
  for (i in which(res$status == "pending")) {
    same_md5 <- which(reg$md5 %in% res$md5[[i]])
    same_key <- which(reg$survey_id == res$survey_id[[i]] & reg$recode == res$recode[[i]])
    if (!overwrite && length(same_md5)) {
      res$status[[i]] <- "already"
      res$note[[i]] <- paste("as", reg$survey_id[[same_md5[[1]]]], reg$recode[[same_md5[[1]]]])
    } else if (!overwrite && length(same_key)) {
      res$status[[i]] <- "exists"
    }
  }

  # Store.
  new_rows <- list()
  warned <- FALSE
  for (i in which(res$status == "pending")) {
    sid <- res$survey_id[[i]]
    rec <- res$recode[[i]]
    old <- which(reg$survey_id == sid & reg$recode == rec)
    linked <- !copy && !res$in_zip[[i]]
    if (!linked && !warned) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      warn_insecure_store(dir)
      warned <- TRUE
    }
    stored <- tryCatch(
      store_one(res[i, ], dir, linked = linked, store_format = store_format),
      error = function(e) e
    )
    if (inherits(stored, "error")) {
      res$status[[i]] <- "error"
      res$note[[i]] <- conditionMessage(stored)
      next
    }
    if (length(old)) {
      old_paths <- registry_abs_path(reg[old, ], dir)
      drop <- !reg$linked[old] & normalize_path(old_paths) != normalize_path(file.path(dir, stored$path))
      unlink(store_files(old_paths[drop], reg$format[old][drop]))
      reg <- reg[-old, ]
    }
    if (!copy && res$in_zip[[i]]) {
      stored$note <- "extracted from a zip archive, so a copy was stored"
    }
    row <- tibble::tibble(
      survey_id = sid,
      recode = rec,
      file_stem = res$file_stem[[i]],
      format = stored$format,
      path = stored$path,
      linked = linked,
      source_path = res$origin[[i]],
      md5 = res$md5[[i]],
      n_rows = as.integer(res$n_rows[[i]]),
      n_cols = as.integer(res$n_cols[[i]]),
      imported_at = .POSIXct(as.numeric(Sys.time()), tz = "UTC"),
      bdhs_version = bdhs_version()
    )
    reg <- dplyr::bind_rows(reg, row)
    new_rows <- c(new_rows, list(row))
    res$status[[i]] <- "imported"
    res$stored_as[[i]] <- stored$format
    if (!is.null(stored$note)) res$note[[i]] <- stored$note
  }
  if (length(new_rows)) {
    registry_write(reg, dir)
  }

  import_warnings(res)
  if (!quiet) {
    import_summary(res, dir)
  }
  out <- if (length(new_rows)) dplyr::bind_rows(new_rows) else registry_template()
  invisible(out)
}

bdhs_version <- function() {
  as.character(utils::packageVersion("bdhs"))
}

# Expand paths into candidate data files (extracting zips into `tmp`).
collect_candidates <- function(path, tmp, dir) {
  files <- unlist(lapply(path, function(p) {
    if (dir.exists(p)) list_data_files(p, zips = TRUE) else normalize_path(p)
  }), use.names = FALSE)
  files <- unique(files)
  dir_prefix <- paste0(normalize_path(dir), "/")
  rows <- list()
  add <- function(path, origin, format, in_zip, status = "pending", note = NA_character_) {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      path = path, origin = origin, format = format, in_zip = in_zip,
      status = status, note = note
    )
  }
  for (f in files) {
    fmt <- file_format(f)
    if (startsWith(f, dir_prefix)) {
      add(f, f, fmt, FALSE, "refused", "it is inside the bdhs data directory")
    } else if (identical(fmt, "zip")) {
      members <- extract_zip_data(f, tempfile("zip-", tmpdir = tmp))
      if (!nrow(members)) {
        add(f, f, "zip", FALSE, "nodata", "no Stata, SPSS or SAS data file inside (flat ASCII files are not supported)")
      }
      for (j in seq_len(nrow(members))) {
        add(members$path[[j]], members$origin[[j]], members$format[[j]], TRUE)
      }
    } else if (fmt %in% c("dta", "sav", "sas7bdat")) {
      add(f, f, fmt, FALSE)
    }
  }
  if (!length(rows)) {
    return(tibble::tibble())
  }
  res <- dplyr::bind_rows(rows)
  n <- nrow(res)
  res$survey_id <- NA_character_
  res$recode <- NA_character_
  res$file_stem <- NA_character_
  res$md5 <- NA_character_
  res$n_rows <- NA_integer_
  res$n_cols <- NA_integer_
  res$stored_as <- rep(NA_character_, n)
  res
}

# Extract the data files of a zip (and of zips nested in it) into `exdir`.
extract_zip_data <- function(zip, exdir, depth = 0L, label = basename(zip)) {
  empty <- tibble::tibble(path = character(), origin = character(), format = character())
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  lst <- tryCatch(utils::unzip(zip, list = TRUE), error = function(e) NULL)
  if (is.null(lst)) {
    cli::cli_warn("Could not open the zip archive {.path {zip}}.")
    return(empty)
  }
  members <- lst$Name[!grepl("/$", lst$Name)]
  fmt <- file_format(members)
  members <- members[!is.na(fmt) & fmt %in% c("dta", "sav", "sas7bdat", "zip")]
  out <- list(empty)
  for (m in members) {
    sub <- tempfile("m", tmpdir = exdir)
    dir.create(sub)
    # junkpaths: never write outside `sub`, whatever the member name.
    utils::unzip(zip, files = m, exdir = sub, junkpaths = TRUE)
    f <- file.path(sub, basename(m))
    if (!file.exists(f)) {
      next
    }
    origin <- paste0(label, " > ", basename(m))
    if (identical(file_format(m), "zip")) {
      if (depth < 3L) {
        out <- c(out, list(extract_zip_data(f, sub, depth + 1L, origin)))
      }
    } else {
      out <- c(out, list(tibble::tibble(path = f, origin = origin, format = file_format(m))))
    }
  }
  dplyr::bind_rows(out)
}

# Store one identified file. Returns list(format, path, note).
store_one <- function(cand, dir, linked, store_format) {
  src <- cand$path[[1]]
  src_format <- cand$format[[1]]
  if (linked) {
    return(list(format = src_format, path = normalize_path(src), note = NULL))
  }
  sid <- cand$survey_id[[1]]
  stem <- cand$file_stem[[1]]
  sub <- file.path(dir, sid)
  dir.create(sub, recursive = TRUE, showWarnings = FALSE)
  note <- NULL
  if (store_format == "parquet") {
    dest <- file.path(sub, paste0(stem, ".parquet"))
    ok <- tryCatch(write_store_parquet(src, src_format, dest), error = function(e) e)
    if (isTRUE(ok)) {
      return(list(format = "parquet", path = file.path(sid, basename(dest)), note = NULL))
    }
    why <- if (inherits(ok, "error")) conditionMessage(ok) else "labels did not survive the conversion"
    cli::cli_warn(c(
      "Could not store {cand$origin[[1]]} as Parquet ({why}).",
      "i" = "The original file was copied instead."
    ))
    note <- "Parquet conversion failed; original file copied"
  }
  ext <- switch(src_format, dta = "dta", sav = "sav", sas7bdat = "sas7bdat")
  dest <- file.path(sub, paste0(stem, ".", ext))
  if (!file.copy(src, dest, overwrite = TRUE)) {
    cli::cli_abort("Could not copy the file to {.path {dest}}.")
  }
  list(format = src_format, path = file.path(sid, basename(dest)), note = note)
}

# Convert a data file to Parquet (+ attribute sidecar) and check that labels
# round-trip.
write_store_parquet <- function(src, src_format, dest) {
  df <- read_columns(src, src_format)
  df <- lower_names(df)
  attributes(df) <- attributes(df)[c("names", "row.names", "class")]
  df <- tibble::as_tibble(df)
  attrs <- lapply(df, attributes)
  plain <- strip_column_attributes(add_tagged_na_columns(df))
  tmp <- paste0(dest, ".part")
  tmp_attrs <- paste0(parquet_attrs_path(dest), ".part")
  on.exit(unlink(c(tmp, tmp_attrs)), add = TRUE)
  compression <- if (arrow::codec_is_available("zstd")) "zstd" else "snappy"
  arrow::write_parquet(plain, tmp, compression = compression)
  saveRDS(attrs, tmp_attrs)
  rm(plain)
  if (!parquet_round_trips(df, tmp, tmp_attrs)) {
    return(FALSE)
  }
  unlink(c(dest, parquet_attrs_path(dest)))
  file.rename(tmp_attrs, parquet_attrs_path(dest)) && file.rename(tmp, dest)
}

# Do labels, classes and tagged NAs of a sample of columns survive?
parquet_round_trips <- function(df, path, attrs_path = parquet_attrs_path(path)) {
  real <- names(df)
  labelled <- real[vapply(df[real], function(x) !is.null(attr(x, "labels", exact = TRUE)), logical(1))]
  tagged <- real[vapply(df, function(x) is.double(x) && any(!is.na(haven::na_tag(as.double(unclass(x))))), logical(1))]
  check <- unique(c(utils::head(real, 20), utils::head(labelled, 20), tagged, utils::tail(real, 5)))
  back <- read_parquet_columns(path, check, attrs_path = attrs_path)
  if (nrow(back) != nrow(df) || !identical(names(back), check)) {
    return(FALSE)
  }
  for (nm in check) {
    a <- df[[nm]]
    b <- back[[nm]]
    if (!identical(attributes(a), attributes(b)) || !identical(is.na(a), is.na(b))) {
      return(FALSE)
    }
    if (is.double(a) && !identical(haven::na_tag(as.double(unclass(a))), haven::na_tag(as.double(unclass(b))))) {
      return(FALSE)
    }
    if (!isTRUE(all.equal(unclass(a), unclass(b), check.attributes = FALSE))) {
      return(FALSE)
    }
  }
  TRUE
}

import_warnings <- function(res) {
  derived <- res[res$status == "derived", ]
  if (nrow(derived)) {
    msg <- paste0(derived$origin, ": ", derived$note %|NA|% "derived file")
    cli::cli_warn(c(
      "Skipped {nrow(derived)} derived or pooled file{?s}; bdhs stores original DHS recode files only.",
      stats::setNames(cli_escape(msg), rep("*", length(msg))),
      "i" = "Import the original DHS files and combine rounds with {.fn bdhs_pool}."
    ))
  }
  exists <- res[res$status == "exists", ]
  if (nrow(exists)) {
    msg <- paste0(exists$origin, " (", exists$survey_id, " ", exists$recode, ")")
    cli::cli_warn(c(
      "{nrow(exists)} dataset{?s} {?is/are} already registered from a different file and {?was/were} not replaced:",
      stats::setNames(cli_escape(msg), rep("*", length(msg))),
      "i" = "Use {.code overwrite = TRUE} to replace {cli::qty(nrow(exists))}{?it/them}."
    ))
  }
}

import_summary <- function(res, dir) {
  ok <- res[res$status == "imported", ]
  if (nrow(ok)) {
    reg <- registry_read(dir)
    i <- match(paste(ok$survey_id, ok$recode), paste(reg$survey_id, reg$recode))
    how <- ifelse(reg$linked[i], "registered in place",
      ifelse(reg$format[i] == "parquet", "stored as Parquet", paste0("copied (.", reg$format[i], ")"))
    )
    lines <- sprintf(
      "%s %s (%s): %s, %s rows x %s variables",
      ok$survey_id, ok$recode, ok$file_stem, how,
      format(ok$n_rows, big.mark = ","), format(ok$n_cols, big.mark = ",")
    )
    cli::cli_inform(c(
      "v" = "Imported {nrow(ok)} dataset{?s} into {.path {dir}}:",
      stats::setNames(cli_escape(lines), rep("*", length(lines)))
    ))
  }
  other <- res[res$status != "imported", ]
  if (nrow(other)) {
    what <- c(
      already = "already imported",
      duplicate = "duplicate",
      exists = "not replaced (use overwrite = TRUE)",
      derived = "derived or pooled file, skipped",
      unidentified = "not recognised as a BDHS recode file",
      refused = "not imported",
      nodata = "skipped",
      error = "failed"
    )
    type <- c(already = "i", duplicate = "i", exists = "!", derived = "!",
              unidentified = "x", refused = "x", nodata = "i", error = "x")
    lines <- paste0(
      other$origin, ": ", what[other$status],
      ifelse(is.na(other$note), "", paste0(" (", other$note, ")"))
    )
    cli::cli_inform(c(
      "i" = "Skipped {nrow(other)} file{?s}:",
      stats::setNames(cli_escape(lines), unname(type[other$status]))
    ))
  }
  if (!nrow(ok) && !nrow(other)) {
    cli::cli_inform("Nothing to import.")
  }
  invisible(res)
}

# Escape braces so that file names are printed literally by cli.
cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}
