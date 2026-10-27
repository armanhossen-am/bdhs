# Reading DHS recode files -------------------------------------------------------------

# Identifier and complex-design variables kept by bdhs_read(keep_design = TRUE).
.read_keep_vars <- c(
  "caseid", "mcaseid", "hhid", "hvidx", "bidx", "midx", "hwhhid", "hwline", "whhid",
  "v001", "v002", "v003", "v005", "v021", "v022", "v023", "v024", "v025",
  "mv001", "mv002", "mv003", "mv005", "mv021", "mv022", "mv023", "mv024", "mv025",
  "hv001", "hv002", "hv003", "hv005", "hv021", "hv022", "hv023", "hv024", "hv025",
  # Survey identification and interview dates, needed to detect the round and
  # for every time-windowed indicator.
  "v000", "v007", "v008", "hv000", "hv007", "hv008", "mv000", "mv007", "mv008",
  # Child's date of birth and age, which define the births and children
  # files' reference periods.
  "b3", "b19"
)

# Prefix of the companion columns that keep Stata extended missing values
# (haven tagged NAs, ".a" to ".z") in Parquet copies, which cannot store them.
.tag_prefix <- "__bdhs_tagged_na__"

#' Read a BDHS recode file from your data store
#'
#' Loads any round and recode you have imported with [bdhs_import()] in one
#' line. Only the requested columns are read from disk, which makes reading a
#' few variables from even the largest files fast.
#'
#' Column names are returned in lower case. Variable labels and value labels
#' are kept as [haven::labelled()] columns by default, and Stata extended
#' missing values (`.a` to `.z`) as tagged `NA`s.
#'
#' @param survey A survey reference understood by [bdhs_survey_id()], e.g.
#'   `2022`, `"2017-18"` or `"BD2014DHS"`.
#' @param recode A recode code (see [bdhs_recodes()]), e.g. `"IR"` or `"KR"`.
#' @param vars Variables to read (case-insensitive). `NULL` reads all of them
#'   unless `vars_regex` is given. Requested variables that do not exist in
#'   the file are skipped with a warning.
#' @param vars_regex Regular expressions; variables whose (lower-case) names
#'   match any of them are read too.
#' @param indicators Indicator ids from [bdhs_indicator_list()]. Every
#'   variable these indicators need is read, so the result can go straight
#'   into [bdhs_estimate()]. Combines with `vars` and `vars_regex`.
#' @param keep_design If `TRUE` (default) and variables are selected, also
#'   read the identifiers, survey design variables and interview dates needed
#'   to link files, detect the round and build a survey design with
#'   [bdhs_design()]: `caseid`, `hhid`, `hvidx`, `bidx`, `midx`,
#'   `v000`-`v003`, `v005`, `v007`, `v008`, `v021`-`v025` (and their `hv`/`mv`
#'   equivalents), the child's birth date and age (`b3`, `b19`) and the
#'   all-woman factors `awfact*`.
#' @param labels How to return labelled variables: `"haven"` (default) keeps
#'   [haven::labelled()] columns; `"factor"` converts every variable whose
#'   values are all labelled to a factor with [haven::as_factor()] (`levels =
#'   "labels"`), leaving partially labelled variables such as z-scores with
#'   flag codes as labelled numbers; `"codes"` drops value labels (keeping
#'   variable labels) and turns extended missing values into plain `NA`.
#' @param n_max Maximum number of rows to read.
#'
#' @return A tibble with the attributes `bdhs_survey_id`, `bdhs_recode` and
#'   `bdhs_file` (the DHS file stem, e.g. `"BDIR81FL"`).
#' @seealso [bdhs_read_file()] to read a file that is not in the store,
#'   [bdhs_inventory()] to see what you have.
#' @family data store
#' @export
#' @examples
#' \dontshow{old <- options(bdhs.data_dir = file.path(tempdir(), "bdhs-read-example"))}
#' # Synthetic data written under a DHS file name, for illustration only
#' f <- file.path(tempdir(), "BDIR81FL.DTA")
#' haven::write_dta(bdhs_simulate(2022, "IR", n_clusters = 6, households = 4), f)
#' bdhs_import(f, format = "dta", quiet = TRUE)
#'
#' ir <- bdhs_read(2022, "IR", vars = c("v012", "v106"))
#' names(ir)
#'
#' # Exactly the columns two indicators need
#' ir <- bdhs_read(2022, "IR", indicators = c("fp_cpr_modern", "fp_unmet_total"))
#' bdhs_estimate(ir, c("fp_cpr_modern", "fp_unmet_total"))
#' attr(ir, "bdhs_file")
#' bdhs_read(2022, "IR", vars_regex = "^v1[0-9]{2}$", keep_design = FALSE, labels = "factor")
#' \dontshow{bdhs_forget(2022, "IR", delete = TRUE); options(old)}
bdhs_read <- function(survey, recode, vars = NULL, vars_regex = NULL, indicators = NULL,
                      keep_design = TRUE, labels = c("haven", "factor", "codes"), n_max = Inf) {
  labels <- rlang::arg_match(labels)
  check_n_max(n_max)
  survey_id <- bdhs_survey_id(survey, type = c("DHS", "SPA", "OTH"))
  if (length(survey_id) != 1L) {
    cli::cli_abort(c(
      "{.arg survey} must refer to a single survey round.",
      "i" = "To combine rounds, read them one by one and use {.fn bdhs_pool}."
    ))
  }
  recode <- check_recode(recode)
  dir <- store_dir()
  reg <- registry_read(dir)
  row <- reg[reg$survey_id == survey_id & reg$recode == recode, ]
  if (!nrow(row)) {
    abort_not_available(survey_id, recode)
  }
  row <- row[1, ]
  path <- registry_abs_path(row, dir)
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "The file registered for {survey_id} {recode} is missing: {.path {path}}.",
      "i" = if (row$linked) {
        "It was registered in place ({.code copy = FALSE}) and has been moved or deleted. Run {.fn bdhs_import} on its new location."
      } else {
        "Import it again with {.fn bdhs_import}."
      }
    ))
  }
  out <- read_dhs(path, row$format, vars, vars_regex, keep_design, labels, n_max,
                  what = paste(survey_id, recode), indicators = indicators)
  set_dhs_attrs(out, survey_id, recode, row$file_stem)
}

#' Read a DHS data file directly
#'
#' Reads a Stata (`.dta`), SPSS (`.sav`) or SAS (`.sas7bdat`) DHS file, or a
#' DHS zip archive containing one, without importing it, using the same
#' conventions as [bdhs_read()]: lower-case names, labels kept, column
#' selection without reading the whole file. The survey round and recode are
#' identified with [bdhs_identify_file()] and stored as attributes.
#'
#' @param path Path to the file.
#' @inheritParams bdhs_read
#' @return A tibble with the attributes `bdhs_survey_id`, `bdhs_recode` and
#'   `bdhs_file` when the file could be identified.
#' @family data store
#' @export
#' @examples
#' f <- file.path(tempdir(), "BDHS2022KR.dta")
#' haven::write_dta(bdhs_simulate(2022, "KR", n_clusters = 4, households = 3), f)
#' kr <- bdhs_read_file(f, vars = c("hw70", "b4"))
#' attr(kr, "bdhs_survey_id")
bdhs_read_file <- function(path, vars = NULL, vars_regex = NULL, indicators = NULL,
                           keep_design = TRUE, labels = c("haven", "factor", "codes"), n_max = Inf) {
  labels <- rlang::arg_match(labels)
  check_n_max(n_max)
  if (!rlang::is_string(path) || !file.exists(path) || dir.exists(path)) {
    cli::cli_abort("{.arg path} must be the path of an existing file.")
  }
  format <- file_format(path)
  display <- path
  if (identical(format, "zip")) {
    tmp <- tempfile("bdhs-zip-")
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    members <- extract_zip_data(path, tmp)
    if (!nrow(members)) {
      cli::cli_abort(c(
        "{.path {path}} contains no Stata, SPSS or SAS data file.",
        "i" = "Flat ASCII (.DAT) files are not supported; download the Stata version of the dataset."
      ))
    }
    members <- members[order(format_priority(members$format)), ]
    stems <- unique(toupper(tools::file_path_sans_ext(basename(members$origin))))
    if (length(stems) > 1L) {
      cli::cli_abort(c(
        "{.path {path}} contains several datasets: {.file {basename(members$origin)}}.",
        "i" = "Import them all with {.code bdhs_import(\"{path}\")} and read them with {.fn bdhs_read}."
      ))
    }
    display <- members$origin[[1]]
    path <- members$path[[1]]
    format <- members$format[[1]]
  }
  if (!format %in% c("dta", "sav", "sas7bdat", "parquet")) {
    cli::cli_abort("Can't read {.path {path}}: bdhs reads Stata (.dta), SPSS (.sav), SAS (.sas7bdat) and zip files.")
  }
  id <- identify_one(path, content = "auto", display = display)
  if (isTRUE(id$derived)) {
    cli::cli_warn(c(
      "{.file {basename(display)}} looks like a derived or pooled file, not an original DHS recode.",
      "i" = id$note,
      "i" = "Survey attributes are not set."
    ))
  } else if (is.na(id$survey_id) || is.na(id$recode)) {
    cli::cli_warn(c(
      "Could not identify the survey round and recode of {.file {basename(display)}}.",
      "i" = if (!is.na(id$note %||% NA)) cli_escape(id$note),
      "i" = "Survey attributes are not set; supply {.arg survey} and {.arg recode} to functions that need them."
    ))
  }
  out <- read_dhs(path, format, vars, vars_regex, keep_design, labels, n_max,
                  what = basename(display), indicators = indicators)
  if (!isTRUE(id$derived) && !is.na(id$survey_id) && !is.na(id$recode)) {
    out <- set_dhs_attrs(out, id$survey_id, id$recode, id$file_stem)
  }
  out
}

# Helpers ---------------------------------------------------------------------------------

check_n_max <- function(n_max, call = rlang::caller_env()) {
  if (!is.numeric(n_max) || length(n_max) != 1L || is.na(n_max) || n_max < 0) {
    cli::cli_abort("{.arg n_max} must be a single non-negative number.", call = call)
  }
}

set_dhs_attrs <- function(df, survey_id, recode, file_stem) {
  attr(df, "bdhs_survey_id") <- survey_id
  attr(df, "bdhs_recode") <- recode
  attr(df, "bdhs_file") <- file_stem
  df
}

abort_not_available <- function(survey_id, recode, call = rlang::caller_env()) {
  cr <- catalogue_recodes(c("DHS", "SPA", "OTH"))
  this <- cr[cr$survey_id == survey_id, ]
  rname <- .recodes$name[match(recode, .recodes$recode)]
  label <- paste(survey_label(survey_id), .rounds$survey_type[match(survey_id, .rounds$survey_id)])
  if (!recode %in% this$recode) {
    other <- cr$survey_id[cr$recode == recode]
    cli::cli_abort(c(
      "The {label} ({survey_id}) has no {rname} ({recode}): the DHS Program never released one for this round.",
      "i" = "Recodes released for {survey_id}: {.val {this$recode}}.",
      "i" = if (length(other)) "{rname} files exist for {.val {other}}." else NULL
    ), call = call)
  }
  stata <- this$stata_file[this$recode == recode][[1]]
  cli::cli_abort(c(
    "{survey_id} {recode} ({rname}) is not in your bdhs data store.",
    "i" = "DHS microdata are free for registered users: {.url https://dhsprogram.com/data/new-user-registration.cfm}.",
    "i" = "Download it from the DHS Program{if (!is.na(stata)) paste0(' (Stata file ', stata, ')') else ''}, then register it once with {.code bdhs_import(\"path/to/the/file\")}.",
    "i" = "Data directory: {.path {store_dir()}}. See {.fn bdhs_inventory} for what you have."
  ), call = call)
}

# Read selected columns of a DHS file, with bdhs conventions.
read_dhs <- function(path, format, vars, vars_regex, keep_design, labels, n_max, what,
                     indicators = NULL) {
  names <- read_header_names(path, format)
  lower <- tolower(names)
  if (!is.null(indicators)) {
    vars <- union(vars, indicator_columns(indicators, lower))
  }
  if (is.null(vars) && is.null(vars_regex)) {
    sel <- names
  } else {
    want <- character()
    if (!is.null(vars)) {
      v <- unique(tolower(trimws(as.character(vars))))
      miss <- setdiff(v, lower)
      if (length(miss)) {
        cli::cli_warn("{cli::qty(length(miss))}Variable{?s} not found in {what} and skipped: {.val {miss}}.")
      }
      want <- intersect(v, lower)
    }
    if (!is.null(vars_regex)) {
      rx <- lower[grepl(paste0("(", vars_regex, ")", collapse = "|"), lower, perl = TRUE)]
      if (!length(rx)) {
        cli::cli_warn("No variable in {what} matches {.val {vars_regex}}.")
      }
      want <- union(want, rx)
    }
    if (!length(want)) {
      cli::cli_abort("None of the requested variables are in {what}.")
    }
    if (keep_design) {
      want <- union(lower[lower %in% .read_keep_vars | grepl("^awfact", lower)], want)
    }
    sel <- names[lower %in% want]
  }
  df <- read_columns(path, format, sel, n_max)
  df <- tibble::as_tibble(lower_names(df))
  apply_labels(df, labels)
}

read_header_names <- function(path, format = file_format(path)) {
  switch(format,
    dta = names(haven::read_dta(path, n_max = 0)),
    sav = names(haven::read_sav(path, n_max = 0)),
    sas7bdat = names(haven::read_sas(path, n_max = 0)),
    parquet = {
      nm <- parquet_names(path)
      nm[!startsWith(nm, .tag_prefix)]
    },
    cli::cli_abort("Can't read {.path {path}}: unsupported file format.")
  )
}

# Read columns `cols` (exact names; NULL = all) from a file.
read_columns <- function(path, format, cols = NULL, n_max = Inf) {
  if (identical(format, "parquet")) {
    return(read_parquet_columns(path, cols, n_max))
  }
  reader <- switch(format,
    dta = haven::read_dta,
    sav = haven::read_sav,
    sas7bdat = haven::read_sas,
    cli::cli_abort("Can't read {.path {path}}: unsupported file format.")
  )
  if (is.null(cols)) {
    reader(path, n_max = n_max)
  } else {
    reader(path, col_select = dplyr::all_of(cols), n_max = n_max)
  }
}

parquet_names <- function(path) {
  check_installed("arrow", "to read datasets stored in Parquet format")
  file <- arrow::ReadableFile$create(path)
  on.exit(file$close())
  arrow::ParquetFileReader$create(file)$GetSchema()$names
}

# Column attributes (variable and value labels, classes) of a Parquet copy are
# kept in a sidecar .rds file: arrow's own R metadata becomes very slow for
# files with thousands of labelled columns, such as DHS women's recodes.
parquet_attrs_path <- function(path) {
  sub("\\.parquet$", ".attrs.rds", path)
}

read_parquet_columns <- function(path, cols = NULL, n_max = Inf, attrs_path = parquet_attrs_path(path)) {
  all <- parquet_names(path)
  if (is.null(cols)) {
    cols <- all[!startsWith(all, .tag_prefix)]
  }
  tags <- intersect(paste0(.tag_prefix, cols), all)
  tab <- arrow::read_parquet(path, col_select = dplyr::all_of(c(cols, tags)), as_data_frame = FALSE)
  if (is.finite(n_max) && n_max < tab$num_rows) {
    tab <- tab$Slice(0, as.integer(n_max))
  }
  df <- tibble::as_tibble(as.data.frame(tab))
  if (file.exists(attrs_path)) {
    attrs <- readRDS(attrs_path)
    for (nm in intersect(names(df), names(attrs))) {
      if (!is.null(attrs[[nm]])) attributes(df[[nm]]) <- attrs[[nm]]
    }
  }
  restore_tagged_na(df)
}

# Plain vectors (no attributes) for writing to Parquet.
strip_column_attributes <- function(df) {
  for (nm in names(df)) {
    x <- df[[nm]]
    attributes(x) <- NULL
    df[[nm]] <- x
  }
  df
}

# Files that make up a stored dataset.
store_files <- function(path, format) {
  c(path, parquet_attrs_path(path[format %in% "parquet"]))
}

# Parquet cannot hold tagged NAs: keep their tags in companion columns.
add_tagged_na_columns <- function(df) {
  for (nm in names(df)) {
    x <- df[[nm]]
    if (is.double(x) && anyNA(x)) {
      tags <- haven::na_tag(as.double(unclass(x)))
      if (any(!is.na(tags))) {
        df[[paste0(.tag_prefix, nm)]] <- tags
      }
    }
  }
  df
}

restore_tagged_na <- function(df) {
  tag_cols <- names(df)[startsWith(names(df), .tag_prefix)]
  for (tc in tag_cols) {
    col <- substring(tc, nchar(.tag_prefix) + 1L)
    tags <- df[[tc]]
    idx <- which(!is.na(tags))
    if (col %in% names(df) && length(idx)) {
      x <- df[[col]]
      attrs <- attributes(x)
      v <- as.double(unclass(x))
      v[idx] <- haven::tagged_na(tags[idx])
      attributes(v) <- attrs
      df[[col]] <- v
    }
    df[[tc]] <- NULL
  }
  df
}

apply_labels <- function(df, labels) {
  if (labels == "haven") {
    return(df)
  }
  for (nm in names(df)) {
    x <- df[[nm]]
    if (labels == "codes") {
      if (inherits(x, "haven_labelled") || is.double(x)) {
        df[[nm]] <- haven::zap_labels(haven::zap_missing(x))
      }
    } else if (inherits(x, "haven_labelled") && fully_labelled(x)) {
      df[[nm]] <- haven::as_factor(x, levels = "labels")
    }
  }
  df
}

# Are all observed values of a labelled vector labelled?
fully_labelled <- function(x) {
  labs <- attr(x, "labels", exact = TRUE)
  if (is.null(labs)) {
    return(FALSE)
  }
  vals <- unique(as.vector(unclass(haven::zap_missing(x))))
  vals <- vals[!is.na(vals)]
  all(vals %in% unclass(labs))
}

format_priority <- function(format) {
  match(format, c("dta", "sav", "sas7bdat"), nomatch = 9L)
}
