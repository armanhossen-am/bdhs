# Identify which survey round and recode a DHS file holds -------------------------------

# File formats bdhs can read.
.data_exts <- c("dta", "sav", "zsav", "sas7bdat")

# Column names that people add when they build derived or pooled files.
.derived_cols <- c(
  "survey_id", "surveyid", "survey", "survey_year", "year", "round", "wave",
  "source", "dataset", "pooled", "_merge", "merge", "append", "appended"
)

#' Identify DHS microdata files
#'
#' Works out which survey round and recode each file holds, so that files can
#' be imported whatever they are called. Standard DHS names (`BDIR81FL.DTA`,
#' `BDKR7RDT.ZIP`, lower-case or prefixed, e.g. `2022BDPR81FL.DTA`) are
#' recognised from the name alone. Renamed files such as `BDHS2018KR.DTA` are
#' recognised from the year and recode code in the name and confirmed from
#' their content; anything else is identified from its content, reading only
#' the header and a few columns (`v000`/`hv000`, `v007`/`hv007` and the
#' identifier variables).
#'
#' Derived files -- several rounds appended together, or files with variables
#' that are not in the DHS recode -- are flagged with `derived = TRUE`.
#' [bdhs_import()] skips them, because bdhs only stores original DHS files.
#'
#' @param path Paths to files, folders (searched recursively) or zip archives
#'   (their members are listed, not extracted).
#' @return A tibble with one row per data file and columns `path`,
#'   `file_stem` (the DHS file stem, e.g. `"BDIR81FL"`), `survey_id`,
#'   `recode`, `format`, `method` (`"filename"` or `"content"`), `derived` and
#'   `note`.
#' @family data store
#' @export
#' @examples
#' f <- file.path(tempdir(), "BDHS2022KR.dta")
#' haven::write_dta(bdhs_simulate(2022, "KR", n_clusters = 4, households = 3), f)
#' bdhs_identify_file(f)
bdhs_identify_file <- function(path) {
  if (!is.character(path) || !length(path)) {
    cli::cli_abort("{.arg path} must be a character vector of file or folder paths.")
  }
  missing <- path[!file.exists(path)]
  if (length(missing)) {
    cli::cli_abort("Can't find {.path {missing}}.")
  }
  rows <- list()
  for (p in path) {
    files <- if (dir.exists(p)) list_data_files(p, zips = TRUE) else p
    for (f in files) {
      if (identical(file_format(f), "zip")) {
        rows <- c(rows, identify_zip_members(f))
      } else {
        rows <- c(rows, list(identify_one(f, content = "auto")))
      }
    }
  }
  identification_tibble(rows)
}

identification_tibble <- function(rows) {
  if (!length(rows)) {
    return(tibble::tibble(
      path = character(), file_stem = character(), survey_id = character(),
      recode = character(), format = character(), method = character(),
      derived = logical(), note = character()
    ))
  }
  get <- function(field, type) vapply(rows, function(r) r[[field]] %||% type[NA_integer_], type)
  tibble::tibble(
    path = get("path", character(1)),
    file_stem = get("file_stem", character(1)),
    survey_id = get("survey_id", character(1)),
    recode = get("recode", character(1)),
    format = get("format", character(1)),
    method = get("method", character(1)),
    derived = get("derived", logical(1)),
    note = get("note", character(1))
  )
}

# Data files (and optionally zips) below a folder.
list_data_files <- function(dir, zips = TRUE) {
  exts <- c(.data_exts, if (zips) "zip")
  pattern <- paste0("\\.(", paste(exts, collapse = "|"), ")$")
  files <- list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  sort(normalize_path(files))
}

file_format <- function(path) {
  map <- c(dta = "dta", sav = "sav", zsav = "sav", sas7bdat = "sas7bdat", parquet = "parquet", zip = "zip")
  unname(map[tolower(tools::file_ext(path))])
}

# Identification by file name -----------------------------------------------------------

# Parse a file name. Returns a list with survey_id, recode, file_stem, kind
# ("standard", "renamed", "none"), derived and note.
parse_dhs_name <- function(path) {
  stem <- toupper(tools::file_path_sans_ext(basename(path)))
  out <- list(survey_id = NA_character_, recode = NA_character_, file_stem = NA_character_,
              kind = "none", derived = FALSE, note = NA_character_)
  # Standard DHS stems, optionally prefixed with a year: BDIR81FL, BDKR7RDT,
  # BDIR81, 2022BDPR81FL, 2017_BDPR7RFL.
  m <- regmatches(stem, regexec(
    "^(?:((?:19|20)[0-9]{2})[ _.-]?)?BD([A-Z]{2})([0-9][0-9A-Z])(FL|DT|SV|SD)?$", stem, perl = TRUE
  ))[[1]]
  if (length(m)) {
    core <- paste0("BD", m[[3]], m[[4]])
    cat <- catalogue()
    hit <- which(substr(cat$file_stem, 1, 6) == core)
    if (length(hit)) {
      out$survey_id <- cat$survey_id[[hit[[1]]]]
      out$recode <- cat$file_type_code[[hit[[1]]]]
    } else if (m[[3]] %in% .recodes$recode) {
      out$survey_id <- bdhs_survey_id(m[[4]], error = FALSE)
      out$recode <- m[[3]]
      out$note <- "file version not in the DHS catalogue"
    }
    if (!is.na(out$survey_id)) {
      out$kind <- "standard"
      out$file_stem <- paste0(core, "FL")
      if (nzchar(m[[2]])) {
        prefix_round <- bdhs_survey_id(m[[2]], error = FALSE)
        if (!identical(prefix_round, out$survey_id)) {
          out$note <- paste0("year prefix ", m[[2]], " does not match the DHS file code")
        }
      }
      return(out)
    }
  }
  # Renamed files: a survey year and a recode code, e.g. BDHS2018KR, bdhs_2014_ir.
  years <- regmatches(stem, gregexpr("(?<![0-9])(19|20)[0-9]{2}(?![0-9])", stem, perl = TRUE))[[1]]
  rest <- gsub("(19|20)[0-9]{2}", " ", stem)
  tokens <- strsplit(rest, "[^A-Z]+")[[1]]
  tokens <- sub("^(BDHS|BDDHS|DHS|BD)(?=[A-Z]{2}$)", "", tokens, perl = TRUE)
  recodes <- unique(tokens[tokens %in% .recodes$recode])
  rounds <- unique(stats::na.omit(bdhs_survey_id(years, type = "DHS", error = FALSE)))
  if (length(recodes) == 1L && length(rounds)) {
    out$recode <- recodes
    out$kind <- "renamed"
    if (length(rounds) > 1L) {
      out$derived <- TRUE
      out$note <- paste0("name refers to several rounds (", paste(unique(years), collapse = ", "), ")")
    } else {
      out$survey_id <- rounds
      out$file_stem <- catalogue_stem(rounds, recodes)
    }
  }
  if (grepl("APPEND|POOL|MERGE|COMBIN|STACK", stem)) {
    out$derived <- TRUE
    out$note <- paste(stats::na.omit(c(out$note, "name suggests a derived (appended/pooled/merged) file")), collapse = "; ")
  }
  out
}

# Identification by content -----------------------------------------------------------

# Read only what is needed to identify a file: the header and the country,
# year and identifier columns.
inspect_content <- function(path, format = file_format(path)) {
  names <- read_header_names(path, format)
  nm <- tolower(names)
  pick <- function(cands) {
    i <- match(cands, nm)
    i <- i[!is.na(i)]
    if (length(i)) names[[i[[1]]]] else NULL
  }
  c000 <- pick(c("v000", "hv000", "mv000"))
  c007 <- pick(c("v007", "hv007", "mv007"))
  c008 <- if (is.null(c007)) pick(c("v008", "hv008", "mv008")) else NULL
  recode <- recode_from_names(nm)
  # KR and BR can have the same variables (the 2017-18 BR carries midx): the
  # children's ages decide, so read the interview and birth dates too.
  ages <- if (recode %in% c("KR", "BR")) c(pick("v008"), pick("b3")) else NULL
  cols <- unique(c(c000, c007, c008, if (length(ages) == 2L) ages))
  if (!length(cols)) {
    cols <- names[[1]]
  }
  x <- read_columns(path, format, cols)
  if (length(ages) == 2L) {
    age <- as_codes(x[[ages[[1]]]]) - as_codes(x[[ages[[2]]]])
    if (any(!is.na(age))) {
      recode <- if (max(age, na.rm = TRUE) <= 60) "KR" else "BR"
    }
  }
  out <- list(
    names = names, n_cols = length(names), n_rows = nrow(x),
    survey_id = NA_character_, recode = recode,
    derived = FALSE, notes = character()
  )
  v000 <- if (!is.null(c000)) unique(trimws(as.character(zap_all(x[[c000]])))) else character()
  v000 <- v000[!is.na(v000) & nzchar(v000)]
  if (length(v000) && !all(substr(v000, 1, 2) == "BD")) {
    out$notes <- c(out$notes, paste0("not a Bangladesh file (country code ", paste(unique(substr(v000, 1, 2)), collapse = ", "), ")"))
  }
  years <- if (!is.null(c007)) {
    normalize_years(as_codes(x[[c007]]))
  } else if (!is.null(c008)) {
    cmc <- as_codes(x[[c008]])
    1900 + (cmc - 1) %/% 12
  } else {
    numeric()
  }
  rounds <- unique(stats::na.omit(bdhs_survey_id(unique(stats::na.omit(years)), type = "DHS", error = FALSE)))
  if (length(v000) > 1L || length(rounds) > 1L) {
    out$derived <- TRUE
    what <- c(
      if (length(v000) > 1L) paste0("country-phase codes ", paste(sort(v000), collapse = ", ")),
      if (length(rounds) > 1L) paste0("rounds ", paste(rounds, collapse = ", "))
    )
    out$notes <- c(out$notes, paste0("pooled file: several survey rounds (", paste(what, collapse = "; "), ")"))
  } else if (length(rounds) == 1L) {
    out$survey_id <- rounds
  } else if (length(v000) == 1L && substr(v000, 1, 2) == "BD") {
    out$survey_id <- survey_from_v000(v000)
  }
  extra <- intersect(nm, .derived_cols)
  odd <- nm[!dhs_like_name(nm)]
  core_recode <- out$recode %in% c("IR", "MR", "KR", "BR", "HR", "PR", "CR")
  if (length(extra) || (core_recode && length(odd) >= 3L && length(odd) / length(nm) >= 0.1)) {
    out$derived <- TRUE
    shown <- utils::head(unique(c(extra, odd)), 6)
    out$notes <- c(out$notes, paste0("has variables that are not in DHS recodes (", paste(shown, collapse = ", "), if (length(unique(c(extra, odd))) > 6) ", ..." else "", ")"))
  }
  out
}

# 2-digit and 4-digit interview years -> 4-digit years (0 -> 2000, 99 -> 1999).
normalize_years <- function(y) {
  y <- as.numeric(y)
  ifelse(is.na(y), NA_real_, ifelse(y < 50, 2000 + y, ifelse(y < 100, 1900 + y, y)))
}

# Recode type from the variables present (lower-case names).
recode_from_names <- function(nm) {
  has <- function(...) all(c(...) %in% nm)
  if (has("hwhhid")) {
    "HW"
  } else if (has("whhid")) {
    "WI"
  } else if (has("midx")) {
    "KR" # or BR (2017-18): inspect_content() checks the children's ages
  } else if (has("bidx", "caseid")) {
    "BR"
  } else if (has("v001", "mv001")) {
    "CR"
  } else if (has("mv001")) {
    "MR"
  } else if (has("caseid", "v001")) {
    "IR"
  } else if (has("hvidx")) {
    "PR"
  } else if (has("hhid", "hv001")) {
    "HR"
  } else {
    NA_character_
  }
}

# Does a (lower-case) variable name look like a DHS recode variable?
dhs_like_name <- function(nm) {
  grepl("^[a-z]{1,8}_?[0-9]", nm) |
    grepl("^s[a-z0-9_$]*$", nm) |
    grepl("idx|^awfact|^caseid$|^mcaseid$|^hhid$|^whhid$|^hwhhid$|^hwline$", nm)
}

# Combined identification ---------------------------------------------------------------

# content: "auto" reads the content unless a standard DHS name identifies the
# file; "always" always reads it (needed by bdhs_import() for row counts).
identify_one <- function(path, content = c("auto", "always", "never"), display = path) {
  content <- rlang::arg_match(content)
  format <- file_format(path)
  by_name <- parse_dhs_name(path)
  out <- list(
    path = display, file_stem = by_name$file_stem, survey_id = by_name$survey_id,
    recode = by_name$recode, format = format, method = "filename",
    derived = by_name$derived, note = by_name$note, n_rows = NA_integer_, n_cols = NA_integer_
  )
  if (!format %in% c("dta", "sav", "sas7bdat")) {
    if (is.na(out$survey_id)) out$method <- NA_character_
    if (is.na(format)) out$note <- "not a Stata, SPSS or SAS data file"
    return(out)
  }
  read_content <- content == "always" || (content == "auto" && by_name$kind != "standard")
  if (!read_content) {
    return(out)
  }
  info <- tryCatch(inspect_content(path, format), error = function(e) e)
  if (inherits(info, "error")) {
    out$note <- paste0("could not read the file: ", conditionMessage(info))
    if (is.na(out$survey_id)) out$method <- NA_character_
    return(out)
  }
  out$n_rows <- as.integer(info$n_rows)
  out$n_cols <- as.integer(info$n_cols)
  notes <- c(if (!is.na(out$note)) out$note, info$notes)
  if (info$derived) {
    out$derived <- TRUE
  }
  if (out$derived) {
    out$method <- "content"
    if (!is.na(info$recode) && is.na(out$recode)) out$recode <- info$recode
    if (!is.na(info$survey_id) && is.na(out$survey_id)) out$survey_id <- info$survey_id
  } else if (is.na(out$survey_id) || is.na(out$recode)) {
    # Name was not enough: use the content.
    out$method <- "content"
    if (is.na(out$survey_id)) out$survey_id <- info$survey_id
    if (is.na(out$recode)) out$recode <- info$recode
  } else {
    if (!is.na(info$survey_id) && !identical(info$survey_id, out$survey_id)) {
      notes <- c(notes, paste0("name suggests ", out$survey_id, " but the interview years are from ", info$survey_id))
      out$survey_id <- info$survey_id
      out$method <- "content"
    }
    if (!is.na(info$recode) && !identical(info$recode, out$recode)) {
      notes <- c(notes, paste0("name suggests ", out$recode, " but the variables look like ", info$recode))
    }
  }
  if (!is.na(out$survey_id) && !is.na(out$recode)) {
    out$file_stem <- catalogue_stem(out$survey_id, out$recode) %|NA|% out$file_stem
  }
  if (is.na(out$survey_id) && !out$derived) {
    out$method <- NA_character_
    notes <- c(notes, "could not identify the survey round")
  }
  out$note <- if (length(notes)) paste(unique(notes), collapse = "; ") else NA_character_
  out
}

# Members of a zip, identified from their names only.
identify_zip_members <- function(zip) {
  members <- tryCatch(utils::unzip(zip, list = TRUE), error = function(e) NULL)
  if (is.null(members)) {
    return(list(list(path = zip, format = "zip", method = NA_character_, derived = FALSE,
                     note = "could not open the zip archive")))
  }
  names <- members$Name[!grepl("/$", members$Name)]
  names <- names[file_format(names) %in% c("dta", "sav", "sas7bdat", "zip")]
  if (!length(names)) {
    by_name <- parse_dhs_name(zip)
    return(list(list(
      path = zip, file_stem = by_name$file_stem, survey_id = by_name$survey_id,
      recode = by_name$recode, format = "zip", method = if (!is.na(by_name$survey_id)) "filename" else NA_character_,
      derived = by_name$derived,
      note = "no Stata, SPSS or SAS data file inside (flat ASCII files are not supported)"
    )))
  }
  lapply(names, function(n) {
    r <- identify_one(n, content = "never", display = file.path(zip, n))
    r$format <- file_format(n)
    r
  })
}
