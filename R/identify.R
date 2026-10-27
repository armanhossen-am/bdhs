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
#' recognised from the name alone (their variable names are checked against
#' the recode). Renamed files such as `BDHS2018KR.DTA` or `SPA2014_FC.dta`
#' are recognised from the year and recode code in the name and must be
#' confirmed by their content: a Bangladesh country code (`v000`, `hv000` or
#' `mv000`) and the identifier variables of the recode. Anything else is
#' identified from its content, reading only the header and a few columns
#' (`v000`/`hv000`, `v007`/`hv007` and the identifier variables).
#'
#' Files whose name and content disagree (another round, recode or country),
#' and renamed files that their content does not confirm, are left
#' unidentified (`survey_id` and `recode` are `NA`) with a warning; the
#' `note` column says why. [bdhs_import()] does not import them.
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
  warn_refused(rows)
  identification_tibble(rows)
}

# Warn about files left unidentified because their name and content disagree
# or their content does not confirm their name. `rows`: identify_one() results
# (lists or a data frame with columns path/origin, refused and note).
warn_refused <- function(rows, label = "path", done = "identified") {
  if (!is.data.frame(rows)) {
    rows <- list(
      path = vapply(rows, function(r) r$path %||% NA_character_, character(1)),
      refused = vapply(rows, function(r) r$refused %||% NA_character_, character(1)),
      note = vapply(rows, function(r) r$note %||% NA_character_, character(1))
    )
  }
  bullets <- function(i) {
    msg <- paste0(rows[[label]][i], ": ", rows$note[i] %|NA|% "")
    stats::setNames(cli_escape(msg), rep("*", length(msg)))
  }
  conflict <- which(rows$refused %in% "conflict")
  if (length(conflict)) {
    cli::cli_warn(c(
      "The name and the content of {length(conflict)} file{?s} disagree, so {?it was/they were} not {done}:",
      bullets(conflict),
      "i" = "Check that each file is what its name says; download the original DHS file again if in doubt."
    ))
  }
  unconfirmed <- which(rows$refused %in% "unconfirmed")
  if (length(unconfirmed)) {
    cli::cli_warn(c(
      "{length(unconfirmed)} renamed file{?s} could not be confirmed as {?a /}BDHS recode file{?s} and {?was/were} not {done}:",
      bullets(unconfirmed),
      "i" = "A renamed file is identified only when its content confirms its name."
    ))
  }
  invisible(rows)
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
# ("standard", "renamed", "none"), derived, ambiguous and note.
parse_dhs_name <- function(path) {
  stem <- toupper(tools::file_path_sans_ext(basename(path)))
  out <- list(survey_id = NA_character_, recode = NA_character_, file_stem = NA_character_,
              kind = "none", derived = FALSE, ambiguous = FALSE, note = NA_character_)
  # Standard DHS stems, optionally prefixed with a year: BDIR81FL, BDKR7RDT,
  # BDIR81, 2022BDPR81FL, 2017_BDPR7RFL; SPA files end in SP (BDFC7AFLSP).
  m <- regmatches(stem, regexec(
    "^(?:((?:19|20)[0-9]{2})[ _.-]?)?BD([A-Z]{2})([0-9][0-9A-Z])(FL|DT|SV|SD)?(SP)?$", stem, perl = TRUE
  ))[[1]]
  if (length(m)) {
    core <- paste0("BD", m[[3]], m[[4]])
    cat <- catalogue()
    hit <- which(substr(cat$file_stem, 1, 6) == core)
    if (nzchar(m[[6]])) {
      # The SP suffix is only used by Service Provision Assessment files.
      hit <- hit[cat$survey_type[hit] == "SPA"]
    }
    if (length(hit)) {
      out$survey_id <- cat$survey_id[[hit[[1]]]]
      out$recode <- cat$file_type_code[[hit[[1]]]]
    } else if (!nzchar(m[[6]]) && m[[3]] %in% .recodes$recode) {
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
  # Renamed files: a survey year and a recode code, e.g. BDHS2018KR,
  # bdhs_2014_ir, SPA2014_FC.
  years <- regmatches(stem, gregexpr("(?<![0-9])(19|20)[0-9]{2}(?![0-9])", stem, perl = TRUE))[[1]]
  rest <- gsub("(19|20)[0-9]{2}", " ", stem)
  tokens <- strsplit(rest, "[^A-Z]+")[[1]]
  says_spa <- any(grepl("^(BD)?SPA$", tokens))
  says_bmms <- any(grepl("^(BD)?BMMS$", tokens))
  tokens <- sub("^(BDHS|BDDHS|DHS|BD|SPA|BMMS)(?=[A-Z]{2}$)", "", tokens, perl = TRUE)
  recodes <- unique(tokens[tokens %in% .recodes$recode])
  type <- if (length(recodes) == 1L) name_survey_type(recodes, says_spa, says_bmms) else "DHS"
  if (length(recodes) == 1L && length(years) && is.na(type)) {
    # e.g. "SPA2014_KR": an SPA name with a recode that SPA surveys do not have.
    out$recode <- recodes
    out$kind <- "renamed"
    out$ambiguous <- TRUE
    out$note <- paste0("the name mixes ", if (says_spa) "SPA" else "BMMS", " with the ", recodes,
                       " recode, so the survey is ambiguous")
    return(out)
  }
  rounds <- if (length(years)) rounds_from_years(years, type) else character()
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

# Survey type ("DHS", "SPA" or "OTH") that a renamed file refers to, from its
# recode and an explicit "SPA"/"BMMS" in the name; NA when they contradict
# each other. Recodes that only one survey type has (FC: SPA) decide alone.
name_survey_type <- function(recode, says_spa = FALSE, says_bmms = FALSE) {
  cr <- catalogue_recodes()
  avail <- unique(cr$survey_type[cr$recode == recode])
  named <- c(if (says_spa) "SPA", if (says_bmms) "OTH")
  if (length(named) > 1L) {
    return(NA_character_)
  }
  if (length(named) == 1L) {
    return(if (named %in% avail) named else NA_character_)
  }
  if (!length(avail) || "DHS" %in% avail) "DHS" else if (length(avail) == 1L) avail else NA_character_
}

# Survey ids of the rounds of one survey type held in the given years.
rounds_from_years <- function(years, type = "DHS") {
  key <- switch(type, DHS = years, SPA = paste0("spa", years), OTH = paste0("bmms", years))
  unique(stats::na.omit(bdhs_survey_id(key, type = type, error = FALSE)))
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
  # Without the children's ages a midx file may be a KR or a 2017-18 BR.
  recode_certain <- !identical(recode, "KR")
  if (length(ages) == 2L) {
    age <- as_codes(x[[ages[[1]]]]) - as_codes(x[[ages[[2]]]])
    if (any(!is.na(age))) {
      recode <- if (max(age, na.rm = TRUE) <= 60) "KR" else "BR"
      recode_certain <- TRUE
    }
  }
  out <- list(
    names = names, n_cols = length(names), n_rows = nrow(x),
    survey_id = NA_character_, recode = recode, recode_certain = recode_certain,
    round_from_years = FALSE, country = character(),
    derived = FALSE, notes = character()
  )
  v000 <- if (!is.null(c000)) unique(trimws(as.character(zap_all(x[[c000]])))) else character()
  v000 <- toupper(v000[!is.na(v000) & nzchar(v000)])
  out$country <- unique(substr(v000, 1, 2))
  if (length(v000) && !all(out$country == "BD")) {
    out$notes <- c(out$notes, paste0("not a Bangladesh file (country code ", paste(out$country, collapse = ", "), ")"))
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
    out$round_from_years <- TRUE
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

# Recodes whose variables recode_from_names() recognises: only for these can
# the content confirm or contradict the recode given by a file name.
.content_recodes <- c("IR", "MR", "KR", "BR", "HR", "PR", "CR", "HW", "WI")

# Identifier variables (lower case) that every genuine file of a recode has.
# Files whose name is not a standard DHS name are imported only when their
# content has these and a Bangladesh country code (v000, hv000 or mv000);
# other recodes must keep their DHS name.
.recode_id_vars <- list(
  IR = c("caseid", "v001", "v002", "v003"),
  KR = c("caseid", "midx", "v001", "v002"),
  BR = c("caseid", "bidx", "v001", "v002"),
  MR = c("mcaseid", "mv001", "mv002"),
  HR = c("hhid", "hv001", "hv002"),
  PR = c("hhid", "hvidx", "hv001", "hv002"),
  CR = c("v001", "v002", "mv001")
)

# Formats whose content can be inspected.
content_formats <- function() {
  c("dta", "sav", "sas7bdat", if (rlang::is_installed("arrow")) "parquet")
}

# content: "auto" reads the content unless a standard DHS name identifies the
# file (and its variable names do not contradict it); "always" always reads it
# (needed by bdhs_import() for row counts); "never" uses the name only.
#
# Files are refused (survey_id and recode NA, `refused` set) when
# * the name and the content disagree on the round, the recode or the country
#   (refused = "conflict"), or
# * the name is not a standard DHS name and the content cannot confirm it
#   (refused = "unconfirmed").
identify_one <- function(path, content = c("auto", "always", "never"), display = path) {
  content <- rlang::arg_match(content)
  format <- file_format(path)
  by_name <- parse_dhs_name(path)
  standard <- by_name$kind == "standard"
  out <- list(
    path = display, file_stem = by_name$file_stem, survey_id = by_name$survey_id,
    recode = by_name$recode, format = format, method = "filename",
    derived = by_name$derived, note = by_name$note, n_rows = NA_integer_, n_cols = NA_integer_,
    refused = NA_character_
  )
  if (isTRUE(by_name$ambiguous)) {
    return(refuse_identification(out, "unconfirmed", NULL))
  }
  if (!format %in% content_formats() || content == "never") {
    if (is.na(out$survey_id)) out$method <- NA_character_
    if (is.na(format)) {
      out$note <- "not a Stata, SPSS or SAS data file"
    } else if (!standard && !is.na(out$survey_id) && !out$derived) {
      out$note <- join_notes(out$note, "identified from the name only; the content is checked when the file is imported")
    }
    return(out)
  }
  read_content <- content == "always" || !standard || header_contradicts(path, format, out$recode)
  if (!read_content) {
    return(out)
  }
  info <- tryCatch(inspect_content(path, format), error = function(e) e)
  if (inherits(info, "error")) {
    note <- paste0("could not read the file: ", conditionMessage(info))
    if (!standard && !is.na(out$survey_id) && !out$derived) {
      # The name alone is not enough for a renamed file.
      return(refuse_identification(out, "unconfirmed", note))
    }
    out$note <- join_notes(out$note, note)
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
  } else {
    clash <- name_content_clash(out$survey_id, out$recode, info)
    if (length(clash)) {
      shown <- paste(stats::na.omit(c(out$survey_id, out$recode)), collapse = " ")
      notes <- c(notes, paste0("name and content disagree: the name says ", shown, " but ", paste(clash, collapse = " and ")))
      return(refuse_identification(out, "conflict", notes))
    }
    if (is.na(out$survey_id) || is.na(out$recode)) {
      # Name was not enough: use the content.
      out$method <- "content"
      if (is.na(out$survey_id)) out$survey_id <- info$survey_id
      if (is.na(out$recode)) out$recode <- info$recode
    }
    if (is.na(out$survey_id)) {
      notes <- c(notes, "could not identify the survey round")
    } else if (!standard) {
      # A name that is not a standard DHS name needs positive confirmation.
      why <- if (is.na(out$recode)) "could not identify the recode (file type)" else content_unconfirmed(info, out$recode)
      if (length(why) && by_name$kind == "renamed") {
        stem <- catalogue_stem(out$survey_id, out$recode)
        hint <- if (is.null(.recode_id_vars[[out$recode]]) && !is.na(stem)) {
          paste0("; if it is a genuine DHS file, give it its DHS name (", stem, ".", toupper(tools::file_ext(path)), ")")
        }
        note <- paste0("the name suggests ", out$survey_id, " ", out$recode, " but the content does not confirm it (",
                       paste(why, collapse = "; "), ")", hint)
        return(refuse_identification(out, "unconfirmed", c(notes, note)))
      }
      if (length(why)) {
        if (!is.na(out$recode)) {
          why <- paste0("the variables look like ", out$survey_id, " ", out$recode,
                        " but the content does not confirm it (", paste(why, collapse = "; "), ")")
        }
        notes <- c(notes, why)
        out$survey_id <- NA_character_
        out$recode <- NA_character_
      }
    }
  }
  out$file_stem <- if (!is.na(out$survey_id) && !is.na(out$recode)) {
    catalogue_stem(out$survey_id, out$recode) %|NA|% out$file_stem
  } else if (!out$derived) {
    NA_character_
  } else {
    out$file_stem
  }
  if (is.na(out$survey_id) && !out$derived) {
    out$method <- NA_character_
  }
  out$note <- join_notes(notes)
  out
}

join_notes <- function(...) {
  x <- c(...)
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (length(x)) paste(x, collapse = "; ") else NA_character_
}

# Leave a file unidentified, keeping the reason in its note.
refuse_identification <- function(out, why, notes) {
  out$refused <- why
  out$note <- join_notes(out$note, notes)
  out$survey_id <- NA_character_
  out$recode <- NA_character_
  out$file_stem <- NA_character_
  out$method <- NA_character_
  out
}

# Where the content positively contradicts the name: descriptions of the
# disagreements (character(0) when there are none or the content cannot tell).
name_content_clash <- function(survey_id, recode, info) {
  clash <- character()
  if (!is.na(survey_id) && length(info$country) && !all(info$country == "BD")) {
    clash <- c(clash, paste0("the country code is ", paste(info$country, collapse = ", ")))
  }
  # Interview dates identify DHS rounds only; SPA and BMMS names are not checked.
  name_type <- .rounds$survey_type[match(survey_id, .rounds$survey_id)]
  if (identical(name_type, "DHS") && isTRUE(info$round_from_years) && !is.na(info$survey_id) &&
      info$survey_id != survey_id) {
    clash <- c(clash, paste0("the interview dates are from ", info$survey_id))
  }
  if (!is.na(recode) && recode %in% .content_recodes && !is.na(info$recode) && info$recode != recode &&
      (isTRUE(info$recode_certain) || !all(c(info$recode, recode) %in% c("KR", "BR")))) {
    clash <- c(clash, paste0("the variables are those of a ", info$recode, " file"))
  }
  clash
}

# Why the content does not confirm that a file is a Bangladesh DHS `recode`
# file (character(0) when it does).
content_unconfirmed <- function(info, recode) {
  why <- character()
  if (!length(info$country)) {
    why <- c(why, "no country code in v000, hv000 or mv000")
  } else if (!all(info$country == "BD")) {
    why <- c(why, paste0("country code ", paste(info$country, collapse = ", "), ", not BD"))
  }
  ids <- .recode_id_vars[[recode]]
  if (is.null(ids)) {
    why <- c(why, paste0("bdhs cannot check the variables of ", recode, " files"))
  } else {
    miss <- setdiff(ids, tolower(info$names))
    if (length(miss)) {
      why <- c(why, paste0("no ", paste(miss, collapse = ", ")))
    }
  }
  why
}

# Do the variable names of a file contradict the recode of its (standard)
# name? Reads the header only; KR and BR need the data to be told apart.
header_contradicts <- function(path, format, recode) {
  if (is.na(recode) || !recode %in% .content_recodes) {
    return(FALSE)
  }
  nm <- tryCatch(tolower(read_header_names(path, format)), error = function(e) NULL)
  if (!length(nm)) {
    return(FALSE)
  }
  guess <- recode_from_names(nm)
  !is.na(guess) && guess != recode && !all(c(guess, recode) %in% c("KR", "BR"))
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
