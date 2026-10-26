# =============================================================================
# data-raw/03-build-codebooks.R
#
# Builds the METADATA-ONLY cross-round codebooks
#
#   bdhs_codebook      one row per variable per (survey, recode) file:
#                      survey_id, recode, file_stem, position, variable, label,
#                      type, n_value_labels, n_valid
#   bdhs_value_labels  one row per value label:
#                      survey_id, recode, variable, value, label
#
# from the maintainer's own DHS Stata files. DHS microdata must never be
# redistributed, so only names, labels, value labels and per-variable counts
# of non-missing values are kept: no values, ranges, frequencies or observed
# codes.
#
# Files are found by scanning the directories in the environment variable
# BDHS_LOCAL_DIR (several directories separated by ":"; default
# ~/Documents/MU/Research) for *.dta files whose name starts with "BD".
# Each candidate is identified BY CONTENT, so renamed files such as
# BDHS1993KR.DTA work:
#   * recode type from the variable structure (caseid/v000, bidx, hhid/hvidx,
#     mv000, ...); KR and BR have the same variables in recent rounds, so they
#     are told apart by the age of the oldest child (max(v008 - b3) <= 60
#     months: KR = births in the last 5 years; BR = full birth history);
#   * survey round from the interview dates (v008/hv008/mv008 CMC), which must
#     ALL fall inside one survey's fieldwork window (pooled/derived files are
#     skipped); files without interview dates (FW, SQ, VA) need a standard DHS
#     file name (e.g. BDFW7RFL.DTA);
#   * the DHS file stem (BDKR31FL) from bdhs_datasets.
# Identical files (same md5) are processed once. If two different files claim
# the same (survey, recode), the one with the standard DHS name wins.
#
# Re-running is safe and incremental: per-file metadata is cached by md5 in
# data-raw/cache/codebooks/, and rows already in data/bdhs_codebook.rda for
# (survey, recode) files that are NOT found locally are kept (set
# BDHS_CODEBOOK_KEEP_EXISTING=false to rebuild from local files only).
#
# Usage (from the package root, after 02-build-api-datasets.R):
#   BDHS_LOCAL_DIR="/path/to/dhs/files" Rscript data-raw/03-build-codebooks.R
# Writes data/bdhs_codebook.rda, data/bdhs_value_labels.rda and
# data-raw/codebook-sources.csv (which file each (survey, recode) came from).
# Needs: haven, tibble, usethis.
# =============================================================================

if (!file.exists("DESCRIPTION") || !dir.exists("data-raw")) {
  stop("Run this script from the bdhs package root.", call. = FALSE)
}

local_dirs <- Sys.getenv("BDHS_LOCAL_DIR", "~/Documents/MU/Research")
local_dirs <- path.expand(strsplit(local_dirs, .Platform$path.sep, fixed = TRUE)[[1]])
local_dirs <- local_dirs[dir.exists(local_dirs)]
CACHE <- file.path("data-raw", "cache", "codebooks")
KEEP_EXISTING <- !tolower(Sys.getenv("BDHS_CODEBOOK_KEEP_EXISTING", "true")) %in% c("false", "0", "no")
CACHE_VERSION <- 1L # bump to invalidate the per-file cache
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

msg <- function(...) message(format(Sys.time(), "%H:%M:%S "), sprintf(...))

# Survey catalogue (from step 02) gives fieldwork windows and file stems ------
load(file.path("data", "bdhs_surveys.rda"))
load(file.path("data", "bdhs_datasets.rda"))

cmc <- function(d) (as.integer(format(d, "%Y")) - 1900L) * 12L + as.integer(format(d, "%m"))
fieldwork <- data.frame(
  survey_id = bdhs_surveys$survey_id,
  survey_type = bdhs_surveys$survey_type,
  cmc_start = cmc(bdhs_surveys$fieldwork_start) - 2L,
  cmc_end = cmc(bdhs_surveys$fieldwork_end) + 2L
)
fieldwork <- fieldwork[fieldwork$survey_type != "SPA", ] # SPA files have no v008
survey_order <- bdhs_surveys$survey_id[order(bdhs_surveys$survey_year, bdhs_surveys$survey_type)]

fl_stems <- bdhs_datasets[bdhs_datasets$format_code %in% "FL" &
  bdhs_datasets$dataset_type != "GPS Datasets", ]

# Standard DHS file name: BD + recode + version (+ FL/DT/SV/SD) (+ SP/SR).
parse_std_name <- function(path) {
  b <- toupper(tools::file_path_sans_ext(basename(path)))
  m <- regmatches(b, regexec("^BD([A-Z]{2})([0-9][0-9A-Z])(FL|DT|SV|SD)?(SP|SR)?$", b))[[1]]
  if (!length(m)) {
    return(NULL)
  }
  hit <- fl_stems[fl_stems$file_type_code == m[2] & fl_stems$recode_version == m[3], ]
  list(
    recode = m[2], version = m[3],
    survey_id = if (nrow(hit)) hit$survey_id[1] else NA_character_,
    stem = paste0("BD", m[2], m[3], "FL", m[5])
  )
}

# Recode type from variable names (DHS Recode Manual conventions) -----------
detect_recode <- function(nm) {
  has <- function(...) all(c(...) %in% nm)
  if (has("whhid") && any(c("wlthind5", "wlthindf") %in% nm)) {
    return("WI")
  }
  if (has("hwhhid") || (has("hwline") && !has("hv000"))) {
    return("HW")
  }
  if (any(c("fw000", "fw101") %in% nm)) {
    return("FW")
  }
  if (has("mv000", "v000")) {
    return("CR")
  }
  if ((has("mcaseid") || has("mv000")) && !has("v000")) {
    return("MR")
  }
  if (has("hhid", "hv000")) {
    return(if (has("hvidx")) "PR" else "HR")
  }
  if (has("caseid", "v000")) {
    if (has("pidx")) {
      return(if (any(startsWith(nm, "m1")) || has("m15")) "NR" else "GR")
    }
    if (has("bidx")) {
      return("KR/BR") # resolved from content below
    }
    return("IR")
  }
  if (any(c("coclust", "codiv") %in% nm) || mean(startsWith(nm, "co")) > 0.8) {
    return("SQ")
  }
  if (any(c("qncluster", "qnline") %in% nm) || mean(startsWith(nm, "q")) > 0.8) {
    return("VA")
  }
  NA_character_
}

first_value <- function(x) {
  x <- as.character(haven::zap_labels(x))
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x)) x[1] else NA_character_
}

# Keep labels ASCII (avoids non-portable strings in the package data): typographic
# quotes and the mojibake of a right single quote become plain quotes.
ascii_label <- function(x) {
  x <- gsub("[‘’´]", "'", x)
  x <- gsub("[“”]", "\"", x)
  x <- gsub("â\\?\\?", "'", x)
  x <- gsub("â\u0080\u0099", "'", x)
  bad <- !is.na(x) & grepl("[^ -~]", x)
  if (any(bad)) x[bad] <- iconv(x[bad], "UTF-8", "ASCII//TRANSLIT", sub = "?")
  x
}

# Identify one file from its header plus a few identification columns -------
identify_file <- function(path, meta) {
  nm_orig <- names(meta)
  nm <- tolower(nm_orig)
  col <- function(v) nm_orig[match(v, nm)]
  std <- parse_std_name(path)
  recode <- detect_recode(nm)
  evidence <- character()
  out <- list(
    survey_id = NA_character_, recode = NA_character_, file_stem = NA_character_,
    n_rows = NA_real_, evidence = NA_character_, skip = NA_character_
  )

  id_vars <- intersect(c("v000", "hv000", "mv000", "v008", "hv008", "mv008", "b3"), nm)
  probe <- if (length(id_vars)) haven::read_dta(path, col_select = tidyselect::all_of(col(id_vars))) else NULL
  if (!is.null(probe)) {
    names(probe) <- tolower(names(probe))
    out$n_rows <- nrow(probe)
    cc <- intersect(c("v000", "hv000", "mv000"), names(probe))
    if (length(cc)) {
      v000 <- unique(trimws(as.character(haven::zap_labels(probe[[cc[1]]]))))
      v000 <- v000[!is.na(v000) & nzchar(v000)]
      if (length(v000) && !all(substr(toupper(v000), 1, 2) == "BD")) {
        out$skip <- sprintf("not a Bangladesh file (%s = %s)", cc[1], paste(v000, collapse = ","))
        return(out)
      }
    }
    cv <- intersect(c("v008", "hv008", "mv008"), names(probe))
    if (length(cv)) {
      x <- as.numeric(haven::zap_labels(probe[[cv[1]]]))
      x <- x[!is.na(x)]
      if (length(x)) {
        rng <- range(x)
        hit <- fieldwork$survey_id[rng[1] >= fieldwork$cmc_start & rng[2] <= fieldwork$cmc_end]
        if (length(hit) != 1L) {
          out$skip <- sprintf("interview dates (%s) do not fall inside one survey's fieldwork", cv[1])
          return(out)
        }
        out$survey_id <- hit
        evidence <- c(evidence, sprintf("%s within fieldwork of %s", cv[1], hit))
      }
    }
    if (identical(recode, "KR/BR") && all(c("v008", "b3") %in% names(probe))) {
      age <- as.numeric(haven::zap_labels(probe$v008)) - as.numeric(haven::zap_labels(probe$b3))
      recode <- if (max(age, na.rm = TRUE) <= 60) "KR" else "BR"
      evidence <- c(evidence, sprintf("bidx + oldest child %s 60 months -> %s",
        if (recode == "KR") "<=" else ">", recode))
    }
  }
  if (!is.null(std)) {
    if (is.na(out$survey_id) && !is.na(std$survey_id)) {
      out$survey_id <- std$survey_id
      evidence <- c(evidence, sprintf("standard file name (%s%s)", std$recode, std$version))
    }
    if (is.na(recode) || recode == "KR/BR") {
      recode <- std$recode
    }
    if (!is.na(std$survey_id) &&
      (!identical(std$survey_id, out$survey_id) || !identical(std$recode, recode))) {
      warning(sprintf(
        "%s: file name says %s %s but content says %s %s; using content.",
        basename(path), std$survey_id, std$recode, out$survey_id, recode
      ), call. = FALSE)
    }
  }
  if (is.na(out$survey_id) || is.na(recode) || recode == "KR/BR") {
    out$skip <- "could not identify survey round and recode"
    return(out)
  }
  out$recode <- recode
  stem <- fl_stems$file_stem[fl_stems$survey_id == out$survey_id & fl_stems$file_type_code == recode]
  if (!is.null(std) && std$stem %in% stem) stem <- std$stem
  out$file_stem <- if (length(stem)) stem[1] else NA_character_
  if (is.na(out$file_stem)) {
    out$skip <- sprintf("no %s file listed for %s in bdhs_datasets", recode, out$survey_id)
  }
  out$evidence <- paste(evidence, collapse = "; ")
  out
}

# Metadata of one file: variables + value labels (+ non-missing counts) ------
extract_file <- function(path, chunk_cells = 2.5e7) {
  meta <- haven::read_dta(path, n_max = 0)
  id <- identify_file(path, meta)
  res <- list(cache_version = CACHE_VERSION, file = basename(path), md5 = NA_character_, id = id)
  if (!is.na(id$skip)) {
    return(res)
  }
  nm_orig <- names(meta)
  label <- vapply(meta, function(x) {
    l <- attr(x, "label", exact = TRUE)
    if (is.null(l) || !length(l)) NA_character_ else as.character(l)
  }, character(1))
  labs <- lapply(meta, function(x) attr(x, "labels", exact = TRUE))
  type <- vapply(meta, function(x) class(x)[1], character(1))

  # non-missing counts, read in column chunks to cap memory
  n_rows <- if (is.na(id$n_rows)) nrow(haven::read_dta(path, col_select = 1)) else id$n_rows
  per <- max(25L, floor(chunk_cells / max(1, n_rows)))
  groups <- split(nm_orig, ceiling(seq_along(nm_orig) / per))
  n_valid <- integer(length(nm_orig))
  names(n_valid) <- nm_orig
  for (g in groups) {
    dat <- haven::read_dta(path, col_select = tidyselect::all_of(g))
    n_valid[g] <- vapply(dat, function(x) {
      if (is.character(x)) sum(!is.na(x) & nzchar(trimws(x))) else sum(!is.na(x))
    }, integer(1))
    rm(dat)
    invisible(gc(verbose = FALSE))
  }

  variables <- data.frame(
    position = seq_along(nm_orig),
    variable = tolower(nm_orig),
    label = ascii_label(unname(label)),
    type = unname(type),
    n_value_labels = vapply(labs, function(l) if (is.null(l)) 0L else length(l), integer(1), USE.NAMES = FALSE),
    n_valid = unname(n_valid),
    stringsAsFactors = FALSE
  )
  has_labs <- !vapply(labs, is.null, logical(1))
  vl <- lapply(nm_orig[has_labs], function(v) {
    l <- labs[[v]]
    data.frame(
      variable = tolower(v), value = as.numeric(unname(l)),
      label = ascii_label(names(l)), stringsAsFactors = FALSE
    )
  })
  value_labels <- if (length(vl)) do.call(rbind, vl) else
    data.frame(variable = character(), value = numeric(), label = character())
  res$id$n_rows <- n_rows
  res$variables <- variables
  res$value_labels <- value_labels
  res
}

# 1. Scan --------------------------------------------------------------------
if (!length(local_dirs)) {
  stop("None of the BDHS_LOCAL_DIR directories exist.", call. = FALSE)
}
files <- list.files(local_dirs, pattern = "\\.dta$", ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
files <- files[grepl("^BD", basename(files), ignore.case = TRUE)]
msg("%d candidate .dta files under %s", length(files), paste(local_dirs, collapse = ", "))

md5 <- unname(tools::md5sum(files))
std_name <- !vapply(files, function(f) is.null(parse_std_name(f)), logical(1))
ord <- order(md5, !std_name, files) # identical files: keep the standard name
files <- files[ord]
md5 <- md5[ord]
keep <- !duplicated(md5)
if (any(!keep)) msg("skipping %d identical copies (same md5)", sum(!keep))
files <- files[keep]
md5 <- md5[keep]

# 2. Extract (cached by md5) -------------------------------------------------
results <- lapply(seq_along(files), function(i) {
  cf <- file.path(CACHE, paste0(md5[i], ".rds"))
  if (file.exists(cf)) {
    r <- readRDS(cf)
    if (identical(r$cache_version, CACHE_VERSION)) {
      return(r)
    }
  }
  msg("reading %s", basename(files[i]))
  r <- extract_file(files[i])
  r$md5 <- md5[i]
  r$std_name <- !is.null(parse_std_name(files[i]))
  saveRDS(r, cf, compress = "xz")
  r
})

skipped <- vapply(results, function(r) !is.na(r$id$skip), logical(1))
for (r in results[skipped]) msg("skipped %s: %s", r$file, r$id$skip)
results <- results[!skipped]

# 3. One file per (survey, recode) -------------------------------------------
key <- vapply(results, function(r) paste(r$id$survey_id, r$id$recode), "")
pref <- order(key, !vapply(results, function(r) isTRUE(r$std_name), TRUE),
  -vapply(results, function(r) nrow(r$variables), 1L))
results <- results[pref]
key <- key[pref]
if (anyDuplicated(key)) {
  for (k in unique(key[duplicated(key)])) {
    msg("several different files for %s: using %s", k, results[[match(k, key)]]$file)
  }
  results <- results[!duplicated(key)]
}

sources <- do.call(rbind, lapply(results, function(r) {
  data.frame(
    survey_id = r$id$survey_id, recode = r$id$recode, file_stem = r$id$file_stem,
    source_file = r$file, md5 = r$md5, n_rows = r$id$n_rows, n_vars = nrow(r$variables),
    identified_by = r$id$evidence, stringsAsFactors = FALSE
  )
}))

codebook <- do.call(rbind, lapply(results, function(r) {
  cbind(
    survey_id = r$id$survey_id, recode = r$id$recode, file_stem = r$id$file_stem,
    r$variables, stringsAsFactors = FALSE
  )
}))
value_labels <- do.call(rbind, lapply(results, function(r) {
  if (!nrow(r$value_labels)) {
    return(NULL)
  }
  cbind(survey_id = r$id$survey_id, recode = r$id$recode, r$value_labels, stringsAsFactors = FALSE)
}))

# 4. Keep (survey, recode) files that are bundled but not available locally --
cb_cols <- c(
  "survey_id", "recode", "file_stem", "position", "variable", "label", "type",
  "n_value_labels", "n_valid"
)
vl_cols <- c("survey_id", "recode", "variable", "value", "label")
old_cb <- file.path("data", "bdhs_codebook.rda")
if (KEEP_EXISTING && file.exists(old_cb)) {
  e <- new.env()
  load(old_cb, envir = e)
  load(file.path("data", "bdhs_value_labels.rda"), envir = e)
  if (all(cb_cols %in% names(e$bdhs_codebook))) {
    old_keys <- unique(paste(e$bdhs_codebook$survey_id, e$bdhs_codebook$recode))
    missing <- setdiff(old_keys, unique(paste(codebook$survey_id, codebook$recode)))
    if (length(missing)) {
      msg("keeping bundled codebook rows for files not found locally: %s", paste(missing, collapse = ", "))
      codebook <- rbind(codebook, as.data.frame(e$bdhs_codebook[
        paste(e$bdhs_codebook$survey_id, e$bdhs_codebook$recode) %in% missing, cb_cols
      ]))
      value_labels <- rbind(value_labels, as.data.frame(e$bdhs_value_labels[
        paste(e$bdhs_value_labels$survey_id, e$bdhs_value_labels$recode) %in% missing, vl_cols
      ]))
      old_src <- file.path("data-raw", "codebook-sources.csv")
      if (file.exists(old_src)) {
        os <- utils::read.csv(old_src, stringsAsFactors = FALSE)
        sources <- rbind(sources, os[paste(os$survey_id, os$recode) %in% missing, names(sources)])
      }
    }
  }
}

# 5. Tidy, check, write ------------------------------------------------------
sid_rank <- match(codebook$survey_id, survey_order)
codebook <- codebook[order(sid_rank, codebook$recode, codebook$position), cb_cols]
vl_rank <- match(value_labels$survey_id, survey_order)
pos <- codebook$position[match(
  paste(value_labels$survey_id, value_labels$recode, value_labels$variable),
  paste(codebook$survey_id, codebook$recode, codebook$variable)
)]
value_labels <- value_labels[order(vl_rank, value_labels$recode, pos, value_labels$value), vl_cols]
sources <- sources[order(match(sources$survey_id, survey_order), sources$recode), ]

codebook$position <- as.integer(codebook$position)
codebook$n_value_labels <- as.integer(codebook$n_value_labels)
codebook$n_valid <- as.integer(codebook$n_valid)
stopifnot(
  !anyDuplicated(codebook[c("survey_id", "recode", "variable")]),
  !anyDuplicated(value_labels[c("survey_id", "recode", "variable", "value")]),
  !anyNA(codebook$file_stem),
  all(codebook$survey_id %in% bdhs_surveys$survey_id),
  !any(grepl("[^ -~]", c(codebook$label, value_labels$label)))
)

bdhs_codebook <- tibble::as_tibble(codebook)
bdhs_value_labels <- tibble::as_tibble(value_labels)
usethis::use_data(bdhs_codebook, bdhs_value_labels, overwrite = TRUE, compress = "xz")
utils::write.csv(sources, file.path("data-raw", "codebook-sources.csv"), row.names = FALSE)
print(sources[, c("survey_id", "recode", "file_stem", "source_file", "n_vars")], row.names = FALSE)
msg("bdhs_codebook: %d rows; bdhs_value_labels: %d rows", nrow(bdhs_codebook), nrow(bdhs_value_labels))
