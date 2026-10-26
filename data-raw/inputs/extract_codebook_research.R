# extract_codebook.R
# -----------------------------------------------------------------------------
# Codebook builder for DHS / BDHS Stata (.dta) files.
#
# Produces a METADATA-ONLY codebook for one file:
#   * file-level info: format version, n rows/cols, md5, identified survey,
#     recode type, DHS dataset code, and the evidence used to identify it;
#   * one row per variable: position, name, label, Stata storage type
#     (byte/int/long/float/double/strN read from the .dta header), display
#     format, value-label set name, n non-missing, n distinct, min/max,
#     observed codes and observed-but-unlabelled codes (labelled vars only);
#   * value labels in long form: variable, value, label.
#
# No row-level values are ever stored. Summary statistics are aggregates.
# Designed to become the package's internal codebook builder (bdhs:::).
# Dependencies: haven (required), tibble (optional, only for printing).
# -----------------------------------------------------------------------------

# ---- static reference: BD surveys + dataset codes (source: api.dhsprogram.com,
#      /rest/dhs/surveys and /rest/dhs/datasets?countryIds=BD, fetched 2026-10-26)
.bd_surveys <- data.frame(
  survey_id   = c("BD1994DHS", "BD1997DHS", "BD1999SPA", "BD2000DHS", "BD2001OTH",
                  "BD2004DHS", "BD2007DHS", "BD2011DHS", "BD2014DHS", "BD2014SPA",
                  "BD2017DHS", "BD2017SPA", "BD2022DHS"),
  year_label  = c("1993-94", "1996-97", "1999-00", "1999-00", "2001", "2004", "2007",
                  "2011", "2014", "2014", "2017-18", "2017", "2022"),
  survey_type = c("DHS", "DHS", "SPA", "DHS", "OTH", "DHS", "DHS", "DHS", "DHS",
                  "SPA", "DHS", "SPA", "DHS"),
  fw_start_cmc = c(1127, 1163, 1197, 1199, 1213, 1249, 1287, 1339, 1374, 1374, 1414, 1412, 1472),
  fw_end_cmc   = c(1131, 1167, 1200, 1203, 1218, 1253, 1292, 1344, 1379, 1377, 1419, 1413, 1476),
  stringsAsFactors = FALSE
)

.bd_dataset_codes <- list(
  BD1994DHS = c(BR = "31", CR = "31", HR = "31", IR = "31", KR = "31", MR = "31", PR = "31", SQ = "35", WI = "31"),
  BD1997DHS = c(BR = "3A", CR = "3A", HR = "3A", HW = "3A", IR = "3A", KR = "3A", MR = "3A", PR = "3A", SQ = "3A", WI = "3A"),
  BD1999SPA = c(CO = "4R", FC = "4R", PV = "4R"),
  BD2000DHS = c(BR = "41", CR = "41", HR = "41", HW = "41", IR = "41", KR = "41", MR = "41", PR = "41", WI = "41"),
  BD2001OTH = c(HH = "4A", IQ = "4A", VA = "4A"),
  BD2004DHS = c(BR = "4J", CR = "4J", HR = "4J", HW = "4J", IR = "4J", KR = "4J", MR = "4J", PR = "4J", SQ = "4I", VA = "4I"),
  BD2007DHS = c(BR = "51", CR = "51", HR = "51", IR = "51", KR = "51", MR = "51", PR = "51", SQ = "51"),
  BD2011DHS = c(BR = "61", CR = "61", HR = "61", IR = "61", KR = "61", MR = "61", PR = "61", SQ = "61", VA = "62"),
  BD2014DHS = c(BR = "72", HR = "72", IR = "72", KR = "72", PR = "72", SQ = "71"),
  BD2014SPA = c(FC = "7A", PV = "7A", SL = "7A"),
  BD2017DHS = c(BR = "7R", FW = "7R", HR = "7R", IR = "7R", KR = "7R", PR = "7R", SQ = "7R", VA = "7R"),
  BD2017SPA = c(FC = "7I", PV = "7I", SL = "7I"),
  BD2022DHS = c(BR = "81", FW = "81", GR = "81", HR = "81", IR = "81", KR = "81", NR = "81", PR = "81", VA = "81")
)

# ---- low-level: read the .dta header (version, dims, storage types) ----------
# Supports Stata formats 111-115 (binary header) and 117/118/119 (tagged).
# Returns list(version, byteorder, nvar, nobs, data_label, timestamp,
#              storage = named character vector of storage types).
dta_header_info <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  first <- readBin(con, "raw", n = 11)
  out <- list(version = NA_integer_, byteorder = NA_character_, nvar = NA_integer_,
              nobs = NA_real_, data_label = NA_character_, timestamp = NA_character_,
              storage = character())
  if (length(first) < 11) return(out)

  if (identical(first[1:11], charToRaw("<stata_dta>"))) {
    # ---- formats 117 / 118 / 119 (tagged) ----
    seek(con, 0)
    hdr <- readBin(con, "raw", n = 4096)
    txt <- rawToChar(hdr[hdr != as.raw(0)])
    rel <- regmatches(txt, regexpr("<release>[0-9]+</release>", txt))
    ver <- as.integer(gsub("\\D", "", rel))
    bo  <- regmatches(txt, regexpr("<byteorder>[A-Z]+</byteorder>", txt))
    endian <- if (grepl("LSF", bo)) "little" else "big"
    find_tag <- function(raw, tag) {
      pat <- charToRaw(tag)
      n <- length(raw) - length(pat) + 1
      for (i in seq_len(n)) if (identical(raw[i:(i + length(pat) - 1)], pat)) return(i)
      NA_integer_
    }
    k_pos <- find_tag(hdr, "<K>")
    seek(con, k_pos - 1 + 3)
    nvar <- if (ver >= 119) readBin(con, "integer", size = 4, endian = endian)
            else readBin(con, "integer", size = 2, signed = FALSE, endian = endian)
    n_pos <- find_tag(hdr, "<N>")
    seek(con, n_pos - 1 + 3)
    nobs <- if (ver >= 118) {
      lo <- readBin(con, "integer", size = 4, endian = endian)
      hi <- readBin(con, "integer", size = 4, endian = endian)
      as.numeric(lo %% 2^32) + as.numeric(hi) * 2^32
    } else readBin(con, "integer", size = 4, endian = endian)
    # map section gives offset of <variable_types>
    m_pos <- find_tag(hdr, "<map>")
    seek(con, m_pos - 1 + 5)
    offs <- vapply(1:14, function(i) {
      lo <- readBin(con, "integer", size = 4, endian = endian)
      hi <- readBin(con, "integer", size = 4, endian = endian)
      as.numeric(lo %% 2^32) + as.numeric(hi) * 2^32
    }, numeric(1))
    seek(con, offs[3] + nchar("<variable_types>"))
    codes <- readBin(con, "integer", size = 2, signed = FALSE, n = nvar, endian = endian)
    storage <- ifelse(codes == 65530, "byte",
               ifelse(codes == 65529, "int",
               ifelse(codes == 65528, "long",
               ifelse(codes == 65527, "float",
               ifelse(codes == 65526, "double",
               ifelse(codes == 32768, "strL",
               ifelse(codes >= 1 & codes <= 2045, paste0("str", codes), NA_character_)))))))
    seek(con, offs[4] + nchar("<varnames>"))
    nlen <- if (ver >= 118) 129 else 33
    nms <- vapply(seq_len(nvar), function(i) {
      r <- readBin(con, "raw", n = nlen); rawToChar(r[seq_len(max(0, which(r == as.raw(0))[1] - 1))])
    }, character(1))
    names(storage) <- nms
    out[c("version", "byteorder", "nvar", "nobs", "storage")] <-
      list(ver, endian, nvar, nobs, storage)
    return(out)
  }

  # ---- formats 111 / 112 / 113 / 114 / 115 (binary header; 111 = Stata 7SE,
  #      same header layout and 251-255 numeric type codes as 113) ----
  ver <- as.integer(first[1])
  if (!ver %in% 111:115) return(out)
  endian <- if (as.integer(first[2]) == 2L) "little" else "big"
  seek(con, 4)
  nvar <- readBin(con, "integer", size = 2, signed = FALSE, endian = endian)
  nobs <- readBin(con, "integer", size = 4, endian = endian)
  lab  <- readBin(con, "raw", n = 81)
  ts   <- readBin(con, "raw", n = 18)
  typ  <- as.integer(readBin(con, "raw", n = nvar))
  nmr  <- readBin(con, "raw", n = 33 * nvar)
  nms  <- vapply(seq_len(nvar), function(i) {
    r <- nmr[((i - 1) * 33 + 1):(i * 33)]
    z <- which(r == as.raw(0))[1]
    rawToChar(r[seq_len(if (is.na(z)) 33 else z - 1)])
  }, character(1))
  storage <- ifelse(typ == 251, "byte", ifelse(typ == 252, "int", ifelse(typ == 253, "long",
             ifelse(typ == 254, "float", ifelse(typ == 255, "double",
             ifelse(typ >= 1 & typ <= 244, paste0("str", typ), NA_character_))))))
  names(storage) <- nms
  clean <- function(r) { r <- r[r != as.raw(0)]; if (length(r)) rawToChar(r) else "" }
  out[c("version", "byteorder", "nvar", "nobs", "data_label", "timestamp", "storage")] <-
    list(ver, endian, nvar, nobs, clean(lab), clean(ts), storage)
  out
}

# ---- recode detection from variable names (+ optional content probe) --------
# Returns list(recode, evidence). Structural rules (DHS Recode Manual conventions):
#   IR  caseid + v000, no unsuffixed bidx/midx          (woman-level)
#   KR  caseid + v000 + bidx + b3, all births < 60 months before interview,
#       or hidx/hidxa present                            (child-level, under-5)
#   BR  caseid + v000 + bidx + b3, some births >= 60 months (full birth history)
#   GR  caseid + v000 + pidx + p-vars, no m-vars          (pregnancy recode, DHS-8)
#   NR  caseid + v000 + pidx + m-vars                     (pregnancy & PNC, DHS-8)
#   MR  mcaseid / mv000 without v000                      (man-level)
#   CR  mv000 and v000                                     (couple-level)
#   PR  hhid + hv000 + hvidx                               (household member)
#   HR  hhid + hv000, no hvidx                             (household)
#   WI  whhid + wlthind5                                   (wealth index, early rounds)
#   HW  hwhhid + hwline                                    (WHO z-scores, 1996-2004)
#   FW  fw000 / fw101                                      (fieldworker)
#   SQ  co* community vars (coclust)                       (service availability raw)
#   VA  qn* verbal-autopsy raw vars                        (verbal autopsy)
detect_recode <- function(nm, probe = NULL) {
  has <- function(x) all(x %in% nm)
  any_pref <- function(p) any(startsWith(nm, p))
  ev <- character()
  if (has("whhid") && any(c("wlthind5", "wlthindf") %in% nm))
    return(list(recode = "WI", evidence = "whhid + wlthind5/wlthindf"))
  if (has("hwhhid") || (has("hwline") && !has("hv000")))
    return(list(recode = "HW", evidence = "hwhhid/hwline"))
  if (any(c("fw000", "fw101") %in% nm))
    return(list(recode = "FW", evidence = "fw000/fw101"))
  if (has("mv000") && has("v000"))
    return(list(recode = "CR", evidence = "mv000 + v000"))
  if ((has("mcaseid") || has("mv000")) && !has("v000"))
    return(list(recode = "MR", evidence = "mcaseid/mv000 without v000"))
  if (has("hv000") && has("hhid")) {
    if (has("hvidx")) return(list(recode = "PR", evidence = "hhid + hv000 + hvidx"))
    return(list(recode = "HR", evidence = "hhid + hv000, no hvidx"))
  }
  if (has(c("caseid", "v000"))) {
    if (has("pidx")) {
      if (any_pref("m1") || has("m15")) return(list(recode = "NR", evidence = "caseid + pidx + m-vars"))
      return(list(recode = "GR", evidence = "caseid + pidx, no m-vars"))
    }
    if (has("bidx")) {
      ev <- "caseid + v000 + bidx"
      # KR = births in the last 5 (1993-94: 3) years only, so max(v008-b3) <= 60
      # (60 occurs in 2017-18/2022 where eligibility uses day-level dates, v008a);
      # BR = full birth history, max(v008-b3) is hundreds of months.
      if (!is.null(probe) && !is.null(probe$max_age_months)) {
        if (probe$max_age_months <= 60)
          return(list(recode = "KR", evidence = sprintf("%s; max(v008-b3)=%d <= 60 months", ev, probe$max_age_months)))
        return(list(recode = "BR", evidence = sprintf("%s; max(v008-b3)=%d > 60 months", ev, probe$max_age_months)))
      }
      return(list(recode = "KR/BR", evidence = paste(ev, "(content probe unavailable; hidx/hidxa occur in both KR and BR)")))
    }
    return(list(recode = "IR", evidence = "caseid + v000, no bidx/midx"))
  }
  if (any(c("coclust", "codiv") %in% nm) || sum(startsWith(nm, "co")) > 0.8 * length(nm))
    return(list(recode = "SQ", evidence = "co* community-questionnaire variables"))
  if (any(c("qncluster", "qnline") %in% nm) || sum(startsWith(nm, "q")) > 0.8 * length(nm))
    return(list(recode = "VA", evidence = "qn*/q* verbal-autopsy raw variables (qncluster/qnline)"))
  list(recode = NA_character_, evidence = "no structural match")
}

# ---- survey identification ---------------------------------------------------
# Uses: country/phase (v000 / hv000 / mv000 / fw000 value), interview CMC range
# (v008 / hv008 / mv008) matched to DHS API fieldwork windows, and as a last
# resort a standard DHS filename (CCRRvvFL.DTA).
identify_file <- function(path, nm, probe = list()) {
  rec <- detect_recode(nm, probe)
  base <- toupper(basename(path))
  std <- regmatches(base, regexec("^([A-Z]{2})([A-Z]{2})([0-9A-Z]{2})(FL|SV|SD|DT)?\\.", base))[[1]]
  ev <- c(recode = rec$evidence)
  country <- NA_character_; phase <- NA_character_
  if (!is.null(probe$v000) && !is.na(probe$v000) && nzchar(probe$v000)) {
    country <- substr(probe$v000, 1, 2)
    phase   <- substr(probe$v000, 3, 3)
    ev["v000"] <- sprintf("%s value = '%s'", probe$v000_var, probe$v000)
  }
  survey_id <- NA_character_
  if (!is.null(probe$cmc_med) && is.finite(probe$cmc_med)) {
    cand <- .bd_surveys[.bd_surveys$survey_type != "SPA", ]
    hit <- cand$survey_id[probe$cmc_med >= cand$fw_start_cmc - 1 & probe$cmc_med <= cand$fw_end_cmc + 1]
    if (length(hit) >= 1) {
      survey_id <- hit[1]
      ev["cmc"] <- sprintf("%s range %d-%d (median %d) within fieldwork of %s",
                           probe$cmc_var, probe$cmc_min, probe$cmc_max, probe$cmc_med, survey_id)
    }
  }
  if (is.na(survey_id) && !is.null(probe$year_med) && is.finite(probe$year_med)) {
    yr <- probe$year_med
    hit <- .bd_surveys$survey_id[.bd_surveys$survey_type == "DHS" &
             (yr >= floor(.bd_surveys$fw_start_cmc / 12 + 1900 - 1e-9) &
              yr <= floor((.bd_surveys$fw_end_cmc - 1) / 12 + 1900))]
    if (length(hit)) { survey_id <- hit[1]; ev["year"] <- sprintf("interview year %d -> %s", yr, survey_id) }
  }
  recode <- rec$recode
  if (length(std) >= 4 && std[2] == "BD") {
    ev["filename"] <- sprintf("standard DHS filename %s (recode %s, version %s)", base, std[3], std[4])
    if (is.na(recode) || recode == "KR/BR") recode <- std[3]
    if (is.na(country)) country <- std[2]
    if (is.na(survey_id)) {
      for (sid in names(.bd_dataset_codes)) {
        cc <- .bd_dataset_codes[[sid]]
        if (!is.na(cc[std[3]]) && cc[std[3]] == std[4]) { survey_id <- sid; break }
      }
      if (!is.na(survey_id)) ev["filename_lookup"] <- sprintf("%s%s -> %s via DHS API dataset list", std[3], std[4], survey_id)
    }
  }
  dataset_code <- NA_character_
  if (!is.na(survey_id) && !is.na(recode) && !is.na(country)) {
    v <- .bd_dataset_codes[[survey_id]][recode]
    if (!is.na(v)) dataset_code <- paste0(country, recode, v, "FL")
  }
  if (!is.na(dataset_code) && length(std) >= 4 && paste0(std[2], std[3], std[4], "FL") != dataset_code)
    ev["warning"] <- sprintf("filename suggests %s%s%s but content suggests %s", std[2], std[3], std[4], dataset_code)
  yl <- if (!is.na(survey_id)) .bd_surveys$year_label[.bd_surveys$survey_id == survey_id] else NA_character_
  list(country = country, phase = phase, recode = recode, survey_id = survey_id,
       year_label = yl, dataset_code = dataset_code, evidence = ev)
}

# ---- helpers -----------------------------------------------------------------
.cb_first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (is.character(x)) x <- x[nzchar(trimws(x))]
  if (length(x)) x[1] else NA
}

.cb_summarise_var <- function(x, max_codes) {
  labs <- attr(x, "labels", exact = TRUE)
  is_chr <- is.character(x)
  nn <- if (is_chr) sum(!is.na(x) & nzchar(trimws(x))) else sum(!is.na(x))
  res <- list(n_nonmissing = nn, n_distinct = NA_integer_, min = NA_real_, max = NA_real_,
              max_nchar = NA_integer_, n_tagged_na = NA_integer_,
              codes_observed = list(NULL), codes_unlabelled = list(NULL),
              labels_unused = list(NULL))
  if (is_chr) {
    res$max_nchar <- if (nn) max(nchar(x[!is.na(x)], type = "bytes")) else 0L
    return(res)
  }
  xv <- unclass(x)
  if (requireNamespace("haven", quietly = TRUE)) {
    tg <- haven::na_tag(x)
    res$n_tagged_na <- sum(!is.na(tg))
  }
  xv <- xv[!is.na(xv)]
  if (!length(xv)) {
    if (!is.null(labs)) res$labels_unused <- list(unname(as.numeric(labs)))
    return(res)
  }
  u <- unique(xv)
  res$n_distinct <- length(u)
  res$min <- min(xv); res$max <- max(xv)
  if (!is.null(labs)) {
    lv <- as.numeric(labs)
    if (length(u) <= max_codes) res$codes_observed <- list(sort(u))
    unl <- setdiff(u, lv)
    # store unlabelled codes only when few (categorical); for continuous vars
    # with a handful of special-code labels (e.g. 9996-9999) record count only
    res$codes_unlabelled <- list(if (length(unl) <= max_codes) sort(unl) else NA_real_)
    res$labels_unused <- list(sort(setdiff(lv, u)))
  }
  res
}

# ---- main --------------------------------------------------------------------
#' Extract a metadata-only codebook from a DHS Stata file
#'
#' @param path        path to a .dta file
#' @param stats       compute aggregate stats (n non-missing, distinct, range,
#'                    observed codes); FALSE = labels only (reads 0 rows)
#' @param max_codes   max distinct values for which observed codes are recorded
#' @param chunk_cells approx. cells per column chunk when reading (memory cap)
#' @param encoding    passed to haven::read_dta (NULL = haven default)
#' @return object of class "dhs_codebook": list(file, variables, value_labels)
extract_codebook <- function(path, stats = TRUE, max_codes = 200,
                             chunk_cells = 2.5e7, encoding = NULL, verbose = TRUE) {
  stopifnot(file.exists(path))
  t0 <- Sys.time()
  hdr <- dta_header_info(path)
  meta <- haven::read_dta(path, n_max = 0, encoding = encoding)
  nm <- names(meta)
  nvar <- length(nm)

  # --- variable-level static metadata
  var_label <- vapply(meta, function(x) { l <- attr(x, "label", exact = TRUE); if (is.null(l)) NA_character_ else l }, character(1))
  fmt <- vapply(meta, function(x) { f <- attr(x, "format.stata", exact = TRUE); if (is.null(f)) NA_character_ else f }, character(1))
  r_class <- vapply(meta, function(x) class(x)[1], character(1))
  has_labs <- vapply(meta, function(x) !is.null(attr(x, "labels", exact = TRUE)), logical(1))
  storage <- if (length(hdr$storage)) unname(hdr$storage[nm]) else rep(NA_character_, nvar)

  vl <- lapply(nm[has_labs], function(v) {
    l <- attr(meta[[v]], "labels", exact = TRUE)
    data.frame(variable = v, value = as.numeric(unname(l)), label = names(l),
               is_tagged_na = is.na(unname(l)), stringsAsFactors = FALSE)
  })
  value_labels <- if (length(vl)) do.call(rbind, vl) else
    data.frame(variable = character(), value = numeric(), label = character(), is_tagged_na = logical())
  n_lab <- table(factor(value_labels$variable, levels = nm))

  variables <- data.frame(
    position = seq_len(nvar), name = nm, label = unname(var_label),
    storage_type = storage, format_stata = unname(fmt), r_class = unname(r_class),
    has_value_labels = unname(has_labs), n_value_labels = as.integer(n_lab[nm]),
    stringsAsFactors = FALSE)

  # --- content probe for identification (constants / ranges only)
  probe <- list()
  id_vars <- intersect(c("v000", "hv000", "mv000", "fw000", "v008", "hv008", "mv008",
                         "v007", "hv007", "mv007", "b3", "midx"), nm)
  nobs <- if (is.finite(hdr$nobs)) hdr$nobs else NA_real_

  # --- aggregate stats in column chunks
  st <- NULL
  if (stats) {
    if (is.na(nobs)) nobs <- nrow(haven::read_dta(path, col_select = 1, encoding = encoding))
    per <- max(25L, floor(chunk_cells / max(1, nobs)))
    groups <- split(nm, ceiling(seq_along(nm) / per))
    st_list <- vector("list", nvar); names(st_list) <- nm
    for (g in seq_along(groups)) {
      cols <- groups[[g]]
      if (verbose) message(sprintf("  [%s] chunk %d/%d (%d vars)", basename(path), g, length(groups), length(cols)))
      dat <- haven::read_dta(path, col_select = tidyselect::all_of(cols), encoding = encoding)
      for (v in cols) st_list[[v]] <- .cb_summarise_var(dat[[v]], max_codes)
      # identification probe from this chunk
      for (v in intersect(id_vars, cols)) {
        x <- dat[[v]]
        if (v %in% c("v000", "hv000", "mv000", "fw000") && is.null(probe$v000)) {
          probe$v000 <- as.character(.cb_first_nonmissing(x)); probe$v000_var <- v
        }
        if (v %in% c("v008", "hv008", "mv008") && is.null(probe$cmc_med)) {
          xv <- as.numeric(x); xv <- xv[!is.na(xv)]
          if (length(xv)) { probe$cmc_var <- v; probe$cmc_min <- min(xv); probe$cmc_max <- max(xv)
                            probe$cmc_med <- as.integer(stats::median(xv)); probe$.cmc <- NULL }
        }
        if (v %in% c("v007", "hv007", "mv007") && is.null(probe$year_med)) {
          xv <- as.numeric(x); xv <- xv[!is.na(xv)]
          if (length(xv)) { xv <- ifelse(xv < 100, xv + 1900, xv); probe$year_med <- as.integer(stats::median(xv)) }
        }
      }
      if (all(c("v008", "b3") %in% nm) && is.null(probe$max_age_months) &&
          any(c("v008", "b3") %in% cols)) {
        both <- haven::read_dta(path, col_select = tidyselect::all_of(c("v008", "b3")), encoding = encoding)
        a <- as.numeric(both$v008) - as.numeric(both$b3)
        probe$max_age_months <- as.integer(max(a, na.rm = TRUE))
        rm(both)
      }
      rm(dat); gc(verbose = FALSE)
    }
    st <- data.frame(
      n_nonmissing = vapply(st_list, `[[`, numeric(1), "n_nonmissing"),
      n_distinct   = vapply(st_list, function(s) as.integer(s$n_distinct), integer(1)),
      min          = vapply(st_list, function(s) as.numeric(s$min), numeric(1)),
      max          = vapply(st_list, function(s) as.numeric(s$max), numeric(1)),
      max_nchar    = vapply(st_list, function(s) as.integer(s$max_nchar), integer(1)),
      n_tagged_na  = vapply(st_list, function(s) as.integer(s$n_tagged_na), integer(1)),
      stringsAsFactors = FALSE)
    st$codes_observed   <- lapply(st_list, function(s) s$codes_observed[[1]])
    st$codes_unlabelled <- lapply(st_list, function(s) s$codes_unlabelled[[1]])
    st$labels_unused    <- lapply(st_list, function(s) s$labels_unused[[1]])
    variables <- cbind(variables, st)
    variables$all_missing <- variables$n_nonmissing == 0
    rownames(variables) <- NULL
  } else {
    # minimal probe: read only identification columns
    if (length(id_vars)) {
      dat <- haven::read_dta(path, col_select = tidyselect::all_of(id_vars), encoding = encoding)
      v0 <- intersect(c("v000", "hv000", "mv000", "fw000"), id_vars)[1]
      if (!is.na(v0)) { probe$v000 <- as.character(.cb_first_nonmissing(dat[[v0]])); probe$v000_var <- v0 }
      cv <- intersect(c("v008", "hv008", "mv008"), id_vars)[1]
      if (!is.na(cv)) { xv <- as.numeric(dat[[cv]]); xv <- xv[!is.na(xv)]
        probe$cmc_var <- cv; probe$cmc_min <- min(xv); probe$cmc_max <- max(xv); probe$cmc_med <- as.integer(stats::median(xv)) }
      if (all(c("v008", "b3") %in% names(dat)))
        probe$max_age_months <- as.integer(max(as.numeric(dat$v008) - as.numeric(dat$b3), na.rm = TRUE))
      nobs <- nrow(dat)
      rm(dat)
    }
  }

  id <- identify_file(path, nm, probe)
  file_info <- list(
    file = basename(path), path = normalizePath(path), size_bytes = file.info(path)$size,
    md5 = unname(tools::md5sum(path)), dta_version = hdr$version, n_rows = nobs, n_vars = nvar,
    data_label = hdr$data_label, timestamp = hdr$timestamp,
    country = id$country, phase = id$phase, recode = id$recode, survey_id = id$survey_id,
    year_label = id$year_label, dataset_code = id$dataset_code,
    id_evidence = id$evidence,
    interview_cmc_range = if (!is.null(probe$cmc_min)) c(probe$cmc_min, probe$cmc_max) else NULL,
    max_child_age_months = probe$max_age_months,
    extracted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    extractor = "extract_codebook.R v0.1", seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  structure(list(file = file_info, variables = variables, value_labels = value_labels),
            class = "dhs_codebook")
}

print.dhs_codebook <- function(x, ...) {
  f <- x$file
  cat(sprintf("<dhs_codebook> %s  [%s %s | %s | %s]\n  %s rows x %d vars; %d value-label rows; dta v%s\n",
              f$file, f$survey_id, f$year_label, f$recode, f$dataset_code,
              format(f$n_rows, big.mark = ","), f$n_vars, nrow(x$value_labels), f$dta_version))
  cat("  evidence:\n"); for (e in names(f$id_evidence)) cat(sprintf("    - %s: %s\n", e, f$id_evidence[[e]]))
  invisible(x)
}

#' Save a codebook as RDS (named <dataset_code or file>__<file>.rds)
save_codebook <- function(cb, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stem <- if (!is.na(cb$file$dataset_code)) cb$file$dataset_code else "UNKNOWN"
  fn <- file.path(dir, sprintf("%s__%s.rds", stem, tools::file_path_sans_ext(cb$file$file)))
  saveRDS(cb, fn, compress = "xz")
  fn
}

#' Stack codebooks into one long variable table (for cross-round comparison)
stack_codebooks <- function(cbs) {
  do.call(rbind, lapply(cbs, function(cb) {
    v <- cb$variables[, c("position", "name", "label", "storage_type", "has_value_labels",
                          "n_value_labels", intersect(c("n_nonmissing", "n_distinct", "min", "max"), names(cb$variables)))]
    cbind(survey_id = cb$file$survey_id, year_label = cb$file$year_label,
          recode = cb$file$recode, file = cb$file$file, v, stringsAsFactors = FALSE)
  }))
}
