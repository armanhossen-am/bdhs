# =============================================================================
# data-raw/01-fetch-dhs-api.R
#
# Step 1 of the bundled-data pipeline: download every Bangladesh record from
# the DHS Program Indicator Data API (https://api.dhsprogram.com) into a local
# cache. Step 2 (02-build-api-datasets.R) turns the cache into data/*.rda.
#
# Pipeline (run each script from the package root, in this order):
#   01-fetch-dhs-api.R, 02-build-api-datasets.R  -> surveys, datasets,
#                                                   indicators, catalogue
#   03-build-codebooks.R (needs local DHS files)  -> codebook, value labels
#   04-build-crosswalk.R                          -> inst/extdata/crosswalk.rds
#   boundaries/01-download-sdr.R, 02-build-boundaries.R, 03-validate-boundaries.R
#                                                 -> inst/extdata/bdhs_boundaries.rds
#   05-build-regions.R                            -> bdhs_regions
#   06-build-pool-population.R                    -> bdhs_pool_population
#
# Everything downloaded here is public aggregate data or metadata (indicator
# values, survey/dataset catalogues). No microdata is involved.
#
#   info                  API version + official citation string
#   surveys               BD, surveyStatus = all (DHS + SPA + OTH/BMMS)
#   surveycharacteristics global list + per-survey characteristic map
#   tags                  global tag list
#   indicators            indicators with BD data (+ the global list, see below)
#   data                  every BD indicator value, breakdown = all, per survey
#   data membership       DataIds returned for national/subnational/background
#   datasets              BD dataset (file) catalogue
#   publications          BD survey reports
#
# Usage (from the package root):
#   Rscript data-raw/01-fetch-dhs-api.R            # cache: data-raw/cache/dhs-api
#   Rscript data-raw/01-fetch-dhs-api.R <out_dir>
# Env:  DHS_API_KEY (optional; not needed -- the unauthenticated page size cap
#       was 5000 rows on 2026-10-26).
# Writes data-raw/dhs-api-manifest.json (committed): retrieval date, API
# version, citation and row counts of this pull.
# Needs: httr2, jsonlite, data.table. Runtime ~2-3 min, ~280 MB transferred.
# =============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(data.table)
})

if (!file.exists("DESCRIPTION") || !dir.exists("data-raw")) {
  stop("Run this script from the bdhs package root.", call. = FALSE)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

args    <- commandArgs(trailingOnly = TRUE)
OUT     <- args[1] %||% file.path("data-raw", "cache", "dhs-api")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

BASE    <- "https://api.dhsprogram.com/rest/dhs"
COUNTRY <- "BD"
PERPAGE <- 5000L                     # server-side cap (larger values are clamped)
API_KEY <- Sys.getenv("DHS_API_KEY", "")
UA      <- "bdhs R package data-raw (https://github.com/armanhossen-am/bdhs)"
LOG     <- new.env()                 # request log for the manifest
LOG$calls <- list()

msg <- function(...) message(format(Sys.time(), "%H:%M:%S "), sprintf(...))
# dataupdates dates look like "September, 18 2024 05:00:07": English month
# names, so parse dates in the C locale regardless of the user's locale.
invisible(Sys.setlocale("LC_TIME", "C"))

# -----------------------------------------------------------------------------
# HTTP layer
# -----------------------------------------------------------------------------
dhs_request <- function(endpoint, query = list(), timeout = 300) {
  q <- utils::modifyList(list(f = "json"), query)
  if (nzchar(API_KEY)) q$apiKey <- API_KEY
  request(BASE) |>
    req_url_path_append(endpoint) |>
    req_url_query(!!!q) |>
    req_user_agent(UA) |>
    req_timeout(timeout) |>
    req_throttle(capacity = 4, fill_time_s = 1) |> # <= 4 requests/s
    req_retry(
      max_tries = 6,
      retry_on_failure = TRUE,
      is_transient = function(resp) resp_status(resp) %in% c(408, 429, 500, 502, 503, 504),
      backoff = function(i) min(60, 2^i)
    ) |>
    req_error(is_error = function(resp) resp_status(resp) >= 400)
}

# One page -> list(meta, data.table of character columns)
dhs_page <- function(endpoint, query, page, perpage, timeout = 300) {
  query$page <- page
  query$perpage <- perpage
  req <- dhs_request(endpoint, query, timeout)
  t0 <- Sys.time()
  resp <- req_perform(req)
  txt <- resp_body_string(resp, encoding = "UTF-8")
  js <- fromJSON(txt, simplifyVector = TRUE)
  LOG$calls[[length(LOG$calls) + 1]] <- list(
    url = req$url, status = resp_status(resp),
    secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2),
    bytes = nchar(txt, type = "bytes")
  )
  dat <- js$Data
  if (is.null(dat) || length(dat) == 0) {
    dat <- data.table()
  } else {
    dat <- as.data.table(dat)
    # The API returns "" for missing numbers: keep everything character here
    # and type columns explicitly later (avoids rbind type clashes).
    for (j in names(dat)) {
      v <- dat[[j]]
      if (is.list(v)) v <- vapply(v, function(x) paste(x, collapse = ","), "")
      set(dat, j = j, value = as.character(v))
    }
  }
  list(
    meta = list(
      RecordCount = as.integer(js$RecordCount %||% js$Recordcount %||% NA),
      TotalPages = as.integer(js$TotalPages %||% NA)
    ),
    data = dat
  )
}

# All pages of a query, with a completeness check against RecordCount.
dhs_get <- function(endpoint, query = list(), perpage = PERPAGE, timeout = 300) {
  first <- dhs_page(endpoint, query, 1L, perpage, timeout)
  pages <- list(first$data)
  tp <- first$meta$TotalPages %||% 1L
  if (!is.na(tp) && tp > 1) {
    for (p in 2:tp) pages[[p]] <- dhs_page(endpoint, query, p, perpage, timeout)$data
  }
  out <- rbindlist(pages, fill = TRUE)
  rc <- first$meta$RecordCount
  if (!is.na(rc) && nrow(out) != rc) {
    stop(sprintf(
      "%s %s: got %d rows, RecordCount says %d", endpoint,
      paste(names(query), query, sep = "=", collapse = "&"), nrow(out), rc
    ), call. = FALSE)
  }
  out
}

# -----------------------------------------------------------------------------
# Typing helpers ("" -> NA, explicit classes)
# -----------------------------------------------------------------------------
na_blank <- function(x) {
  x[!is.na(x) & trimws(x) == ""] <- NA_character_
  x
}
as_int <- function(x) suppressWarnings(as.integer(na_blank(x)))
as_num <- function(x) suppressWarnings(as.numeric(na_blank(x)))
as_date <- function(x) {
  x <- na_blank(x)
  d <- as.Date(substr(x, 1, 10), format = "%Y-%m-%d")
  bad <- is.na(d) & !is.na(x) # some endpoints use "Month, dd yyyy"
  if (any(bad)) {
    d[bad] <- as.Date(x[bad],
      tryFormats = c("%B, %d %Y %H:%M:%S", "%B, %d %Y", "%m/%d/%Y"),
      optional = TRUE
    )
  }
  d
}
type_cols <- function(dt, int = character(), num = character(), date = character()) {
  for (j in intersect(names(dt), int)) set(dt, j = j, value = as_int(dt[[j]]))
  for (j in intersect(names(dt), num)) set(dt, j = j, value = as_num(dt[[j]]))
  for (j in intersect(names(dt), date)) set(dt, j = j, value = as_date(dt[[j]]))
  chr <- names(dt)[vapply(dt, is.character, TRUE)]
  for (j in chr) set(dt, j = j, value = na_blank(dt[[j]]))
  dt
}

save_rds <- function(x, name) {
  f <- file.path(OUT, paste0(name, ".rds"))
  saveRDS(x, f, compress = "xz")
  msg(
    "saved %-34s %9s rows  %9.1f KB", basename(f),
    format(NROW(x), big.mark = ","), file.size(f) / 1024
  )
  invisible(f)
}

retrieved_at <- Sys.time()

# -----------------------------------------------------------------------------
# 1. API info
# -----------------------------------------------------------------------------
msg("info")
api_version <- dhs_get("info", list(infoType = "version"))$Value
api_citation <- dhs_get("info", list(infoType = "citation"))$Value

# -----------------------------------------------------------------------------
# 2. reference lists
# -----------------------------------------------------------------------------
msg("tags / survey characteristics")
tags <- type_cols(dhs_get("tags"), int = c("TagID", "TagType", "TagOrder"))
save_rds(tags, "api_tags")

survey_characteristics <- type_cols(dhs_get("surveycharacteristics"),
  int = "SurveyCharacteristicID"
)
save_rds(survey_characteristics, "api_surveycharacteristics")

# -----------------------------------------------------------------------------
# 3. surveys (all statuses, all types: DHS, SPA, OTH = BMMS 2001)
# -----------------------------------------------------------------------------
msg("surveys")
surveys <- dhs_get("surveys", list(countryIds = COUNTRY, surveyStatus = "all"))
surveys <- type_cols(surveys,
  int = c(
    "SurveyYear", "SurveyNum", "IndicatorData", "NumberOfSamplePoints",
    "NumberofHouseholds", "NumberOfWomen", "MinAgeWomen", "MaxAgeWomen",
    "NumberOfMen", "MinAgeMen", "MaxAgeMen", "NumberOfFacilities"
  ),
  date = c("PublicationDate", "ReleaseDate", "FieldworkStart", "FieldworkEnd")
)
setorder(surveys, SurveyYear, SurveyType)
save_rds(surveys, "api_surveys")

# survey -> characteristic map, from the per-survey endpoint (authoritative)
sc_map <- rbindlist(lapply(surveys$SurveyId, function(s) {
  x <- dhs_get("surveycharacteristics", list(surveyIds = s))
  if (!nrow(x)) {
    return(NULL)
  }
  data.table(SurveyId = s, SurveyCharacteristicID = as_int(x$SurveyCharacteristicID))
}))
save_rds(sc_map, "api_survey_characteristic_map")

# -----------------------------------------------------------------------------
# 4. indicator catalogue
# -----------------------------------------------------------------------------
msg("indicators")
ind_int <- c(
  "NumberScale", "IndicatorOrder", "IsQuickStat", "IndicatorTotalId",
  "IndicatorSpecialTypeId"
)
indicators <- dhs_get("indicators", list(countryIds = COUNTRY), perpage = 3000L)
indicators <- type_cols(indicators, int = ind_int)
save_rds(indicators, "api_indicators")

# The countryIds=BD filter MISSES ~31 indicator ids that do have BD data
# (e.g. RH_DURH_W_*, CN_IFPS_C_*, FP_EFPM_W_SMD), so also pull the global
# list (documented cap 3000/page) and complete the catalogue from it in the
# build step, keyed on (IndicatorId, IndicatorOrder).
indicators_global <- dhs_get("indicators", list(), perpage = 3000L)
indicators_global <- type_cols(indicators_global, int = ind_int)
save_rds(indicators_global, "api_indicators_global")

# -----------------------------------------------------------------------------
# 5. indicator data, breakdown = all, one survey at a time (a single
#    countryIds=BD&breakdown=all call takes ~50 s per page server side;
#    per-survey calls return in < 1 s)
# -----------------------------------------------------------------------------
data_surveys <- surveys[IndicatorData == 1L, SurveyId]
msg("data: %d surveys with indicator data", length(data_surveys))
data_all <- rbindlist(lapply(data_surveys, function(s) {
  msg("  data %s breakdown=all", s)
  dhs_get("data", list(surveyIds = s, breakdown = "all"))
}), fill = TRUE)

# SPA / OTH surveys: confirm that there really is no indicator data
no_data_check <- rbindlist(lapply(surveys[IndicatorData != 1L, SurveyId], function(s) {
  x <- dhs_page("data", list(surveyIds = s, breakdown = "all"), 1L, 1L)
  data.table(SurveyId = s, RecordCount = x$meta$RecordCount)
}))

data_all <- type_cols(data_all,
  int = c(
    "DataId", "Precision", "SurveyYear", "IndicatorOrder", "CharacteristicId",
    "CharacteristicOrder", "ByVariableId", "IsTotal", "IsPreferred", "LevelRank"
  ),
  num = c("Value", "DenominatorWeighted", "DenominatorUnweighted", "CILow", "CIHigh")
)
setorder(data_all, SurveyYear, IndicatorOrder, CharacteristicOrder, ByVariableId)
save_rds(data_all, "api_data_all")

# Breakdown membership: which DataIds come back for each named breakdown.
# Used by the build step to check its national/subnational/background rule.
msg("data: breakdown membership")
membership <- rbindlist(lapply(data_surveys, function(s) {
  rbindlist(lapply(c("national", "subnational", "background"), function(b) {
    x <- dhs_get("data", list(
      surveyIds = s, breakdown = b,
      returnFields = "DataId,IndicatorId,CharacteristicId,ByVariableId,RegionId"
    ))
    if (!nrow(x)) {
      return(NULL)
    }
    x[, `:=`(SurveyId = s, breakdown = b)]
  }), fill = TRUE)
}), fill = TRUE)
membership <- type_cols(membership, int = c("DataId", "CharacteristicId", "ByVariableId"))
save_rds(membership, "api_data_breakdown_membership")

# -----------------------------------------------------------------------------
# 6. datasets, publications, data update log
# -----------------------------------------------------------------------------
msg("datasets / publications / data updates")
datasets <- type_cols(dhs_get("datasets", list(countryIds = COUNTRY)),
  int = c("SurveyYear", "SurveyNum", "FileSize"),
  date = "FileDateLastModified"
)
save_rds(datasets, "api_datasets")

publications <- type_cols(dhs_get("publications", list(countryIds = COUNTRY)),
  int = c("PublicationId", "SurveyYear", "PublicationSize")
)
save_rds(publications, "api_publications")

dataupdates <- type_cols(dhs_get("dataupdates"),
  int = c("RowNumber", "SurveyYear"),
  date = "UpdateDate"
)
save_rds(dataupdates[DHS_CountryCode == COUNTRY], "api_dataupdates")

# -----------------------------------------------------------------------------
# 7. manifest
# -----------------------------------------------------------------------------
calls <- rbindlist(LOG$calls)
manifest <- list(
  retrieved_at = format(retrieved_at, "%Y-%m-%dT%H:%M:%S%z"),
  retrieved_date = format(retrieved_at, "%Y-%m-%d"),
  api_base = BASE,
  api_version = api_version,
  api_citation = api_citation,
  user_agent = UA,
  country = COUNTRY,
  perpage = PERPAGE,
  api_key_used = nzchar(API_KEY),
  n_requests = nrow(calls),
  total_mb = round(sum(calls$bytes) / 1e6, 2),
  data_surveys = data_surveys,
  no_data_surveys = no_data_check,
  data_rows_raw = nrow(data_all),
  counts = list(
    indicators = nrow(indicators),
    indicators_global = nrow(indicators_global),
    surveys = nrow(surveys),
    datasets = nrow(datasets),
    publications = nrow(publications),
    tags = nrow(tags),
    surveycharacteristics = nrow(survey_characteristics)
  )
)
write_json(manifest, file.path(OUT, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
write_json(manifest, file.path("data-raw", "dhs-api-manifest.json"),
  auto_unbox = TRUE, pretty = TRUE
)
fwrite(calls, file.path(OUT, "request_log.csv"))
msg("done: %d requests, %.1f MB transferred", nrow(calls), sum(calls$bytes) / 1e6)
