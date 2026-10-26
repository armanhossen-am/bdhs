# =============================================================================
# data-raw/02-build-api-datasets.R
#
# Step 2 of the bundled-data pipeline: turn the DHS API cache written by
# 01-fetch-dhs-api.R into the bundled datasets
#
#   bdhs_indicators         official indicator values (long; one row per
#                           survey x indicator x characteristic x by-variable)
#   bdhs_indicator_catalog  indicator definitions
#   bdhs_surveys            survey catalogue
#   bdhs_datasets           dataset (file) catalogue
#
# Usage (from the package root):
#   Rscript data-raw/02-build-api-datasets.R            # cache: data-raw/cache/dhs-api
#   Rscript data-raw/02-build-api-datasets.R <raw_dir>
# Writes data/*.rda (usethis::use_data, xz) and data-raw/dhs-api-audit.json.
# Needs: data.table, jsonlite, tibble, usethis.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

if (!file.exists("DESCRIPTION") || !dir.exists("data-raw")) {
  stop("Run this script from the bdhs package root.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
RAW <- if (length(args) >= 1) args[1] else file.path("data-raw", "cache", "dhs-api")
if (!file.exists(file.path(RAW, "api_data_all.rds"))) {
  stop("No API cache in ", RAW, ": run data-raw/01-fetch-dhs-api.R first.", call. = FALSE)
}
rd <- function(x) readRDS(file.path(RAW, paste0(x, ".rds")))

manifest <- jsonlite::read_json(file.path(RAW, "manifest.json"))
d_raw    <- rd("api_data_all")
memb     <- rd("api_data_breakdown_membership")
ind_bd   <- rd("api_indicators")
ind_gl   <- rd("api_indicators_global")
surveys  <- rd("api_surveys")
dsets    <- rd("api_datasets")
scm      <- rd("api_survey_characteristic_map")
tags     <- rd("api_tags")
pubs     <- rd("api_publications")
updates  <- rd("api_dataupdates")

audit <- list(retrieved_date = manifest$retrieved_date, api_version = manifest$api_version)

# -----------------------------------------------------------------------------
# 1. bdhs_indicators (long)
# -----------------------------------------------------------------------------
# (a) Drop exact duplicates (identical in every column except DataId). Cause:
#     BD2022DHS has every region polygon twice in the API's geometry table and
#     the data view fans out on that join, so every 2022 Region row comes
#     back twice (10,912 rows on 2026-10-26).
allbut <- setdiff(names(d_raw), "DataId")
audit$raw_rows <- nrow(d_raw)
audit$exact_duplicates_removed <- sum(duplicated(d_raw, by = allbut))
audit$exact_duplicates_by_survey <- as.list(
  d_raw[duplicated(d_raw, by = allbut), .N, by = SurveyId][, setNames(N, SurveyId)]
)
d <- unique(d_raw, by = allbut)

# (b) Primary key. IndicatorId alone is NOT unique: some ids are reused for two
#     different indicators (different IndicatorOrder, sometimes a different
#     label, e.g. RH_ANCS_W_WGT "of all" vs "of ANC"); IndicatorOrder is not
#     unique either. The pair is unique.
key <- c("SurveyId", "IndicatorId", "IndicatorOrder", "CharacteristicId", "ByVariableId")
stopifnot(!anyDuplicated(d, by = key))

# (c) breakdown is not a field in the API; reproduce the server's partition:
#     national    <=> IsTotal == 1
#     subnational <=> CharacteristicCategory == "Region"
#     background  <=> every other non-total row
#     (the API's breakdown=background also returns the subnational rows)
d[, breakdown := fifelse(
  IsTotal == 1L, "national",
  fifelse(CharacteristicCategory == "Region", "subnational", "background")
)]
# Check the rule against what the API returns for each named breakdown.
# DataId is NOT stable across queries, so compare on a composite key.
mkey <- function(x) paste(x$SurveyId, x$IndicatorId, x$CharacteristicId, x$ByVariableId, sep = "|")
d_key <- mkey(d)
m_key <- split(mkey(memb), memb$breakdown)
stopifnot(
  setequal(d_key[d$breakdown == "national"], m_key$national),
  setequal(d_key[d$breakdown == "subnational"], m_key$subnational),
  setequal(d_key[d$breakdown != "national"], m_key$background)
)
rm(d_key, m_key)

# (d) IsPreferred marks the headline value where an indicator is published for
#     several reference periods / by-variables. It is NOT "one row per cell":
#     some (survey, indicator, characteristic) cells have no preferred row at
#     all (e.g. mortality by wealth only for the 10-year period). default_view
#     = the preferred row if the cell has one, otherwise every row of the cell.
d[, n_pref := sum(IsPreferred), by = .(SurveyId, IndicatorId, IndicatorOrder, CharacteristicId)]
d[, default_view := IsPreferred == 1L | n_pref == 0L]
audit$rows_not_preferred <- d[IsPreferred == 0L, .N]
audit$rows_in_cells_without_preferred <- d[n_pref == 0L, .N]

bdhs_indicators <- d[, .(
  survey_id               = SurveyId,
  survey_year             = SurveyYear,
  survey_label            = SurveyYearLabel,
  indicator_id            = IndicatorId,
  indicator_order         = IndicatorOrder,
  indicator               = Indicator,
  indicator_type          = IndicatorType,
  breakdown               = breakdown,
  characteristic_id       = CharacteristicId,
  characteristic_category = CharacteristicCategory,
  characteristic_label    = CharacteristicLabel,
  characteristic_order    = CharacteristicOrder,
  by_variable_id          = ByVariableId,
  by_variable_label       = ByVariableLabel,
  region_id               = RegionId,
  level_rank              = LevelRank,
  value                   = Value,
  precision               = Precision,
  denom_weighted          = DenominatorWeighted,
  denom_unweighted        = DenominatorUnweighted,
  ci_low                  = CILow,
  ci_high                 = CIHigh,
  is_total                = IsTotal == 1L,
  is_preferred            = IsPreferred == 1L,
  default_view            = default_view
)]
setorder(
  bdhs_indicators, survey_year, indicator_order, indicator_id,
  characteristic_order, by_variable_id
)
stopifnot(!anyDuplicated(bdhs_indicators))

# -----------------------------------------------------------------------------
# 2. bdhs_indicator_catalog -- keyed (indicator_id, indicator_order)
# -----------------------------------------------------------------------------
pairs <- unique(d[, .(IndicatorId, IndicatorOrder)])
cat_src <- rbind(
  copy(ind_bd)[, src := "countryIds=BD"],
  ind_gl[!ind_bd, on = .(IndicatorId, IndicatorOrder)][, src := "global"],
  fill = TRUE
)
cat_src <- cat_src[pairs, on = .(IndicatorId, IndicatorOrder), nomatch = NULL]
stopifnot(!anyDuplicated(cat_src, by = c("IndicatorId", "IndicatorOrder")))
audit$catalog_rows_from_bd_list <- cat_src[src == "countryIds=BD", .N]
audit$catalog_rows_from_global_list <- cat_src[src == "global", .N]
audit$data_pairs_without_catalog <- nrow(pairs[!cat_src, on = .(IndicatorId, IndicatorOrder)])
stopifnot(audit$data_pairs_without_catalog == 0)

avail <- d[, .(
  n_surveys = uniqueN(SurveyId),
  surveys = paste(sort(unique(SurveyId)), collapse = ","),
  first_year = min(SurveyYear), last_year = max(SurveyYear),
  has_subnational = any(breakdown == "subnational"),
  has_ci = any(!is.na(CILow)),
  n_rows = .N
), by = .(IndicatorId, IndicatorOrder)]

tag_name <- setNames(tags$TagName, tags$TagID)
split_ids <- function(x) {
  lapply(strsplit(fifelse(is.na(x), "", x), ",\\s*"), function(z) as.integer(z[nzchar(z)]))
}

catalog <- merge(cat_src, avail, by = c("IndicatorId", "IndicatorOrder"))[, .(
  indicator_id        = IndicatorId,
  indicator_order     = IndicatorOrder,
  label               = Label,
  short_name          = ShortName,
  definition          = Definition,
  denominator         = Denominator,
  measurement_type    = MeasurementType,
  indicator_type      = IndicatorType,
  topic               = Level1,
  subtopic            = Level2,
  population          = Level3,
  precision           = NumberScale,
  tag_ids             = split_ids(TagIds),
  is_quick_stat       = IsQuickStat == 1L,
  quick_stat_order    = as.integer(QuickStatOrder),
  denom_weighted_id   = DenominatorWeightedId,
  denom_unweighted_id = DenominatorUnweightedId,
  special1_id         = IndicatorSpecial1Id,
  special2_id         = IndicatorSpecial2Id,
  old_id              = IndicatorOldId,
  id_is_shared        = FALSE,
  n_surveys, surveys, first_year, last_year, has_subnational, has_ci, n_rows
)]
catalog[, tags := vapply(tag_ids, function(z) {
  z <- tag_name[as.character(z)]
  if (length(z)) paste(z[!is.na(z)], collapse = "; ") else NA_character_
}, "")]
catalog[tags == "", tags := NA_character_]
catalog[, id_is_shared := .N > 1L, by = indicator_id]
setcolorder(catalog, c(setdiff(names(catalog), c("tags", "n_surveys", "surveys", "first_year",
  "last_year", "has_subnational", "has_ci", "n_rows")), "tags"))
setorder(catalog, indicator_order, indicator_id)
stopifnot(!anyDuplicated(catalog, by = c("indicator_id", "indicator_order")))

# -----------------------------------------------------------------------------
# 3. bdhs_surveys
# -----------------------------------------------------------------------------
# Recode version from the dataset list (first IR/HR file for DHS; IQ/HH for
# the BMMS; the facility files for SPAs).
phase <- dsets[
  DatasetType == "Survey Datasets" & grepl("^BD(IR|HR|IQ|HH)", toupper(FileName)),
  .(recode = toupper(substr(FileName, 5, 6))),
  by = SurveyId
][, .SD[1], SurveyId]
phase_spa <- dsets[DatasetType == "SPA Datasets",
  .(recode = toupper(substr(FileName, 5, 6))),
  by = SurveyId
][, .SD[1], SurveyId]
phase <- rbind(phase, phase_spa)

sc_list <- scm[, .(characteristic_ids = list(sort(SurveyCharacteristicID))), by = SurveyId]
report <- pubs[PublicationTitle == "Final Report" |
  grepl("Maternal Mortality Survey 2001", PublicationTitle),
  .(report_url = PublicationURL[1]),
  by = SurveyId
]
upd <- updates[!is.na(SurveyId), .(indicators_updated = max(UpdateDate)), by = SurveyId]

surv <- merge(surveys, phase, by = "SurveyId", all.x = TRUE)
surv <- merge(surv, sc_list, by = "SurveyId", all.x = TRUE)
surv <- merge(surv, report, by = "SurveyId", all.x = TRUE)
surv <- merge(surv, upd, by = "SurveyId", all.x = TRUE)
# BD2001OTH lists characteristic ids in /surveys but none in
# /surveycharacteristics?surveyIds=, so fall back to the /surveys field.
surv[, characteristic_ids := Map(function(a, b) {
  if (length(a)) a else sort(as.integer(trimws(strsplit(fifelse(is.na(b), "", b), ",")[[1]])))
}, characteristic_ids, SurveyCharacteristicIds)]

bdhs_surveys <- surv[, .(
  survey_id = SurveyId, survey_type = SurveyType, survey_year = SurveyYear,
  survey_label = SurveyYearLabel, survey_num = SurveyNum,
  recode_version = recode, dhs_phase = as.integer(substr(recode, 1, 1)),
  has_indicator_data = IndicatorData == 1L, status = SurveyStatus,
  fieldwork_start = FieldworkStart, fieldwork_end = FieldworkEnd,
  publication_date = PublicationDate, release_date = ReleaseDate,
  implementing_org = gsub("\\s+", " ", trimws(ImplementingOrg)),
  n_clusters = NumberOfSamplePoints, n_households = NumberofHouseholds,
  women_universe = UniverseOfWomen, n_women = NumberOfWomen,
  women_age_min = MinAgeWomen, women_age_max = MaxAgeWomen,
  men_universe = UniverseOfMen, n_men = NumberOfMen,
  men_age_min = MinAgeMen, men_age_max = MaxAgeMen,
  n_facilities = NumberOfFacilities, footnotes = Footnotes,
  characteristic_ids, report_url, indicators_updated
)]
setorder(bdhs_surveys, survey_year, survey_type)
stopifnot(nrow(bdhs_surveys) == 13L, !anyDuplicated(bdhs_surveys$survey_id))

# -----------------------------------------------------------------------------
# 4. bdhs_datasets
# -----------------------------------------------------------------------------
fn <- toupper(sub("\\.zip$", "", dsets$FileName, ignore.case = TRUE))
bdhs_datasets <- dsets[, .(
  survey_id      = SurveyId,
  survey_type    = SurveyType,
  survey_year    = SurveyYear,
  file_name      = FileName,
  file_stem      = fn,
  file_type_code = substr(fn, 3, 4),
  file_type      = FileType,
  recode_version = substr(fn, 5, 6),
  format_code    = fifelse(nchar(fn) >= 8, substr(fn, 7, 8), NA_character_),
  file_format    = trimws(FileFormat),
  spa_variant    = fifelse(nchar(fn) == 10, substr(fn, 9, 10), NA_character_),
  dataset_type   = DatasetType,
  file_size      = FileSize,
  last_modified  = FileDateLastModified
)]
setorder(bdhs_datasets, survey_year, survey_type, file_type_code, format_code)
stopifnot(!anyDuplicated(bdhs_datasets$file_name))

# -----------------------------------------------------------------------------
# 5. write
# -----------------------------------------------------------------------------
as_tbl <- function(x) tibble::as_tibble(as.data.frame(x))
bdhs_indicators        <- as_tbl(bdhs_indicators)
bdhs_indicator_catalog <- as_tbl(catalog)
bdhs_surveys           <- as_tbl(bdhs_surveys)
bdhs_datasets          <- as_tbl(bdhs_datasets)

usethis::use_data(
  bdhs_indicators, bdhs_indicator_catalog, bdhs_surveys, bdhs_datasets,
  overwrite = TRUE, compress = "xz"
)

audit$rows <- list(
  bdhs_indicators = nrow(bdhs_indicators),
  bdhs_indicator_catalog = nrow(bdhs_indicator_catalog),
  bdhs_surveys = nrow(bdhs_surveys),
  bdhs_datasets = nrow(bdhs_datasets)
)
jsonlite::write_json(audit, file.path("data-raw", "dhs-api-audit.json"),
  auto_unbox = TRUE, pretty = TRUE
)
str(audit)
