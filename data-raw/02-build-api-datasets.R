#!/usr/bin/env Rscript
# =============================================================================
# build_bundle_prototype.R -- turn the raw API pulls (fetch_api.R output) into
# the proposed tidy {bdhs} bundle tables and measure their size.
#
# Intended to become data-raw/build_api_tables.R.
# Usage: Rscript build_bundle_prototype.R <raw_dir> <out_dir>
# =============================================================================
suppressPackageStartupMessages({ library(data.table) })

args <- commandArgs(trailingOnly = TRUE)
RAW  <- if (length(args) >= 1) args[1] else "out"
OUT  <- if (length(args) >= 2) args[2] else "bundle"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
rd <- function(x) readRDS(file.path(RAW, paste0(x, ".rds")))

d_raw   <- rd("api_data_all")
ind_bd  <- rd("api_indicators")
ind_gl  <- rd("api_indicators_global")
surveys <- rd("api_surveys")
dsets   <- rd("api_datasets")
scm     <- rd("api_survey_characteristic_map")
sc      <- rd("api_surveycharacteristics")
tags    <- rd("api_tags")
geom    <- rd("api_geometry_wkt")

audit <- list()

# -----------------------------------------------------------------------------
# 1. bdhs_indicators (long)
# -----------------------------------------------------------------------------
# (a) drop exact duplicates (identical in every column except DataId).
#     Cause: BD2022DHS has every region polygon twice in /geometry and the
#     API's data view fans out on that join -> 10,912 duplicated Region rows.
allbut <- setdiff(names(d_raw), "DataId")
audit$raw_rows        <- nrow(d_raw)
audit$exact_dups      <- sum(duplicated(d_raw, by = allbut))
d <- unique(d_raw, by = allbut)

# (b) primary key. IndicatorId alone is NOT unique: 21 IDs are reused for two
#     different indicators (different IndicatorOrder, sometimes different
#     label, e.g. RH_ANCS_W_WGT "of all" vs "of ANC"). IndicatorOrder is not
#     unique either (24 orders shared by several IDs). The pair is unique.
key <- c("SurveyId", "IndicatorId", "IndicatorOrder", "CharacteristicId", "ByVariableId")
stopifnot(!anyDuplicated(d, by = key))

# (c) breakdown: not a field in the API; reproduce the server's partition
#     national    <=> IsTotal == 1
#     subnational <=> CharacteristicCategory == "Region"   (also in background)
#     background  <=> IsTotal == 0
d[, breakdown := fifelse(IsTotal == 1L, "national",
                  fifelse(CharacteristicCategory == "Region", "subnational", "background"))]

# (d) is_preferred is NOT "one row per cell": 10,049 rows sit in
#     (survey, indicator, characteristic) cells with no preferred row at all
#     (e.g. mortality by wealth only for the 10-year period, IsPreferred = 0).
#     Provide default_view = preferred row if the cell has one, else every row.
d[, n_pref := sum(IsPreferred), by = .(SurveyId, IndicatorId, IndicatorOrder, CharacteristicId)]
d[, default_view := IsPreferred == 1L | n_pref == 0L]
audit$rows_no_preferred_cell <- d[n_pref == 0L, .N]

clean_region <- function(x) sub("^\\.\\.", "", x)

ind_long <- d[, .(
  survey_id            = SurveyId,
  survey_year          = SurveyYear,
  survey_label         = SurveyYearLabel,
  indicator_id         = IndicatorId,
  indicator_order      = IndicatorOrder,
  indicator            = Indicator,
  indicator_type       = IndicatorType,
  breakdown            = breakdown,
  characteristic_id    = CharacteristicId,
  characteristic_category = CharacteristicCategory,
  characteristic_label = CharacteristicLabel,
  characteristic_order = CharacteristicOrder,
  by_variable_id       = ByVariableId,
  by_variable_label    = ByVariableLabel,
  region_id            = RegionId,
  level_rank           = LevelRank,
  value                = Value,
  precision            = Precision,
  denom_weighted       = DenominatorWeighted,
  denom_unweighted     = DenominatorUnweighted,
  ci_low               = CILow,
  ci_high              = CIHigh,
  is_total             = IsTotal == 1L,
  is_preferred         = IsPreferred == 1L,
  default_view         = default_view
)]
setorder(ind_long, survey_year, indicator_order, indicator_id, characteristic_order, by_variable_id)

# -----------------------------------------------------------------------------
# 2. bdhs_indicator_catalog -- keyed (indicator_id, indicator_order)
# -----------------------------------------------------------------------------
pairs <- unique(d[, .(IndicatorId, IndicatorOrder)])
cat_src <- rbind(ind_bd[, src := "countryIds=BD"],
                 ind_gl[!ind_bd, on = .(IndicatorId, IndicatorOrder)][, src := "global"],
                 fill = TRUE)
cat_src <- cat_src[pairs, on = .(IndicatorId, IndicatorOrder), nomatch = NULL]
audit$catalog_rows_from_bd     <- cat_src[src == "countryIds=BD", .N]
audit$catalog_rows_from_global <- cat_src[src == "global", .N]
audit$data_pairs_without_catalog <- nrow(pairs[!cat_src, on = .(IndicatorId, IndicatorOrder)])

avail <- d[, .(n_surveys = uniqueN(SurveyId),
               surveys   = paste(sort(unique(SurveyId)), collapse = ","),
               first_year = min(SurveyYear), last_year = max(SurveyYear),
               has_subnational = any(breakdown == "subnational"),
               has_ci = any(!is.na(CILow)),
               n_rows = .N),
           by = .(IndicatorId, IndicatorOrder)]

catalog <- merge(cat_src, avail, by = c("IndicatorId", "IndicatorOrder"))[, .(
  indicator_id      = IndicatorId,
  indicator_order   = IndicatorOrder,
  label             = Label,
  short_name        = ShortName,
  definition        = Definition,
  denominator       = Denominator,
  measurement_type  = MeasurementType,
  indicator_type    = IndicatorType,
  topic             = Level1,
  subtopic          = Level2,
  population        = Level3,
  precision         = NumberScale,
  tag_ids           = lapply(strsplit(fifelse(is.na(TagIds), "", TagIds), ",\\s*"), as.integer),
  is_quick_stat     = IsQuickStat == 1L,
  quick_stat_order  = as.integer(QuickStatOrder),
  denom_weighted_id   = DenominatorWeightedId,
  denom_unweighted_id = DenominatorUnweightedId,
  special1_id       = IndicatorSpecial1Id,
  special2_id       = IndicatorSpecial2Id,
  old_id            = IndicatorOldId,
  id_is_shared      = FALSE,
  n_surveys, surveys, first_year, last_year, has_subnational, has_ci, n_rows
)]
catalog[, id_is_shared := .N > 1L, by = indicator_id]
setorder(catalog, indicator_order, indicator_id)

# -----------------------------------------------------------------------------
# 3. bdhs_surveys
# -----------------------------------------------------------------------------
phase <- dsets[DatasetType == "Survey Datasets" & grepl("^BD(IR|HR|IQ|HH)", toupper(FileName)),
               .(recode = toupper(substr(FileName, 5, 6))), by = SurveyId][, .SD[1], SurveyId]
phase_spa <- dsets[DatasetType == "SPA Datasets", .(recode = toupper(substr(FileName, 5, 6))), by = SurveyId][, .SD[1], SurveyId]
phase <- rbind(phase, phase_spa)
sc_list <- scm[, .(characteristic_ids = list(sort(SurveyCharacteristicID))), by = SurveyId]
surv <- merge(surveys, phase, by = "SurveyId", all.x = TRUE)
surv <- merge(surv, sc_list, by = "SurveyId", all.x = TRUE)
# BD2001OTH has ids in /surveys but none from /surveycharacteristics?surveyIds=
surv[, characteristic_ids := Map(function(a, b) if (length(a)) a else
        sort(as.integer(trimws(strsplit(fifelse(is.na(b), "", b), ",")[[1]]))),
        characteristic_ids, SurveyCharacteristicIds)]
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
  characteristic_ids
)]
setorder(bdhs_surveys, survey_year, survey_type)

# -----------------------------------------------------------------------------
# 4. bdhs_datasets
# -----------------------------------------------------------------------------
fn <- toupper(sub("\\.zip$", "", dsets$FileName, ignore.case = TRUE))
bdhs_datasets <- dsets[, .(
  survey_id = SurveyId, survey_type = SurveyType, survey_year = SurveyYear,
  file_name = FileName,
  file_stem = fn,
  file_type_code = substr(fn, 3, 4),
  file_type = FileType,
  recode_version = substr(fn, 5, 6),
  format_code = fifelse(nchar(fn) >= 8, substr(fn, 7, 8), NA_character_),
  file_format = trimws(FileFormat),
  spa_variant = fifelse(nchar(fn) == 10, substr(fn, 9, 10), NA_character_),
  dataset_type = DatasetType,
  file_size = FileSize,
  last_modified = FileDateLastModified
)]
setorder(bdhs_datasets, survey_year, survey_type, file_type_code, format_code)

# -----------------------------------------------------------------------------
# 5. small lookups
# -----------------------------------------------------------------------------
bdhs_characteristics <- unique(d[, .(characteristic_id = CharacteristicId,
  category = CharacteristicCategory, label = CharacteristicLabel,
  order = CharacteristicOrder)])[order(order)]
stopifnot(!anyDuplicated(bdhs_characteristics$characteristic_id))

reg <- d[CharacteristicCategory == "Region",
         .(level_rank = LevelRank[1]), by = .(survey_id = SurveyId, characteristic_id = CharacteristicId,
                                               label = CharacteristicLabel, region_id = RegionId)]
reg[, region := clean_region(label)]
reg[, is_subregion := grepl("^\\.\\.", label)]
reg[, in_geometry := region_id %in% geom$RegionID]
bdhs_api_regions <- reg[order(survey_id, characteristic_id)]

# -----------------------------------------------------------------------------
# 6. sizes
# -----------------------------------------------------------------------------
sz <- function(x, name) {
  f1 <- tempfile(fileext = ".rds"); saveRDS(x, f1, compress = FALSE)
  f2 <- tempfile(fileext = ".rds"); saveRDS(x, f2, compress = "xz")
  f3 <- tempfile(fileext = ".rda"); e <- new.env(); assign(name, x, e)
  save(list = name, envir = e, file = f3, compress = "bzip2")
  f4 <- tempfile(fileext = ".rda"); save(list = name, envir = e, file = f4, compress = "xz", compression_level = 9)
  data.table(object = name, rows = NROW(x), cols = NCOL(x),
             mem_MB = round(as.numeric(object.size(x)) / 2^20, 2),
             rds_raw_MB = round(file.size(f1) / 2^20, 2),
             rds_xz_KB = round(file.size(f2) / 1024, 1),
             rda_bzip2_KB = round(file.size(f3) / 1024, 1),
             rda_xz9_KB = round(file.size(f4) / 1024, 1))
}
as_df <- function(x) as.data.frame(x)  # bundle as plain data.frame / tibble

v_full      <- as_df(ind_long)
v_lean      <- as_df(ind_long[, !c("indicator", "characteristic_category",
                                    "characteristic_label", "characteristic_order",
                                    "by_variable_label", "survey_label", "survey_year",
                                    "level_rank", "precision")])
v_factor    <- copy(ind_long)
for (j in c("survey_id", "survey_label", "indicator_id", "indicator", "indicator_type",
            "breakdown", "characteristic_category", "characteristic_label",
            "by_variable_label", "region_id")) set(v_factor, j = j, value = factor(v_factor[[j]]))
v_factor    <- as_df(v_factor)
v_I_only    <- as_df(ind_long[indicator_type == "I"])
v_I_noden   <- as_df(ind_long[indicator_type == "I"][, !c("denom_weighted", "denom_unweighted")])
v_pref      <- as_df(ind_long[default_view == TRUE])
v_rawapi    <- as.data.frame(d_raw)

sizes <- rbindlist(list(
  sz(v_rawapi,  "raw_api_all_columns_with_dups"),
  sz(v_full,    "bdhs_indicators_full"),
  sz(v_factor,  "bdhs_indicators_full_factors"),
  sz(v_lean,    "bdhs_indicators_normalized"),
  sz(v_pref,    "bdhs_indicators_default_view_only"),
  sz(v_I_only,  "bdhs_indicators_type_I_only"),
  sz(v_I_noden, "bdhs_indicators_type_I_no_denoms"),
  sz(as_df(catalog),       "bdhs_indicator_catalog"),
  sz(as_df(bdhs_surveys),  "bdhs_surveys"),
  sz(as_df(bdhs_datasets), "bdhs_datasets"),
  sz(as_df(bdhs_characteristics), "bdhs_characteristics"),
  sz(as_df(bdhs_api_regions),     "bdhs_api_regions")
))
print(sizes)

# column-wise contribution (xz) for the full long table
colsz <- rbindlist(lapply(names(v_full), function(j) {
  f <- tempfile(); saveRDS(v_full[[j]], f, compress = "xz")
  data.table(column = j, class = class(v_full[[j]])[1], xz_KB = round(file.size(f) / 1024, 1),
             n_unique = uniqueN(v_full[[j]]), n_na = sum(is.na(v_full[[j]])))
}))[order(-xz_KB)]
print(colsz)

# -----------------------------------------------------------------------------
# 7. write prototype outputs (+ audit)
# -----------------------------------------------------------------------------
saveRDS(as_df(ind_long),             file.path(OUT, "bdhs_indicators.rds"), compress = "xz")
saveRDS(as_df(catalog),              file.path(OUT, "bdhs_indicator_catalog.rds"), compress = "xz")
saveRDS(as_df(bdhs_surveys),         file.path(OUT, "bdhs_surveys.rds"), compress = "xz")
saveRDS(as_df(bdhs_datasets),        file.path(OUT, "bdhs_datasets.rds"), compress = "xz")
saveRDS(as_df(bdhs_characteristics), file.path(OUT, "bdhs_characteristics.rds"), compress = "xz")
saveRDS(as_df(bdhs_api_regions),     file.path(OUT, "bdhs_api_regions.rds"), compress = "xz")
fwrite(sizes, file.path(OUT, "sizes.csv"))
fwrite(colsz, file.path(OUT, "column_sizes.csv"))
jsonlite::write_json(audit, file.path(OUT, "audit.json"), auto_unbox = TRUE, pretty = TRUE)
print(audit)
