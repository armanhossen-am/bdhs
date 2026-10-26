# =============================================================================
# data-raw/boundaries/01-download-sdr.R
#
# Boundaries pipeline, step 1 of 3: download the full-resolution subnational
# boundaries of every Bangladesh survey from the DHS Spatial Data Repository
# (SDR) ArcGIS REST query endpoint, as GeoJSON in EPSG:4326:
#
#   https://gis.dhsprogram.com/arcgis/rest/services/Geometry/SDR_Regions/
#     MapServer/0/query?where=SVYID%3D<SurveyNum>&outFields=*
#     &returnGeometry=true&outSR=4326&f=geojson
#
# (SurveyNum = bdhs_surveys$survey_num.) Unlike the DHS API geometry endpoint
# (f=geojson drops multipolygon parts) and the asynchronous SDR download
# service, this returns complete geometries synchronously.
#
# Licence: SDR boundaries are released under ODbL 1.0 (database) and DbCL 1.0
# (contents). Cite: "Spatial Data Repository, The Demographic and Health
# Surveys Program. ICF International. Available from
# spatialdata.dhsprogram.com [Accessed <date>]".
#
# Usage (from the package root):
#   Rscript data-raw/boundaries/01-download-sdr.R
# Env: BDHS_REFRESH=true re-downloads files already in the cache.
# Writes data-raw/boundaries/cache/sdr_<survey_id>.geojson (not committed) and
# data-raw/boundaries/sdr-download-log.csv (committed: access date, sizes).
# Next: 02-build-boundaries.R. Needs: httr2.
# =============================================================================

if (!file.exists("DESCRIPTION") || !dir.exists("data-raw")) {
  stop("Run this script from the bdhs package root.", call. = FALSE)
}

CACHE <- file.path("data-raw", "boundaries", "cache")
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
REFRESH <- tolower(Sys.getenv("BDHS_REFRESH", "false")) %in% c("true", "1", "yes")
QUERY <- "https://gis.dhsprogram.com/arcgis/rest/services/Geometry/SDR_Regions/MapServer/0/query"
UA <- "bdhs R package data-raw (https://github.com/armanhossen-am/bdhs)"

load(file.path("data", "bdhs_surveys.rda"))
surveys <- bdhs_surveys[, c("survey_id", "survey_num")]

download_one <- function(survey_id, survey_num) {
  dest <- file.path(CACHE, paste0("sdr_", survey_id, ".geojson"))
  if (file.exists(dest) && !REFRESH) {
    message(survey_id, ": cached")
  } else {
    message(survey_id, ": downloading (SVYID = ", survey_num, ")")
    httr2::request(QUERY) |>
      httr2::req_url_query(
        where = paste0("SVYID=", survey_num), outFields = "*",
        returnGeometry = "true", outSR = 4326, f = "geojson"
      ) |>
      httr2::req_user_agent(UA) |>
      httr2::req_timeout(600) |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_perform(path = dest)
  }
  js <- jsonlite::read_json(dest, simplifyVector = FALSE)
  if (!is.null(js$error) || !length(js$features)) {
    stop(survey_id, ": SDR query returned no features", call. = FALSE)
  }
  data.frame(
    survey_id = survey_id, survey_num = survey_num,
    n_features = length(js$features),
    bytes = file.size(dest),
    accessed = format(file.mtime(dest), "%Y-%m-%d")
  )
}

log <- do.call(rbind, Map(download_one, surveys$survey_id, surveys$survey_num))
print(log, row.names = FALSE)
utils::write.csv(log, file.path("data-raw", "boundaries", "sdr-download-log.csv"), row.names = FALSE)
