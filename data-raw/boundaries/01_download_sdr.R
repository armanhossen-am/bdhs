# Download survey-specific subnational boundaries for every Bangladesh survey
# from the DHS Spatial Data Repository (SDR) ArcGIS geoprocessing service.
# Endpoint learned from ropensci/rdhs R/shapes.R (download_boundaries()), but
# made robust: poll job status and read the result URL from results/result
# instead of guessing the date-stamped file name.
suppressPackageStartupMessages({ library(httr2); library(jsonlite) })

out_root <- "/private/tmp/claude-501/-Users-armanhossen-Documents-MU-Github/ad97b126-e74a-4eff-860f-a1892e94a270/scratchpad/research/spatial/raw"
dir.create(out_root, showWarnings = FALSE, recursive = TRUE)

gp <- "https://gis.dhsprogram.com/arcgis/rest/services/Tools/DownloadSubnationalData/GPServer/downloadSubNationalBoundaries"

sdr_download <- function(survey_num, survey_id, fmt = "shp", max_wait = 120) {
  sub <- request(paste0(gp, "/submitJob")) |>
    req_url_query(survey_ids = survey_num, spatial_format = fmt, f = "json") |>
    req_timeout(60) |> req_retry(max_tries = 3) |> req_perform() |>
    resp_body_json()
  job <- sub$jobId
  status <- sub$jobStatus
  t0 <- Sys.time()
  while (!status %in% c("esriJobSucceeded", "esriJobFailed", "esriJobCancelled")) {
    Sys.sleep(2)
    st <- request(paste0(gp, "/jobs/", job)) |> req_url_query(f = "json") |>
      req_perform() |> resp_body_json()
    status <- st$jobStatus
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > max_wait) stop("timeout")
  }
  if (status != "esriJobSucceeded") stop("job failed: ", status)
  res <- request(paste0(gp, "/jobs/", job, "/results/result")) |>
    req_url_query(f = "json") |> req_perform() |> resp_body_json()
  errs <- request(paste0(gp, "/jobs/", job, "/results/errors")) |>
    req_url_query(f = "json") |> req_perform() |> resp_body_json()
  url <- res$value$url
  dest_dir <- file.path(out_root, survey_id)
  dir.create(dest_dir, showWarnings = FALSE)
  zip <- file.path(dest_dir, paste0(survey_id, "_sdr_subnational_boundaries.zip"))
  request(url) |> req_timeout(300) |> req_perform(path = zip)
  ex <- file.path(dest_dir, "unzipped")
  dir.create(ex, showWarnings = FALSE)
  files <- unzip(zip, exdir = ex)
  data.frame(survey_id = survey_id, survey_num = survey_num, job = job,
             url = url, zip_bytes = file.size(zip), n_files = length(files),
             errors = paste(unlist(errs$value), collapse = ";"),
             files = paste(basename(files), collapse = ";"))
}

surveys <- data.frame(
  survey_id = c("BD1994DHS", "BD1997DHS", "BD1999SPA", "BD2000DHS", "BD2001OTH",
                "BD2004DHS", "BD2007DHS", "BD2011DHS", "BD2014DHS", "BD2014SPA",
                "BD2017DHS", "BD2017SPA", "BD2022DHS"),
  survey_num = c(59, 89, 136, 135, 137, 236, 287, 349, 461, 441, 536, 531, 584)
)

log <- list()
for (i in seq_len(nrow(surveys))) {
  s <- surveys[i, ]
  message("Downloading ", s$survey_id, " (", s$survey_num, ")")
  log[[i]] <- tryCatch(sdr_download(s$survey_num, s$survey_id),
    error = function(e) data.frame(survey_id = s$survey_id, survey_num = s$survey_num,
      job = NA, url = NA, zip_bytes = NA, n_files = 0,
      errors = conditionMessage(e), files = NA))
  Sys.sleep(1)
}
log <- do.call(rbind, log)
print(log[, c("survey_id", "survey_num", "zip_bytes", "n_files", "errors")])
write.csv(log, file.path(dirname(out_root), "download_log.csv"), row.names = FALSE)
