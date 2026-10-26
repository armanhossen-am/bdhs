# =============================================================================
# data-raw/boundaries/03-validate-boundaries.R
#
# Boundaries pipeline, step 3 of 3: check inst/extdata/bdhs_boundaries.rds
# against the raw SDR geometry and the other bundled data. Stops on failure.
#
#   * sf, EPSG:4326, 86 rows, valid, non-empty MULTIPOLYGONs
#   * area change from simplification < 0.1 % per region; no overlaps
#     between the regions of a survey (< 0.01 km2)
#   * shape error: directed Hausdorff distance from the simplified boundary to
#     the raw boundary <= 1.5 x tolerance (islets smaller than the tolerance
#     may disappear, so the reverse direction can be larger)
#   * every DHS reg_id joins to the DHS API region_id in bdhs_indicators
#   * consistent with bdhs_regions (survey_id, region_code, unit_id, reg_id,
#     code_verified), if data/bdhs_regions.rda exists
# Optionally (if ggplot2 is installed) draws check maps into the cache dir.
#
# Usage (from the package root, after 02-build-boundaries.R):
#   Rscript data-raw/boundaries/03-validate-boundaries.R
# Needs: sf (+ ggplot2 for the maps).
# =============================================================================

if (!file.exists("DESCRIPTION") || !dir.exists("data-raw")) {
  stop("Run this script from the bdhs package root.", call. = FALSE)
}
suppressPackageStartupMessages(library(sf))
invisible(sf_use_s2(FALSE))

CACHE <- file.path("data-raw", "boundaries", "cache")
TOL <- as.numeric(Sys.getenv("BDHS_TOL", "100"))
b <- readRDS(file.path("inst", "extdata", "bdhs_boundaries.rds"))
long_p <- readRDS(file.path(CACHE, "sdr_long_utm46.rds"))
load(file.path("data", "bdhs_indicators.rda"))

check <- function(ok, what) {
  message(if (isTRUE(ok)) "ok    " else "FAIL  ", what)
  if (!isTRUE(ok)) stop("Validation failed: ", what, call. = FALSE)
}

check(inherits(b, "sf") && identical(st_crs(b)$epsg, 4326L), "sf object in EPSG:4326")
check(nrow(b) == 86L, "86 survey x region rows")
check(all(st_geometry_type(b) == "MULTIPOLYGON"), "all MULTIPOLYGON")
check(all(st_is_valid(b)) && !any(st_is_empty(b)), "valid and non-empty")
check(!anyDuplicated(st_drop_geometry(b)[c("survey_id", "region_code")]), "unique (survey_id, region_code)")

bp <- st_transform(b, 32646)
m <- match(paste(bp$survey_id, bp$unit_id), paste(long_p$survey_id, long_p$unit))
check(!anyNA(m), "every region matches a raw SDR feature")
area_change <- as.numeric(st_area(bp)) / 1e6 / long_p$area_km2[m] - 1
check(max(abs(area_change)) < 0.001, sprintf("area change < 0.1%% (max %.4f%%)", 100 * max(abs(area_change))))

overlap <- vapply(split(bp, bp$survey_id), function(z) {
  sum(as.numeric(st_area(z))) / 1e6 - as.numeric(st_area(st_union(z))) / 1e6
}, 1)
check(max(overlap) < 0.01, sprintf("no overlaps within a survey (max %.4f km2)", max(overlap)))

directed <- vapply(seq_len(nrow(bp)), function(i) {
  # densify first: simplified vertices are raw vertices, the error is in between
  p <- st_cast(st_segmentize(st_boundary(st_geometry(bp)[i]), TOL / 4), "POINT")
  max(as.numeric(st_distance(p, st_boundary(st_geometry(long_p)[m[i]]))))
}, 1)
check(max(directed) <= 1.5 * TOL, sprintf("simplified -> raw distance <= %g m (max %.0f m)", 1.5 * TOL, max(directed)))

per_survey <- data.frame(
  survey_id = names(split(bp, bp$survey_id)),
  regions = as.vector(table(bp$survey_id)),
  raw_vertices = tapply(long_p$npts[m], bp$survey_id, sum),
  vertices = tapply(vapply(st_geometry(bp), function(z) nrow(st_coordinates(z)), 1L), bp$survey_id, sum),
  max_area_change_pct = round(100 * tapply(abs(area_change), bp$survey_id, max), 3),
  max_shape_error_m = round(tapply(directed, bp$survey_id, max))
)
print(per_survey, row.names = FALSE)

api_regions <- unique(bdhs_indicators$region_id[!is.na(bdhs_indicators$region_id)])
check(all(b$reg_id[b$survey_type == "DHS"] %in% api_regions), "DHS reg_id values join to bdhs_indicators$region_id")

if (file.exists(file.path("data", "bdhs_regions.rda"))) {
  load(file.path("data", "bdhs_regions.rda"))
  k_b <- paste(b$survey_id, b$region_code)
  k_r <- paste(bdhs_regions$survey_id, bdhs_regions$region_code)
  j <- match(k_b, k_r)
  check(setequal(k_b, k_r), "same (survey_id, region_code) as bdhs_regions")
  check(identical(b$unit_id, bdhs_regions$unit_id[j]), "same unit_id as bdhs_regions")
  check(identical(b$reg_id, bdhs_regions$reg_id[j]), "same reg_id as bdhs_regions")
  check(identical(b$code_verified, bdhs_regions$code_verified[j]), "same code_verified as bdhs_regions")
  check(identical(b$region_var, bdhs_regions$region_var[j]), "same region_var as bdhs_regions")
}

if (requireNamespace("ggplot2", quietly = TRUE)) {
  b$lab <- paste0(b$region_code, ":", substr(b$region_name, 1, 5))
  p <- ggplot2::ggplot(b) +
    ggplot2::geom_sf(ggplot2::aes(fill = factor(unit_id)), colour = "black", linewidth = 0.15) +
    ggplot2::geom_sf_text(ggplot2::aes(label = lab), size = 1.8) +
    ggplot2::facet_wrap(~survey_id) +
    ggplot2::theme_void(base_size = 8) +
    ggplot2::theme(legend.position = "none")
  ggplot2::ggsave(file.path(CACHE, "check_map_codes.png"), p, width = 9, height = 9, dpi = 110)
  message("check map: ", file.path(CACHE, "check_map_codes.png"))
}
message("all boundary checks passed")
