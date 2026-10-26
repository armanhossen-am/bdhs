# =============================================================================
# data-raw/boundaries/02-build-boundaries.R
#
# Boundaries pipeline, step 2 of 3: build inst/extdata/bdhs_boundaries.rds
# (sf, EPSG:4326, one row per survey x region) from the SDR GeoJSON files
# downloaded by 01-download-sdr.R.
#
# 1. Read every survey's features; drop duplicated features (the BD2022DHS
#    layer contains every division twice; the copies must be identical).
# 2. Stable geographic unit (4-digit, see data-raw/inputs/region_units.csv)
#    from the REG_ID suffix; SPA/BMMS layers without REG_ID get the unit of
#    the DHS feature with the same name and the largest overlap (IoU > 0.95).
# 3. Topology-preserving simplification of each survey's coverage, 100 m
#    tolerance in UTM 46N (EPSG:32646), pure sf/GEOS: node the shared
#    linework once (st_union of boundaries), st_simplify(preserveTopology =
#    TRUE) on the whole network (arc end-nodes are kept, so neighbours still
#    share identical edges: no gaps or slivers), polygonize, give each face
#    back to its region by point-on-surface and dissolve. Back to EPSG:4326,
#    coordinates rounded to 1e-5 degrees (~1 m).
# 4. Attributes. region_code is the code of the survey's microdata region
#    variable. The SDR REGCODE is used except for BD2017DHS, where the SDR
#    codes 5-8 (5 Rajshahi, 6 Rangpur, 7 Sylhet, 8 Mymensingh) contradict the
#    2017-18 recode files (5 mymensingh, 6 rajshahi, 7 rangpur, 8 sylhet).
#    The BD2017SPA layer uses the same suspicious SDR coding; it is kept but
#    not verified. code_verified = TRUE only where every code was checked
#    against v024/hv024 value labels of the maintainer's files
#    (data/bdhs_value_labels.rda).
#
# Usage (from the package root, after 01-download-sdr.R and
# 03-build-codebooks.R):
#   Rscript data-raw/boundaries/02-build-boundaries.R
# Env: BDHS_TOL = simplification tolerance in metres (default 100).
# Writes inst/extdata/bdhs_boundaries.rds,
#        data-raw/inputs/bdhs_boundaries_attributes.csv,
#        data-raw/inputs/sdr_region_table.csv (raw areas / vertex counts) and
#        data-raw/boundaries/cache/sdr_long_utm46.rds (for 03-validate).
# Next: 03-validate-boundaries.R, then data-raw/05-build-regions.R.
# Needs: sf (GEOS >= 3.9).
# =============================================================================

if (!file.exists("DESCRIPTION") || !dir.exists("data-raw")) {
  stop("Run this script from the bdhs package root.", call. = FALSE)
}
suppressPackageStartupMessages(library(sf))
invisible(sf_use_s2(FALSE))

CACHE <- file.path("data-raw", "boundaries", "cache")
TOL <- as.numeric(Sys.getenv("BDHS_TOL", "100"))
load(file.path("data", "bdhs_surveys.rda"))
load(file.path("data", "bdhs_value_labels.rda"))
log <- utils::read.csv(file.path("data-raw", "boundaries", "sdr-download-log.csv"),
  stringsAsFactors = FALSE
)
accessed <- max(as.Date(log$accessed))

# 1. Read + de-duplicate -------------------------------------------------------
read_survey <- function(survey_id) {
  f <- file.path(CACHE, paste0("sdr_", survey_id, ".geojson"))
  if (!file.exists(f)) stop("Missing ", f, ": run 01-download-sdr.R first.", call. = FALSE)
  x <- st_read(f, quiet = TRUE)
  x$survey_id <- survey_id
  x$REG_ID[!is.na(x$REG_ID) & !nzchar(trimws(x$REG_ID))] <- NA_character_
  dup <- !is.na(x$REG_ID) & duplicated(x$REG_ID)
  for (r in unique(x$REG_ID[dup])) {
    g <- st_geometry(x)[which(x$REG_ID == r)]
    same <- vapply(seq_along(g)[-1], function(k) identical(st_as_binary(g[k]), st_as_binary(g[1])), TRUE)
    if (!all(same)) stop(survey_id, ": duplicated feature ", r, " with different geometry", call. = FALSE)
  }
  if (any(dup)) message(survey_id, ": dropped ", sum(dup), " duplicated features")
  x[!dup, c(
    "survey_id", "SVYTYPE", "SVYYEAR", "SVYID", "REG_ID", "DHSREGEN", "REGVAR",
    "REGCODE", "REGNAME"
  )]
}
raw <- do.call(rbind, lapply(bdhs_surveys$survey_id, read_survey))
raw$REGCODE <- as.integer(raw$REGCODE)
raw$unit <- substr(raw$REG_ID, 12, 15)

long_p <- st_transform(raw, 32646)
long_p$area_km2 <- as.numeric(st_area(long_p)) / 1e6
long_p$npts <- vapply(st_geometry(long_p), function(z) nrow(st_coordinates(z)), 1L)
long_p$nparts <- lengths(st_geometry(long_p))

# 2. Units for layers without REG_ID ------------------------------------------
ref <- long_p[!is.na(long_p$unit) & long_p$SVYTYPE == "DHS", ]
for (i in which(is.na(long_p$unit))) {
  same_name <- tolower(substr(ref$DHSREGEN, 1, 4)) == tolower(substr(long_p$DHSREGEN[i], 1, 4))
  cand <- ref[same_name, ]
  cand <- cand[!duplicated(cand$unit), ]
  ov <- vapply(seq_len(nrow(cand)), function(j) {
    a <- suppressWarnings(st_intersection(st_geometry(long_p[i, ]), st_geometry(cand[j, ])))
    if (length(a) == 0) 0 else sum(as.numeric(st_area(a))) / 1e6
  }, 1)
  iou <- ov / (long_p$area_km2[i] + cand$area_km2 - ov)
  if (!length(iou) || max(iou) < 0.95) {
    stop("Can't match ", long_p$survey_id[i], " ", long_p$DHSREGEN[i], " to a unit", call. = FALSE)
  }
  long_p$unit[i] <- cand$unit[which.max(iou)]
}
stopifnot(!anyDuplicated(st_drop_geometry(long_p)[c("survey_id", "unit")]))
saveRDS(long_p, file.path(CACHE, "sdr_long_utm46.rds"))

tab <- st_drop_geometry(long_p)[, c(
  "survey_id", "REG_ID", "unit", "DHSREGEN", "REGVAR", "REGCODE", "REGNAME",
  "area_km2", "npts", "nparts"
)]
tab <- tab[order(tab$unit, tab$survey_id), ]
tab$area_km2 <- round(tab$area_km2, 4)
utils::write.csv(tab, file.path("data-raw", "inputs", "sdr_region_table.csv"), row.names = FALSE)

# 3. Simplify -----------------------------------------------------------------
simplify_coverage <- function(x, tol) {
  g <- st_geometry(x)
  lines <- st_line_merge(st_union(st_boundary(g))) # noded; shared edges once
  if (tol > 0) lines <- st_simplify(lines, preserveTopology = TRUE, dTolerance = tol)
  faces <- st_collection_extract(st_polygonize(lines), "POLYGON")
  faces <- st_sf(geometry = st_cast(faces, "POLYGON"))
  faces <- faces[as.numeric(st_area(faces)) > 0, ]
  owner <- st_intersects(st_point_on_surface(faces), g)
  faces$owner <- vapply(owner, function(k) if (length(k)) k[1] else NA_integer_, 1L)
  faces <- faces[!is.na(faces$owner), ]
  geoms <- lapply(seq_len(nrow(x)), function(k) st_union(st_geometry(faces)[faces$owner == k]))
  st_cast(st_make_valid(do.call(c, geoms)), "MULTIPOLYGON")
}
round_geom <- function(g, digits = 5) {
  st_sfc(lapply(g, function(p) {
    st_multipolygon(lapply(p, function(poly) lapply(poly, round, digits = digits)))
  }), crs = st_crs(g))
}
parts <- lapply(unique(long_p$survey_id), function(id) {
  x <- long_p[long_p$survey_id == id, ]
  st_sf(st_drop_geometry(x), geometry = simplify_coverage(x, TOL))
})
s <- st_transform(do.call(rbind, parts), 4326)
st_geometry(s) <- round_geom(st_geometry(s), 5)
st_geometry(s) <- st_cast(st_make_valid(st_geometry(s)), "MULTIPOLYGON")

# 4. Attributes ---------------------------------------------------------------
unit_names <- c(
  "0001" = "Barishal", "0002" = "Chattogram (incl. Sylhet)", "0003" = "Chattogram",
  "0004" = "Sylhet", "0005" = "Dhaka (incl. Mymensingh)", "0006" = "Khulna",
  "0007" = "Rajshahi (incl. Rangpur)", "0008" = "Rangpur", "0009" = "Rajshahi",
  "0010" = "Dhaka", "0011" = "Mymensingh"
)
unit_check <- c(
  "0001" = "baris", "0002" = "chatt|chitt", "0003" = "chatt|chitt", "0004" = "sylhet",
  "0005" = "dhaka", "0006" = "khulna", "0007" = "raj", "0008" = "rangpur",
  "0009" = "raj", "0010" = "dhaka", "0011" = "mymen"
)
code_2017 <- c(
  "0001" = 1L, "0003" = 2L, "0010" = 3L, "0006" = 4L, "0011" = 5L, "0009" = 6L,
  "0008" = 7L, "0004" = 8L
)
year_label <- c(
  BD1994DHS = "1993-94", BD1997DHS = "1996-97", BD1999SPA = "1999-2000",
  BD2000DHS = "1999-2000", BD2001OTH = "2001", BD2004DHS = "2004", BD2007DHS = "2007",
  BD2011DHS = "2011", BD2014DHS = "2014", BD2014SPA = "2014", BD2017DHS = "2017-18",
  BD2017SPA = "2017", BD2022DHS = "2022"
)

s$region_code <- s$REGCODE
i17 <- s$survey_id == "BD2017DHS"
s$region_code[i17] <- unname(code_2017[s$unit[i17]])
# DHS recode files: v024 (IR/KR/BR), hv024 (HR/PR), mv024 (MR) share codes
s$region_var <- ifelse(s$SVYTYPE == "DHS", "v024", s$REGVAR)

vl <- bdhs_value_labels[bdhs_value_labels$variable %in% c("v024", "hv024"), ]
vl <- vl[!duplicated(vl[c("survey_id", "value")]), ]
lab <- vl$label[match(paste(s$survey_id, s$region_code), paste(vl$survey_id, vl$value))]
s$code_verified <- !is.na(lab) & mapply(grepl, unit_check[s$unit], lab, ignore.case = TRUE)
s$code_verified[is.na(s$code_verified)] <- FALSE
if (any(!is.na(lab) & !s$code_verified)) {
  stop("Region codes contradict the microdata value labels.", call. = FALSE)
}

bdhs_boundaries <- st_sf(
  survey_id = s$survey_id,
  survey_type = s$SVYTYPE,
  year = as.integer(s$SVYYEAR),
  survey_year_label = unname(year_label[s$survey_id]),
  region_code = as.integer(s$region_code),
  region_name = unname(unit_names[s$unit]),
  region_var = s$region_var,
  unit_id = s$unit,
  reg_id = s$REG_ID,
  sdr_region_name = s$DHSREGEN,
  sdr_regcode = as.integer(s$REGCODE),
  sdr_regname = s$REGNAME,
  code_verified = s$code_verified,
  geometry = st_geometry(s)
)
bdhs_boundaries <- bdhs_boundaries[order(
  bdhs_boundaries$year, bdhs_boundaries$survey_type, bdhs_boundaries$region_code
), ]
row.names(bdhs_boundaries) <- NULL
stopifnot(
  nrow(bdhs_boundaries) == 86L,
  !anyNA(bdhs_boundaries$region_code),
  !anyDuplicated(st_drop_geometry(bdhs_boundaries)[c("survey_id", "region_code")]),
  all(st_is_valid(bdhs_boundaries)), !any(st_is_empty(bdhs_boundaries))
)
attr(bdhs_boundaries, "source") <- paste0(
  "Spatial Data Repository, The Demographic and Health Surveys Program. ICF International. ",
  "Available from spatialdata.dhsprogram.com [Accessed ", format(accessed, "%d %B %Y"), "]. ",
  "Licensed under ODbL 1.0 (database) / DbCL 1.0 (contents). Simplified (topology-preserving, ",
  TOL, " m tolerance in EPSG:32646) by the bdhs package build scripts."
)

saveRDS(bdhs_boundaries, file.path("inst", "extdata", "bdhs_boundaries.rds"), compress = "xz")
utils::write.csv(st_drop_geometry(bdhs_boundaries),
  file.path("data-raw", "inputs", "bdhs_boundaries_attributes.csv"),
  row.names = FALSE
)
message(
  "inst/extdata/bdhs_boundaries.rds: ", nrow(bdhs_boundaries), " rows, ",
  nrow(st_coordinates(bdhs_boundaries)), " vertices, ",
  round(file.size(file.path("inst", "extdata", "bdhs_boundaries.rds")) / 1024, 1), " KB"
)
