# Build the bundle-ready, topology-preserving simplified boundaries for all BD surveys.
# Method (pure sf/GEOS, no rmapshaper/V8 needed):
#   per survey: project to UTM 46N (EPSG:32646) -> extract shared linework (st_union of boundaries,
#   which nodes all arcs) -> st_line_merge -> st_simplify(preserveTopology = TRUE) on the WHOLE
#   multilinestring (GEOS TopologyPreservingSimplifier keeps arc end-nodes and prevents arcs from
#   crossing each other) -> st_polygonize -> assign faces back to regions by point-on-surface ->
#   dissolve (min_area = 0: keep every face so overlaps resolve without creating gaps) -> back to EPSG:4326, round to 1e-5 deg.
suppressPackageStartupMessages({ library(sf); library(dplyr) })
sf_use_s2(FALSE)
out <- "/private/tmp/claude-501/-Users-armanhossen-Documents-MU-Github/ad97b126-e74a-4eff-860f-a1892e94a270/scratchpad/research/spatial"
long_p <- readRDS(file.path(out, "sdr_long_utm46.rds"))

# ---- Region code crosswalk (code in the MICRODATA region variable) -------------------------
# Verified against value labels of v024/hv024 in local files where available (see REPORT.md):
#  1993-94 KR v024, 1999-2000 KR v024, 2004 KR v024, 2007 KR v024, 2011 KR v024, 2014 KR v024,
#  2017-18 KR/IR/HR/PR v024/hv024, 2022 KR/IR/PR v024/hv024.
#  1996-97 DHS, all SPA and BMMS 2001 codes are taken from SDR REGCODE (NOT verified locally).
#  NOTE: SDR REGCODE for BD2017DHS is WRONG for 4 divisions (it uses 5=Rajshahi, 6=Rangpur,
#  7=Sylhet, 8=Mymensingh) whereas the 2017-18 recode files use 5=mymensingh, 6=rajshahi,
#  7=rangpur, 8=sylhet. We override with the microdata coding.
code_2017_data <- c("0001" = 1, "0003" = 2, "0010" = 3, "0006" = 4, "0011" = 5, "0009" = 6, "0008" = 7, "0004" = 8)

# canonical modern English names per unit (Bangladesh 2018 official romanisation)
unit_names <- c("0001" = "Barishal", "0002" = "Chattogram (incl. Sylhet)", "0003" = "Chattogram",
                "0004" = "Sylhet", "0005" = "Dhaka (incl. Mymensingh)", "0006" = "Khulna",
                "0007" = "Rajshahi (incl. Rangpur)", "0008" = "Rangpur", "0009" = "Rajshahi",
                "0010" = "Dhaka", "0011" = "Mymensingh")

meta <- st_drop_geometry(long_p) |>
  transmute(survey_id, survey_type = SVYTYPE, year = as.integer(SVYYEAR), sdr_survey_num = as.integer(SVYID),
            reg_id = REG_ID, unit_id = unit, sdr_region_name = DHSREGEN,
            region_var = ifelse(REGVAR == "hv024", "hv024", REGVAR),
            sdr_regcode = as.integer(REGCODE), sdr_regname = REGNAME)
meta$region_code <- meta$sdr_regcode
i17 <- meta$survey_id == "BD2017DHS"
meta$region_code[i17] <- unname(code_2017_data[meta$unit_id[i17]])
meta$region_code <- as.integer(meta$region_code)
meta$region_var[i17] <- "hv024"   # SDR says "region"; the 2017-18 recode variable is hv024/v024
meta$region_name <- unname(unit_names[meta$unit_id])
meta$code_verified <- meta$survey_id %in% c("BD1994DHS", "BD2000DHS", "BD2004DHS", "BD2007DHS",
                                            "BD2011DHS", "BD2014DHS", "BD2017DHS", "BD2022DHS")
# survey-year label as DHS uses (e.g. 1993-94)
yl <- c(BD1994DHS = "1993-94", BD1997DHS = "1996-97", BD1999SPA = "1999-2000", BD2000DHS = "1999-2000",
        BD2001OTH = "2001", BD2004DHS = "2004", BD2007DHS = "2007", BD2011DHS = "2011",
        BD2014DHS = "2014", BD2014SPA = "2014", BD2017DHS = "2017-18", BD2017SPA = "2017",
        BD2022DHS = "2022")
meta$survey_year_label <- unname(yl[meta$survey_id])

simplify_coverage <- function(x, tol, min_area_m2) {
  g <- st_geometry(x)
  lines <- st_union(st_boundary(g))            # noded linework, shared edges appear once
  lines <- st_line_merge(lines)
  if (tol > 0) lines <- st_simplify(lines, preserveTopology = TRUE, dTolerance = tol)
  faces <- st_collection_extract(st_polygonize(lines), "POLYGON")
  faces <- st_sf(geometry = st_cast(faces, "POLYGON"))
  faces <- faces[as.numeric(st_area(faces)) > 0, ]
  pos <- st_point_on_surface(faces)
  idx <- st_intersects(pos, g)
  faces$owner <- vapply(idx, function(k) if (length(k)) k[1] else NA_integer_, 1L)
  faces <- faces[!is.na(faces$owner), ]
  faces <- faces[as.numeric(st_area(faces)) >= min_area_m2, ]
  res <- faces |> group_by(owner) |> summarise(do_union = TRUE, .groups = "drop")
  res <- st_cast(st_make_valid(res), "MULTIPOLYGON")
  geom <- st_geometry(res)[match(seq_len(nrow(x)), res$owner)]
  geom
}

round_geom <- function(g, digits = 5) {
  crs <- st_crs(g)
  st_sfc(lapply(g, function(p) st_multipolygon(lapply(p, function(poly) lapply(poly, round, digits = digits)))), crs = crs)
}

build <- function(tol, min_area_km2 = 0) {
  parts <- lapply(unique(long_p$survey_id), function(id) {
    x <- long_p[long_p$survey_id == id, ]
    geom <- simplify_coverage(x, tol, min_area_km2 * 1e6)
    st_sf(survey_id = x$survey_id, reg_id = x$REG_ID, unit_id = x$unit, geometry = geom)
  })
  s <- do.call(rbind, parts)
  s <- st_transform(s, 4326)
  st_geometry(s) <- round_geom(st_geometry(s), 5)
  st_geometry(s) <- st_cast(st_make_valid(st_geometry(s)), "MULTIPOLYGON")
  s
}

eval_one <- function(tol) {
  s <- build(tol)
  f <- tempfile(fileext = ".rds"); saveRDS(s, f, compress = "xz")
  f2 <- tempfile(fileext = ".rda"); obj <- s; save(obj, file = f2, compress = "xz")
  sp <- st_transform(s, 32646)
  a_new <- as.numeric(st_area(sp)) / 1e6
  key <- paste(long_p$survey_id, long_p$unit)
  a_old <- long_p$area_km2[match(paste(s$survey_id, s$unit_id), key)]
  # gaps/overlaps per survey after simplification
  go <- sapply(unique(s$survey_id), function(id) {
    z <- sp[sp$survey_id == id, ]
    sum(as.numeric(st_area(z))) / 1e6 - as.numeric(st_area(st_union(z))) / 1e6
  })
  data.frame(tol_m = tol, n_vertices = nrow(st_coordinates(s)), rds_kb = round(file.size(f) / 1024, 1),
             rda_xz_kb = round(file.size(f2) / 1024, 1), all_valid = all(st_is_valid(s)),
             any_empty = any(st_is_empty(s)),
             max_abs_area_change_pct = round(max(abs(a_new / a_old - 1)) * 100, 3),
             max_overlap_km2 = round(max(go), 4))
}

cat("Raw (unsimplified, deduplicated) vertex count:", nrow(st_coordinates(long_p)), "\n")
f <- tempfile(fileext = ".rds"); saveRDS(long_p, f, compress = "xz"); cat("Raw rds xz KB:", round(file.size(f) / 1024), "\n")
grid <- do.call(rbind, lapply(c(50, 100, 250, 500, 1000), eval_one))
print(grid)
write.csv(grid, file.path(out, "simplify_grid.csv"), row.names = FALSE)

# ---- final choice -------------------------------------------------------------------------
tol <- as.numeric(Sys.getenv("BDHS_TOL", "100"))
s <- build(tol)
s <- s |> left_join(meta, by = c("survey_id", "unit_id"), relationship = "one-to-one")
s$reg_id <- s$reg_id.x; s$reg_id.x <- NULL; s$reg_id.y <- NULL
bdhs_boundaries <- s |>
  transmute(survey_id, survey_type, year, survey_year_label, region_code, region_name,
            region_var, unit_id, reg_id, sdr_region_name, sdr_regcode, sdr_regname,
            code_verified, geometry) |>
  arrange(year, survey_type, region_code)
attr(bdhs_boundaries, "source") <- paste0(
  "Spatial Data Repository, The Demographic and Health Surveys Program. ICF International. ",
  "Available from spatialdata.dhsprogram.com [Accessed ", format(Sys.Date(), "%d %B %Y"), "]. ",
  "Licensed under ODbL 1.0 (database) / DbCL 1.0 (contents). Simplified (topology-preserving, ",
  tol, " m tolerance in EPSG:32646) by the bdhs package build scripts.")
saveRDS(bdhs_boundaries, file.path(out, "bdhs_boundaries.rds"), compress = "xz")
save(bdhs_boundaries, file = file.path(out, "bdhs_boundaries.rda"), compress = "xz")
cat("\nFinal tol =", tol, " rds KB =", round(file.size(file.path(out, "bdhs_boundaries.rds")) / 1024, 1),
    " rda KB =", round(file.size(file.path(out, "bdhs_boundaries.rda")) / 1024, 1), "\n")
print(st_drop_geometry(bdhs_boundaries), row.names = FALSE)
write.csv(st_drop_geometry(bdhs_boundaries), file.path(out, "bdhs_boundaries_attributes.csv"), row.names = FALSE)
