suppressPackageStartupMessages({ library(sf); library(dplyr); library(ggplot2); library(jsonlite) })
sf_use_s2(FALSE)
out <- "/private/tmp/claude-501/-Users-armanhossen-Documents-MU-Github/ad97b126-e74a-4eff-860f-a1892e94a270/scratchpad/research/spatial"
b <- readRDS(file.path(out, "bdhs_boundaries.rds"))
long_p <- readRDS(file.path(out, "sdr_long_utm46.rds"))
cat("class:", class(b), "\nCRS:", st_crs(b)$epsg, "\nrows:", nrow(b), "\n")
cat("object.size (MB):", round(as.numeric(object.size(b)) / 2^20, 2), "\n")
bp <- st_transform(b, 32646)
bp$area_km2 <- as.numeric(st_area(bp)) / 1e6
bp$nparts <- sapply(st_geometry(bp), length)
bp$npts <- sapply(st_geometry(bp), function(z) nrow(st_coordinates(z)))
key <- paste(long_p$survey_id, long_p$unit)
m <- match(paste(bp$survey_id, bp$unit_id), key)
bp$raw_area <- long_p$area_km2[m]; bp$raw_parts <- long_p$nparts[m]; bp$raw_pts <- long_p$npts[m]
# Hausdorff distance raw vs simplified (m)
bp$hausdorff_m <- sapply(seq_len(nrow(bp)), function(i)
  as.numeric(st_distance(st_boundary(st_geometry(bp)[i]), st_boundary(st_geometry(long_p)[m[i]]), which = "Hausdorff")))
per <- st_drop_geometry(bp) |> group_by(survey_id) |>
  summarise(n = n(), raw_pts = sum(raw_pts), pts = sum(npts), raw_parts = sum(raw_parts), parts = sum(nparts),
            max_area_change_pct = round(max(abs(area_km2 / raw_area - 1)) * 100, 3),
            max_hausdorff_m = round(max(hausdorff_m)))
print(as.data.frame(per))
# per-survey size if bundled alone
sz <- sapply(split(b, b$survey_id), function(z) { f <- tempfile(); saveRDS(z, f, compress = "xz"); file.size(f) / 1024 })
cat("\nPer-survey xz rds KB if saved alone:\n"); print(round(sz, 1))
# deduplicated 'units' alternative: one geometry per distinct (geometry family, unit)
fam <- c(BD1994DHS = "A", BD1997DHS = "A", BD1999SPA = "A", BD2000DHS = "A", BD2001OTH = "A", BD2004DHS = "A",
         BD2007DHS = "A", BD2011DHS = "A", BD2014DHS = "A", BD2014SPA = "B", BD2017DHS = "B", BD2017SPA = "B", BD2022DHS = "C")
b$geom_family <- unname(fam[b$survey_id])
u <- b[!duplicated(paste(b$geom_family, b$unit_id)), c("geom_family", "unit_id", "region_name")]
f <- tempfile(); saveRDS(u, f, compress = "xz")
cat("\nDistinct geometry units:", nrow(u), " xz rds KB:", round(file.size(f) / 1024, 1), "\n")

# ---- join test 1: DHS API subnational TFR (RegionId) -> reg_id -----------------------------
api <- fromJSON("https://api.dhsprogram.com/rest/dhs/data?countryIds=BD&indicatorIds=FE_FRTR_W_TFR&breakdown=subnational&f=json&perpage=500")$Data
api <- api[!duplicated(api[, c("SurveyId", "RegionId")]), ]
j <- b |> filter(survey_type == "DHS") |> left_join(api[, c("RegionId", "CharacteristicLabel", "Value")], by = c("reg_id" = "RegionId"))
cat("\nAPI join: DHS boundary rows =", sum(b$survey_type == "DHS"), " matched =", sum(!is.na(j$Value)), "\n")
print(st_drop_geometry(j)[, c("survey_id", "region_code", "region_name", "CharacteristicLabel", "Value")] |> head(12), row.names = FALSE)

# ---- visual check -------------------------------------------------------------------------
j$lab <- paste0(j$region_code, " ", j$region_name)
p <- ggplot(j) + geom_sf(aes(fill = Value), colour = "white", linewidth = 0.15) +
  facet_wrap(~ paste(survey_id, "(", survey_year_label, ")")) +
  scale_fill_viridis_c(name = "TFR (DHS API)", direction = -1) +
  theme_void(base_size = 8) + theme(strip.text = element_text(size = 7))
ggsave(file.path(out, "check_map_tfr.png"), p, width = 9, height = 8, dpi = 110)
# raw vs simplified overlay for 2022 (zoom on coast)
r22 <- st_transform(long_p[long_p$survey_id == "BD2022DHS", ], 4326)
s22 <- b[b$survey_id == "BD2022DHS", ]
p2 <- ggplot() + geom_sf(data = r22, fill = NA, colour = "grey40", linewidth = 0.3) +
  geom_sf(data = s22, fill = NA, colour = "red", linewidth = 0.2) +
  coord_sf(xlim = c(90.2, 91.4), ylim = c(21.8, 22.9)) + theme_minimal(base_size = 8) +
  ggtitle("BD2022DHS: raw SDR (grey) vs simplified 100 m (red), Meghna estuary")
ggsave(file.path(out, "check_overlay_2022_coast.png"), p2, width = 6, height = 5, dpi = 110)
p3 <- ggplot(b) + geom_sf(aes(fill = factor(region_code)), colour = "black", linewidth = 0.15) +
  geom_sf_text(aes(label = paste0(region_code, ":", substr(region_name, 1, 5))), size = 1.8) +
  facet_wrap(~ survey_id) + theme_void(base_size = 8) + theme(legend.position = "none")
ggsave(file.path(out, "check_map_codes.png"), p3, width = 9, height = 9, dpi = 110)
cat("maps written\n")
