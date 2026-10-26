# =============================================================================
# data-raw/05-build-regions.R
#
# Builds `bdhs_regions`: one row per survey (all 13) x region code of the
# survey's region/division variable, with the division it denotes, its stable
# geographic unit, the DHS API region id and two trend-consistent groupings.
#
# Sources
#   data-raw/inputs/bdhs_boundaries_attributes.csv  region codes per survey, as
#       in inst/extdata/bdhs_boundaries.rds (SDR codes; the BD2017DHS SDR code
#       bug corrected to the microdata coding)
#   data-raw/inputs/region_units.csv  stable 4-digit geographic units
#   data/bdhs_value_labels.rda        value labels of v024/hv024 in the
#       maintainer's files (03-build-codebooks.R): exact labels + verification
#   inst/extdata/crosswalk.rds        division_surveyed / division_trend6 /
#       division_trend5 maps (cross-checked, never contradicted)
#
# Usage (from the package root, after 03-build-codebooks.R):
#   Rscript data-raw/05-build-regions.R
# Writes data/bdhs_regions.rda. Needs: tibble, usethis.
# =============================================================================

if (!file.exists("DESCRIPTION") || !dir.exists("data-raw")) {
  stop("Run this script from the bdhs package root.", call. = FALSE)
}

attrs <- utils::read.csv(file.path("data-raw", "inputs", "bdhs_boundaries_attributes.csv"),
  colClasses = c(unit_id = "character"), stringsAsFactors = FALSE
)
units <- utils::read.csv(file.path("data-raw", "inputs", "region_units.csv"),
  colClasses = "character", stringsAsFactors = FALSE
)
load(file.path("data", "bdhs_value_labels.rda"))
load(file.path("data", "bdhs_surveys.rda"))
load(file.path("data", "bdhs_indicators.rda"))
crosswalk <- readRDS(file.path("inst", "extdata", "crosswalk.rds"))

stopifnot(
  setequal(attrs$survey_id, bdhs_surveys$survey_id),
  !anyDuplicated(attrs[c("survey_id", "region_code")]),
  all(attrs$unit_id %in% units$unit_id)
)

# Unit -> division (canonical lower-case 2022 spelling) ----------------------
unit_division <- c(
  "0001" = "barishal", "0002" = "chattogram", "0003" = "chattogram",
  "0004" = "sylhet", "0005" = "dhaka", "0006" = "khulna", "0007" = "rajshahi",
  "0008" = "rangpur", "0009" = "rajshahi", "0010" = "dhaka", "0011" = "mymensingh"
)
# Later divisions contained in a pre-split unit
unit_includes <- c("0002" = "Sylhet", "0005" = "Mymensingh", "0007" = "Rangpur")
title_case <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

# 6-division trend grouping (Rangpur -> Rajshahi, Mymensingh -> Dhaka). The
# 1993-94 Chittagong region still contains Sylhet: it can only be split with
# the district variable, so trend6 is NA there.
trend6 <- c(
  barishal = "barishal", chattogram = "chattogram", dhaka = "dhaka",
  khulna = "khulna", mymensingh = "dhaka", rajshahi = "rajshahi",
  rangpur = "rajshahi", sylhet = "sylhet"
)
# 5-division grouping of 1993-94 (additionally Sylhet -> Chattogram)
trend5 <- trend6
trend5[["sylhet"]] <- "chattogram"

regions <- data.frame(
  survey_id = attrs$survey_id,
  survey_type = attrs$survey_type,
  region_var = attrs$region_var,
  region_code = as.integer(attrs$region_code),
  unit_id = attrs$unit_id,
  reg_id = ifelse(attrs$reg_id %in% "", NA_character_, attrs$reg_id),
  sdr_regname = attrs$sdr_regname,
  stringsAsFactors = FALSE
)
# DHS recode files: v024 (women/children/births), hv024 (households and
# household members) and mv024 (men) share the same codes.
regions$region_var[regions$survey_type == "DHS"] <- "v024"
regions$division <- unname(unit_division[regions$unit_id])
regions$division_label <- title_case(regions$division)
regions$includes <- unname(unit_includes[regions$unit_id])
regions$trend6 <- unname(trend6[regions$division])
regions$trend6[regions$unit_id == "0002"] <- NA_character_
regions$trend5 <- unname(trend5[regions$division])

# Exact labels and verification from the microdata value labels -------------
label_check <- c(
  barishal = "baris", chattogram = "chatt|chitt", dhaka = "dhaka", khulna = "khulna",
  mymensingh = "mymen", rajshahi = "raj", rangpur = "rangpur", sylhet = "sylhet"
)
vl <- bdhs_value_labels[bdhs_value_labels$variable %in% c("v024", "hv024", "mv024"), ]
vl <- vl[order(match(vl$variable, c("v024", "hv024", "mv024")), vl$recode), ]
vl <- vl[!duplicated(vl[c("survey_id", "value")]), ]
hit <- match(paste(regions$survey_id, regions$region_code), paste(vl$survey_id, vl$value))
regions$region_label <- vl$label[hit]
regions$code_verified <- !is.na(hit) &
  mapply(grepl, label_check[regions$division], regions$region_label, ignore.case = TRUE)
regions$code_verified[is.na(regions$code_verified)] <- FALSE
bad <- !is.na(hit) & !regions$code_verified
if (any(bad)) {
  print(regions[bad, c("survey_id", "region_code", "division", "region_label")])
  stop("Region codes contradict the microdata value labels.", call. = FALSE)
}
# Every code of a verified survey must be labelled (no missing divisions)
verified_surveys <- unique(vl$survey_id)
stopifnot(all(regions$code_verified[regions$survey_id %in% verified_surveys]))
regions$label_source <- ifelse(regions$code_verified, "microdata", "sdr")
regions$region_label[!regions$code_verified] <- regions$sdr_regname[!regions$code_verified]

# Notes ----------------------------------------------------------------------
regions$note <- NA_character_
regions$note[regions$survey_id == "BD1994DHS" & regions$region_code == 5] <-
  "File value label is misspelled 'rajshani'."
regions$note[regions$survey_id == "BD1994DHS" & regions$region_code == 2] <-
  "Includes Sylhet (division created 1995); trend6 needs the district variable (sdistr)."
regions$note[regions$survey_id == "BD2017DHS"] <-
  "SDR boundary REGCODE differs for codes 5-8 (SDR: 5 Rajshahi, 6 Rangpur, 7 Sylhet, 8 Mymensingh); microdata coding used."
regions$note[regions$survey_id == "BD2017SPA"] <-
  "SDR region codes, unverified: the same SDR coding is wrong for BD2017DHS, so check the SPA file's labels."
regions$note[regions$survey_id %in% c("BD1999SPA", "BD2001OTH", "BD2014SPA", "BD1997DHS") &
  is.na(regions$note)] <- "Codes and labels from the DHS Spatial Data Repository; not checked against microdata."

# Cross-checks against the crosswalk and the DHS API -------------------------
cw_names <- vapply(crosswalk$variables, function(v) v$name, "")
cw_map <- function(name) {
  v <- crosswalk$variables[[match(name, cw_names)]]
  do.call(rbind, lapply(names(v$rounds), function(s) {
    m <- v$rounds[[s]]$map
    if (is.null(m)) {
      return(NULL)
    }
    data.frame(survey_id = s, region_code = as.integer(names(m)), value = unlist(m), stringsAsFactors = FALSE)
  }))
}
check_against <- function(name, column) {
  m <- cw_map(name)
  j <- match(paste(m$survey_id, m$region_code), paste(regions$survey_id, regions$region_code))
  stopifnot(!anyNA(j))
  ok <- regions[[column]][j] == m$value
  if (!all(ok %in% TRUE)) {
    print(cbind(m[!ok %in% TRUE, ], ours = regions[[column]][j][!ok %in% TRUE]))
    stop("bdhs_regions$", column, " disagrees with crosswalk ", name, call. = FALSE)
  }
}
check_against("division_surveyed", "division")
check_against("division_trend6", "trend6")
check_against("division_trend5", "trend5")

api_regions <- unique(bdhs_indicators$region_id[!is.na(bdhs_indicators$region_id)])
stopifnot(all(regions$reg_id[regions$survey_type == "DHS"] %in% api_regions))

# Write -----------------------------------------------------------------------
survey_order <- bdhs_surveys$survey_id[order(bdhs_surveys$survey_year, bdhs_surveys$survey_type)]
regions <- regions[order(match(regions$survey_id, survey_order), regions$region_code), ]
bdhs_regions <- tibble::as_tibble(regions[, c(
  "survey_id", "region_var", "region_code", "region_label", "division",
  "division_label", "includes", "unit_id", "reg_id", "trend6", "trend5",
  "code_verified", "label_source", "note"
)])
stopifnot(nrow(bdhs_regions) == 86L, !anyDuplicated(bdhs_regions[c("survey_id", "region_code")]))
usethis::use_data(bdhs_regions, overwrite = TRUE, compress = "xz")
print(as.data.frame(bdhs_regions[, 1:12]), row.names = FALSE)
