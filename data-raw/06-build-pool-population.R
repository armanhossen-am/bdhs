# =============================================================================
# data-raw/06-build-pool-population.R
#
# Builds `bdhs_pool_population`: for each DHS round, the national female
# population aged 15-49 at the survey's reference date (UN World Population
# Prospects 2024) and the number of women interviewed. bdhs_pool() uses the
# ratio female_15_49 / n_women_sample to rescale DHS weights (which are
# normalised to the sample size of each round) so that pooled rounds are
# weighted in proportion to the population they represent.
#
# Sources
#   data-raw/inputs/wpp2024_bd_female15_49_annual.csv  UN WPP 2024, Bangladesh,
#       female population 15-49 and total population (thousands, 1 July
#       estimates, 1990-2025; medium variant).
#   data-raw/inputs/bdhs_pooling_population.csv  per-round reference dates
#       (ref_year: mean interview date (v008) of the maintainer's microdata
#       where available, fieldwork midpoint otherwise) and the API sample size.
#   data/bdhs_indicators.rda  share of women 15-49 never married
#       (MA_MSTA_W_NMA, all-women denominator) -> p_ever_married.
#
# The WPP estimate is linearly interpolated between the 1 July values that
# bracket ref_year (WPP year t is centred on t + 0.5).
#
# Usage (from the package root, after 02-build-api-datasets.R):
#   Rscript data-raw/06-build-pool-population.R
# Writes data/bdhs_pool_population.rda. Needs: tibble, usethis.
# =============================================================================

if (!file.exists("DESCRIPTION") || !dir.exists("data-raw")) {
  stop("Run this script from the bdhs package root.", call. = FALSE)
}

wpp <- utils::read.csv(file.path("data-raw", "inputs", "wpp2024_bd_female15_49_annual.csv"))
pool <- utils::read.csv(file.path("data-raw", "inputs", "bdhs_pooling_population.csv"),
  stringsAsFactors = FALSE
)
load(file.path("data", "bdhs_surveys.rda"))
load(file.path("data", "bdhs_indicators.rda"))

interp <- function(x) {
  stats::approx(wpp$Time + 0.5, wpp$female_15_49 * 1000, xout = x, rule = 1)$y
}
female <- round(interp(pool$ref_year))
stopifnot(
  !anyNA(female),
  # agrees with the values computed during the research phase
  all(abs(female - pool$wpp_female_15_49) <= 1)
)

sv <- bdhs_surveys[match(pool$SurveyId, bdhs_surveys$survey_id), ]
stopifnot(identical(as.integer(pool$n_women_api), sv$n_women))

never <- bdhs_indicators[bdhs_indicators$indicator_id == "MA_MSTA_W_NMA" &
  bdhs_indicators$is_total & bdhs_indicators$is_preferred, c("survey_id", "value")]
p_ever_married <- 1 - never$value[match(pool$SurveyId, never$survey_id)] / 100
stopifnot(!anyNA(p_ever_married), all(abs(p_ever_married - pool$p_ever_married) < 1e-9))

bdhs_pool_population <- tibble::tibble(
  survey_id = pool$SurveyId,
  survey_label = sv$survey_label,
  ref_year = round(pool$ref_year, 3),
  female_15_49 = female,
  n_women_sample = as.integer(pool$n_women_api),
  women_age_min = sv$women_age_min,
  women_age_max = sv$women_age_max,
  p_ever_married = p_ever_married,
  source = paste0(
    "female_15_49: UN World Population Prospects 2024 (medium variant), ",
    "interpolated to ref_year; n_women_sample: DHS Program API (",
    "ever-married women interviewed)"
  )
)
bdhs_pool_population <- bdhs_pool_population[order(bdhs_pool_population$ref_year), ]
stopifnot(nrow(bdhs_pool_population) == 9L, !anyDuplicated(bdhs_pool_population$survey_id))
usethis::use_data(bdhs_pool_population, overwrite = TRUE, compress = "xz")
print(as.data.frame(bdhs_pool_population[, 1:8]))
