# DHS recode (file type) dictionary ---------------------------------------------

.recodes <- tibble::tribble(
  ~recode, ~name,                                    ~unit,                         ~family,
  "IR",    "Individual (women's) Recode",            "Woman",                       "DHS",
  "MR",    "Men's Recode",                           "Man",                         "DHS",
  "KR",    "Children's Recode",                      "Child under 5 (born to interviewed women)", "DHS",
  "BR",    "Births Recode",                          "Birth (full birth history)",  "DHS",
  "HR",    "Household Recode",                       "Household",                   "DHS",
  "PR",    "Household Member Recode",                "Household member",            "DHS",
  "CR",    "Couples' Recode",                        "Couple",                      "DHS",
  "GR",    "Pregnancies Recode",                     "Pregnancy (pregnancy history)", "DHS",
  "NR",    "Pregnancy and Postnatal Care Recode",    "Recent pregnancy",            "DHS",
  "HW",    "Height and Weight (WHO Growth Standards)", "Child measured in household", "DHS",
  "WI",    "Wealth Index",                           "Household",                   "DHS",
  "SQ",    "Service Availability",                   "Cluster (community)",         "DHS",
  "FW",    "Fieldworker Questionnaire",              "Fieldworker",                 "DHS",
  "VA",    "Verbal Autopsy",                         "Death",                       "DHS",
  "GE",    "Geographic Data (cluster GPS)",          "Cluster",                     "Spatial",
  "GC",    "Geospatial Covariates",                  "Cluster",                     "Spatial",
  "FC",    "Facility Inventory",                     "Health facility",             "SPA",
  "PV",    "Provider Interview",                     "Health provider",             "SPA",
  "SL",    "Staff/Provider Listing",                 "Staff member",                "SPA",
  "CO",    "Community",                              "Community",                   "SPA",
  "HH",    "Household (raw)",                        "Household",                   "OTH",
  "IQ",    "Individual (raw)",                       "Woman",                       "OTH"
)

#' DHS recode file types
#'
#' Lists the DHS recode (file type) codes that occur for Bangladesh surveys,
#' with the unit of observation of each file. Use these codes wherever a
#' function takes a `recode` argument.
#'
#' @return A tibble with columns `recode`, `name`, `unit` and `family`.
#' @export
#' @examples
#' bdhs_recodes()
bdhs_recodes <- function() {
  .recodes
}

# Validate and normalise a recode code ("ir" -> "IR").
check_recode <- function(recode, arg = rlang::caller_arg(recode),
                         call = rlang::caller_env()) {
  recode <- toupper_trim(recode)
  if (length(recode) != 1L || is.na(recode) || !recode %in% .recodes$recode) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be one DHS recode code.",
        "i" = "Valid codes: {.val {(.recodes$recode)}}."
      ),
      call = call
    )
  }
  recode
}
