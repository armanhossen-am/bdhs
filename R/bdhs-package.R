#' bdhs: Tools and Data for the Bangladesh Demographic and Health Survey
#'
#' @description
#' `bdhs` covers every round of the Bangladesh Demographic and Health Survey
#' (1993-94, 1996-97, 1999-2000, 2004, 2007, 2011, 2014, 2017-18 and 2022)
#' together with the Bangladesh Service Provision Assessments and the 2001
#' Maternal Health Services and Maternal Mortality Survey.
#'
#' The package has two halves:
#'
#' * **Bundled, ready-to-use data.** Official estimates for every DHS indicator
#'   published for Bangladesh ([bdhs_indicators]), the indicator definitions
#'   ([bdhs_indicator_catalog]), survey and dataset catalogues ([bdhs_surveys],
#'   [bdhs_datasets]), cross-round codebooks ([bdhs_codebook]), division
#'   crosswalks ([bdhs_regions]) and subnational boundaries
#'   ([bdhs_boundaries()]).
#' * **Your own microdata, made easy.** The DHS Program licenses respondent-level
#'   data to registered users only and does not allow it to be redistributed,
#'   so it is not shipped with the package. Once you have downloaded it,
#'   [bdhs_import()] registers it in a fast local store, [bdhs_read()] loads any
#'   round and recode in one line, [bdhs_harmonize()] makes variables comparable
#'   across rounds, [bdhs_design()] builds the correct complex survey design and
#'   [bdhs_estimate()] computes indicators that are validated against the
#'   official DHS values.
#'
#' @section Getting access to the microdata:
#' Register at <https://dhsprogram.com/data/new-user-registration.cfm>, request
#' access to the Bangladesh surveys, download the files (Stata format is
#' recommended) and run `bdhs_import("path/to/downloads")`.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data .env %||%
## usethis namespace: end
NULL
