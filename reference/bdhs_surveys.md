# Bangladesh surveys of the DHS Program

Catalogue of the 13 Bangladesh surveys distributed by the DHS Program:
the nine Demographic and Health Surveys (1993-94 to 2022), the three
Service Provision Assessments (1999-2000, 2014, 2017) and the 2001
Maternal Health Services and Maternal Mortality Survey (BMMS, survey
type `"OTH"`).

## Usage

``` r
bdhs_surveys
```

## Format

A tibble with 13 rows (one per survey) and 29 columns:

- survey_id:

  DHS survey identifier, e.g. `"BD2017DHS"`; the key used throughout the
  package (see
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md)).

- survey_type:

  `"DHS"`, `"SPA"` or `"OTH"` (BMMS 2001).

- survey_year:

  Survey year as used in the survey id (end year of the fieldwork for
  surveys spanning two years).

- survey_label:

  Year label used by the DHS Program, e.g. `"2017-18"`.

- survey_num:

  Numeric DHS survey number (`SurveyNum`); also the survey key of the
  DHS Spatial Data Repository.

- recode_version:

  Two-character version code of the survey's recode files, e.g. `"7R"`
  in `BDIR7RFL`.

- dhs_phase:

  DHS phase (first digit of `recode_version`).

- has_indicator_data:

  Whether the DHS API publishes indicator values for the survey (only
  the nine DHS rounds; see
  [bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md)).

- status:

  Survey status in the DHS API.

- fieldwork_start, fieldwork_end:

  First and last month of fieldwork (dates set to the first day of the
  month).

- publication_date, release_date:

  Dates of the final report and of the data release.

- implementing_org:

  Implementing organization(s).

- n_clusters:

  Number of sample points (clusters, PSUs); for SPAs, the number of
  facilities in the sample.

- n_households:

  Number of households interviewed.

- women_universe:

  Population of the women's questionnaire (always `"Ever Married Women"`
  for the DHS rounds).

- n_women:

  Number of women interviewed.

- women_age_min, women_age_max:

  Age range of eligible women (10-49 in 1993-94 to 2004 and 12-49 in
  2011; DHS indicators refer to women 15-49).

- men_universe:

  Population of the men's questionnaire, if any.

- n_men:

  Number of men interviewed.

- men_age_min, men_age_max:

  Age range of eligible men.

- n_facilities:

  Number of facilities: SPA facilities, or for the 1993-94 and 1996-97
  DHS the facilities of the service availability survey.

- footnotes:

  DHS API footnotes about the survey.

- characteristic_ids:

  List column of DHS survey-characteristic ids (topics covered, e.g.
  anthropometry, biomarkers).

- report_url:

  URL of the survey's final report (PDF).

- indicators_updated:

  Date of the most recent revision of the survey's indicator values in
  the DHS API, if any was announced.

## Source

The DHS Program Indicator Data API, <https://api.dhsprogram.com>
(`surveys`, `datasets`, `publications`, `surveycharacteristics` and
`dataupdates` endpoints), API version 8.1, retrieved 2026-10-26. Built
by `data-raw/01-fetch-dhs-api.R` and `data-raw/02-build-api-datasets.R`.

## Details

Every BDHS women's sample is a sample of **ever-married** women; see
`women_universe`.
[`bdhs_rounds()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_rounds.md)
gives a compact version of this table that does not depend on the
bundled data.

## See also

[`bdhs_rounds()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_rounds.md),
[`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md),
[bdhs_datasets](https://armanhossen-am.github.io/bdhs/reference/bdhs_datasets.md),
[bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md).

## Examples

``` r
bdhs_surveys[, c("survey_id", "survey_label", "n_clusters", "n_women")]
#> # A tibble: 13 × 4
#>    survey_id survey_label n_clusters n_women
#>    <chr>     <chr>             <int>   <int>
#>  1 BD1994DHS 1993-94             301    9640
#>  2 BD1997DHS 1996-97             313    9127
#>  3 BD1999SPA 1999-00             341      NA
#>  4 BD2000DHS 1999-00             341   10544
#>  5 BD2001OTH 2001                808  103796
#>  6 BD2004DHS 2004                361   11440
#>  7 BD2007DHS 2007                361   10996
#>  8 BD2011DHS 2011                600   17842
#>  9 BD2014DHS 2014                600   17863
#> 10 BD2014SPA 2014               1548      NA
#> 11 BD2017DHS 2017-18             672   20127
#> 12 BD2017SPA 2017               1600      NA
#> 13 BD2022DHS 2022                675   30078

# Rounds with official indicator values
bdhs_surveys$survey_id[bdhs_surveys$has_indicator_data]
#> [1] "BD1994DHS" "BD1997DHS" "BD2000DHS" "BD2004DHS" "BD2007DHS" "BD2011DHS"
#> [7] "BD2014DHS" "BD2017DHS" "BD2022DHS"
```
