# Official DHS indicator estimates for Bangladesh

Every indicator value the DHS Program publishes for the nine Bangladesh
DHS rounds (1993-94 to 2022), nationally and by region and background
characteristics, as released through the DHS Program Indicator Data API.
These are the official estimates of the survey final reports and
STATcompiler; use them directly or to validate your own estimates.

## Usage

``` r
bdhs_indicators
```

## Format

A tibble with 310,528 rows and 25 columns:

- survey_id:

  DHS survey identifier, e.g. `"BD2022DHS"`.

- survey_year:

  Survey year.

- survey_label:

  Year label, e.g. `"2017-18"`.

- indicator_id:

  DHS indicator id, e.g. `"FE_FRTR_W_TFR"` (total fertility rate). A few
  ids are reused for two indicators; the pair (`indicator_id`,
  `indicator_order`) is unique (see
  [bdhs_indicator_catalog](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_catalog.md)).

- indicator_order:

  DHS sort order of the indicator; together with `indicator_id` it
  identifies the indicator.

- indicator:

  Indicator label.

- indicator_type:

  Type of value: `"I"` indicator, `"D"` weighted denominator, `"U"`
  unweighted denominator, `"T"` distribution total (100 percent), `"S"`
  special answers (don't know/missing), `"E"` sampling error, `"C"`
  confidence interval, `"N"` number of cases (e.g. women interviewed).

- breakdown:

  `"national"` (the total, `is_total`), `"subnational"` (by region) or
  `"background"` (any other characteristic). The DHS API also returns
  subnational rows for `breakdown=background`.

- characteristic_id:

  DHS characteristic id (1000 = total).

- characteristic_category:

  Characteristic category, e.g. `"Residence"`, `"Region"`,
  `"Wealth quintile"`, `"Total"`.

- characteristic_label:

  Characteristic, e.g. `"Urban"`. Region labels use the current
  spellings; labels starting with `".."` are subregions of the region
  above (e.g. `"..Rangpur"` within `"Rajshahi/Rangpur"`), so
  trend-consistent aggregates and the regions as surveyed are both
  present.

- characteristic_order:

  DHS sort order of the characteristic.

- by_variable_id:

  Id of a further disaggregation (0 = none).

- by_variable_label:

  Label of the further disaggregation, e.g. a reference period such as
  `"Five years preceding the survey"`, or an age group; `NA` if none.

- region_id:

  DHS API region id for regional rows (e.g. `"BDDHS2022360004"`); joins
  to `reg_id` in
  [bdhs_regions](https://armanhossen-am.github.io/bdhs/reference/bdhs_regions.md)
  and in
  [`bdhs_boundaries()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_boundaries.md).
  `NA` for other rows.

- level_rank:

  Administrative level of a regional row (1 = the regions as surveyed;
  `NA` for trend aggregates and non-regional rows).

- value:

  The estimate, in the units of the indicator (percent, rate per 1,000,
  mean, number, ...).

- precision:

  Number of decimals with which the DHS Program presents the value.

- denom_weighted, denom_unweighted:

  Weighted and unweighted denominators (number of cases), where
  published.

- ci_low, ci_high:

  Lower and upper bounds of the 95 percent confidence interval;
  published only for a few indicators (e.g. maternal mortality).

- is_total:

  `TRUE` for the national total.

- is_preferred:

  `TRUE` for the DHS Program's preferred (headline) value when an
  indicator is published several ways (e.g. for several reference
  periods).

- default_view:

  `TRUE` for the preferred row of a survey x indicator x characteristic
  cell, or for every row of a cell that has no preferred row (e.g.
  mortality by wealth, published only for the 10-year period).

## Source

The DHS Program Indicator Data API, `data` endpoint (`breakdown=all`),
<https://api.dhsprogram.com>, API version 8.1, retrieved 2026-10-26:
"The DHS Program Indicator Data API, The Demographic and Health Surveys
(DHS) Program. ICF. Originally funded by the United States Agency for
International Development (USAID). Available from api.dhsprogram.com.
\[Accessed 2026-10-26\]". Built by `data-raw/01-fetch-dhs-api.R` and
`data-raw/02-build-api-datasets.R`.

## Details

One row is one value: survey x indicator x characteristic (e.g. "Urban",
"Rajshahi", "Highest wealth quintile") x by-variable (e.g. reference
period). Where an indicator is published several ways, `is_preferred`
flags the headline value; `default_view` keeps exactly what a user
should see by default.

The API returned 321,440 rows on 2026-10-26, of which 10,912 were exact
duplicates (every 2022 region row twice, an artefact of the API's
geometry table); they have been removed. Non-preferred rows are kept.

## See also

[bdhs_indicator_catalog](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_catalog.md)
for definitions,
[`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md)
to query these values,
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
and
[`bdhs_validate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validate.md)
to reproduce them from microdata.

## Examples

``` r
# Total fertility rate, national, every round
tfr <- bdhs_indicators[bdhs_indicators$indicator_id == "FE_FRTR_W_TFR" &
  bdhs_indicators$is_total & bdhs_indicators$is_preferred, ]
tfr[, c("survey_label", "value")]
#> # A tibble: 9 × 2
#>   survey_label value
#>   <chr>        <dbl>
#> 1 1993-94        3.4
#> 2 1996-97        3.3
#> 3 1999-00        3.3
#> 4 2004           3  
#> 5 2007           2.7
#> 6 2011           2.3
#> 7 2014           2.3
#> 8 2017-18        2.3
#> 9 2022           2.3

# Under-5 mortality by division, 2022 (regions as surveyed)
u5 <- bdhs_indicators[bdhs_indicators$indicator_id == "CM_ECMR_C_U5M" &
  bdhs_indicators$survey_id == "BD2022DHS" &
  bdhs_indicators$breakdown == "subnational" &
  bdhs_indicators$level_rank %in% 1 & bdhs_indicators$default_view, ]
u5[, c("characteristic_label", "by_variable_label", "value")]
#> # A tibble: 8 × 3
#>   characteristic_label by_variable_label              value
#>   <chr>                <chr>                          <dbl>
#> 1 Barishal             Ten years preceding the survey    43
#> 2 ..Chattogram         Ten years preceding the survey    36
#> 3 ..Sylhet             Ten years preceding the survey    61
#> 4 ..Dhaka              Ten years preceding the survey    36
#> 5 ..Mymensingh         Ten years preceding the survey    44
#> 6 Khulna               Ten years preceding the survey    31
#> 7 ..Rangpur            Ten years preceding the survey    46
#> 8 ..Rajshahi           Ten years preceding the survey    27
```
