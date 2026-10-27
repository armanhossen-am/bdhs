# Validate indicator calculations against official DHS values

Computes national estimates with
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
and compares them with the official values published by the DHS Program
API (bundled as
[bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md)).
Use it to check that your copy of the data and the package's definitions
reproduce the published figures before analysing subgroups or pooled
rounds.

## Usage

``` r
bdhs_validate(data, indicators = NULL, tolerance = 0.5, survey = NULL)
```

## Arguments

- data:

  A BDHS recode data frame for one round (or pooled rounds).

- indicators:

  Indicator ids to validate. `NULL` (default) uses every indicator for
  the recode and round that has an official DHS value.

- tolerance:

  Allowed absolute difference, in the units of the official value
  (percentage points for percentages).

- survey:

  Optional survey reference, overriding detection.

## Value

A tibble with columns `survey_id`, `indicator`, `label`, `api_id`,
`computed`, `official`, `diff` (computed - official), `tolerance`
(effective tolerance), `pass`, `n_unweighted` and
`official_n_unweighted` (the official unweighted denominator, when
published).

## Details

The official value is the national total (`is_total`) for the round,
taken for the indicator's reference period (`by_variable_label`, e.g.
"Three years preceding the survey") when the indicator defines one, and
the DHS-preferred value (`is_preferred`) otherwise. Indicators defined
as a sum of API categories (for example postnatal care within two days)
are compared with the sum.

A computation passes when `abs(computed - official)` does not exceed
`tolerance` (scaled for indicators in other units, e.g. 0.1 x
`tolerance` for the total fertility rate) plus half a unit of the
official value's rounding precision.

## Examples

``` r
# Simulated data are random, so they are not expected to pass:
kr <- bdhs_simulate(2017, "KR")
bdhs_validate(kr, c("rh_del_facility", "rh_del_csection"))
#> # A tibble: 2 × 11
#>   survey_id indicator       label api_id computed official  diff tolerance pass 
#>   <chr>     <chr>           <chr> <chr>     <dbl>    <dbl> <dbl>     <dbl> <lgl>
#> 1 BD2017DHS rh_del_facility Deli… RH_DE…     54.7     49.4  5.34      0.55 FALSE
#> 2 BD2017DHS rh_del_csection Deli… RH_DE…     33.7     32.7  1.03      0.55 FALSE
#> # ℹ 2 more variables: n_unweighted <int>, official_n_unweighted <dbl>
```
