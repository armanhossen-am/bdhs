# Childhood mortality rates

Computes neonatal (NNMR), postneonatal (PNNMR), infant (IMR), child
(CMR, 4q1) and under-five (U5MR) mortality rates per 1,000 live births
with the DHS synthetic cohort life-table method: probabilities of dying
are estimated for the age segments 0, 1-2, 3-5, 6-11, 12-23, 24-35,
36-47 and 48-59 months from the births and deaths in the reference
period, counting half of the exposure (and deaths) of cohorts only
partly exposed at the start of the period, and half of the exposure of
cohorts partly exposed at its end. Standard errors use the DHS jackknife
over clusters.

## Usage

``` r
bdhs_mortality(
  data,
  period = 5,
  rates = c("nnmr", "pnnmr", "imr", "cmr", "u5mr"),
  by = NULL,
  se = TRUE,
  level = 0.95,
  survey = NULL
)
```

## Arguments

- data:

  A births recode (BR, one row per birth with `b3`, `b7`) or a women's
  recode (IR, wide birth history `b3_01`, `b7_01`, ...), with `v008`,
  `v005` and `v021`.

- period:

  Length of the reference period in **years** before the interview (DHS
  standard: 5; 10 for subgroups).

- rates:

  Which rates to return.

- by:

  Optional character vector of grouping variables (birth- or
  woman-level).

- se:

  If `TRUE` (default), compute jackknife standard errors and confidence
  intervals.

- level:

  Confidence level.

- survey:

  Optional survey reference, overriding detection.

## Value

A tibble with columns `survey_id`, the `by` variables, `indicator`,
`estimate` (per 1,000 live births), `se`, `ci_low`, `ci_high`,
`n_births` (unweighted births contributing to the rates), `n_deaths`,
`n_psu` and `period` (years).

## See also

[`bdhs_fertility()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_fertility.md),
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)

## Examples

``` r
br <- bdhs_simulate(2017, "BR")
bdhs_mortality(br, se = FALSE)
#> # A tibble: 5 × 10
#>   survey_id indicator estimate    se ci_low ci_high n_births n_deaths n_psu
#>   <chr>     <chr>        <dbl> <dbl>  <dbl>   <dbl>    <int>    <int> <int>
#> 1 BD2017DHS nnmr          33.8    NA     NA      NA      608       25    40
#> 2 BD2017DHS pnnmr         27.7    NA     NA      NA      608       25    40
#> 3 BD2017DHS imr           61.5    NA     NA      NA      608       25    40
#> 4 BD2017DHS cmr           20.2    NA     NA      NA      608       25    40
#> 5 BD2017DHS u5mr          80.5    NA     NA      NA      608       25    40
#> # ℹ 1 more variable: period <dbl>
bdhs_mortality(br, period = 10, rates = c("imr", "u5mr"), by = "v025")
#> # A tibble: 4 × 11
#>   survey_id v025  indicator estimate    se ci_low ci_high n_births n_deaths
#>   <chr>     <fct> <chr>        <dbl> <dbl>  <dbl>   <dbl>    <int>    <int>
#> 1 BD2017DHS urban imr           57.4  14.1   29.8    85.1      453       24
#> 2 BD2017DHS urban u5mr          75.9  15.8   44.8   107.       453       24
#> 3 BD2017DHS rural imr           53.5  11.9   30.2    76.8      538       24
#> 4 BD2017DHS rural u5mr          74.2  16.7   41.5   107.       538       24
#> # ℹ 2 more variables: n_psu <int>, period <dbl>
```
