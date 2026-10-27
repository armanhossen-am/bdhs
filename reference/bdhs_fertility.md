# Fertility rates from the birth history

Computes the total fertility rate (TFR), age-specific fertility rates
(ASFR) or the general fertility rate (GFR) with the DHS method: births
in the reference period are classified by the mother's age at birth, and
woman-years of exposure in each five-year age group are counted month by
month within the period. Because every BDHS interviewed only
ever-married women, exposure is multiplied by the all-woman factor
(`awfact/100`) so that the rates refer to all women.

## Usage

``` r
bdhs_fertility(
  data,
  type = c("tfr", "asfr", "gfr"),
  period = 36,
  by = NULL,
  se = TRUE,
  awfact = NULL,
  level = 0.95,
  survey = NULL
)
```

## Arguments

- data:

  A women's recode (IR) data frame with the birth history in wide form
  (`b3_01`, `b3_02`, ...), the interview date `v008`, the woman's date
  of birth `v011`, the weight `v005`, the cluster `v021` and the
  all-woman factors (`awfactt`, ...).

- type:

  One of `"tfr"` (births per woman), `"asfr"` (births per 1,000 women,
  by age group) or `"gfr"` (births per 1,000 women age 15-44).

- period:

  Length of the reference period in months before the interview (DHS
  standard: 36 for TFR/ASFR/GFR). The month of the interview is
  excluded.

- by:

  Optional character vector of grouping variables (woman-level).

- se:

  If `TRUE` (default), compute jackknife standard errors and confidence
  intervals.

- awfact:

  All-woman factor. `NULL` (default) chooses it from `by` (`awfactu` for
  `v025`, `awfactr` for `v024`, `awfacte` for `v106`, `awfactw` for
  `v190`, otherwise `awfactt`); `FALSE` disables the adjustment (for
  all-woman samples); a column name uses that column.

- level:

  Confidence level.

- survey:

  Optional survey reference, overriding detection.

## Value

A tibble with columns `survey_id`, the `by` variables, `indicator`,
`age_group`, `estimate`, `se`, `ci_low`, `ci_high`, `n_births`
(unweighted births in the period), `exposure` (weighted woman-years),
`n_women`, `n_psu` and `period`.

## Details

Standard errors use the DHS jackknife: each primary sampling unit is
deleted in turn and the rate recomputed, giving \\SE = \sqrt{(k-1)/k
\sum (r\_{(j)} - r)^2}\\ for \\k\\ clusters.

## See also

[`bdhs_mortality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_mortality.md),
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)

## Examples

``` r
ir <- bdhs_simulate(2017, "IR")
bdhs_fertility(ir)
#> # A tibble: 1 × 12
#>   survey_id indicator age_group estimate    se ci_low ci_high n_births exposure
#>   <chr>     <chr>     <chr>        <dbl> <dbl>  <dbl>   <dbl>    <dbl>    <dbl>
#> 1 BD2017DHS tfr       15-49         2.71 0.179   2.36    3.06      167    1928.
#> # ℹ 3 more variables: n_women <int>, n_psu <int>, period <int>
bdhs_fertility(ir, "asfr", se = FALSE)
#> # A tibble: 7 × 12
#>   survey_id indicator age_group estimate    se ci_low ci_high n_births exposure
#>   <chr>     <chr>     <chr>        <dbl> <dbl>  <dbl>   <dbl>    <dbl>    <dbl>
#> 1 BD2017DHS asfr      15-19         95.1    NA     NA      NA      167    1928.
#> 2 BD2017DHS asfr      20-24        215.     NA     NA      NA      167    1928.
#> 3 BD2017DHS asfr      25-29        105.     NA     NA      NA      167    1928.
#> 4 BD2017DHS asfr      30-34         71.5    NA     NA      NA      167    1928.
#> 5 BD2017DHS asfr      35-39         27.6    NA     NA      NA      167    1928.
#> 6 BD2017DHS asfr      40-44         26.8    NA     NA      NA      167    1928.
#> 7 BD2017DHS asfr      45-49          0      NA     NA      NA      167    1928.
#> # ℹ 3 more variables: n_women <int>, n_psu <int>, period <int>
bdhs_fertility(ir, "tfr", by = "v025", se = FALSE)
#> # A tibble: 2 × 13
#>   survey_id v025  indicator age_group estimate    se ci_low ci_high n_births
#>   <chr>     <fct> <chr>     <chr>        <dbl> <dbl>  <dbl>   <dbl>    <dbl>
#> 1 BD2017DHS urban tfr       15-49         2.63    NA     NA      NA       81
#> 2 BD2017DHS rural tfr       15-49         2.85    NA     NA      NA       86
#> # ℹ 4 more variables: exposure <dbl>, n_women <int>, n_psu <int>, period <int>
```
