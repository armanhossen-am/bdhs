# Estimate indicators with correct survey standard errors

Computes indicators from the registry
([`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md))
for one or several BDHS rounds, nationally or by subgroups, using the
complex survey design of the data
([`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md)):
clusters, sampling-error strata and sample weights. Standard errors use
Taylor linearisation (proportions, means, all-women ratios) or the DHS
jackknife (medians, fertility and mortality rates).

## Usage

``` r
bdhs_estimate(
  data,
  indicators = NULL,
  by = NULL,
  survey = NULL,
  level = 0.95,
  scale = 100,
  ci_method = c("logit", "wald"),
  design_args = list()
)
```

## Arguments

- data:

  A BDHS recode data frame (from
  [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
  [`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md)
  or pooled rounds with a `survey_id` column) with DHS variable names.

- indicators:

  Character vector of indicator ids (see
  [`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md)).
  `NULL` estimates every indicator available for the recode and round.

- by:

  Optional character vector of grouping variables. Labelled variables
  are shown with their value labels.

- survey:

  Optional survey reference, overriding detection.

- level:

  Confidence level.

- scale:

  Multiplier for proportions (100 gives percentages, as in DHS reports).
  Means, medians and rates are never rescaled.

- ci_method:

  `"logit"` (default) computes confidence intervals for proportions on
  the logit scale (as
  [`survey::svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html)),
  which keeps them inside 0-100; `"wald"` uses estimate +/- t \* SE.
  Means, medians and rates always use Wald intervals.

- design_args:

  A list of further arguments passed to
  [`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md),
  e.g. `list(strata = "design")` or `list(weight = "dv")`.

## Value

A tibble of class `bdhs_estimate` with columns `survey_id`, `indicator`,
`label`, the `by` variables, `estimate`, `se`, `ci_low`, `ci_high`,
`n_unweighted` (records in the denominator), `n_weighted` (weighted
denominator; for all-women indicators the estimated number of all women)
and `api_id` (DHS Program API indicator id for comparison with
[bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md)).

## Details

- Proportions and means are design-based means over the indicator's
  denominator population (domain estimation, so standard errors account
  for the full design).

- Indicators whose denominator is *all* women (for example child
  marriage or teenage childbearing) use the ratio estimator \\\sum w y /
  \sum w \\ awfact / 100\\, because only ever-married women were
  interviewed. The all-woman factor matching a single `by` variable is
  used (`awfactu` for urban/rural, `awfactr` for division, `awfacte` for
  education, `awfactw` for wealth); other groupings use `awfactt`.

- Medians (age at first marriage) use the DHS interpolation on completed
  ages and jackknife standard errors.

- Rates call
  [`bdhs_fertility()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_fertility.md)
  and
  [`bdhs_mortality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_mortality.md).

Singleton strata are handled with
`options(survey.lonely.psu = "adjust")`, set only while the function
runs.

## See also

[`bdhs_validate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validate.md)
to compare with the official values.

## Examples

``` r
kr <- bdhs_simulate(2017, "KR")
bdhs_estimate(kr, c("rh_del_facility", "rh_del_csection"))
#> # BDHS estimates: 2 indicators | BD2017DHS | 95% CI
#> # A tibble: 2 × 10
#>   survey_id indicator       label     estimate    se ci_low ci_high n_unweighted
#>   <chr>     <chr>           <chr>        <dbl> <dbl>  <dbl>   <dbl>        <int>
#> 1 BD2017DHS rh_del_facility Delivery…     54.7  4.24   45.9    63.3          161
#> 2 BD2017DHS rh_del_csection Delivery…     33.7  3.92   26.2    42.2          161
#> # ℹ 2 more variables: n_weighted <dbl>, api_id <chr>
bdhs_estimate(kr, "rh_del_facility", by = "v025")
#> # BDHS estimates: 1 indicator | BD2017DHS | 95% CI
#> # A tibble: 2 × 11
#>   survey_id indicator     label v025  estimate    se ci_low ci_high n_unweighted
#>   <chr>     <chr>         <chr> <fct>    <dbl> <dbl>  <dbl>   <dbl>        <int>
#> 1 BD2017DHS rh_del_facil… Deli… urban     57.5  6.62   43.6    70.3           77
#> 2 BD2017DHS rh_del_facil… Deli… rural     52.5  5.54   41.2    63.6           84
#> # ℹ 2 more variables: n_weighted <dbl>, api_id <chr>

ir <- bdhs_simulate(2017, "IR")
bdhs_estimate(ir, c("fp_cpr_modern", "ms_married_by_18"), by = "v025")
#> # BDHS estimates: 2 indicators | BD2017DHS | 95% CI
#> # A tibble: 4 × 11
#>   survey_id indicator     label v025  estimate    se ci_low ci_high n_unweighted
#>   <chr>     <chr>         <chr> <fct>    <dbl> <dbl>  <dbl>   <dbl>        <int>
#> 1 BD2017DHS fp_cpr_modern Cont… urban     57.9  2.34   53.0    62.7          255
#> 2 BD2017DHS fp_cpr_modern Cont… rural     53.9  3.18   47.3    60.4          258
#> 3 BD2017DHS ms_married_b… Wome… urban     52.0  5.38   41.0    62.9           42
#> 4 BD2017DHS ms_married_b… Wome… rural     46.0  4.18   37.6    54.7           43
#> # ℹ 2 more variables: n_weighted <dbl>, api_id <chr>
```
