# Socioeconomic inequality in an indicator

Computes the standard summary measures of inequality of an indicator
across ordered socioeconomic groups (by default household wealth
quintiles), with design-based standard errors and confidence intervals,
for every round in the data.

## Usage

``` r
bdhs_inequality(
  data,
  indicator,
  by = NULL,
  measures = .inequality_measures,
  level = 0.95,
  survey = NULL,
  design_args = list()
)

# S3 method for class 'bdhs_inequality'
plot(x, ...)

# S3 method for class 'bdhs_inequality'
autoplot(object, ...)
```

## Arguments

- data:

  A BDHS recode data frame (one round or pooled rounds).

- indicator:

  A single indicator id (see
  [`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md));
  a proportion or a mean.

- by:

  Ordered grouping variable, lowest group first. `NULL` (default) uses
  the harmonized `wealth_quintile` if present, else `v190` (`hv270` in
  household files). Labelled variables are ordered by their codes and
  factors by their levels.

- measures:

  Measures to compute, any of `"difference"`, `"ratio"`, `"sii"`,
  `"rii"`, `"ci"` and `"eci"`.

- level:

  Confidence level.

- survey:

  Optional survey reference, overriding detection.

- design_args:

  A list of further arguments passed to
  [`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md).

- x, object:

  A `bdhs_inequality` result.

- ...:

  Unused.

## Value

A tibble of class `bdhs_inequality` with columns `survey_id`,
`indicator`, `label`, `by`, `measure`, `measure_label`, `estimate`,
`se`, `ci_low`, `ci_high`, `lowest`, `highest` (labels of the extreme
groups) and `n_groups`.

## Details

Let the groups be ordered from poorest (1) to richest (\\K\\), with
weighted population shares \\p_k\\ in the indicator's denominator
population, and let \\R_k = \sum\_{j\<k} p_j + p_k / 2\\ be the weighted
ridit (fractional rank) of group \\k\\, so that \\R\\ runs from 0
(poorest) to 1 (richest).

- `difference`: \\\mu_K - \mu_1\\ (richest minus poorest group), and
  `ratio`: \\\mu_K / \mu_1\\. The group means are estimated jointly
  ([`survey::svyby()`](https://rdrr.io/pkg/survey/man/svyby.html) with
  `covmat = TRUE`) and the measures with
  [`survey::svycontrast()`](https://rdrr.io/pkg/survey/man/svycontrast.html)
  (delta method; the ratio's interval is computed on the log scale).

- `sii`, the slope index of inequality: the slope \\\beta\\ of the
  design-based linear regression \\E(y) = \alpha + \beta R\\ of the 0/1
  outcome on the ridit
  ([`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html));
  the absolute difference between the top and the bottom of the
  socioeconomic scale (Barros and Victora 2013).

- `rii`, the relative index of inequality: \\\exp(\beta)\\ from the
  log-link (quasi-Poisson) regression \\\log E(y) = \alpha + \beta R\\,
  the ratio of the predicted values at the top and the bottom of the
  scale (Barros and Victora 2013).

- `ci`, the concentration index, in its covariance ("convenient
  regression") form with design weights, \\C = 2\\\mathrm{cov}\_w(y, R)
  / \mu\\ (Kakwani, Wagstaff and van Doorslaer 1997; Wagstaff, Paci and
  van Doorslaer 1991). It is written as a smooth function of survey
  totals, \\C = 2 T\_{yR} / T_y - 2 T_R / N\\, and its standard error is
  obtained by linearization with
  [`survey::svycontrast()`](https://rdrr.io/pkg/survey/man/svycontrast.html).

- `eci`, Erreygers' corrected concentration index for a bounded outcome
  in \\\[a, b\]\\: \\E = 4 \mu C / (b - a) = 8\\\mathrm{cov}\_w(y, R)\\
  for a binary outcome (Erreygers 2009); only for proportions.

Positive values of `difference`, `sii`, `ci` and `eci` (and `ratio`,
`rii` above 1) mean that the indicator is higher among the richer.
Ridits are treated as fixed (conditional on the observed group shares),
as is usual. Proportions are on the 0-100 scale, so `difference` and
`sii` are in percentage points; the other measures are scale-free.

Indicators with all-women denominators (ratio estimators) get the
`difference` and `ratio` only. Lonely PSUs are handled with
`options(survey.lonely.psu = "adjust")`, set while the function runs.

## References

Barros AJD, Victora CG (2013). Measuring coverage in MNCH: determining
and interpreting inequalities in coverage of maternal, newborn, and
child health interventions. *PLoS Medicine* 10(5): e1001390.

Erreygers G (2009). Correcting the concentration index. *Journal of
Health Economics* 28(2): 504-515.

Kakwani N, Wagstaff A, van Doorslaer E (1997). Socioeconomic
inequalities in health: measurement, computation, and statistical
inference. *Journal of Econometrics* 77(1): 87-103.

Wagstaff A, Paci P, van Doorslaer E (1991). On the measurement of
inequalities in health. *Social Science & Medicine* 33(5): 545-557.

## See also

[`bdhs_inequality_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality_official.md)
for the same gap measures from the official estimates;
[`bdhs_compare()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_compare.md).

## Examples

``` r
kr <- bdhs_simulate(2022, "KR", n_clusters = 40)
bdhs_inequality(kr, "rh_del_csection")
#> # BDHS inequality: rh_del_csection by v190 | BD2022DHS
#> # A tibble: 6 × 13
#>   survey_id indicator label by    measure measure_label estimate      se  ci_low
#>   <chr>     <chr>     <chr> <chr> <chr>   <chr>            <dbl>   <dbl>   <dbl>
#> 1 BD2022DHS rh_del_c… Deli… v190  differ… Difference (…   27.2   14.0    -1.67  
#> 2 BD2022DHS rh_del_c… Deli… v190  ratio   Ratio (highe…    3.31   2.22    0.831 
#> 3 BD2022DHS rh_del_c… Deli… v190  sii     Slope index …   35.6   13.7     7.32  
#> 4 BD2022DHS rh_del_c… Deli… v190  rii     Relative ind…    3.30   1.46    1.33  
#> 5 BD2022DHS rh_del_c… Deli… v190  ci      Concentratio…    0.186  0.0672  0.0473
#> 6 BD2022DHS rh_del_c… Deli… v190  eci     Erreygers co…    0.226  0.0900  0.0407
#> # ℹ 4 more variables: ci_high <dbl>, lowest <chr>, highest <chr>,
#> #   n_groups <int>
bdhs_inequality(kr, "rh_del_facility", by = "v106", measures = c("difference", "sii"))
#> # BDHS inequality: rh_del_facility by v106 | BD2022DHS
#> # A tibble: 2 × 13
#>   survey_id indicator    label by    measure measure_label estimate    se ci_low
#>   <chr>     <chr>        <chr> <chr> <chr>   <chr>            <dbl> <dbl>  <dbl>
#> 1 BD2022DHS rh_del_faci… Deli… v106  differ… Difference (…     11.3  14.1 -17.8 
#> 2 BD2022DHS rh_del_faci… Deli… v106  sii     Slope index …     27.2  10.9   4.72
#> # ℹ 4 more variables: ci_high <dbl>, lowest <chr>, highest <chr>,
#> #   n_groups <int>
```
