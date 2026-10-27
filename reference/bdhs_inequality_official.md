# Inequality gaps from the official estimates

Computes the inequality measures of
[`bdhs_inequality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality.md)
from the official DHS estimates by wealth quintile
([`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md)),
for every round with such estimates. These are point estimates only (the
official tables carry no covariances). The measures follow from the
group values \\\mu_k\\ and the weighted denominators \\p_k\\ of the five
quintiles: `difference` \\\mu_5 - \mu_1\\, `ratio` \\\mu_5 / \mu_1\\,
`sii` and `rii` from the weighted (least squares and log-link)
regression of \\\mu_k\\ on the ridit \\R_k\\, which equals the
individual-level regression because the ridit is constant within groups,
`ci` \\= 2 \sum_k p_k \mu_k R_k / \mu - 1\\ and `eci` \\= 4 \mu C\\ (for
percentages, with \\\mu\\ as a proportion). Official values are rounded,
so the measures are approximate.

## Usage

``` r
bdhs_inequality_official(
  indicator,
  survey = NULL,
  measures = .inequality_measures,
  period = NULL
)
```

## Arguments

- indicator:

  A DHS indicator id (e.g. `"RH_DELA_C_CSC"`) or a bdhs indicator id
  with a DHS equivalent (e.g. `"rh_del_csection"`).

- survey:

  Optional survey references; `NULL` uses every round.

- measures:

  Measures to compute (see
  [`bdhs_inequality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality.md)).

- period:

  Reference period of the official values, as in the `by_variable_label`
  column of `bdhs_official(preferred = FALSE)` (e.g.
  `"Three years preceding the survey"`). `NULL` (default) uses the
  period that bdhs computes for a bdhs indicator id (so the result is
  comparable with
  [`bdhs_inequality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality.md)
  on the microdata), and the DHS headline (preferred) value for a DHS
  id.

## Value

A tibble of class `bdhs_inequality` with columns `survey_id`,
`indicator`, `label`, `by` (`"Wealth quintile"`), `measure`,
`measure_label`, `estimate`, `lowest`, `highest` and `n_groups`; `se`,
`ci_low` and `ci_high` are `NA`.

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

## Examples

``` r
bdhs_inequality_official("RH_DELA_C_CSC")
#> # BDHS inequality: RH_DELA_C_CSC by Wealth quintile | BD2000DHS, BD2004DHS, BD2007DHS, BD2011DHS, BD2014DHS, BD2017DHS, BD2022DHS
#> # A tibble: 42 × 13
#>    survey_id indicator   label by    measure measure_label estimate    se ci_low
#>    <chr>     <chr>       <chr> <chr> <chr>   <chr>            <dbl> <dbl>  <dbl>
#>  1 BD2000DHS RH_DELA_C_… Deli… Weal… differ… Difference (…  11.2       NA     NA
#>  2 BD2000DHS RH_DELA_C_… Deli… Weal… ratio   Ratio (highe…  17         NA     NA
#>  3 BD2000DHS RH_DELA_C_… Deli… Weal… sii     Slope index …  11.2       NA     NA
#>  4 BD2000DHS RH_DELA_C_… Deli… Weal… rii     Relative ind… 140.        NA     NA
#>  5 BD2000DHS RH_DELA_C_… Deli… Weal… ci      Concentratio…   0.588     NA     NA
#>  6 BD2000DHS RH_DELA_C_… Deli… Weal… eci     Erreygers co…   0.0711    NA     NA
#>  7 BD2004DHS RH_DELA_C_… Deli… Weal… differ… Difference (…  19.3       NA     NA
#>  8 BD2004DHS RH_DELA_C_… Deli… Weal… ratio   Ratio (highe…  97.5       NA     NA
#>  9 BD2004DHS RH_DELA_C_… Deli… Weal… sii     Slope index …  19.8       NA     NA
#> 10 BD2004DHS RH_DELA_C_… Deli… Weal… rii     Relative ind… 432.        NA     NA
#> # ℹ 32 more rows
#> # ℹ 4 more variables: ci_high <dbl>, lowest <chr>, highest <chr>,
#> #   n_groups <int>
bdhs_inequality_official("rh_del_facility", survey = c(2004, 2022),
                         measures = c("difference", "ratio"))
#> # BDHS inequality: RH_DELP_C_DHF by Wealth quintile | BD2004DHS, BD2022DHS
#> # A tibble: 4 × 13
#>   survey_id indicator    label by    measure measure_label estimate    se ci_low
#>   <chr>     <chr>        <chr> <chr> <chr>   <chr>            <dbl> <dbl>  <dbl>
#> 1 BD2004DHS RH_DELP_C_D… Plac… Weal… differ… Difference (…    35.1     NA     NA
#> 2 BD2004DHS RH_DELP_C_D… Plac… Weal… ratio   Ratio (highe…    15.0     NA     NA
#> 3 BD2022DHS RH_DELP_C_D… Plac… Weal… differ… Difference (…    46.8     NA     NA
#> 4 BD2022DHS RH_DELP_C_D… Plac… Weal… ratio   Ratio (highe…     2.16    NA     NA
#> # ℹ 4 more variables: ci_high <dbl>, lowest <chr>, highest <chr>,
#> #   n_groups <int>
```
