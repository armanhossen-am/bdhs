# Decompose the change of an indicator between two rounds

Splits the change of an indicator between two BDHS rounds into a
**composition** effect (the population moved between groups, e.g. more
mothers with secondary education) and a **within-group** (rate) effect
(the indicator changed inside the groups), following Kitagawa (1955)
with the symmetric weighting of Das Gupta (1993).

## Usage

``` r
bdhs_decompose(
  data1,
  data2,
  indicator,
  by,
  level = 0.95,
  se = c("jackknife", "none")
)

# S3 method for class 'bdhs_decomposition'
plot(x, ...)

# S3 method for class 'bdhs_decomposition'
autoplot(object, ...)
```

## Arguments

- data1, data2:

  Data frames of the same recode from two rounds (earlier round first).

- indicator:

  A single indicator id (a proportion or a mean without an all-women
  denominator).

- by:

  A categorical grouping variable present (or harmonizable) in both data
  frames.

- level:

  Confidence level.

- se:

  `"jackknife"` (default) or `"none"`.

- x, object:

  A `bdhs_decomposition` result.

- ...:

  Unused.

## Value

A tibble of class `bdhs_decomposition` with columns `component`
(`"composition"`, `"within"`, `"total"`), `group` (a group, or `"All"`
for the sum over groups), `estimate`, `se`, `ci_low`, `ci_high` and
`percent` (share of the total change). Proportions are on the 0-100
scale. The attribute `groups` holds the shares and rates by group and
round; `survey_1`, `survey_2`, `indicator` and `by` describe the
decomposition.

## Details

With group shares \\c\_{tg}\\ (weighted share of the indicator's
denominator population in group \\g\\ in round \\t\\) and group rates
\\r\_{tg}\\, the overall level is \\\bar r_t = \sum_g c\_{tg} r\_{tg}\\
and \$\$\bar r_2 - \bar r_1 = \sum_g (c\_{2g} - c\_{1g}) \frac{r\_{1g} +
r\_{2g}}{2} + \sum_g (r\_{2g} - r\_{1g}) \frac{c\_{1g} +
c\_{2g}}{2},\$\$ the composition and within-group effects. The
decomposition is exact (no interaction term), and each term is also
reported by group.

**Standard errors.** Every term is a smooth function of weighted survey
totals of the two rounds, which are independent samples, so its variance
is the sum of the variances from each round. These are estimated with
the delete-one-PSU jackknife within each round (the DHS Program's method
for non-linear statistics such as rates): \\v_t = (k_t - 1) / k_t \sum_j
(\hat\theta\_{(j)} - \hat\theta)^2\\ over the \\k_t\\ PSUs of round
\\t\\, holding the other round at its full-sample value. The jackknife
accounts for the clustering and the randomness of both the shares and
the rates.

The groups must mean the same in both rounds: use harmonized variables
(`mother_edu_level`, `wealth_quintile`, `residence`, `division_trend6`,
see
[`bdhs_harmonized_vars()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonized_vars.md)).
If `by` names a harmonized variable that is not yet in the data, it is
created with
[`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md).

## References

Kitagawa EM (1955). Components of a difference between two rates.
*Journal of the American Statistical Association* 50(272): 1168-1194.

Das Gupta P (1993). *Standardization and Decomposition of Rates: A
User's Manual*. US Bureau of the Census, Current Population Reports
P23-186.

## Examples

``` r
kr14 <- bdhs_simulate(2014, "KR")
kr22 <- bdhs_simulate(2022, "KR")
dec <- bdhs_decompose(kr14, kr22, "rh_del_csection", by = "residence")
dec
#> # Kitagawa decomposition: rh_del_csection by residence, BD2014DHS -> BD2022DHS
#> # A tibble: 7 × 7
#>   component   group estimate    se  ci_low ci_high percent
#>   <chr>       <chr>    <dbl> <dbl>   <dbl>   <dbl>   <dbl>
#> 1 composition urban  0.203   4.25   -8.13    8.54   -6.11 
#> 2 composition rural -0.196   4.11   -8.25    7.86    5.90 
#> 3 composition All    0.00669 0.175  -0.336   0.350  -0.201
#> 4 within      urban -3.12    3.77  -10.5     4.26   93.9  
#> 5 within      rural -0.208   3.72   -7.51    7.09    6.27 
#> 6 within      All   -3.33    5.30  -13.7     7.06  100.   
#> 7 total       All   -3.32    5.21  -13.5     6.88  100    
attr(dec, "groups")
#> # A tibble: 2 × 7
#>   group share_1 share_2 rate_1 rate_2   n_1   n_2
#>   <chr>   <dbl>   <dbl>  <dbl>  <dbl> <int> <int>
#> 1 urban    44.2    44.8   36.2   29.2    77    78
#> 2 rural    55.8    55.2   31.8   31.4    84    84
```
