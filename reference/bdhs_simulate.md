# Simulate synthetic BDHS-like recode data

Generates **fake** data with the structure of a BDHS recode file: the
same variable names, codes, value labels, identifiers and complex survey
design variables (clusters, strata, weights, all-women factors) as the
real files. Every value is randomly generated, so the output can be
shared freely. It powers the package examples, tests and vignettes, and
lets you write and test analysis code before you have DHS access.

## Usage

``` r
bdhs_simulate(
  survey = 2022,
  recode = c("IR", "KR", "BR", "HR", "PR"),
  n_clusters = 40,
  households = 15,
  seed = 1
)
```

## Arguments

- survey:

  A survey reference understood by
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md).
  Only the standard DHS rounds are supported.

- recode:

  One of `"IR"` (women), `"KR"` (children under 5), `"BR"` (births),
  `"HR"` (households) or `"PR"` (household members).

- n_clusters:

  Number of clusters (primary sampling units) to simulate.

- households:

  Households per cluster.

- seed:

  Random seed. The same seed always gives the same population; the
  global random number generator state is left untouched.

## Value

A tibble with lower-case DHS variable names and haven-labelled columns,
carrying the attributes `bdhs_survey_id`, `bdhs_recode` and
`bdhs_simulated = TRUE`.

## Details

All recodes simulated with the same `survey` and `seed` describe the
same synthetic population, so they can be linked with
[`bdhs_merge()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_merge.md)
exactly like real files: women in `"IR"` are household members in
`"PR"`, children in `"KR"` are births in `"BR"`, and so on.

The data are *structurally* faithful: region codes follow the division
layout of the requested round (5 divisions in 1993-94, 8 from 2017-18),
and the core variables use DHS-7 coding. They are not *statistically*
faithful, and round-specific coding changes in less common variables are
not reproduced. Never use simulated data for substantive conclusions.

## Examples

``` r
ir <- bdhs_simulate(2022, "IR")
ir[1:5, c("caseid", "v001", "v005", "v022", "v024", "v025", "v012", "v313")]
#> # A tibble: 5 × 8
#>   caseid             v001   v005 v022              v024    v025     v012 v313   
#>   <chr>             <dbl>  <dbl> <dbl+lbl>         <dbl+l> <dbl+l> <dbl> <dbl+l>
#> 1 "       1   2  2"     1 744251 1 [barishal urba… 1 [bar… 1 [urb…    22 3 [mod…
#> 2 "       1   3  2"     1 744251 1 [barishal urba… 1 [bar… 1 [urb…    20 0 [no …
#> 3 "       1   4  2"     1 744251 1 [barishal urba… 1 [bar… 1 [urb…    32 0 [no …
#> 4 "       1   5  2"     1 744251 1 [barishal urba… 1 [bar… 1 [urb…    19 0 [no …
#> 5 "       1   6  2"     1 744251 1 [barishal urba… 1 [bar… 1 [urb…    49 3 [mod…

# The same synthetic population in household-member form
pr <- bdhs_simulate(2022, "PR")
nrow(pr)
#> [1] 3006
```
