# Compare indicators between survey rounds and between subgroups

Tests differences of indicators between BDHS rounds (for example the
change in caesarean deliveries from 2017-18 to 2022) and between groups
within a round (for example urban versus rural), with design-based
standard errors, confidence intervals and z tests.

## Usage

``` r
bdhs_compare(
  ...,
  indicators = NULL,
  by = NULL,
  level = 0.95,
  rounds = c("consecutive", "first_last", "all"),
  groups = c("reference", "pairwise"),
  design_args = list()
)
```

## Arguments

- ...:

  Either data frames of BDHS recode data (one per round, or one pooled
  data frame with a `survey_id` column, see
  [`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md)),
  or `bdhs_estimate` objects from
  [`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
  (not a mix).

- indicators:

  Indicator ids (see
  [`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md)).
  Required for data frames; for `bdhs_estimate` inputs it optionally
  selects indicators.

- by:

  Optional single grouping variable. For `bdhs_estimate` inputs it is
  detected from the columns.

- level:

  Confidence level of the intervals of the differences.

- rounds:

  Which pairs of rounds to compare: `"consecutive"` (default),
  `"first_last"` or `"all"` pairs.

- groups:

  Contrasts between groups within a round: `"reference"` (every group
  minus the first group, default) or `"pairwise"`.

- design_args:

  A list of further arguments passed to
  [`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md).

## Value

A tibble of class `bdhs_comparison` with one row per comparison and
columns `comparison` (`"rounds"` or `"groups"`), `indicator`, `label`,
`by` (grouping variable or `NA`), `survey_1`, `survey_2`, `group_1`,
`group_2`, `estimate_1`, `estimate_2`, `difference` (second minus
first), `se`, `ci_low`, `ci_high`, `z`, `p_value`, `years` (between
survey mid-points), `annual_change` and `annual_change_se` (per year;
`NA` for group contrasts). Proportions are on the 0-100 scale, so
differences are in percentage points.

## Details

**Between rounds.** BDHS rounds are independent samples, so the
difference \\d = \hat\theta_2 - \hat\theta_1\\ has \\SE(d) =
\sqrt{SE_1^2 + SE_2^2}\\; \\z = d / SE(d)\\ and the p-value is two-sided
from the standard normal. The annualized change is \\d / (t_2 - t_1)\\,
where \\t\\ is the mid-point of each survey's fieldwork (from
[bdhs_surveys](https://armanhossen-am.github.io/bdhs/reference/bdhs_surveys.md)),
with standard error \\SE(d) / (t_2 - t_1)\\. Rates (fertility,
mortality) and medians use their jackknife standard errors.

**Between groups within a round.** Groups of the same survey are not
independent (they share strata and clusters), so their estimates are
computed jointly with
[`survey::svyby()`](https://rdrr.io/pkg/survey/man/svyby.html)
(`covmat = TRUE`) and the difference uses
[`survey::svycontrast()`](https://rdrr.io/pkg/survey/man/svycontrast.html),
which accounts for the covariance. Indicators with all-women
denominators use the ratio estimator of
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
with the all-woman factor of the `by` variable. Within-round contrasts
need the microdata; they are not computed from `bdhs_estimate` objects,
nor for rates and medians.

**Divisions.** Bangladesh had 5 divisions in 1993-94, 6 to 2007, 7 in
2011-2014 and 8 from 2017-18, and the codes of `v024` changed. Comparing
`v024` (or `division_surveyed`) across rounds with different layouts
compares different areas under the same name; a warning recommends the
harmonized `division_trend6` from
[`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md).

## See also

[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md),
[`bdhs_inequality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality.md),
[`bdhs_decompose()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_decompose.md).

## Examples

``` r
kr17 <- bdhs_simulate(2017, "KR")
kr22 <- bdhs_simulate(2022, "KR")
bdhs_compare(kr17, kr22, indicators = "rh_del_csection")
#> # BDHS comparisons: 1 between rounds, 0 between groups | 95% CI
#> # A tibble: 1 × 19
#>   comparison indicator  label by    survey_1 survey_2 group_1 group_2 estimate_1
#>   <chr>      <chr>      <chr> <chr> <chr>    <chr>    <chr>   <chr>        <dbl>
#> 1 rounds     rh_del_cs… Deli… NA    BD2017D… BD2022D… NA      NA            33.7
#> # ℹ 10 more variables: estimate_2 <dbl>, difference <dbl>, se <dbl>,
#> #   ci_low <dbl>, ci_high <dbl>, z <dbl>, p_value <dbl>, years <dbl>,
#> #   annual_change <dbl>, annual_change_se <dbl>

# Urban-rural gap within each round and its change between rounds
bdhs_compare(kr17, kr22, indicators = "rh_del_csection", by = "v025")
#> # BDHS comparisons: 2 between rounds, 2 between groups | 95% CI
#> # A tibble: 4 × 19
#>   comparison indicator  label by    survey_1 survey_2 group_1 group_2 estimate_1
#>   <chr>      <chr>      <chr> <chr> <chr>    <chr>    <chr>   <chr>        <dbl>
#> 1 rounds     rh_del_cs… Deli… v025  BD2017D… BD2022D… urban   urban         36.2
#> 2 rounds     rh_del_cs… Deli… v025  BD2017D… BD2022D… rural   rural         31.8
#> 3 groups     rh_del_cs… Deli… v025  BD2017D… BD2017D… urban   rural         36.2
#> 4 groups     rh_del_cs… Deli… v025  BD2022D… BD2022D… urban   rural         29.2
#> # ℹ 10 more variables: estimate_2 <dbl>, difference <dbl>, se <dbl>,
#> #   ci_low <dbl>, ci_high <dbl>, z <dbl>, p_value <dbl>, years <dbl>,
#> #   annual_change <dbl>, annual_change_se <dbl>

# From saved estimates
e1 <- bdhs_estimate(kr17, "rh_del_facility")
e2 <- bdhs_estimate(kr22, "rh_del_facility")
bdhs_compare(e1, e2)
#> # BDHS comparisons: 1 between rounds, 0 between groups | 95% CI
#> # A tibble: 1 × 19
#>   comparison indicator  label by    survey_1 survey_2 group_1 group_2 estimate_1
#>   <chr>      <chr>      <chr> <chr> <chr>    <chr>    <chr>   <chr>        <dbl>
#> 1 rounds     rh_del_fa… Deli… NA    BD2017D… BD2022D… NA      NA            54.7
#> # ℹ 10 more variables: estimate_2 <dbl>, difference <dbl>, se <dbl>,
#> #   ci_low <dbl>, ci_high <dbl>, z <dbl>, p_value <dbl>, years <dbl>,
#> #   annual_change <dbl>, annual_change_se <dbl>
```
