# Survey design, weights and pooling

``` r

library(bdhs)
library(survey)
#> Loading required package: grid
#> Loading required package: Matrix
#> Loading required package: survival
#> 
#> Attaching package: 'survey'
#> The following object is masked from 'package:graphics':
#> 
#>     dotchart
```

The BDHS is a stratified two-stage cluster sample. In the first stage,
enumeration areas (clusters) are drawn within strata defined by division
and urban/rural residence. In the second, a fixed number of households
is drawn in each cluster. Treating the data as a simple random sample
gives the wrong estimates (no weights) and standard errors that are too
small (no clustering).
[`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md)
sets the design up correctly for every round and recode.

This article uses synthetic data from
[`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md).
The code is the same for your real data from
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md).

## The design in one line

``` r

ir <- bdhs_simulate(2022, "IR")
des <- bdhs_design(ir)
des
#> Stratified 1 - level Cluster Sampling design (with replacement)
#> With (40) clusters.
#> bdhs_design(ir)
```

[`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md)
detects the round and recode and then chooses:

- **PSU**: the cluster, `v021` (`hv021` in household files, `mv021` for
  men).
- **Strata**: the DHS sampling-error strata, `v022`. The DHS Program
  recommends these for variance estimation in every round.
- **Weight**: `v005 / 1e6`. DHS weights are stored as integers with six
  implied decimals.

[`bdhs_design_info()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design_info.md)
documents the design of each round, including its peculiarities:

``` r

info <- bdhs_design_info(recode = "IR")
info[, c("year_label", "psu", "strata", "weight", "n_strata", "strata_type")]
#> # A tibble: 9 × 6
#>   year_label psu   strata weight n_strata strata_type                           
#>   <chr>      <chr> <chr>  <chr>     <int> <chr>                                 
#> 1 1993-94    v021  v022   v005        148 paired-PSU sampling-error strata      
#> 2 1996-97    v021  v022   v005        154 paired-PSU sampling-error strata      
#> 3 1999-2000  v021  v022   v005        168 paired-PSU sampling-error strata      
#> 4 2004       v021  v022   v005        177 paired-PSU sampling-error strata      
#> 5 2007       v021  v022   v005        179 paired-PSU sampling-error strata      
#> 6 2011       v021  v022   v005         20 division x residence (city corporatio…
#> 7 2014       v021  v022   v005         20 division x residence (city corporatio…
#> 8 2017-18    v021  v022   v005         22 division x residence (city corporatio…
#> 9 2022       v021  v022   v005         16 division x urban/rural
```

A few points worth knowing:

- Up to 2007, `v022` holds 148 to 179 values with about two clusters
  each. These are the paired-cluster strata the DHS used for its
  sampling-error tables, and they are still the right choice. From 2011
  the strata are the division x residence design strata (20 in 2011 and
  2014, 22 in 2017-18, 16 in 2022).
- `strata = "design"` uses `v023` instead, and
  `strata = "region_residence"` rebuilds the strata from division and
  residence. Both are offered for sensitivity checks.

## Estimating

Use the survey package (or srvyr) on the design:

``` r

svymean(~v201, des)
#>        mean     SE
#> v201 3.5713 0.1092
svyby(~v201, ~v025, des, svymean)
#>   v025     v201        se
#> 1    1 3.534211 0.1695193
#> 2    2 3.606190 0.1402556
```

For named DHS indicators,
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
builds the design for you and returns tidy estimates. See
[`vignette("indicators")`](https://armanhossen-am.github.io/bdhs/articles/indicators.md).

### Subgroups: subset the design, not the data

Filtering the data before building the design drops clusters and gives
wrong standard errors. Subset the design instead:

``` r

young <- subset(des, v012 < 25)
svymean(~v201, young)
#>        mean     SE
#> v201 1.1391 0.0642
```

### Strata with a single PSU

Subgroup analyses sometimes leave a stratum with only one cluster, and
the survey package then refuses to compute a variance. The standard fix,
which also matches the DHS Program’s own software, is:

``` r

options(survey.lonely.psu = "adjust")
```

[`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md)
tells you when this applies, and
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
handles it automatically.

## Other weights

| `weight =` | Weight | Use it for |
|----|----|----|
| `"default"` | `v005`, `hv005`, `mv005` | Almost everything |
| `"dv"` | `d005` (`md005` for men) | Domestic violence module |
| `"male"` | `hv028` | Men’s subsample in household files |
| `"household"` | `hv005` | Household-level analyses of member files |
| a column name | any weight | Special subsamples, such as biomarkers |

``` r

pr <- bdhs_simulate(2022, "PR")
bdhs_design(pr, weight = "household")
#> Stratified 1 - level Cluster Sampling design (with replacement)
#> With (40) clusters.
#> bdhs_design(pr, weight = "household")
```

## Ever-married samples and “all women” indicators

Every BDHS women’s sample consists of **ever-married** women aged 15-49.
Estimates for “all women”, such as the percentage of women aged 20-24
who married before 18, need a denominator that includes never-married
women. The DHS Program supplies **all-woman factors** for this purpose:
`awfactt` (total), `awfactu` (urban/rural), `awfactr` (division),
`awfacte` (education) and `awfactw` (wealth, from 2004). Each factor,
divided by 100, says how many women of all marital statuses one
ever-married woman stands for.

An all-women percentage is then a ratio estimate. The numerator comes
from ever-married women, and the denominator is inflated by the factor:

``` r

ir$married_before_18 <- as.numeric(ir$v511 < 18)
ir$aw <- ir$awfactt / 100
des <- bdhs_design(ir)
svyratio(~married_before_18, ~aw, subset(des, v012 >= 20 & v012 <= 24))
#> Ratio estimator: svyratio.survey.design2(~married_before_18, ~aw, subset(des, 
#>     v012 >= 20 & v012 <= 24))
#> Ratios=
#>                          aw
#> married_before_18 0.4651413
#> SEs=
#>                           aw
#> married_before_18 0.03722989
```

Use the factor that matches the breakdown: `awfactu` for estimates by
residence, `awfactr` by division, and so on.
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
does this for the indicators that need it. All-woman factors only apply
to characteristics that never-married women cannot have, such as
marriage or births. They cannot be used for literacy, media exposure and
similar characteristics.

## Pooling rounds

[`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md)
stacks rounds, adds a `survey_id` column and builds a pooled weight
`pool_wt`.
[`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md)
then gives every round its own strata and clusters:

``` r

pooled <- bdhs_pool(bdhs_simulate(2014, "IR"), bdhs_simulate(2022, "IR"), weights = "denormalized")
#> ℹ Value labels differ between rounds for 3 columns; kept the codes and dropped
#>   the labels: `v022`, `v023`, and `v024`.
#> ℹ DHS codes are not always comparable across rounds; use `harmonize = TRUE` or
#>   `bdhs_harmonize()` for comparable variables.
table(pooled$survey_id)
#> 
#> BD2014DHS BD2022DHS 
#>       551       552
pdes <- bdhs_design(pooled)
svyby(~v201, ~survey_id, pdes, svymean)
#>           survey_id     v201        se
#> BD2014DHS BD2014DHS 3.420633 0.1122980
#> BD2022DHS BD2022DHS 3.571310 0.1091613
```

The three weighting choices are:

- `"normalized"`: the DHS weights as they are. Rounds count in
  proportion to their sample size.
- `"denormalized"`: weights scaled to the population each round
  represents, using the bundled `bdhs_pool_population` (UN WPP 2024
  female population 15-49 times the proportion ever married). This is
  the DHS Program’s recommendation for pooled estimates that describe
  the population.
- `"equal"`: every round carries the same total weight.

``` r

bdhs_pool_population[, c("survey_id", "ref_year", "female_15_49", "n_women_sample")]
#> # A tibble: 9 × 4
#>   survey_id ref_year female_15_49 n_women_sample
#>   <chr>        <dbl>        <dbl>          <int>
#> 1 BD1994DHS    1994.     28096489           9640
#> 2 BD1997DHS    1997.     30782776           9127
#> 3 BD2000DHS    2000.     33689291          10544
#> 4 BD2004DHS    2004.     36965636          11440
#> 5 BD2007DHS    2007.     39019159          10996
#> 6 BD2011DHS    2012.     41427050          17842
#> 7 BD2014DHS    2015.     43360901          17863
#> 8 BD2017DHS    2018.     45459595          20127
#> 9 BD2022DHS    2023.     48342561          30078
```

Add `harmonize = TRUE` to make variables comparable across the pooled
rounds first (see
[`vignette("harmonization")`](https://armanhossen-am.github.io/bdhs/articles/harmonization.md)).

## srvyr

Prefer dplyr-style code? Ask for a `tbl_svy`:

``` r

library(srvyr, warn.conflicts = FALSE)
bdhs_design(ir, srvyr = TRUE) |>
  group_by(v025 = haven::as_factor(v025)) |>
  summarise(children = survey_mean(v201, vartype = "ci"))
#> # A tibble: 2 × 4
#>   v025  children children_low children_upp
#>   <fct>    <dbl>        <dbl>        <dbl>
#> 1 urban     3.53         3.18         3.88
#> 2 rural     3.61         3.32         3.90
```
