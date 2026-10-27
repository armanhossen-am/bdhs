# Build the complex survey design for BDHS data

Creates a
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
object with the clustering, stratification and weights that the DHS
Program uses for the given recode, so standard errors and confidence
intervals are correct. The round and recode are detected automatically
for data read with
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
or
[`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md).
[`bdhs_design_info()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design_info.md)
documents the design of every round.

## Usage

``` r
bdhs_design(
  data,
  weight = "default",
  strata = c("auto", "design", "region_residence", "none"),
  survey = NULL,
  recode = NULL,
  srvyr = FALSE
)
```

## Arguments

- data:

  A data frame of BDHS recode data (one round, or several rounds pooled
  with
  [`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md)).

- weight:

  Which weight to use.

  - `"default"`: `pool_wt` for pooled data made by
    [`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md),
    otherwise the standard weight of the recode (`v005` for women,
    children and births, `hv005` for households and members, `mv005` for
    men).

  - `"dv"`: the domestic violence module weight (`d005`; `md005` for
    men).

  - `"male"`: the household weight for the men's subsample (`hv028`) in
    household and member files; `mv005` in the men's file.

  - `"household"`: the household weight `hv005`.

  - A column name. DHS weights with six implied decimals (`*005`,
    `hv028`, `ha69`, `hb69`) are divided by 1e6.

- strata:

  Stratification. `"auto"` (default) uses the DHS sampling-error strata
  (`v022`/`hv022`/`mv022`), which is the DHS Program's recommendation
  for variance estimation. `"design"` uses `v023`, and
  `"region_residence"` uses region x urban/rural. `"none"` ignores
  stratification (not recommended).

- survey, recode:

  Optional explicit survey and recode, overriding detection.

- srvyr:

  If `TRUE`, return a `tbl_svy` object for use with the srvyr package.

## Value

A `survey.design2` object (or a `tbl_svy` when `srvyr = TRUE`) with
attributes `bdhs_survey_id`, `bdhs_recode`, `bdhs_weight` (the weight
column used) and `bdhs_singleton_strata` (number of strata with one
PSU).

## Details

Pooled data (several rounds, see
[`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md))
get round-specific PSU and strata identifiers, and the pooled weight
`pool_wt` is used when present.

Some strata can contain a single PSU (for example after subsetting, or
in small domains). The variance of such strata cannot be estimated with
the default `options(survey.lonely.psu = "fail")`; `bdhs_design()` tells
you once per session when this happens. The usual fix, used by the DHS
Program's own software (centering at the grand mean), is
`options(survey.lonely.psu = "adjust")`.

## Examples

``` r
ir <- bdhs_simulate(2022, "IR")
des <- bdhs_design(ir)
survey::svymean(~v201, des)
#>        mean     SE
#> v201 3.5713 0.1092

# Pooled rounds: round-specific strata and PSUs, pooled weights
p <- bdhs_pool(bdhs_simulate(2017, "IR"), bdhs_simulate(2022, "IR"))
#> ℹ Value labels differ between rounds for 3 columns; kept the codes and dropped
#>   the labels: `v022`, `v023`, and `v024`.
#> ℹ DHS codes are not always comparable across rounds; use `harmonize = TRUE` or
#>   `bdhs_harmonize()` for comparable variables.
survey::svyby(~v201, ~survey_id, bdhs_design(p), survey::svymean)
#>           survey_id     v201        se
#> BD2017DHS BD2017DHS 3.420633 0.1107607
#> BD2022DHS BD2022DHS 3.571310 0.1091613
```
