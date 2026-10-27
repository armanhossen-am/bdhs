# Survey design of each BDHS round

Documents, for every BDHS round and recode, the variables that define
the complex sample design (clusters, strata, weights) and round-specific
caveats, as used by
[`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md).

## Usage

``` r
bdhs_design_info(survey = NULL, recode = NULL)
```

## Arguments

- survey:

  Optional survey reference(s) (see
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md)).
  `NULL` gives all DHS rounds.

- recode:

  Optional recode code(s) (`"IR"`, `"KR"`, `"BR"`, `"HR"`, `"PR"`,
  `"MR"`). `NULL` gives all of them.

## Value

A tibble with one row per round and recode: `survey_id`, `year_label`,
`recode`, `psu`, `strata`, `strata_design`, `weight`, `weight_divisor`,
`n_strata`, `strata_type`, `extra_weights` and `notes`.

## Details

All rounds use a stratified two-stage cluster sample. The PSU (`v021`)
is the cluster (`v001`) in every round. The DHS sampling-error strata
(`v022`) are the strata to use for variance estimation in every round;
in 1993-94 to 2007 they are pairs of PSUs (148-179 strata), from 2011
they are the design strata (division by urban/rural, with city
corporations in 2011-2017-18). Weights have six implied decimals and are
divided by 1,000,000.

## Examples

``` r
bdhs_design_info(recode = "IR")
#> # A tibble: 9 × 12
#>   survey_id year_label recode psu   strata strata_design weight weight_divisor
#>   <chr>     <chr>      <chr>  <chr> <chr>  <chr>         <chr>           <dbl>
#> 1 BD1994DHS 1993-94    IR     v021  v022   v023          v005          1000000
#> 2 BD1997DHS 1996-97    IR     v021  v022   v023          v005          1000000
#> 3 BD2000DHS 1999-2000  IR     v021  v022   v023          v005          1000000
#> 4 BD2004DHS 2004       IR     v021  v022   v023          v005          1000000
#> 5 BD2007DHS 2007       IR     v021  v022   v023          v005          1000000
#> 6 BD2011DHS 2011       IR     v021  v022   v023          v005          1000000
#> 7 BD2014DHS 2014       IR     v021  v022   v023          v005          1000000
#> 8 BD2017DHS 2017-18    IR     v021  v022   v023          v005          1000000
#> 9 BD2022DHS 2022       IR     v021  v022   v023          v005          1000000
#> # ℹ 4 more variables: n_strata <int>, strata_type <chr>, extra_weights <chr>,
#> #   notes <chr>
bdhs_design_info(2004, "KR")$notes
#> [1] "v023 is constant (0), so strata = \"design\" gives an unstratified design; use v022."
```
