# List the indicators bdhs can compute

Returns the registry of indicators that
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md),
[`bdhs_add_indicators()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_add_indicators.md)
and
[`bdhs_validate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validate.md)
understand: what each one measures, which recode file it is computed
from, the denominator rules, the matching DHS Program API indicator and
the survey rounds it supports.

## Usage

``` r
bdhs_indicator_list(topic = NULL, recode = NULL)
```

## Arguments

- topic:

  Optional topic(s) to keep, e.g. `"family_planning"`,
  `"maternal_health"`, `"child_health"`, `"nutrition"`. Matching is
  case-insensitive.

- recode:

  Optional recode code(s) (`"IR"`, `"KR"`, `"BR"`, `"PR"`, ...) to keep
  only indicators computed from those files.

## Value

A tibble with one row per indicator and columns `id`, `label`, `topic`,
`recode`, `type` (`"proportion"`, `"mean"`, `"median"` or `"rate"`),
`api_id`, `all_women` (all-woman factor used for all-women denominators,
or `NA`), `window` (reference period in months, if any), `rounds` and
`validated` (list columns of survey ids), `definition`, `notes` and
`reference`.

## Details

Definitions follow the Guide to DHS Statistics and port the logic of the
DHS Program's DHS-Indicators-R code
(<https://github.com/DHSProgram/DHS-Indicators-R>), adapted to the
Bangladesh questionnaires (for example the country-specific list of
medically trained providers, which changes between rounds, and the 2022
long/short women's questionnaire design). The `validated` column lists
the rounds for which the package's computation was checked against the
official DHS value with the maintainer's copy of the microdata.

## Examples

``` r
bdhs_indicator_list()
#> # A tibble: 97 × 13
#>    id          label topic recode type  api_id all_women window rounds validated
#>    <chr>       <chr> <chr> <chr>  <chr> <chr>  <chr>      <int> <list> <list>   
#>  1 fp_cpr_any  Cont… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  2 fp_cpr_mod… Cont… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  3 fp_cpr_tra… Cont… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  4 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  5 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  6 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  7 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  8 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  9 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#> 10 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#> # ℹ 87 more rows
#> # ℹ 3 more variables: definition <chr>, notes <chr>, reference <chr>
bdhs_indicator_list(topic = "family_planning")
#> # A tibble: 18 × 13
#>    id          label topic recode type  api_id all_women window rounds validated
#>    <chr>       <chr> <chr> <chr>  <chr> <chr>  <chr>      <int> <list> <list>   
#>  1 fp_cpr_any  Cont… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  2 fp_cpr_mod… Cont… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  3 fp_cpr_tra… Cont… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  4 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  5 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  6 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  7 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  8 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#>  9 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#> 10 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#> 11 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#> 12 fp_method_… Marr… fami… IR     prop… FP_CU… NA            NA <chr>  <chr [2]>
#> 13 fp_unmet_t… Unme… fami… IR     prop… FP_NA… NA            NA <chr>  <chr [2]>
#> 14 fp_unmet_s… Unme… fami… IR     prop… FP_NA… NA            NA <chr>  <chr [2]>
#> 15 fp_unmet_l… Unme… fami… IR     prop… FP_NA… NA            NA <chr>  <chr [2]>
#> 16 fp_demand_… Tota… fami… IR     prop… FP_NA… NA            NA <chr>  <chr [2]>
#> 17 fp_demand_… Dema… fami… IR     prop… FP_NA… NA            NA <chr>  <chr [2]>
#> 18 fp_demand_… Dema… fami… IR     prop… FP_NA… NA            NA <chr>  <chr [2]>
#> # ℹ 3 more variables: definition <chr>, notes <chr>, reference <chr>
bdhs_indicator_list(recode = "KR")
#> # A tibble: 29 × 13
#>    id          label topic recode type  api_id all_women window rounds validated
#>    <chr>       <chr> <chr> <chr>  <chr> <chr>  <chr>      <int> <list> <list>   
#>  1 rh_anc_ski… Ante… mate… KR     prop… RH_AN… NA            36 <chr>  <chr [8]>
#>  2 rh_anc_doc… Ante… mate… KR     prop… RH_AN… NA            36 <chr>  <chr [8]>
#>  3 rh_anc_4pl… Four… mate… KR     prop… RH_AN… NA            36 <chr>  <chr [8]>
#>  4 rh_anc_fir… Firs… mate… KR     prop… RH_AN… NA            36 <chr>  <chr [6]>
#>  5 rh_del_fac… Deli… mate… KR     prop… RH_DE… NA            36 <chr>  <chr [8]>
#>  6 rh_del_home Deli… mate… KR     prop… RH_DE… NA            36 <chr>  <chr [8]>
#>  7 rh_del_ski… Deli… mate… KR     prop… RH_DE… NA            36 <chr>  <chr [8]>
#>  8 rh_del_doc… Deli… mate… KR     prop… RH_DE… NA            36 <chr>  <chr [8]>
#>  9 rh_del_cse… Deli… mate… KR     prop… RH_DE… NA            36 <chr>  <chr [7]>
#> 10 rh_pnc_mot… Post… mate… KR     prop… RH_PC… NA            24 <chr>  <chr [5]>
#> # ℹ 19 more rows
#> # ℹ 3 more variables: definition <chr>, notes <chr>, reference <chr>
```
