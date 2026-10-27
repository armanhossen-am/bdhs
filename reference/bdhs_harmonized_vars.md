# Harmonized variables available across BDHS rounds

Lists the variables that
[`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)
can create, with their type, levels, comparability across rounds and,
for each round, the DHS source variable(s) and rule used. The rules were
derived from the value labels and code frequencies of every recode file
and validated against official DHS estimates; see the `notes` column for
known breaks (for example the 2007 change in the ARI definition or the
2022 move of "tin" walls to a new code).

## Usage

``` r
bdhs_harmonized_vars(survey = NULL)
```

## Arguments

- survey:

  Optional survey reference(s) (see
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md)).
  When given, the result has one row per variable and round, with the
  rule used in that round.

## Value

A tibble. Without `survey`: one row per variable with columns `name`,
`label`, `type` (`factor`, `binary` = 0/1 integer, `integer`, `numeric`,
`character`), `unit`, `comparability`, `levels` (a list of tibbles with
`value` and `label`), `rounds` (survey ids where the variable is in the
recode file), `external_rounds` (rounds where it is in a separate WI/HW
file), `sources` (per-round source summary), `household_var` and
`notes`. With `survey`: `name`, `label`, `type`, `unit`,
`comparability`, `survey_id`, `method`, `source`, `available` and
`note`.

## Details

Variables are defined for the children's recode (KR); rules on
woman-level `v` variables also apply to the women's (IR) and births (BR)
recodes, and variables with a `household_var` apply to the household
(HR) and household member (PR) recodes.

## Examples

``` r
vars <- bdhs_harmonized_vars()
vars[, c("name", "type", "comparability")]
#> # A tibble: 112 × 3
#>    name              type      comparability
#>    <chr>             <chr>     <chr>        
#>  1 survey_id         character full         
#>  2 cluster           integer   full         
#>  3 psu               integer   full         
#>  4 strata            integer   full         
#>  5 wt                numeric   full         
#>  6 awfact_total      numeric   full         
#>  7 interview_cmc     integer   full         
#>  8 interview_year    integer   full         
#>  9 division_surveyed factor    break        
#> 10 division_trend6   factor    full         
#> # ℹ 102 more rows

# Rules used in one round
bdhs_harmonized_vars(2022)[, c("name", "method", "source", "available")]
#> # A tibble: 112 × 4
#>    name              method   source                   available
#>    <chr>             <chr>    <chr>                    <lgl>    
#>  1 survey_id         constant "constant \"BD2022DHS\"" TRUE     
#>  2 cluster           numeric  "v001"                   TRUE     
#>  3 psu               numeric  "v021"                   TRUE     
#>  4 strata            numeric  "v022"                   TRUE     
#>  5 wt                numeric  "v005"                   TRUE     
#>  6 awfact_total      numeric  "awfactt"                TRUE     
#>  7 interview_cmc     numeric  "v008"                   TRUE     
#>  8 interview_year    expr     "v008"                   TRUE     
#>  9 division_surveyed map      "v024"                   TRUE     
#> 10 division_trend6   map      "v024"                   TRUE     
#> # ℹ 102 more rows
```
