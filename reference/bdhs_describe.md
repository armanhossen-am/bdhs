# Codebook of a data frame

Summarises every column of a data frame: its variable label, type,
number of valid and missing values and its declared value labels. Works
on any data frame, including data from
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
and
[`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md).

## Usage

``` r
bdhs_describe(data)
```

## Arguments

- data:

  A data frame.

## Value

A tibble with one row per column: `variable`, `label`, `type`,
`n_valid`, `n_missing`, `n_value_labels` and `value_labels` (compact
`"value=label; ..."` string for labelled columns, `"level; ..."` for
factors). The `bdhs_survey_id` and `bdhs_recode` attributes of `data`
are kept.

## Details

Only metadata is reported. No data values are shown, apart from the
declared label sets (value labels of labelled columns and factor
levels), so the result is safe to share even for licensed microdata.

## See also

[`bdhs_variable()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_variable.md)
for a variable across rounds in the bundled codebook.

## Examples

``` r
ir <- bdhs_simulate(2022, "IR", n_clusters = 4, households = 5)
bdhs_describe(ir)
#> # A tibble: 116 × 7
#>    variable label            type  n_valid n_missing n_value_labels value_labels
#>    <chr>    <chr>            <chr>   <int>     <int>          <int> <chr>       
#>  1 caseid   case identifica… char…      20         0              0 ""          
#>  2 v000     country code an… char…      20         0              0 ""          
#>  3 v001     cluster number   nume…      20         0              0 ""          
#>  4 v002     household number nume…      20         0              0 ""          
#>  5 v005     women's individ… nume…      20         0              0 ""          
#>  6 v003     respondent's li… nume…      20         0              0 ""          
#>  7 v007     year of intervi… nume…      20         0              0 ""          
#>  8 v021     primary samplin… nume…      20         0              0 ""          
#>  9 v022     sample strata f… have…      20         0             16 "1=barishal…
#> 10 v023     stratification … have…      20         0             16 "1=barishal…
#> # ℹ 106 more rows
```
