# The validation record shipped with bdhs

Every indicator in
[`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md)
that has an official DHS value was checked by computing it with
[`bdhs_validate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validate.md)
from the maintainer's own copies of the BDHS recode files and comparing
it with the DHS Program API value. This function returns that record:
national aggregates only, no microdata.

## Usage

``` r
bdhs_validation_record()
```

## Value

A tibble with columns `survey_id`, `recode`, `indicator`, `computed`,
`official` and `pass`.

## Details

An indicator-round passes when the computed value is within 0.5 points
(plus half a unit of the official value's rounding) of the official
value. Rounds and recodes not in the record have not been checked,
usually because the maintainer had no copy of that file.

## See also

[`bdhs_validate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validate.md)
to check your own copy of the data.

## Examples

``` r
rec <- bdhs_validation_record()
table(rec$survey_id, rec$pass)
#>            
#>             FALSE TRUE
#>   BD1994DHS     0   24
#>   BD2000DHS     0   27
#>   BD2004DHS     0   26
#>   BD2007DHS     1   27
#>   BD2011DHS     0   27
#>   BD2014DHS     0   28
#>   BD2017DHS     0  104
#>   BD2022DHS     2   89
```
