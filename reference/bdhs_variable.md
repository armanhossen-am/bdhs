# One variable across survey rounds

Shows how a DHS recode variable is labelled and coded in every round in
which it appears, using the bundled
[bdhs_codebook](https://armanhossen-am.github.io/bdhs/reference/bdhs_codebook.md)
and
[bdhs_value_labels](https://armanhossen-am.github.io/bdhs/reference/bdhs_value_labels.md).
Label changes between rounds (e.g. `v024` "region" vs "division",
"barisal" vs "barishal") are flagged so you know when a cross-round
comparison needs care.

## Usage

``` r
bdhs_variable(name, recode = NULL)
```

## Arguments

- name:

  A single variable name, e.g. `"v024"` (case-insensitive).

- recode:

  Optional recode codes to restrict to, e.g. `"IR"`.

## Value

A tibble with one row per survey round and recode: `survey_id`,
`recode`, `label`, `n_valid` (number of non-missing values in the source
file), `value_labels` (compact `"value=label; ..."` string) and
`label_drift` (`TRUE` when the variable label or value labels differ
from those of the most recent round).

## See also

[`bdhs_search()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_search.md)
to find variables.

## Examples

``` r
bdhs_variable("v024")
#> # A tibble: 11 × 6
#>    survey_id recode label    n_valid value_labels                    label_drift
#>    <chr>     <chr>  <chr>      <int> <chr>                           <lgl>      
#>  1 BD1994DHS KR     region      3874 1=barishal; 2=chittagong; 3=dh… TRUE       
#>  2 BD2000DHS KR     division    6832 1=barisal; 2=chittagong; 3=dha… TRUE       
#>  3 BD2004DHS KR     region      6908 1=barisal; 2=chittagong; 3=dha… TRUE       
#>  4 BD2007DHS KR     division    6150 1=barisal; 2=chittagong; 3=dha… TRUE       
#>  5 BD2011DHS KR     region      8753 1=barisal; 2=chittagong; 3=dha… TRUE       
#>  6 BD2014DHS KR     division    7886 1=barisal; 2=chittagong; 3=dha… TRUE       
#>  7 BD2017DHS IR     division   20127 1=barisal; 2=chittagong; 3=dha… TRUE       
#>  8 BD2017DHS KR     division    8759 1=barisal; 2=chittagong; 3=dha… TRUE       
#>  9 BD2017DHS BR     division   47828 1=barisal; 2=chittagong; 3=dha… TRUE       
#> 10 BD2022DHS IR     division   30078 1=barishal; 2=chattogram; 3=dh… FALSE      
#> 11 BD2022DHS KR     division    8784 1=barishal; 2=chattogram; 3=dh… FALSE      
bdhs_variable("h3", recode = "KR")
#> # A tibble: 8 × 6
#>   survey_id recode label               n_valid value_labels          label_drift
#>   <chr>     <chr>  <chr>                 <int> <chr>                 <lgl>      
#> 1 BD1994DHS KR     received dpt 1         3553 0=no; 1=vacc. date o… TRUE       
#> 2 BD2000DHS KR     received dpt 1         6326 0=no; 1=vacc. date o… TRUE       
#> 3 BD2004DHS KR     received dpt 1         6424 0=no; 1=vacc. date o… TRUE       
#> 4 BD2007DHS KR     received dpt 1         5789 0=no; 1=vacc. date o… TRUE       
#> 5 BD2011DHS KR     received dpt 1         8343 0=no; 1=vaccination … TRUE       
#> 6 BD2014DHS KR     received dpt 1         7567 0=no; 1=vaccination … TRUE       
#> 7 BD2017DHS KR     received dpt 1         5117 0=no; 1=vaccination … TRUE       
#> 8 BD2022DHS KR     na - received dpt 1       0 0=no; 1=vaccination … FALSE      
```
