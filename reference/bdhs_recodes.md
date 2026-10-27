# DHS recode file types

Lists the DHS recode (file type) codes that occur for Bangladesh
surveys, with the unit of observation of each file. Use these codes
wherever a function takes a `recode` argument.

## Usage

``` r
bdhs_recodes()
```

## Value

A tibble with columns `recode`, `name`, `unit` and `family`.

## Examples

``` r
bdhs_recodes()
#> # A tibble: 22 × 4
#>    recode name                                     unit                   family
#>    <chr>  <chr>                                    <chr>                  <chr> 
#>  1 IR     Individual (women's) Recode              Woman                  DHS   
#>  2 MR     Men's Recode                             Man                    DHS   
#>  3 KR     Children's Recode                        Child under 5 (born t… DHS   
#>  4 BR     Births Recode                            Birth (full birth his… DHS   
#>  5 HR     Household Recode                         Household              DHS   
#>  6 PR     Household Member Recode                  Household member       DHS   
#>  7 CR     Couples' Recode                          Couple                 DHS   
#>  8 GR     Pregnancies Recode                       Pregnancy (pregnancy … DHS   
#>  9 NR     Pregnancy and Postnatal Care Recode      Recent pregnancy       DHS   
#> 10 HW     Height and Weight (WHO Growth Standards) Child measured in hou… DHS   
#> # ℹ 12 more rows
```
