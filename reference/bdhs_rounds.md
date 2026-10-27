# Survey round facts

A compact table of the Bangladesh surveys distributed by the DHS Program
with their canonical ids, labels, DHS phase and file version code. See
[bdhs_surveys](https://armanhossen-am.github.io/bdhs/reference/bdhs_surveys.md)
for the full catalogue (fieldwork dates, sample sizes and more).

## Usage

``` r
bdhs_rounds(type = "DHS")
```

## Arguments

- type:

  Survey types to consider: any of `"DHS"`, `"SPA"`, `"OTH"`.

## Value

A tibble with columns `survey_id`, `survey_type`, `year`, `year_label`,
`phase` and `file_code`.

## Examples

``` r
bdhs_rounds()
#> # A tibble: 9 × 6
#>   survey_id survey_type  year year_label phase   file_code
#>   <chr>     <chr>       <int> <chr>      <chr>   <chr>    
#> 1 BD1994DHS DHS          1994 1993-94    DHS-III 31       
#> 2 BD1997DHS DHS          1997 1996-97    DHS-III 3A       
#> 3 BD2000DHS DHS          2000 1999-2000  DHS-IV  41       
#> 4 BD2004DHS DHS          2004 2004       DHS-IV  4J       
#> 5 BD2007DHS DHS          2007 2007       DHS-V   51       
#> 6 BD2011DHS DHS          2011 2011       DHS-VI  61       
#> 7 BD2014DHS DHS          2014 2014       DHS-7   72       
#> 8 BD2017DHS DHS          2017 2017-18    DHS-7   7R       
#> 9 BD2022DHS DHS          2022 2022       DHS-8   81       
```
