# What DHS datasets exist, and which ones you have

Lists every dataset in the DHS Program catalogue for Bangladesh (one row
per survey and recode file type, from
[bdhs_datasets](https://armanhossen-am.github.io/bdhs/reference/bdhs_datasets.md))
and whether it is available in your local store.

## Usage

``` r
bdhs_inventory(survey = NULL, recode = NULL, type = "DHS")
```

## Arguments

- survey:

  Optional survey references (see
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md)).

- recode:

  Optional recode codes (see
  [`bdhs_recodes()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_recodes.md)).

- type:

  Survey types to list: any of `"DHS"`, `"SPA"`, `"OTH"`.

## Value

A tibble with columns `survey_id`, `year_label`, `recode`,
`recode_name`, `file_stem`, `available`, `format`, `linked`, `path`,
`n_rows`, `n_cols` and `imported_at`.

## See also

Other data store:
[`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md),
[`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md),
[`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md),
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md),
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md),
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md),
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)

## Examples

``` r
bdhs_inventory(2022)
#> # A tibble: 11 × 12
#>    survey_id year_label recode recode_name     file_stem available format linked
#>    <chr>     <chr>      <chr>  <chr>           <chr>     <lgl>     <chr>  <lgl> 
#>  1 BD2022DHS 2022       BR     Births Recode   BDBR81FL  FALSE     NA     NA    
#>  2 BD2022DHS 2022       FW     Fieldworker Qu… BDFW81FL  FALSE     NA     NA    
#>  3 BD2022DHS 2022       GC     Geospatial Cov… BDGC81FL  FALSE     NA     NA    
#>  4 BD2022DHS 2022       GE     Geographic Dat… BDGE81FL  FALSE     NA     NA    
#>  5 BD2022DHS 2022       GR     Pregnancies Re… BDGR81FL  FALSE     NA     NA    
#>  6 BD2022DHS 2022       HR     Household Reco… BDHR81FL  FALSE     NA     NA    
#>  7 BD2022DHS 2022       IR     Individual (wo… BDIR81FL  FALSE     NA     NA    
#>  8 BD2022DHS 2022       KR     Children's Rec… BDKR81FL  FALSE     NA     NA    
#>  9 BD2022DHS 2022       NR     Pregnancy and … BDNR81FL  FALSE     NA     NA    
#> 10 BD2022DHS 2022       PR     Household Memb… BDPR81FL  FALSE     NA     NA    
#> 11 BD2022DHS 2022       VA     Verbal Autopsy  BDVA81FL  FALSE     NA     NA    
#> # ℹ 4 more variables: path <chr>, n_rows <int>, n_cols <int>,
#> #   imported_at <dttm>
bdhs_inventory(recode = "KR")
#> # A tibble: 9 × 12
#>   survey_id year_label recode recode_name      file_stem available format linked
#>   <chr>     <chr>      <chr>  <chr>            <chr>     <lgl>     <chr>  <lgl> 
#> 1 BD1994DHS 1993-94    KR     Children's Reco… BDKR31FL  FALSE     NA     NA    
#> 2 BD1997DHS 1996-97    KR     Children's Reco… BDKR3AFL  FALSE     NA     NA    
#> 3 BD2000DHS 1999-2000  KR     Children's Reco… BDKR41FL  FALSE     NA     NA    
#> 4 BD2004DHS 2004       KR     Children's Reco… BDKR4JFL  FALSE     NA     NA    
#> 5 BD2007DHS 2007       KR     Children's Reco… BDKR51FL  FALSE     NA     NA    
#> 6 BD2011DHS 2011       KR     Children's Reco… BDKR61FL  FALSE     NA     NA    
#> 7 BD2014DHS 2014       KR     Children's Reco… BDKR72FL  FALSE     NA     NA    
#> 8 BD2017DHS 2017-18    KR     Children's Reco… BDKR7RFL  FALSE     NA     NA    
#> 9 BD2022DHS 2022       KR     Children's Reco… BDKR81FL  FALSE     NA     NA    
#> # ℹ 4 more variables: path <chr>, n_rows <int>, n_cols <int>,
#> #   imported_at <dttm>
```
