# Data files of the Bangladesh surveys

Catalogue of every data file the DHS Program distributes for Bangladesh:
each recode (IR, KR, BR, HR, PR, ...) of each survey in each format,
plus the SPA, BMMS and GPS files. bdhs uses it to recognise files you
have downloaded and to know which recodes exist for a round.

## Usage

``` r
bdhs_datasets
```

## Format

A tibble with 441 rows (one per file) and 14 columns:

- survey_id:

  DHS survey identifier (see
  [bdhs_surveys](https://armanhossen-am.github.io/bdhs/reference/bdhs_surveys.md)).

- survey_type:

  `"DHS"`, `"SPA"` or `"OTH"`.

- survey_year:

  Survey year.

- file_name:

  Name of the zip file as distributed, e.g. `"BDIR7RFL.ZIP"`.

- file_stem:

  Upper-case file name without extension, e.g. `"BDIR7RFL"`.

- file_type_code:

  Two-letter recode/file type code (characters 3-4 of the name): IR
  women, KR children under 5, BR births, HR households, PR household
  members, MR men, CR couples, GR/NR pregnancies (2022), WI wealth index
  and HW height/weight (early rounds), FW fieldworkers, SQ service
  availability, VA verbal autopsy, HH/IQ BMMS household and individual,
  FC/PV/SL/CO SPA facility, provider, staff listing and community, GE/GC
  GPS coordinates and geospatial covariates.

- file_type:

  File type as described by the DHS Program.

- recode_version:

  Two-character version code (characters 5-6).

- format_code:

  File format code (characters 7-8): `"DT"` Stata, `"FL"` flat ASCII,
  `"SD"` SAS, `"SV"` SPSS; `NA` for files whose name has no format code.

- file_format:

  File format as described by the DHS Program.

- spa_variant:

  For SPA files, `"SP"` or `"SR"` (characters 9-10); otherwise `NA`.

- dataset_type:

  `"Survey Datasets"`, `"SPA Datasets"` or `"GPS Datasets"`.

- file_size:

  Size of the zip file in bytes.

- last_modified:

  Date the file was last modified by the DHS Program.

## Source

The DHS Program Indicator Data API, `datasets` endpoint,
<https://api.dhsprogram.com>, retrieved 2026-10-26.

## Details

The files themselves are **not** included: the DHS Program licenses
microdata to registered users only.

## See also

[bdhs_surveys](https://armanhossen-am.github.io/bdhs/reference/bdhs_surveys.md),
[`bdhs_recodes()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_recodes.md),
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md).

## Examples

``` r
# Stata files of the 2022 BDHS
bdhs_datasets[bdhs_datasets$survey_id == "BD2022DHS" &
  bdhs_datasets$format_code %in% "DT", c("file_name", "file_type")]
#> # A tibble: 9 × 2
#>   file_name    file_type                          
#>   <chr>        <chr>                              
#> 1 BDBR81DT.ZIP Births Recode                      
#> 2 BDFW81DT.ZIP Fieldworker Questionnaire          
#> 3 BDGR81DT.ZIP Pregnancies Recode                 
#> 4 BDHR81DT.ZIP Household Recode                   
#> 5 BDIR81DT.ZIP Individual Recode                  
#> 6 BDKR81DT.ZIP Children's Recode                  
#> 7 BDNR81DT.ZIP Pregnancy and Postnatal Care Recode
#> 8 BDPR81DT.ZIP Household Member Recode            
#> 9 BDVA81DT.ZIP Verbal Autopsy                     
```
