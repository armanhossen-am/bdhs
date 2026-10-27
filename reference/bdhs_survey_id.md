# Resolve survey identifiers

Converts the many ways people refer to a BDHS round into the canonical
DHS survey id (for example `"BD2017DHS"`). Years, year ranges, DHS
survey ids, file version codes and phase-qualified codes are all
accepted, so `2017`, `2018`, `"2017-18"`, `"BD2017DHS"` and `"7R"` all
resolve to the 2017-18 BDHS.

## Usage

``` r
bdhs_survey_id(x = NULL, type = c("DHS", "SPA", "OTH"), error = TRUE)
```

## Arguments

- x:

  A vector of survey references (numeric or character). `NULL` returns
  every survey of the requested `type`.

- type:

  Survey types to consider: any of `"DHS"`, `"SPA"`, `"OTH"`.

- error:

  If `TRUE` (default), unresolvable references are an error; otherwise
  they become `NA`.

## Value

A character vector of DHS survey ids, the same length as `x`.

## Details

Plain years and year ranges resolve to the main DHS round. Use the
explicit id (`"BD2014SPA"`) or a prefixed alias (`"spa2014"`,
`"bmms2001"`) for the Service Provision Assessments and the 2001
maternal mortality survey.

## Examples

``` r
bdhs_survey_id(c(2022, "2017-18", "1999-2000", "BD2014DHS", "7R"))
#> [1] "BD2022DHS" "BD2017DHS" "BD2000DHS" "BD2014DHS" "BD2017DHS"
bdhs_survey_id("spa2017")
#> [1] "BD2017SPA"
bdhs_survey_id()
#>  [1] "BD1994DHS" "BD1997DHS" "BD2000DHS" "BD2004DHS" "BD2007DHS" "BD2011DHS"
#>  [7] "BD2014DHS" "BD2017DHS" "BD2022DHS" "BD1999SPA" "BD2014SPA" "BD2017SPA"
#> [13] "BD2001OTH"
```
