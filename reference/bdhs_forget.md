# Remove datasets from the local store

Unregisters datasets so that
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
no longer finds them. Stored copies are deleted only with
`delete = TRUE` (after confirmation in interactive sessions). Original
files that were registered in place with `bdhs_import(copy = FALSE)` are
never deleted.

## Usage

``` r
bdhs_forget(survey, recode = NULL, delete = FALSE)
```

## Arguments

- survey:

  A survey reference (see
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md)).

- recode:

  Recode codes to forget; `NULL` forgets every recode of the survey.

- delete:

  If `TRUE`, also delete the copies kept in the store.

## Value

The removed registry rows, invisibly.

## See also

Other data store:
[`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md),
[`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md),
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md),
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md),
[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md),
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md),
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)

## Examples

``` r
f <- file.path(tempdir(), "BDKR81FL.DTA")
haven::write_dta(bdhs_simulate(2022, "KR", n_clusters = 4, households = 3), f)
bdhs_import(f, format = "dta", quiet = TRUE)
bdhs_forget(2022, "KR", delete = TRUE)
#> ✔ Deleted the stored copy of "BD2022DHS KR".
#> ✔ Unregistered "BD2022DHS KR".
```
