# Read a DHS data file directly

Reads a Stata (`.dta`), SPSS (`.sav`) or SAS (`.sas7bdat`) DHS file, or
a DHS zip archive containing one, without importing it, using the same
conventions as
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md):
lower-case names, labels kept, column selection without reading the
whole file. The survey round and recode are identified with
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md)
and stored as attributes.

## Usage

``` r
bdhs_read_file(
  path,
  vars = NULL,
  vars_regex = NULL,
  indicators = NULL,
  keep_design = TRUE,
  labels = c("haven", "factor", "codes"),
  n_max = Inf
)
```

## Arguments

- path:

  Path to the file.

- vars:

  Variables to read (case-insensitive). `NULL` reads all of them unless
  `vars_regex` is given. Requested variables that do not exist in the
  file are skipped with a warning.

- vars_regex:

  Regular expressions; variables whose (lower-case) names match any of
  them are read too.

- indicators:

  Indicator ids from
  [`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md).
  Every variable these indicators need is read, so the result can go
  straight into
  [`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md).
  Combines with `vars` and `vars_regex`.

- keep_design:

  If `TRUE` (default) and variables are selected, also read the
  identifiers, survey design variables and interview dates needed to
  link files, detect the round and build a survey design with
  [`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md):
  `caseid`, `hhid`, `hvidx`, `bidx`, `midx`, `v000`-`v003`, `v005`,
  `v007`, `v008`, `v021`-`v025` (and their `hv`/`mv` equivalents), the
  child's birth date and age (`b3`, `b19`) and the all-woman factors
  `awfact*`.

- labels:

  How to return labelled variables: `"haven"` (default) keeps
  [`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
  columns; `"factor"` converts every variable whose values are all
  labelled to a factor with
  [`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)
  (`levels = "labels"`), leaving partially labelled variables such as
  z-scores with flag codes as labelled numbers; `"codes"` drops value
  labels (keeping variable labels) and turns extended missing values
  into plain `NA`.

- n_max:

  Maximum number of rows to read.

## Value

A tibble with the attributes `bdhs_survey_id`, `bdhs_recode` and
`bdhs_file` when the file could be identified.

## See also

Other data store:
[`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md),
[`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md),
[`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md),
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md),
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md),
[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md),
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)

## Examples

``` r
f <- file.path(tempdir(), "BDHS2022KR.dta")
haven::write_dta(bdhs_simulate(2022, "KR", n_clusters = 4, households = 3), f)
kr <- bdhs_read_file(f, vars = c("hw70", "b4"))
attr(kr, "bdhs_survey_id")
#> [1] "BD2022DHS"
```
