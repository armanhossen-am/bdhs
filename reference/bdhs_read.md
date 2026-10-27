# Read a BDHS recode file from your data store

Loads any round and recode you have imported with
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)
in one line. Only the requested columns are read from disk, which makes
reading a few variables from even the largest files fast.

## Usage

``` r
bdhs_read(
  survey,
  recode,
  vars = NULL,
  vars_regex = NULL,
  indicators = NULL,
  keep_design = TRUE,
  labels = c("haven", "factor", "codes"),
  n_max = Inf
)
```

## Arguments

- survey:

  A survey reference understood by
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md),
  e.g. `2022`, `"2017-18"` or `"BD2014DHS"`.

- recode:

  A recode code (see
  [`bdhs_recodes()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_recodes.md)),
  e.g. `"IR"` or `"KR"`.

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
`bdhs_file` (the DHS file stem, e.g. `"BDIR81FL"`).

## Details

Column names are returned in lower case. Variable labels and value
labels are kept as
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
columns by default, and Stata extended missing values (`.a` to `.z`) as
tagged `NA`s.

## See also

[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md)
to read a file that is not in the store,
[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md)
to see what you have.

Other data store:
[`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md),
[`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md),
[`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md),
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md),
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md),
[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md),
[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md),
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)

## Examples

``` r
# Synthetic data written under a DHS file name, for illustration only
f <- file.path(tempdir(), "BDIR81FL.DTA")
haven::write_dta(bdhs_simulate(2022, "IR", n_clusters = 6, households = 4), f)
bdhs_import(f, format = "dta", quiet = TRUE)

ir <- bdhs_read(2022, "IR", vars = c("v012", "v106"))
names(ir)
#>  [1] "caseid"  "v000"    "v001"    "v002"    "v005"    "v003"    "v007"   
#>  [8] "v021"    "v022"    "v023"    "v024"    "v025"    "v008"    "v012"   
#> [15] "v106"    "awfactt" "awfactu" "awfactr" "awfacte" "awfactw"

# Exactly the columns two indicators need
ir <- bdhs_read(2022, "IR", indicators = c("fp_cpr_modern", "fp_unmet_total"))
bdhs_estimate(ir, c("fp_cpr_modern", "fp_unmet_total"))
#> # BDHS estimates: 2 indicators | BD2022DHS | 95% CI
#> # A tibble: 2 × 10
#>   survey_id indicator      label      estimate    se ci_low ci_high n_unweighted
#>   <chr>     <chr>          <chr>         <dbl> <dbl>  <dbl>   <dbl>        <int>
#> 1 BD2022DHS fp_cpr_modern  Contracep…    48.0  11.1  18.3      79.1           19
#> 2 BD2022DHS fp_unmet_total Unmet nee…     3.86  4.13  0.115    58.3           19
#> # ℹ 2 more variables: n_weighted <dbl>, api_id <chr>
attr(ir, "bdhs_file")
#> [1] "BDIR81FL"
bdhs_read(2022, "IR", vars_regex = "^v1[0-9]{2}$", keep_design = FALSE, labels = "factor")
#> # A tibble: 22 × 5
#>    v106          v133 v130     v190       v191
#>    <fct>        <dbl> <fct>    <fct>     <dbl>
#>  1 primary          2 islam    poorer       99
#>  2 higher          14 islam    poorest  -34766
#>  3 primary          4 islam    richer    51054
#>  4 no education     0 islam    richer    50078
#>  5 primary          5 islam    richest  105222
#>  6 primary          5 islam    poorer    -9192
#>  7 higher          11 islam    richest  123568
#>  8 primary          2 islam    poorest -153754
#>  9 primary          4 islam    poorest  -52427
#> 10 primary          3 hinduism poorer     7740
#> # ℹ 12 more rows
#> ✔ Deleted the stored copy of "BD2022DHS IR".
#> ✔ Unregistered "BD2022DHS IR".
```
