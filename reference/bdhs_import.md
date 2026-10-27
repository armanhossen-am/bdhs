# Import DHS microdata you have downloaded

Registers DHS data files in the local bdhs data store so that
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
can load any round and recode in one line. Point it at the files, zip
archives or folders you downloaded from the DHS Program: files are
identified with
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md)
whatever they are called, folders are searched recursively and zip
archives (also nested ones, such as the bulk "STATA_BD\_..." downloads)
are unpacked to a temporary folder.

## Usage

``` r
bdhs_import(
  path,
  copy = TRUE,
  format = c("auto", "parquet", "dta"),
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- path:

  Paths to data files (`.dta`, `.sav`, `.sas7bdat`), zip archives or
  folders.

- copy:

  Keep a copy in the data store (`TRUE`, default) or register the files
  in place (`FALSE`).

- format:

  Storage format for copies: `"auto"` (Parquet if arrow is installed,
  otherwise the original file), `"parquet"` or `"dta"` (a byte-for-byte
  copy of the original Stata, SPSS or SAS file).

- overwrite:

  Replace datasets that are already registered from a different file (or
  re-import identical ones).

- quiet:

  If `TRUE`, do not print the import summary. Warnings are still shown.

## Value

The registry rows of the imported datasets, invisibly: columns
`survey_id`, `recode`, `file_stem`, `format`, `path`, `linked`,
`source_path`, `md5`, `n_rows`, `n_cols`, `imported_at` and
`bdhs_version`.

## Details

When the same dataset is found in several formats, the Stata version is
preferred, then SPSS, then SAS. Files that were already imported (same
MD5 checksum) are skipped, as are derived or pooled files (several
rounds appended together, or with variables that are not in the DHS
recode). If the file registered with the same checksum no longer exists
(it was moved or deleted), the file is registered again from its new
location.

Files whose name and content disagree (for example a children's recode
saved as `BDIR81FL.DTA`), and renamed files that their content does not
confirm as Bangladesh DHS recode files, are not imported; see
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md).

Files inside the data directory are not imported, except the zips that
`bdhs_download(import = FALSE)` keeps in its `downloads` folder. Only
one R session at a time can import into (or
[`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md)
from) a data directory; another session waits up to a minute (option
`bdhs.lock_timeout`, in seconds) for it to finish.

With `copy = TRUE` (default) a copy is kept in
[`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md):
with `format = "auto"` it is converted to Parquet when the arrow package
is installed (much faster to read selected columns; variable labels,
value labels and Stata extended missing values are kept), otherwise the
original file is copied. With `copy = FALSE` files are registered where
they are and nothing is copied; files inside zip archives are always
copied.

DHS microdata are licensed to you personally and must not be shared.
bdhs never uploads or shares them, and warns when the data directory is
inside a git repository or a cloud-synchronised folder.

## See also

Other data store:
[`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md),
[`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md),
[`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md),
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md),
[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md),
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md),
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)

## Examples

``` r
# Synthetic files with DHS names, for illustration only
dl <- file.path(tempdir(), "dhs-downloads")
dir.create(dl, showWarnings = FALSE)
haven::write_dta(bdhs_simulate(2022, "KR", n_clusters = 4, households = 3),
                 file.path(dl, "BDKR81FL.DTA"))
haven::write_dta(bdhs_simulate(2022, "HR", n_clusters = 4, households = 3),
                 file.path(dl, "BDHS2022HR.dta"))
bdhs_import(dl, format = "dta")
#> ✔ Imported 2 datasets into /tmp/Rtmpb9FBu8/bdhs-import-example:
#> • BD2022DHS HR (BDHR81FL): copied (.dta), 12 rows x 18 variables
#> • BD2022DHS KR (BDKR81FL): copied (.dta), 2 rows x 49 variables
bdhs_inventory(2022)
#> # A tibble: 11 × 12
#>    survey_id year_label recode recode_name     file_stem available format linked
#>    <chr>     <chr>      <chr>  <chr>           <chr>     <lgl>     <chr>  <lgl> 
#>  1 BD2022DHS 2022       BR     Births Recode   BDBR81FL  FALSE     NA     NA    
#>  2 BD2022DHS 2022       FW     Fieldworker Qu… BDFW81FL  FALSE     NA     NA    
#>  3 BD2022DHS 2022       GC     Geospatial Cov… BDGC81FL  FALSE     NA     NA    
#>  4 BD2022DHS 2022       GE     Geographic Dat… BDGE81FL  FALSE     NA     NA    
#>  5 BD2022DHS 2022       GR     Pregnancies Re… BDGR81FL  FALSE     NA     NA    
#>  6 BD2022DHS 2022       HR     Household Reco… BDHR81FL  TRUE      dta    FALSE 
#>  7 BD2022DHS 2022       IR     Individual (wo… BDIR81FL  FALSE     NA     NA    
#>  8 BD2022DHS 2022       KR     Children's Rec… BDKR81FL  TRUE      dta    FALSE 
#>  9 BD2022DHS 2022       NR     Pregnancy and … BDNR81FL  FALSE     NA     NA    
#> 10 BD2022DHS 2022       PR     Household Memb… BDPR81FL  FALSE     NA     NA    
#> 11 BD2022DHS 2022       VA     Verbal Autopsy  BDVA81FL  FALSE     NA     NA    
#> # ℹ 4 more variables: path <chr>, n_rows <int>, n_cols <int>,
#> #   imported_at <dttm>
#> ✔ Deleted the stored copies of "BD2022DHS HR" and "BD2022DHS KR".
#> ✔ Unregistered "BD2022DHS HR" and "BD2022DHS KR".
```
