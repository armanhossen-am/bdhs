# Identify DHS microdata files

Works out which survey round and recode each file holds, so that files
can be imported whatever they are called. Standard DHS names
(`BDIR81FL.DTA`, `BDKR7RDT.ZIP`, lower-case or prefixed, e.g.
`2022BDPR81FL.DTA`) are recognised from the name alone (their variable
names are checked against the recode). Renamed files such as
`BDHS2018KR.DTA` or `SPA2014_FC.dta` are recognised from the year and
recode code in the name and must be confirmed by their content: a
Bangladesh country code (`v000`, `hv000` or `mv000`) and the identifier
variables of the recode. Anything else is identified from its content,
reading only the header and a few columns (`v000`/`hv000`,
`v007`/`hv007` and the identifier variables).

## Usage

``` r
bdhs_identify_file(path)
```

## Arguments

- path:

  Paths to files, folders (searched recursively) or zip archives (their
  members are listed, not extracted).

## Value

A tibble with one row per data file and columns `path`, `file_stem` (the
DHS file stem, e.g. `"BDIR81FL"`), `survey_id`, `recode`, `format`,
`method` (`"filename"` or `"content"`), `derived` and `note`.

## Details

Files whose name and content disagree (another round, recode or
country), and renamed files that their content does not confirm, are
left unidentified (`survey_id` and `recode` are `NA`) with a warning;
the `note` column says why.
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)
does not import them.

Derived files – several rounds appended together, or files with
variables that are not in the DHS recode – are flagged with
`derived = TRUE`.
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)
skips them, because bdhs only stores original DHS files.

## See also

Other data store:
[`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md),
[`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md),
[`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md),
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md),
[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md),
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md),
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)

## Examples

``` r
f <- file.path(tempdir(), "BDHS2022KR.dta")
haven::write_dta(bdhs_simulate(2022, "KR", n_clusters = 4, households = 3), f)
bdhs_identify_file(f)
#> # A tibble: 1 × 8
#>   path                    file_stem survey_id recode format method derived note 
#>   <chr>                   <chr>     <chr>     <chr>  <chr>  <chr>  <lgl>   <chr>
#> 1 /tmp/Rtmp7On83P/BDHS20… BDKR81FL  BD2022DHS KR     dta    filen… FALSE   NA   
```
