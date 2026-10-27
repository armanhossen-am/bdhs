# Working with your BDHS microdata

``` r

library(bdhs)
```

## Why bdhs does not ship the microdata

The DHS Program gives respondent-level data free of charge to registered
researchers. Its terms of use say the data must not be passed on to
others or redistributed “directly or within any tool”. So bdhs never
bundles, uploads or shares microdata, and you should keep your files
somewhere private: not in a public GitHub repository and preferably not
in a synced cloud folder.

Getting access is quick:

1.  Create an account at
    <https://dhsprogram.com/data/new-user-registration.cfm>.
2.  Describe your research project and request the Bangladesh surveys.
3.  Once approved, download the datasets. The **Stata** format (`.DTA`,
    in files like `BDIR81DT.ZIP`) is recommended because it keeps all
    variable and value labels.

Analyses published from BDHS data must cite the survey’s final report.
The Bangladesh conditions of use also ask for a copy of each publication
to be sent to the NIPORT library.

## What exists

[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md)
lists every dataset the DHS Program distributes for Bangladesh and marks
which ones you already have:

``` r

bdhs_inventory()
```

The main recode files are:

``` r

bdhs_recodes()[1:10, ]
#>    recode                                     name
#> 1      IR              Individual (women's) Recode
#> 2      MR                             Men's Recode
#> 3      KR                        Children's Recode
#> 4      BR                            Births Recode
#> 5      HR                         Household Recode
#> 6      PR                  Household Member Recode
#> 7      CR                          Couples' Recode
#> 8      GR                       Pregnancies Recode
#> 9      NR      Pregnancy and Postnatal Care Recode
#> 10     HW Height and Weight (WHO Growth Standards)
#>                                         unit family
#> 1                                      Woman    DHS
#> 2                                        Man    DHS
#> 3  Child under 5 (born to interviewed women)    DHS
#> 4                 Birth (full birth history)    DHS
#> 5                                  Household    DHS
#> 6                           Household member    DHS
#> 7                                     Couple    DHS
#> 8              Pregnancy (pregnancy history)    DHS
#> 9                           Recent pregnancy    DHS
#> 10               Child measured in household    DHS
```

Not every recode exists for every round. For example, men were last
interviewed in 2011, and the pregnancy (`GR`) and postnatal care (`NR`)
recodes are new in 2022.

## Importing your downloads

Point
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)
at the files, zips or folders you downloaded.

``` r

bdhs_import("~/Downloads/BDHS")
```

[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)
does the following:

- Recognises DHS files by name (`BDIR81FL.DTA`), inside zips
  (`BDIR81DT.ZIP`), with prefixes (`2022BDPR81FL.DTA`), and even when
  you renamed them (`BDHS2018KR.DTA`). In that case it checks the file’s
  contents.
- Skips files that are not original DHS recodes, such as your own merged
  or appended datasets.
- Stores a fast copy in the bdhs data directory. With the arrow package
  installed this is a column-oriented copy, so reading a few variables
  from a 5,000-column file is quick. Use `copy = FALSE` to leave files
  where they are and only register them.
- Warns if the data directory is inside a git repository or a cloud-sync
  folder.

The data directory defaults to `tools::R_user_dir("bdhs", "data")`. Use
`bdhs_data_dir("path")` to change it for the session, or set the
`BDHS_DATA_DIR` environment variable in your `.Renviron` to change it
for good.
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)
summarises your setup.

## A complete example with synthetic files

The chunk below runs the full workflow on synthetic files written to a
temporary folder, so you can see each step without real data.

``` r

# A temporary data directory and some fake DHS files. Both the option and
# the environment variable are set, so your own store is never touched.
store <- file.path(tempdir(), "bdhs-store")
old_opt <- options(bdhs.data_dir = store)
old_env <- Sys.getenv("BDHS_DATA_DIR", unset = NA)
Sys.setenv(BDHS_DATA_DIR = store)

downloads <- file.path(tempdir(), "downloads")
dir.create(downloads, showWarnings = FALSE)
haven::write_dta(bdhs_simulate(2022, "IR"), file.path(downloads, "BDIR81FL.DTA"))
haven::write_dta(bdhs_simulate(2022, "KR"), file.path(downloads, "BDKR81FL.DTA"))
haven::write_dta(bdhs_simulate(2017, "HR"), file.path(downloads, "BDHS2018HR.DTA"))
```

``` r

bdhs_import(downloads)
#> ✔ Imported 3 datasets into /tmp/Rtmp4ESaAC/bdhs-store:
#> • BD2017DHS HR (BDHR7RFL): stored as Parquet, 600 rows x 18 variables
#> • BD2022DHS IR (BDIR81FL): stored as Parquet, 552 rows x 116 variables
#> • BD2022DHS KR (BDKR81FL): stored as Parquet, 296 rows x 49 variables
```

``` r

ir <- bdhs_read(2022, "IR", vars = c("v012", "v106", "v313"))
ir
#> # A tibble: 552 × 21
#>    caseid     v000   v001  v002   v005  v003  v007  v021 v022    v023    v024   
#>    <chr>      <chr> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl+l> <dbl+l> <dbl+l>
#>  1 "       1… BD8       1     2 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#>  2 "       1… BD8       1     3 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#>  3 "       1… BD8       1     4 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#>  4 "       1… BD8       1     5 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#>  5 "       1… BD8       1     6 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#>  6 "       1… BD8       1     7 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#>  7 "       1… BD8       1     8 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#>  8 "       1… BD8       1     9 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#>  9 "       1… BD8       1    10 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#> 10 "       1… BD8       1    11 744251     2  2022     1 1 [bar… 1 [bar… 1 [bar…
#> # ℹ 542 more rows
#> # ℹ 10 more variables: v025 <dbl+lbl>, v008 <dbl>, v012 <dbl>, v106 <dbl+lbl>,
#> #   v313 <dbl+lbl>, awfactt <dbl>, awfactu <dbl>, awfactr <dbl>, awfacte <dbl>,
#> #   awfactw <dbl>
attr(ir, "bdhs_survey_id")
#> [1] "BD2022DHS"
```

When you ask for specific variables,
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
always adds the identifiers and design variables (cluster, strata,
weight, region, residence and the all-woman factors). That way the
result can go straight into
[`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md)
or
[`bdhs_merge()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_merge.md).
Use `labels = "factor"` to get factors instead of labelled numbers.

## Downloading with your DHS account (experimental)

[`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md)
can log in to the DHS website with **your** account and fetch files
directly:

``` r

bdhs_download(2022, c("IR", "KR"), email = "you@university.edu", project = "Your project title")
```

Your password is never stored by bdhs. It is read from the system
keyring (keyring package, service `"bdhs-dhs"`), the `DHS_PASSWORD`
environment variable, or an interactive prompt. The DHS website can
change without notice, so if the downloader fails, download the files in
your browser and use
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md).

## Reading one-off files

[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md)
reads any DHS file without registering it and applies the same
conventions: lower-case names, preserved labels and detected survey and
recode.

``` r

pr <- bdhs_read_file("~/Downloads/BDPR81FL.DTA", vars = c("hv104", "hv105"))
```
