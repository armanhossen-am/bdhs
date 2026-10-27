# Report on your bdhs set-up

Prints where your data store is, whether it is writable and stored
safely (not inside a git repository or a cloud-synchronised folder),
which datasets are registered, the versions of the packages used to read
and store them, and reminders of the DHS Program's conditions of use.

## Usage

``` r
bdhs_sitrep()
```

## Value

A list with elements `data_dir`, `data_dir_source`, `exists`,
`writable`, `git_root`, `sync_service`, `registry` (one row per survey
with its registered recodes) and `packages`, invisibly.

## See also

Other data store:
[`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md),
[`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md),
[`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md),
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md),
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md),
[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md),
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md)

## Examples

``` r
bdhs_sitrep()
#> 
#> ── bdhs situation report ───────────────────────────────────────────────────────
#> 
#> ── Data directory ──
#> 
#> • /home/runner/.local/share/R/bdhs (default (tools::R_user_dir))
#> ℹ Does not exist yet; it is created on the first `bdhs_import()`.
#> ✔ Writable
#> ✔ Not inside a git repository
#> ✔ Not inside a known cloud-sync folder
#> 
#> ── Registered datasets ──
#> 
#> ℹ None yet. Download your files from the DHS Program and run
#>   `bdhs_import("path")`.
#> 
#> ── Packages ──
#> 
#> • bdhs 0.1.0
#> • haven 2.5.5
#> • arrow 26.0.0 (Parquet storage)
#> • httr2 1.3.0 (for the experimental `bdhs_download()`)
#> • keyring 1.4.1 (DHS password store)
#> 
#> ── Conditions of use ──
#> 
#> ! DHS microdata are licensed to you for your registered research project only.
#>   Do not share or redistribute them, directly or within any tool, package,
#>   repository or supplementary file.
#> ! Keep the files secure, and cite the survey (NIPORT and ICF) in every
#>   publication.
#> ! Bangladesh condition: send a copy of every publication that uses BDHS data to
#>   the NIPORT library (niportru@citechco.net).
#> ℹ Full terms: <https://dhsprogram.com/data/terms-of-use.cfm>
```
