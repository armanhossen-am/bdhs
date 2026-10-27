# Where bdhs keeps your DHS microdata

`bdhs_data_dir()` returns the directory of the local data store used by
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)
and
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md).
It is resolved in this order:

## Usage

``` r
bdhs_data_dir(path = NULL)
```

## Arguments

- path:

  Optional new data directory. It is set for the current R session only
  (as the `bdhs.data_dir` option); the message tells you the line to add
  to your `.Renviron` to make it permanent. Nothing is written to your
  configuration files.

## Value

The data directory, as a normalised path (invisibly when `path` is
given).

## Details

1.  the `bdhs.data_dir` option,

2.  the `BDHS_DATA_DIR` environment variable,

3.  `tools::R_user_dir("bdhs", "data")`.

The directory is created only when you first import data. DHS microdata
are licensed to you personally: keep the store on an encrypted disk, and
outside git repositories and cloud-synchronised folders (Dropbox,
OneDrive, Google Drive, iCloud).
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)
and
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)
warn when it is not.

## See also

Other data store:
[`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md),
[`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md),
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md),
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md),
[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md),
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md),
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)

## Examples

``` r
bdhs_data_dir()
#> [1] "/home/runner/.local/share/R/bdhs"

# Use another location for this session
old <- options(bdhs.data_dir = NULL)
bdhs_data_dir(file.path(tempdir(), "bdhs-data"))
#> ✔ bdhs data directory set to /tmp/RtmppOGOQf/bdhs-data for this R session.
#> ℹ To keep it in future sessions, add this line to your .Renviron (for example
#>   with `usethis::edit_r_environ()`):
#>   `BDHS_DATA_DIR=/tmp/RtmppOGOQf/bdhs-data`
bdhs_data_dir()
#> [1] "/tmp/RtmppOGOQf/bdhs-data"
options(old)
```
