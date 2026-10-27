# Download BDHS datasets with your DHS Program account (experimental)

**Experimental.** The DHS Program has no download API, so this function
signs in to <https://dhsprogram.com> with *your* account and uses the
website's download manager, as a browser would (the approach of the rdhs
package). Website changes can break it at any time; the reliable route
is to download the files yourself and run
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md).

You can only download datasets that the DHS Program has approved for
your project. Register at
<https://dhsprogram.com/data/new-user-registration.cfm> and request
access to the Bangladesh surveys first.

## Usage

``` r
bdhs_download(
  survey,
  recode,
  format = "stata",
  email = Sys.getenv("DHS_EMAIL"),
  project = Sys.getenv("DHS_PROJECT"),
  import = TRUE,
  engine = c("bdhs", "rdhs")
)
```

## Arguments

- survey:

  Survey references (see
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md)).

- recode:

  Recode codes (see
  [`bdhs_recodes()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_recodes.md)).

- format:

  File format to download: `"stata"` (recommended), `"spss"` or `"sas"`.

- email:

  Email address of your DHS Program account.

- project:

  Name of your approved DHS project.

- import:

  If `TRUE` (default), import the downloaded files with
  [`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md);
  bdhs's own downloader then deletes the downloaded zips. If `FALSE`,
  the zips are kept in a `downloads` folder of
  [`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md)
  (you can import them later with
  [`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)).

- engine:

  `"bdhs"` (default) uses bdhs's own downloader (needs the httr2
  package). `"rdhs"` delegates to
  [`rdhs::get_datasets()`](https://docs.ropensci.org/rdhs/reference/get_datasets.html);
  configure rdhs first with
  [`rdhs::set_rdhs_config()`](https://docs.ropensci.org/rdhs/reference/set_rdhs_config.html).
  rdhs keeps the zips it downloads in its own cache folder (see
  [`rdhs::get_rdhs_config()`](https://docs.ropensci.org/rdhs/reference/get_rdhs_config.html)),
  and bdhs leaves that cache alone, with `import = TRUE` as well: delete
  the cached copies yourself if you do not want two copies of the data.

## Value

If `import = TRUE`, the registry rows of the imported datasets (see
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md));
otherwise a tibble with the downloaded `file_name` and `path`.
Invisibly.

## Credentials

- `email` and `project` default to the `DHS_EMAIL` and `DHS_PROJECT`
  environment variables (for example set in your `.Renviron`). `project`
  is the name of your approved DHS project (its first 30 characters are
  enough).

- The password is taken from the system keyring when the keyring package
  is installed (service `"bdhs-dhs"`, username = your email; store it
  once with
  `keyring::key_set("bdhs-dhs", username = "you@example.org")`), then
  from the `DHS_PASSWORD` environment variable, and is otherwise asked
  for at a hidden prompt in interactive sessions (this needs the askpass
  package; bdhs never asks for it with a prompt that shows what you
  type). bdhs never stores, prints or logs it, and does not keep it in
  options.

- The password is only sent to `https://dhsprogram.com`: bdhs does not
  follow redirects of the sign-in form to other sites, and only follows
  download links that point to `https://dhsprogram.com` (or its
  subdomains).

In non-interactive sessions without credentials the function stops with
instructions for downloading the files manually.

## See also

Other data store:
[`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md),
[`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md),
[`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md),
[`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md),
[`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md),
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
[`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md),
[`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("DHS_EMAIL"))
bdhs_download(2022, c("IR", "KR"))
bdhs_read(2022, "KR", vars = "hw70")
}
```
