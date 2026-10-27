## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local macOS (aarch64-apple-darwin23), R 4.6.1
* GitHub Actions: macOS, Windows and Ubuntu (R release), Ubuntu (R devel
  and oldrel-1)

## Notes for CRAN

* The package bundles only data the DHS Program allows to be redistributed:
  aggregate statistics from the DHS Program API, survey and dataset
  metadata, codebooks without respondent values, and division boundaries
  under the Open Database License (see LICENSE.note). Survey microdata are
  never bundled; users import their own downloads.
* Examples, tests and vignettes use synthetic data from `bdhs_simulate()`,
  do not access the internet and write only to `tempdir()`.
* Tests that need the maintainer's licensed microdata are skipped unless the
  environment variable `BDHS_LOCAL_TESTS` is set.
