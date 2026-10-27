# Changelog

## bdhs 0.1.0.9000

- First release. Covers all nine BDHS rounds (1993-94 to 2022), the
  three Bangladesh Service Provision Assessments and the 2001 maternal
  mortality survey.
- Bundled data: official DHS estimates for Bangladesh
  (`bdhs_indicators`), indicator definitions, survey and dataset
  catalogues, cross-round codebooks, division crosswalks, pooling
  populations and division boundaries.
- Microdata access for registered DHS users:
  [`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md),
  [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
  [`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md)
  and an experimental
  [`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md).
- Analysis:
  [`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md),
  [`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md),
  [`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md),
  [`bdhs_merge()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_merge.md),
  [`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md),
  [`bdhs_fertility()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_fertility.md),
  [`bdhs_mortality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_mortality.md)
  and
  [`bdhs_validate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validate.md).
- Synthetic data for teaching and testing:
  [`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md).
- Safety: microdata file types can never enter the built package (also
  checked in CI); renamed files must be confirmed by their contents and
  name/content conflicts are refused; the DHS password is only ever sent
  to `https://dhsprogram.com` and never kept after a failed login;
  imports lock the data store, so concurrent sessions cannot lose files.
- Validated statistics: child marriage by subgroup, GFR (births to
  mothers under 15), all-woman factors for harmonized grouping variables
  and the 2017-18 MR vaccine all reproduce the official DHS values.
  Exclusive breastfeeding leaves out country-specific liquids, as in the
  DHS standard definition (2007: 49.8%, official 49.8%); 353 of 355
  national comparisons now match.
- `data-raw/00-rebuild-from-local.R` rebuilds codebooks and the
  validation record from new DHS downloads (zips included) in one
  command.
