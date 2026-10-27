# bdhs 0.1.0.9000

* First release. Covers all nine BDHS rounds (1993-94 to 2022), the three
  Bangladesh Service Provision Assessments and the 2001 maternal mortality
  survey.
* Bundled data: official DHS estimates for Bangladesh (`bdhs_indicators`),
  indicator definitions, survey and dataset catalogues, cross-round codebooks,
  division crosswalks, pooling populations and division boundaries.
* Microdata access for registered DHS users: `bdhs_import()`, `bdhs_read()`,
  `bdhs_inventory()` and an experimental `bdhs_download()`.
* Analysis: `bdhs_harmonize()`, `bdhs_design()`, `bdhs_pool()`, `bdhs_merge()`,
  `bdhs_estimate()`, `bdhs_fertility()`, `bdhs_mortality()` and
  `bdhs_validate()`.
* Synthetic data for teaching and testing: `bdhs_simulate()`.
* Safety: microdata file types can never enter the built package (also
  checked in CI); renamed files must be confirmed by their contents and
  name/content conflicts are refused; the DHS password is only ever sent to
  `https://dhsprogram.com` and never kept after a failed login; imports lock
  the data store, so concurrent sessions cannot lose files.
* Validated statistics: child marriage by subgroup, GFR (births to mothers
  under 15), all-woman factors for harmonized grouping variables and the
  2017-18 MR vaccine all reproduce the official DHS values.
