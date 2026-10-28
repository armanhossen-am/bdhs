# bdhs 0.2.0

* `bdhs_synthesize()` creates synthetic BDHS data calibrated to the official
  estimates (national, division and residence), built from published
  aggregates only, and `bdhs_synthetic_fidelity()` reports how closely they
  match. Across all rounds the largest deviation is 0.21 percentage points.
* `bdhs_quality()` runs DHS data-quality diagnostics: age heaping, age
  displacement at the eligibility boundaries, birth-date completeness, births
  by calendar year, heaping of age at death and anthropometric quality.
* `bdhs_compare()` tests changes between rounds and differences between
  groups with design-based standard errors.
* `bdhs_inequality()` and `bdhs_inequality_official()` compute rich-poor gaps,
  the slope and relative indices of inequality and (Erreygers') concentration
  indices.
* `bdhs_decompose()` splits change between rounds into composition and
  within-group effects (Kitagawa decomposition, jackknife SEs).
* `bdhs_sdg()` tracks progress towards SDG-related targets.
* New vignette: "Synthetic data, data quality and inequality".

# bdhs 0.1.0

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
  2017-18 MR vaccine all reproduce the official DHS values. Exclusive
  breastfeeding leaves out country-specific liquids, as in the DHS standard
  definition (2007: 49.8%, official 49.8%); 353 of 355 national comparisons
  now match.
* `data-raw/00-rebuild-from-local.R` rebuilds codebooks and the validation
  record from new DHS downloads (zips included) in one command.
