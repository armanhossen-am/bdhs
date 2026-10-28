# Changelog

## bdhs 0.2.0

- [`bdhs_synthesize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_synthesize.md)
  creates synthetic BDHS data calibrated to the official estimates
  (national, division and residence), built from published aggregates
  only, and
  [`bdhs_synthetic_fidelity()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_synthetic_fidelity.md)
  reports how closely they match. Across all rounds the largest
  deviation is 0.21 percentage points.
- [`bdhs_quality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_quality.md)
  runs DHS data-quality diagnostics: age heaping, age displacement at
  the eligibility boundaries, birth-date completeness, births by
  calendar year, heaping of age at death and anthropometric quality.
- [`bdhs_compare()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_compare.md)
  tests changes between rounds and differences between groups with
  design-based standard errors.
- [`bdhs_inequality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality.md)
  and
  [`bdhs_inequality_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality_official.md)
  compute rich-poor gaps, the slope and relative indices of inequality
  and (Erreygers’) concentration indices.
- [`bdhs_decompose()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_decompose.md)
  splits change between rounds into composition and within-group effects
  (Kitagawa decomposition, jackknife SEs).
- [`bdhs_sdg()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sdg.md)
  tracks progress towards SDG-related targets.
- New vignette: “Synthetic data, data quality and inequality”.

## bdhs 0.1.0

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
