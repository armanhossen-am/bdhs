# Developing bdhs

Notes for anyone (people or coding agents) changing this package.

## Rules that are never broken

1.  **No DHS microdata in the repository or the package.** DHS terms of
    use forbid redistributing respondent-level data “directly or within
    any tool”. Never commit, bundle, snapshot, print or save row-level
    values. `.gitignore`, `.Rbuildignore` and `tools/check-tarball.sh`
    (run in CI) block data file types; do not weaken them. Metadata
    (names, labels, value labels, counts) and aggregate estimates are
    fine.
2.  **Examples, tests and vignettes run without real data**, using
    [`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md)
    and temporary directories
    ([`withr::local_tempdir()`](https://withr.r-lib.org/reference/with_tempfile.html)).
    They never use the network and never write outside
    [`tempdir()`](https://rdrr.io/r/base/tempfile.html).
3.  **Tests on the maintainer’s real files** are skipped unless
    `BDHS_LOCAL_TESTS=true`; files are found under `BDHS_LOCAL_DIR`
    (default `~/Documents/MU/Research`). Use `skip_if_no_local_data()`
    and `local_data_dir()` from `tests/testthat/helper-local.R`. Such
    tests assert on aggregates only.
4.  **Never handle credentials.**
    [`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md)
    reads the DHS password from keyring, `DHS_PASSWORD` or an askpass
    prompt.

## Layout

| Area | Files | Exported functions |
|----|----|----|
| Rounds, recodes | `surveys.R`, `recodes.R` | [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md), [`bdhs_rounds()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_rounds.md), [`bdhs_recodes()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_recodes.md) |
| Synthetic data | `simulate.R` | [`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md) |
| Bundled data | `data.R`, `data-raw/` | `bdhs_indicators`, `bdhs_indicator_catalog`, `bdhs_surveys`, `bdhs_datasets`, `bdhs_codebook`, `bdhs_value_labels`, `bdhs_regions`, `bdhs_pool_population` |
| Official estimates, search, plots, maps | `official.R`, `search.R`, `describe.R`, `plot.R`, `map.R` | [`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md), [`bdhs_search()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_search.md), [`bdhs_variable()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_variable.md), [`bdhs_describe()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_describe.md), [`bdhs_plot_trend()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_plot_trend.md), [`bdhs_boundaries()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_boundaries.md), [`bdhs_map()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_map.md) |
| Your files | `store.R`, `identify.R`, `import.R`, `read.R`, `download.R`, `sitrep.R` | [`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md), [`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md), [`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md), [`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md), [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md), [`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md), [`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md), [`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md), [`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md) |
| Harmonization | `crosswalk.R`, `harmonize.R`, `pool.R`, `merge.R`, `missing.R` | [`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md), [`bdhs_harmonized_vars()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonized_vars.md), [`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md), [`bdhs_merge()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_merge.md), [`bdhs_na()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_na.md) |
| Design and estimation | `design.R`, `indicators.R`, `indicator-defs-*.R`, `estimate.R`, `rates.R`, `validate.R` | [`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md), [`bdhs_design_info()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design_info.md), [`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md), [`bdhs_add_indicators()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_add_indicators.md), [`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md), [`bdhs_fertility()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_fertility.md), [`bdhs_mortality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_mortality.md), [`bdhs_validate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validate.md), [`bdhs_validation_record()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validation_record.md) |

Key contracts:

- [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
  returns a tibble with lower-case names, haven labels kept and
  attributes `bdhs_survey_id`, `bdhs_recode`, `bdhs_file`.
  `data_survey_id(data)` / `data_recode(data)` detect them otherwise.
  Pooled data carry a `survey_id` column (`is_pooled()`).
- `bdhs_design(data, weight, strata, survey, recode, srvyr)` returns a
  `survey.design2`: PSU `v021`/`hv021`/`mv021`, strata `v022` (DHS
  sampling-error strata, every round), weight `*005 / 1e6`,
  `nest = TRUE`; pooled data use round-prefixed PSUs/strata and
  `pool_wt`. Evaluate variances with
  `options(survey.lonely.psu = "adjust")` set locally.
- [`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
  returns a tibble of class `bdhs_estimate`: `survey_id`, `indicator`,
  `label`, by-columns, `estimate`, `se`, `ci_low`, `ci_high`,
  `n_unweighted`, `n_weighted`, `api_id`; attributes `level`, `scale`,
  `by_labels`. Proportions are on the 0-100 scale.
- Indicators are `new_indicator()` objects in `R/indicator-defs-*.R`
  (`universe`, `value`, `all_women`, `window`, `rounds`, `api_id`,
  `params`).
- [`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md)
  returns tidy official values (`bdhs_indicators`), with
  `preferred = FALSE` exposing alternative reference periods
  (`by_variable_label`).
- Internal helpers in `R/utils.R`: `zap_all()`, `as_codes()`,
  `var_label()`, `lower_names()`, `first_present()`,
  `check_installed()`, `cli_escape()`; `dv(d, var)` (numeric codes of a
  column, `R/indicators.R`).

## Domain facts

- Every BDHS women’s sample is **ever-married** women. All-women
  indicators use the all-woman factors (`awfactt`;
  `awfactu`/`awfactr`/`awfacte`/`awfactw` for subgroups; child-marriage
  tables use `awfactt` for every subgroup).
- Divisions: 5 (1993-94), 6 (to 2007, + Sylhet), 7 (2011-2014, +
  Rangpur), 8 (2017-18 on, + Mymensingh as code 5, renumbering later
  divisions).
- The 2022 women’s survey used a short questionnaire (`sqtype == 2`) for
  a third of women, without family planning, marriage and empowerment
  modules.
- KR covers births in the last 5 years (3 years in 1993-94). BR has the
  full birth history. PR holds anthropometry for children and women.

## Workflow

``` r

devtools::load_all(); devtools::document(); devtools::test()
Sys.setenv(BDHS_LOCAL_TESTS = "true"); devtools::test()   # with real files
```

- New exported functions must be added to `_pkgdown.yml` (the test
  `test-pkgdown.R` and the website build fail otherwise).
- `Rscript tools/check-duplicate-defs.R` catches top-level names defined
  twice (a later file silently overrides an earlier one).
- `data-raw/00-rebuild-from-local.R` rebuilds codebooks and the
  validation record from the maintainer’s files (zips allowed).
- Style: tidyverse style, `cli` for messages,
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html),
  `pkg::fun` for non-base calls, roxygen2 markdown with runnable offline
  examples, testthat 3e. Suggests packages via `check_installed()`.
- Commit messages end with a `Co-Authored-By` trailer when written with
  an AI assistant.
