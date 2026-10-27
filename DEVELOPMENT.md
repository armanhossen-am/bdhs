# Developing bdhs

Notes for anyone (people or coding agents) changing this package.

## Rules that are never broken

1. **No DHS microdata in the repository or the package.** DHS terms of use forbid
   redistributing respondent-level data "directly or within any tool". Never
   commit, bundle, snapshot, print or save row-level values. `.gitignore`,
   `.Rbuildignore` and `tools/check-tarball.sh` (run in CI) block data file
   types; do not weaken them. Metadata (names, labels, value labels, counts)
   and aggregate estimates are fine.
2. **Examples, tests and vignettes run without real data**, using
   `bdhs_simulate()` and temporary directories (`withr::local_tempdir()`).
   They never use the network and never write outside `tempdir()`.
3. **Tests on the maintainer's real files** are skipped unless
   `BDHS_LOCAL_TESTS=true`; files are found under `BDHS_LOCAL_DIR` (default
   `~/Documents/MU/Research`). Use `skip_if_no_local_data()` and
   `local_data_dir()` from `tests/testthat/helper-local.R`. Such tests assert on
   aggregates only.
4. **Never handle credentials.** `bdhs_download()` reads the DHS password from
   keyring, `DHS_PASSWORD` or an askpass prompt.

## Layout

| Area | Files | Exported functions |
|---|---|---|
| Rounds, recodes | `surveys.R`, `recodes.R` | `bdhs_survey_id()`, `bdhs_rounds()`, `bdhs_recodes()` |
| Synthetic data | `simulate.R` | `bdhs_simulate()` |
| Bundled data | `data.R`, `data-raw/` | `bdhs_indicators`, `bdhs_indicator_catalog`, `bdhs_surveys`, `bdhs_datasets`, `bdhs_codebook`, `bdhs_value_labels`, `bdhs_regions`, `bdhs_pool_population` |
| Official estimates, search, plots, maps | `official.R`, `search.R`, `describe.R`, `plot.R`, `map.R` | `bdhs_official()`, `bdhs_search()`, `bdhs_variable()`, `bdhs_describe()`, `bdhs_plot_trend()`, `bdhs_boundaries()`, `bdhs_map()` |
| Your files | `store.R`, `identify.R`, `import.R`, `read.R`, `download.R`, `sitrep.R` | `bdhs_data_dir()`, `bdhs_import()`, `bdhs_identify_file()`, `bdhs_inventory()`, `bdhs_read()`, `bdhs_read_file()`, `bdhs_download()`, `bdhs_forget()`, `bdhs_sitrep()` |
| Harmonization | `crosswalk.R`, `harmonize.R`, `pool.R`, `merge.R`, `missing.R` | `bdhs_harmonize()`, `bdhs_harmonized_vars()`, `bdhs_pool()`, `bdhs_merge()`, `bdhs_na()` |
| Design and estimation | `design.R`, `indicators.R`, `indicator-defs-*.R`, `estimate.R`, `rates.R`, `validate.R` | `bdhs_design()`, `bdhs_design_info()`, `bdhs_indicator_list()`, `bdhs_add_indicators()`, `bdhs_estimate()`, `bdhs_fertility()`, `bdhs_mortality()`, `bdhs_validate()`, `bdhs_validation_record()` |

Key contracts:

* `bdhs_read()` returns a tibble with lower-case names, haven labels kept and
  attributes `bdhs_survey_id`, `bdhs_recode`, `bdhs_file`.
  `data_survey_id(data)` / `data_recode(data)` detect them otherwise.
  Pooled data carry a `survey_id` column (`is_pooled()`).
* `bdhs_design(data, weight, strata, survey, recode, srvyr)` returns a
  `survey.design2`: PSU `v021`/`hv021`/`mv021`, strata `v022` (DHS
  sampling-error strata, every round), weight `*005 / 1e6`, `nest = TRUE`;
  pooled data use round-prefixed PSUs/strata and `pool_wt`.
  Evaluate variances with `options(survey.lonely.psu = "adjust")` set locally.
* `bdhs_estimate()` returns a tibble of class `bdhs_estimate`: `survey_id`,
  `indicator`, `label`, by-columns, `estimate`, `se`, `ci_low`, `ci_high`,
  `n_unweighted`, `n_weighted`, `api_id`; attributes `level`, `scale`,
  `by_labels`. Proportions are on the 0-100 scale.
* Indicators are `new_indicator()` objects in `R/indicator-defs-*.R`
  (`universe`, `value`, `all_women`, `window`, `rounds`, `api_id`, `params`).
* `bdhs_official()` returns tidy official values (`bdhs_indicators`), with
  `preferred = FALSE` exposing alternative reference periods
  (`by_variable_label`).
* Internal helpers in `R/utils.R`: `zap_all()`, `as_codes()`, `var_label()`,
  `lower_names()`, `first_present()`, `check_installed()`, `cli_escape()`;
  `dv(d, var)` (numeric codes of a column, `R/indicators.R`).

## Domain facts

* Every BDHS women's sample is **ever-married** women. All-women indicators
  use the all-woman factors (`awfactt`; `awfactu`/`awfactr`/`awfacte`/`awfactw`
  for subgroups; child-marriage tables use `awfactt` for every subgroup).
* Divisions: 5 (1993-94), 6 (to 2007, + Sylhet), 7 (2011-2014, + Rangpur),
  8 (2017-18 on, + Mymensingh as code 5, renumbering later divisions).
* The 2022 women's survey used a short questionnaire (`sqtype == 2`) for a
  third of women, without family planning, marriage and empowerment modules.
* KR covers births in the last 5 years (3 years in 1993-94). BR has the full
  birth history. PR holds anthropometry for children and women.

## Workflow

```r
devtools::load_all(); devtools::document(); devtools::test()
Sys.setenv(BDHS_LOCAL_TESTS = "true"); devtools::test()   # with real files
```

* `Rscript tools/check-duplicate-defs.R` catches top-level names defined twice
  (a later file silently overrides an earlier one).
* `data-raw/00-rebuild-from-local.R` rebuilds codebooks and the validation
  record from the maintainer's files (zips allowed).
* Style: tidyverse style, `cli` for messages, `rlang::arg_match()`,
  `pkg::fun` for non-base calls, roxygen2 markdown with runnable offline
  examples, testthat 3e. Suggests packages via `check_installed()`.
* Commit messages end with a `Co-Authored-By` trailer when written with an AI
  assistant.
