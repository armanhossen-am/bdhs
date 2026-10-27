# Package index

## Official estimates and metadata (bundled)

Ready to use, no DHS account needed.

- [`bdhs_indicators`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md)
  : Official DHS indicator estimates for Bangladesh
- [`bdhs_indicator_catalog`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_catalog.md)
  : Definitions of the DHS indicators published for Bangladesh
- [`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md)
  : Official DHS estimates for Bangladesh
- [`bdhs_plot_trend()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_plot_trend.md)
  : Plot the trend of an official DHS indicator
- [`bdhs_surveys`](https://armanhossen-am.github.io/bdhs/reference/bdhs_surveys.md)
  : Bangladesh surveys of the DHS Program
- [`bdhs_datasets`](https://armanhossen-am.github.io/bdhs/reference/bdhs_datasets.md)
  : Data files of the Bangladesh surveys
- [`bdhs_rounds()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_rounds.md)
  : Survey round facts
- [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md)
  : Resolve survey identifiers
- [`bdhs_recodes()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_recodes.md)
  : DHS recode file types

## Codebooks and search

- [`bdhs_search()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_search.md)
  : Search variables, indicators and harmonized variables
- [`bdhs_variable()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_variable.md)
  : One variable across survey rounds
- [`bdhs_describe()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_describe.md)
  : Codebook of a data frame
- [`bdhs_codebook`](https://armanhossen-am.github.io/bdhs/reference/bdhs_codebook.md)
  : Codebooks of the BDHS recode files
- [`bdhs_value_labels`](https://armanhossen-am.github.io/bdhs/reference/bdhs_value_labels.md)
  : Value labels of the BDHS recode files

## Your microdata

Import and read the files you downloaded from the DHS Program.

- [`bdhs_data_dir()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_data_dir.md)
  : Where bdhs keeps your DHS microdata
- [`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)
  : Import DHS microdata you have downloaded
- [`bdhs_identify_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_identify_file.md)
  : Identify DHS microdata files
- [`bdhs_inventory()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inventory.md)
  : What DHS datasets exist, and which ones you have
- [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
  : Read a BDHS recode file from your data store
- [`bdhs_read_file()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read_file.md)
  : Read a DHS data file directly
- [`bdhs_download()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_download.md)
  : Download BDHS datasets with your DHS Program account (experimental)
- [`bdhs_forget()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_forget.md)
  : Remove datasets from the local store
- [`bdhs_sitrep()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sitrep.md)
  : Report on your bdhs set-up

## Harmonizing, pooling and merging

- [`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)
  : Harmonize BDHS variables across survey rounds

- [`bdhs_harmonized_vars()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonized_vars.md)
  : Harmonized variables available across BDHS rounds

- [`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md)
  : Pool several BDHS rounds into one dataset

- [`bdhs_pool_population`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool_population.md)
  : Population represented by each BDHS round, for pooling

- [`bdhs_merge()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_merge.md)
  : Merge two BDHS recode files on the right DHS keys

- [`bdhs_na()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_na.md)
  :

  Set DHS special codes to `NA`

## Survey design and estimation

- [`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md)
  : Build the complex survey design for BDHS data
- [`bdhs_design_info()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design_info.md)
  : Survey design of each BDHS round
- [`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
  : Estimate indicators with correct survey standard errors
- [`bdhs_add_indicators()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_add_indicators.md)
  : Add indicator variables to BDHS microdata
- [`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md)
  : List the indicators bdhs can compute
- [`bdhs_fertility()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_fertility.md)
  : Fertility rates from the birth history
- [`bdhs_mortality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_mortality.md)
  : Childhood mortality rates
- [`bdhs_validate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validate.md)
  : Validate indicator calculations against official DHS values
- [`bdhs_validation_record()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_validation_record.md)
  : The validation record shipped with bdhs
- [`plot(`*`<bdhs_estimate>`*`)`](https://armanhossen-am.github.io/bdhs/reference/plot.bdhs_estimate.md)
  [`autoplot(`*`<bdhs_estimate>`*`)`](https://armanhossen-am.github.io/bdhs/reference/plot.bdhs_estimate.md)
  : Plot survey estimates

## Change, inequality and SDG progress

- [`bdhs_compare()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_compare.md)
  : Compare indicators between survey rounds and between subgroups
- [`bdhs_inequality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality.md)
  [`plot(`*`<bdhs_inequality>`*`)`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality.md)
  [`autoplot(`*`<bdhs_inequality>`*`)`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality.md)
  : Socioeconomic inequality in an indicator
- [`bdhs_inequality_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_inequality_official.md)
  : Inequality gaps from the official estimates
- [`bdhs_decompose()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_decompose.md)
  [`plot(`*`<bdhs_decomposition>`*`)`](https://armanhossen-am.github.io/bdhs/reference/bdhs_decompose.md)
  [`autoplot(`*`<bdhs_decomposition>`*`)`](https://armanhossen-am.github.io/bdhs/reference/bdhs_decompose.md)
  : Decompose the change of an indicator between two rounds
- [`bdhs_sdg()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sdg.md)
  [`plot(`*`<bdhs_sdg>`*`)`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sdg.md)
  [`autoplot(`*`<bdhs_sdg>`*`)`](https://armanhossen-am.github.io/bdhs/reference/bdhs_sdg.md)
  : SDG tracker for Bangladesh from the official DHS estimates

## Data quality

- [`bdhs_quality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_quality.md)
  : Data-quality diagnostics for DHS recode files
- [`plot(`*`<bdhs_quality>`*`)`](https://armanhossen-am.github.io/bdhs/reference/plot.bdhs_quality.md)
  [`autoplot(`*`<bdhs_quality>`*`)`](https://armanhossen-am.github.io/bdhs/reference/plot.bdhs_quality.md)
  : Plot data-quality profiles

## Maps

- [`bdhs_boundaries()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_boundaries.md)
  : Division boundaries of a survey round
- [`bdhs_map()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_map.md)
  : Map indicators by division
- [`bdhs_regions`](https://armanhossen-am.github.io/bdhs/reference/bdhs_regions.md)
  : Regions (divisions) of every Bangladesh survey

## Synthetic data

- [`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md)
  : Simulate synthetic BDHS-like recode data

## Package

- [`bdhs`](https://armanhossen-am.github.io/bdhs/reference/bdhs-package.md)
  [`bdhs-package`](https://armanhossen-am.github.io/bdhs/reference/bdhs-package.md)
  : bdhs: Tools and Data for the Bangladesh Demographic and Health
  Survey
