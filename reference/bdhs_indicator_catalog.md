# Definitions of the DHS indicators published for Bangladesh

One row per indicator in
[bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md):
label, definition, denominator, topic, the surveys it is available for
and more.

## Usage

``` r
bdhs_indicator_catalog
```

## Format

A tibble with 2,607 rows and 29 columns:

- indicator_id:

  DHS indicator id.

- indicator_order:

  DHS sort order; (`indicator_id`, `indicator_order`) is the key.

- label:

  Indicator label.

- short_name:

  Short label.

- definition:

  Full definition.

- denominator:

  Description of the denominator.

- measurement_type:

  `"Percent"`, `"Rate"`, `"Mean"`, `"Median"`, `"Number"` or `"Ratio"`.

- indicator_type:

  Type code, as in
  [bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md).

- topic:

  Subject area (DHS level 1), e.g. `"Fertility"`.

- subtopic:

  Topic within the subject area (DHS level 2).

- population:

  Universe (DHS level 3), e.g. `"Women"`, `"Children"`.

- precision:

  Number of decimals with which values are presented.

- tag_ids:

  List column of DHS tag ids.

- is_quick_stat:

  `TRUE` for DHS "Quickstats" (the most used indicators).

- quick_stat_order:

  Sort order among Quickstats.

- denom_weighted_id, denom_unweighted_id:

  Indicator ids of the weighted and unweighted denominators.

- special1_id, special2_id:

  Indicator ids of related special answers (e.g. don't know/missing).

- old_id:

  Former DHS indicator id.

- id_is_shared:

  `TRUE` if `indicator_id` is used for more than one indicator.

- tags:

  Names of the tags (e.g. `"SDGs; MDGs"`), separated by `"; "`.

- n_surveys:

  Number of Bangladesh surveys with values.

- surveys:

  Comma-separated survey ids with values.

- first_year, last_year:

  First and last survey year with values.

- has_subnational:

  Whether regional values are published.

- has_ci:

  Whether confidence intervals are published.

- n_rows:

  Number of rows in
  [bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md).

## Source

The DHS Program Indicator Data API, `indicators` and `tags` endpoints,
<https://api.dhsprogram.com>, retrieved 2026-10-26.

## Details

The DHS API's country-filtered indicator list misses 31 indicators that
do have Bangladesh values; their definitions were taken from the global
list. A few indicator ids are used for two different indicators
(different `indicator_order`), flagged by `id_is_shared`.

## See also

[bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md),
[`bdhs_search()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_search.md),
[`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md).

## Examples

``` r
# Quickstat indicators available for every round
qs <- bdhs_indicator_catalog[bdhs_indicator_catalog$is_quick_stat &
  bdhs_indicator_catalog$n_surveys == 9, ]
head(qs[, c("indicator_id", "label")])
#> # A tibble: 6 × 2
#>   indicator_id  label                                                           
#>   <chr>         <chr>                                                           
#> 1 FE_FRTR_W_TFR Total fertility rate 15-49                                      
#> 2 FP_CUSM_W_ANY Married women currently using any method of contraception       
#> 3 FP_CUSM_W_MOD Married women currently using any modern method of contraception
#> 4 FP_NADM_W_UNT Unmet need for family planning                                  
#> 5 FP_NADM_W_PDM Demand for family planning satisfied by modern methods          
#> 6 MA_AAFM_W_M2B Median age at first marriage [Women]: 25-49                     

# Search definitions
cat_ <- bdhs_indicator_catalog
cat_[grepl("stunt", cat_$label, ignore.case = TRUE), c("indicator_id", "label")]
#> # A tibble: 2 × 2
#>   indicator_id  label                    
#>   <chr>         <chr>                    
#> 1 CN_NUTS_C_HA3 Children severely stunted
#> 2 CN_NUTS_C_HA2 Children stunted         
```
