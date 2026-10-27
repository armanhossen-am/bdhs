# Official DHS estimates for Bangladesh

A friendly, offline accessor for
[bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md),
the official estimates that the DHS Program publishes for every
Bangladesh DHS round (the same numbers as STATcompiler and the final
reports). Pick indicators by DHS id or by searching their labels and
definitions, choose rounds, and get a tidy table back.

## Usage

``` r
bdhs_official(
  indicator = NULL,
  survey = NULL,
  breakdown = c("national", "subnational", "background", "all"),
  characteristic = NULL,
  preferred = TRUE,
  search = NULL
)
```

## Arguments

- indicator:

  Character vector of DHS indicator ids. `NULL` (default) keeps every
  indicator (subject to `search`).

- survey:

  Survey references understood by
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md),
  e.g. `2022`, `"2017-18"` or `c(2014, 2022)`. `NULL` keeps all rounds.

- breakdown:

  Which rows to return: `"national"` totals (default), `"subnational"`
  (regions and divisions), `"background"` characteristics (residence,
  education, wealth, age, ...) or `"all"`. When `characteristic` is
  given and `breakdown` is not, all breakdowns are searched.

- characteristic:

  Characteristic categories to keep, e.g. `"Residence"`,
  `"Wealth quintile"`, `"Education"` or `"Region"` (`"Division"` is
  accepted as a synonym). Case-insensitive. The DHS "Region" category
  holds both the divisions as surveyed and the older groupings kept for
  trends ("Chattogram/Sylhet", "Dhaka before 2015", "Rajshahi/Rangpur");
  tell them apart by `characteristic_label`, or use
  [`bdhs_plot_trend()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_plot_trend.md)
  with `by = "Region"` (comparable groupings) or `by = "Division"`
  (divisions as surveyed).

- preferred:

  If `TRUE` (default), keep only the headline value of each estimate
  (the DHS `IsPreferred` flag). With `FALSE`, alternative reference
  periods are returned too and a `by_variable_label` column says which
  is which.

- search:

  A regular expression matched (case-insensitively) against indicator
  ids, labels, short names and definitions in
  [bdhs_indicator_catalog](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_catalog.md).

## Value

A tibble with columns `survey_id`, `year_label`, `indicator_id`,
`indicator`, `characteristic_category`, `characteristic_label`, `value`,
`ci_low`, `ci_high`, `denom_weighted`, `denom_unweighted`, `region_id`
and `breakdown` (plus `by_variable_label` when `preferred = FALSE`),
ordered by survey year, indicator and characteristic.

## Details

Indicator ids follow the DHS API, e.g. `"FE_FRTR_W_TFR"` (total
fertility rate) or `"CN_NUTS_C_HA2"` (stunting). Use
[`bdhs_search()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_search.md)
to find them.

Subnational rows follow the DHS region hierarchy: the five regions that
are comparable across all rounds (`"Chattogram/Sylhet"`,
`"Dhaka before 2015"`, `"Rajshahi/Rangpur"`, Barishal, Khulna) and,
below them, the divisions of each round (Chattogram, Sylhet, Dhaka,
Mymensingh, Rajshahi, Rangpur). The `region_id` column links to
[`bdhs_boundaries()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_boundaries.md)
(`reg_id`).

## See also

[`bdhs_search()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_search.md)
to find indicators,
[`bdhs_plot_trend()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_plot_trend.md)
and
[`bdhs_map()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_map.md)
to visualise them.

## Examples

``` r
# Total fertility rate in every round
bdhs_official("FE_FRTR_W_TFR")
#> # A tibble: 9 × 13
#>   survey_id year_label indicator_id  indicator            characteristic_categ…¹
#>   <chr>     <chr>      <chr>         <chr>                <chr>                 
#> 1 BD1994DHS 1993-94    FE_FRTR_W_TFR Total fertility rat… Total                 
#> 2 BD1997DHS 1996-97    FE_FRTR_W_TFR Total fertility rat… Total                 
#> 3 BD2000DHS 1999-2000  FE_FRTR_W_TFR Total fertility rat… Total                 
#> 4 BD2004DHS 2004       FE_FRTR_W_TFR Total fertility rat… Total                 
#> 5 BD2007DHS 2007       FE_FRTR_W_TFR Total fertility rat… Total                 
#> 6 BD2011DHS 2011       FE_FRTR_W_TFR Total fertility rat… Total                 
#> 7 BD2014DHS 2014       FE_FRTR_W_TFR Total fertility rat… Total                 
#> 8 BD2017DHS 2017-18    FE_FRTR_W_TFR Total fertility rat… Total                 
#> 9 BD2022DHS 2022       FE_FRTR_W_TFR Total fertility rat… Total                 
#> # ℹ abbreviated name: ¹​characteristic_category
#> # ℹ 8 more variables: characteristic_label <chr>, value <dbl>, ci_low <dbl>,
#> #   ci_high <dbl>, denom_weighted <dbl>, denom_unweighted <dbl>,
#> #   region_id <chr>, breakdown <chr>

# Stunting by wealth quintile in the two latest rounds
bdhs_official("CN_NUTS_C_HA2", survey = c("2017-18", 2022), characteristic = "Wealth quintile")
#> # A tibble: 10 × 13
#>    survey_id year_label indicator_id  indicator        characteristic_category
#>    <chr>     <chr>      <chr>         <chr>            <chr>                  
#>  1 BD2017DHS 2017-18    CN_NUTS_C_HA2 Children stunted Wealth quintile        
#>  2 BD2017DHS 2017-18    CN_NUTS_C_HA2 Children stunted Wealth quintile        
#>  3 BD2017DHS 2017-18    CN_NUTS_C_HA2 Children stunted Wealth quintile        
#>  4 BD2017DHS 2017-18    CN_NUTS_C_HA2 Children stunted Wealth quintile        
#>  5 BD2017DHS 2017-18    CN_NUTS_C_HA2 Children stunted Wealth quintile        
#>  6 BD2022DHS 2022       CN_NUTS_C_HA2 Children stunted Wealth quintile        
#>  7 BD2022DHS 2022       CN_NUTS_C_HA2 Children stunted Wealth quintile        
#>  8 BD2022DHS 2022       CN_NUTS_C_HA2 Children stunted Wealth quintile        
#>  9 BD2022DHS 2022       CN_NUTS_C_HA2 Children stunted Wealth quintile        
#> 10 BD2022DHS 2022       CN_NUTS_C_HA2 Children stunted Wealth quintile        
#> # ℹ 8 more variables: characteristic_label <chr>, value <dbl>, ci_low <dbl>,
#> #   ci_high <dbl>, denom_weighted <dbl>, denom_unweighted <dbl>,
#> #   region_id <chr>, breakdown <chr>

# Anything about anemia, by division, in 2011
bdhs_official(search = "anemia", survey = 2011, breakdown = "subnational")
#> # A tibble: 90 × 13
#>    survey_id year_label indicator_id  indicator           characteristic_categ…¹
#>    <chr>     <chr>      <chr>         <chr>               <chr>                 
#>  1 BD2011DHS 2011       CN_ANMC_C_ANY Children with any … Region                
#>  2 BD2011DHS 2011       CN_ANMC_C_ANY Children with any … Region                
#>  3 BD2011DHS 2011       CN_ANMC_C_ANY Children with any … Region                
#>  4 BD2011DHS 2011       CN_ANMC_C_ANY Children with any … Region                
#>  5 BD2011DHS 2011       CN_ANMC_C_ANY Children with any … Region                
#>  6 BD2011DHS 2011       CN_ANMC_C_ANY Children with any … Region                
#>  7 BD2011DHS 2011       CN_ANMC_C_ANY Children with any … Region                
#>  8 BD2011DHS 2011       CN_ANMC_C_ANY Children with any … Region                
#>  9 BD2011DHS 2011       CN_ANMC_C_ANY Children with any … Region                
#> 10 BD2011DHS 2011       CN_ANMC_C_MLD Children with mild… Region                
#> # ℹ 80 more rows
#> # ℹ abbreviated name: ¹​characteristic_category
#> # ℹ 8 more variables: characteristic_label <chr>, value <dbl>, ci_low <dbl>,
#> #   ci_high <dbl>, denom_weighted <dbl>, denom_unweighted <dbl>,
#> #   region_id <chr>, breakdown <chr>
```
