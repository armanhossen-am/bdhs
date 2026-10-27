# Population represented by each BDHS round, for pooling

The national female population aged 15-49 at the reference date of each
DHS round and the number of women interviewed. DHS weights are
normalised so that they sum to the sample size of each round; when
rounds are pooled,
[`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md)
multiplies the weights of each round by `female_15_49 / n_women_sample`
so that each round counts in proportion to the population it represents
rather than to its sample size.

## Usage

``` r
bdhs_pool_population
```

## Format

A tibble with 9 rows (one per DHS round) and 9 columns:

- survey_id:

  DHS survey identifier.

- survey_label:

  Year label, e.g. `"2017-18"`.

- ref_year:

  Reference date as a decimal year: the mean interview date where the
  microdata were available to the maintainer, otherwise the midpoint of
  fieldwork (1996-97).

- female_15_49:

  Female population aged 15-49 (persons): UN World Population Prospects
  2024 estimate, linearly interpolated between the 1 July estimates
  bracketing `ref_year`.

- n_women_sample:

  Number of (ever-married) women interviewed, from the DHS API.

- women_age_min, women_age_max:

  Age range of eligible women.

- p_ever_married:

  Proportion of women 15-49 ever married: 1 minus the DHS percentage
  never married (indicator `MA_MSTA_W_NMA`) / 100.

- source:

  Sources of `female_15_49` and `n_women_sample`.

## Source

United Nations, Department of Economic and Social Affairs, Population
Division (2024). World Population Prospects 2024,
<https://population.un.org/wpp/> (CC BY 3.0 IGO); The DHS Program
Indicator Data API, retrieved 2026-10-26. Built by
`data-raw/06-build-pool-population.R`.

## Details

BDHS women's samples are ever-married women; `p_ever_married` gives the
share of women 15-49 who were ever married, should you prefer to scale
to the ever-married population.

## See also

[`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md),
[bdhs_surveys](https://armanhossen-am.github.io/bdhs/reference/bdhs_surveys.md).

## Examples

``` r
bdhs_pool_population[, c("survey_id", "ref_year", "female_15_49", "n_women_sample")]
#> # A tibble: 9 × 4
#>   survey_id ref_year female_15_49 n_women_sample
#>   <chr>        <dbl>        <dbl>          <int>
#> 1 BD1994DHS    1994.     28096489           9640
#> 2 BD1997DHS    1997.     30782776           9127
#> 3 BD2000DHS    2000.     33689291          10544
#> 4 BD2004DHS    2004.     36965636          11440
#> 5 BD2007DHS    2007.     39019159          10996
#> 6 BD2011DHS    2012.     41427050          17842
#> 7 BD2014DHS    2015.     43360901          17863
#> 8 BD2017DHS    2018.     45459595          20127
#> 9 BD2022DHS    2023.     48342561          30078

# Pooling weight multipliers
with(bdhs_pool_population, setNames(female_15_49 / n_women_sample, survey_id))
#> BD1994DHS BD1997DHS BD2000DHS BD2004DHS BD2007DHS BD2011DHS BD2014DHS BD2017DHS 
#>  2914.574  3372.716  3195.115  3231.262  3548.487  2321.884  2427.414  2258.637 
#> BD2022DHS 
#>  1607.240 
```
