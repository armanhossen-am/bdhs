# SDG tracker for Bangladesh from the official DHS estimates

Tracks the Sustainable Development Goal indicators that the BDHS
measures, using the bundled official estimates
([`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md)):
stunting and wasting (SDG 2.2.1, 2.2.2), under-five and neonatal
mortality (3.2.1, 3.2.2), skilled birth attendance (3.1.2), family
planning (3.7.1), the adolescent birth rate (3.7.2), child marriage
(5.3.1) and improved water and sanitation (proxies for 6.1.1, 6.2.1).

## Usage

``` r
bdhs_sdg(indicators = NULL, baseline = 2014, target_year = 2030)

# S3 method for class 'bdhs_sdg'
plot(x, ...)

# S3 method for class 'bdhs_sdg'
autoplot(object, ...)
```

## Arguments

- indicators:

  Optional ids (`"stunting"`, `"u5mr"`, ...), SDG numbers (`"3.2.1"`) or
  DHS ids to keep. `NULL` keeps all.

- baseline:

  Survey reference of the baseline round for the AARC (default 2014, the
  round closest to the start of the SDG period).

- target_year:

  Target year (default 2030).

- x, object:

  A `bdhs_sdg` result.

- ...:

  Unused.

## Value

A tibble of class `bdhs_sdg` with columns `id`, `sdg`, `name`, `dhs_id`,
`direction`, `target`, `baseline_survey`, `baseline_value`,
`latest_survey`, `latest_value`, `aarc`, `required_aarc`, `projected`,
`on_track` and `target_source`. The attribute `series` holds every
round's value.

## Details

- The average annual rate of change (UNICEF/WHO) between the `baseline`
  round and the latest round is \\AARC = 1 - (x_2 / x_1)^{1 / (t_2 -
  t_1)}\\, in percent per year, with \\t\\ the mid-point of fieldwork.
  For indicators that should fall it is computed on the value; for
  coverage indicators that should rise it is computed on the shortfall
  \\100 - x\\. Positive values always mean progress.

- The required AARC is the rate that brings the latest value to the
  target by `target_year`; it is `NA` for elimination or universal
  targets (0% or 100%), which an exponential trend never reaches
  exactly.

- The projection continues the current AARC to `target_year`; an
  indicator is on track when the target is already met or the
  projection, rounded to whole units, meets it.

Targets and their sources are in the `target_source` column; where the
SDG framework has no numeric target a widely used benchmark is given, or
none.

## Examples

``` r
bdhs_sdg()
#> # BDHS SDG tracker (official DHS estimates) | target year 2030 | AARC in % per year
#> # A tibble: 12 × 15
#>    id         sdg   name  dhs_id direction target baseline_survey baseline_value
#>    <chr>      <chr> <chr> <chr>  <chr>      <dbl> <chr>                    <dbl>
#>  1 stunting   2.2.1 Stun… CN_NU… lower       20.6 BD2014DHS                 36.1
#>  2 wasting    2.2.2 Wast… CN_NU… lower        3   BD2014DHS                 14.3
#>  3 u5mr       3.2.1 Unde… CM_EC… lower       25   BD2014DHS                 46  
#>  4 nmr        3.2.2 Neon… CM_EC… lower       12   BD2014DHS                 28  
#>  5 sba        3.1.2 Birt… RH_DE… higher      90   BD2014DHS                 44.3
#>  6 demand_sa… 3.7.1 Dema… FP_NA… higher      75   BD2014DHS                 72.6
#>  7 mcpr       3.7.1 Mode… FP_CU… higher      NA   BD2014DHS                 54.1
#>  8 abr        3.7.2 Adol… FE_FR… lower       NA   BD2014DHS                113  
#>  9 married_b… 5.3.1 Wome… MA_MB… lower        0   BD2014DHS                 22.4
#> 10 married_b… 5.3.1 Wome… MA_MB… lower        0   BD2014DHS                 58.6
#> 11 improved_… 6.1.1 Popu… WS_SR… higher     100   BD2014DHS                 97.8
#> 12 improved_… 6.2.1 Popu… WS_TL… higher     100   BD2014DHS                 69.5
#> # ℹ 7 more variables: latest_survey <chr>, latest_value <dbl>, aarc <dbl>,
#> #   required_aarc <dbl>, projected <dbl>, on_track <lgl>, target_source <chr>
bdhs_sdg(c("stunting", "u5mr"))
#> # BDHS SDG tracker (official DHS estimates) | target year 2030 | AARC in % per year
#> # A tibble: 2 × 15
#>   id       sdg   name     dhs_id direction target baseline_survey baseline_value
#>   <chr>    <chr> <chr>    <chr>  <chr>      <dbl> <chr>                    <dbl>
#> 1 stunting 2.2.1 Stuntin… CN_NU… lower       20.6 BD2014DHS                 36.1
#> 2 u5mr     3.2.1 Under-f… CM_EC… lower       25   BD2014DHS                 46  
#> # ℹ 7 more variables: latest_survey <chr>, latest_value <dbl>, aarc <dbl>,
#> #   required_aarc <dbl>, projected <dbl>, on_track <lgl>, target_source <chr>
```
