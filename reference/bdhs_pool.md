# Pool several BDHS rounds into one dataset

Stacks data from several survey rounds of the same recode type, adds a
`survey_id` column and a pooled analysis weight `pool_wt`, and
reconciles columns whose value labels changed between rounds. Use it for
trend analysis and pooled models;
[`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md)
then builds a design with round-specific clusters and strata and uses
`pool_wt` automatically.

## Usage

``` r
bdhs_pool(
  ...,
  weights = c("normalized", "denormalized", "equal"),
  harmonize = FALSE,
  vars = NULL
)
```

## Arguments

- ...:

  Data frames to pool (one round each, or already pooled data), or a
  single list of data frames.

- weights:

  How to build `pool_wt`: `"normalized"`, `"denormalized"` or `"equal"`
  (see Details).

- harmonize:

  If `TRUE`, run
  [`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)
  on every round before stacking, so that comparable variables are
  available in every round.

- vars:

  Optional columns to keep. With `harmonize = TRUE`, names of harmonized
  variables (see
  [`bdhs_harmonized_vars()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonized_vars.md))
  to create; raw DHS columns named in `vars` are kept as well.
  Identifier and design columns are always kept. `NULL` keeps every
  column.

## Value

A tibble with `survey_id` as first column, the stacked data and
`pool_wt`, with attributes `bdhs_recode`, `bdhs_pooled = TRUE` and
`bdhs_pool_weights`.

## Pooled weights

DHS weights are *normalized*: within each round they average 1, so they
sum to the round's sample size (see the Guide to DHS Statistics,
"Sampling weights" and "Normalization of weights"). Three choices are
offered for `pool_wt`:

- `"normalized"` (default): `v005 / 1e6` (`hv005`, `mv005` for household
  and men's files). Rounds contribute in proportion to their sample
  size. Fine for most pooled models with survey fixed effects.

- `"denormalized"`: the weights are scaled back to population totals, so
  each round contributes in proportion to the population it represents:
  `pool_wt = v005 / 1e6 * N / n`, where `N` is the population the sample
  represents (female population aged 15-49 times the proportion ever
  married, because every BDHS women's sample is of ever-married women)
  and `n` the number of women interviewed. This is the de-normalization
  recommended by the DHS Program for pooling surveys (DHS Sampling and
  Household Listing Manual, ICF 2012; Guide to DHS Statistics, Croft et
  al. 2018). Population figures come from the bundled
  `bdhs_pool_population` table (UN World Population Prospects 2024 and
  the DHS API). Household and member files use the same factor, which
  approximates the inverse sampling fraction of households.

- `"equal"`: each round gets the same total weight (`n_total / k` for
  `k` rounds), so each round counts equally whatever its size.

Variance estimation is unaffected by a constant rescaling within rounds;
only the relative contribution of the rounds changes.

## References

Croft, Trevor N., Aileen M. J. Marshall, Courtney K. Allen, et al. 2018.
*Guide to DHS Statistics.* Rockville, Maryland, USA: ICF.
<https://dhsprogram.com/data/Guide-to-DHS-Statistics/>

ICF International. 2012. *Demographic and Health Survey Sampling and
Household Listing Manual.* MEASURE DHS, Calverton, Maryland, USA: ICF
International.

## Examples

``` r
p <- bdhs_pool(bdhs_simulate(2014, "KR"), bdhs_simulate(2022, "KR"))
#> ℹ Value labels differ between rounds for 3 columns; kept the codes and dropped
#>   the labels: `v022`, `v023`, and `v024`.
#> ℹ DHS codes are not always comparable across rounds; use `harmonize = TRUE` or
#>   `bdhs_harmonize()` for comparable variables.
table(p$survey_id)
#> 
#> BD2014DHS BD2022DHS 
#>       286       296 
tapply(p$pool_wt, p$survey_id, sum)
#> BD2014DHS BD2022DHS 
#>  286.4036  296.2882 

p2 <- bdhs_pool(
  list(bdhs_simulate(2014, "KR"), bdhs_simulate(2022, "KR")),
  weights = "equal", harmonize = TRUE,
  vars = c("division_trend6", "residence", "child_age_months")
)
#> ℹ Value labels differ between rounds for 3 columns; kept the codes and dropped
#>   the labels: `v022`, `v023`, and `v024`.
#> ℹ DHS codes are not always comparable across rounds; use `harmonize = TRUE` or
#>   `bdhs_harmonize()` for comparable variables.
#> ℹ Harmonized 3 variables in 2 rounds.
tapply(p2$pool_wt, p2$survey_id, sum)
#> BD2014DHS BD2022DHS 
#>       291       291 
```
