# Map indicators by division

Draws a choropleth of an indicator by division with the boundaries of
the survey round. `x` is either an official DHS indicator id, mapped
with the subnational estimates bundled in
[bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md),
or a data frame of your own estimates by region, such as the output of
`bdhs_estimate(..., by = "v024")`.

## Usage

``` r
bdhs_map(x, survey = NULL, value = NULL, title = NULL)
```

## Arguments

- x:

  An official indicator id (e.g. `"CN_NUTS_C_HA2"`) or a data frame of
  estimates by region.

- survey:

  Survey references. For an indicator id, defaults to the most recent
  round with subnational estimates; several rounds are drawn as panels
  sharing one colour scale.

- value:

  For a data frame, the name of the column to map.

- title:

  Plot title; defaults to the indicator label.

## Value

A ggplot object.

## Details

A data frame needs a region column (`region_code`, `v024`, `hv024`,
`mv024`, `region`, `division`, `region_name`, or the harmonized
`division_surveyed`, `division_trend6` or `division_trend5`) holding
region codes as in the recode files or division names (old and new
spellings such as "Barisal"/"Barishal" are both understood), and a value
column (`value` argument; by default `estimate` or `value`). The survey
round comes from `survey`, a `survey_id` column (several rounds give one
panel each) or the `bdhs_survey_id` attribute.

## See also

[`bdhs_boundaries()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_boundaries.md),
[`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md)

## Examples

``` r
bdhs_map("CN_NUTS_C_HA2")

bdhs_map("FE_FRTR_W_TFR", survey = c(2014, 2022))


# Your own estimates by division (v024 codes of the 2022 round)
est <- data.frame(v024 = 1:8, estimate = c(25, 30, 22, 21, 31, 23, 24, 34))
bdhs_map(est, survey = 2022, title = "My indicator")
```
