# Plot the trend of an official DHS indicator

Draws an official indicator from
[bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md)
across survey rounds, nationally or by a background characteristic, with
95% confidence intervals where the DHS Program publishes them. Survey
rounds are placed on a true time scale and labelled with their survey
years.

## Usage

``` r
bdhs_plot_trend(indicator, by = NULL, survey = NULL, ci = TRUE)
```

## Arguments

- indicator:

  One or more DHS indicator ids (see
  [`bdhs_search()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_search.md)).
  Several indicators are drawn in separate panels.

- by:

  `NULL` (default) for the national value, or a characteristic category
  such as `"Residence"`, `"Wealth quintile"`, `"Education"`, `"Region"`
  or `"Division"`.

- survey:

  Optional survey references to restrict the rounds shown.

- ci:

  If `TRUE` (default), draw 95% confidence intervals when the DHS
  Program publishes them for the indicator.

## Value

A ggplot object.

## Details

Subnational trends come in two flavours. `by = "Region"` uses the five
regions that are comparable across every round (Barishal,
Chattogram/Sylhet, Dhaka before 2015 (incl. Mymensingh), Khulna,
Rajshahi/Rangpur). `by = "Division"` uses the divisions as surveyed in
each round (Sylhet from 1996-97, Rangpur from 2011, Mymensingh from
2017-18); with more than eight series these are drawn as small
multiples.

## See also

[`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md)
for the numbers behind the plot.

## Examples

``` r
bdhs_plot_trend("FE_FRTR_W_TFR")

bdhs_plot_trend("CN_NUTS_C_HA2", by = "Residence")

bdhs_plot_trend("CM_ECMR_C_U5M", by = "Region")
```
