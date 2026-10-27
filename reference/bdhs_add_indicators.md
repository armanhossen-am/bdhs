# Add indicator variables to BDHS microdata

Computes indicators from the registry
([`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md))
and adds them as columns named by the indicator id. Each column holds
the indicator's value (0/1 for proportions) for records in the
indicator's denominator population and `NA` for everyone else, so that
`mean(x, na.rm = TRUE)` weighted by the sample weight gives the
indicator. Use
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
for estimates with correct standard errors and for indicators with
all-women denominators.

## Usage

``` r
bdhs_add_indicators(data, indicators, survey = NULL)
```

## Arguments

- data:

  A BDHS recode data frame, as returned by
  [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
  or
  [`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md),
  with DHS variable names. Pooled data (a `survey_id` column) are
  handled round by round.

- indicators:

  Character vector of indicator ids.

- survey:

  Optional survey reference, overriding detection.

## Value

`data` with one numeric column per indicator. Each column carries the
attributes `label` and, for all-women indicators, `bdhs_all_women` (the
name of the all-woman factor needed for the denominator).

## Details

Round-specific coding (for example which providers count as skilled in
each BDHS round) is applied automatically using the survey round of the
data. Rates (fertility and mortality) are not record-level variables and
are skipped; use
[`bdhs_fertility()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_fertility.md)
and
[`bdhs_mortality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_mortality.md).

## Examples

``` r
kr <- bdhs_simulate(2017, "KR")
kr <- bdhs_add_indicators(kr, c("rh_del_facility", "ch_vac_bcg"))
table(kr$rh_del_facility, useNA = "ifany")
#> 
#>    0    1 <NA> 
#>   73   88  125 
```
