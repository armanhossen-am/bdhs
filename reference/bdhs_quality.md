# Data-quality diagnostics for DHS recode files

Computes the standard DHS data-quality indicators (the "Data quality
tables" of the final report appendices and the DHS Methodological
Reports on data quality) for whichever recode file is supplied. Each
check uses the variables it needs and is skipped, with a message, when
they are absent, so the same call works on PR, KR, BR and IR files.

## Usage

``` r
bdhs_quality(
  data,
  checks = NULL,
  survey = NULL,
  population = c("de_facto", "de_jure", "all"),
  vars = NULL
)
```

## Arguments

- data:

  A recode data frame from
  [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
  (numeric codes, not factors), or pooled data from
  [`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md).

- checks:

  Checks to run (see Details); `NULL` runs all those the data support.

- survey:

  Optional survey reference when `data` does not carry one.

- population:

  For household members (PR): `"de_facto"` (default, `hv103 == 1`),
  `"de_jure"` (`hv102 == 1`) or `"all"`.

- vars:

  Variables for the `"completeness"` check; `NULL` uses a default list
  of key variables, keeping those present.

## Value

A tibble of class `bdhs_quality` with columns `survey_id`, `recode`,
`check`, `statistic`, `group` (sex, year, variable, ...), `value`,
`benchmark` (the value expected with good data), `interpretation` and
`n` (number of cases behind the value). The attribute `profiles` holds
the single-year age distribution (`age`) and births by calendar year
(`births`), drawn by
[plot()](https://armanhossen-am.github.io/bdhs/reference/plot.bdhs_quality.md).

## Details

The checks are:

- `"age_heaping"` (PR, `hv105`, `hv104`): Whipple's index (ages 23-62,
  terminal digits 0 and 5; 100 = no preference, 500 = every age ends in
  0 or 5) and Myers' blended index (ages 10-89; 0 = no preference, 90 =
  every age ends in the same digit), for both sexes and by sex, and the
  share of members with age missing or "don't know". The single-year age
  distribution is kept for
  [plot()](https://armanhossen-am.github.io/bdhs/reference/plot.bdhs_quality.md).

- `"age_displacement"` (PR): ratios of the numbers aged 14 to 15, 50 to
  49 and 50 to 51 (men also 55 to 54). Interviewers who move women out
  of the eligible age range (15-49) inflate the counts at 14 and 50;
  values near 1 are expected. A high 50/49 ratio with an equally high
  50/51 ratio points to heaping on 50 rather than displacement.

- `"birth_date_completeness"` (BR, KR, or IR birth history, `b10`):
  percentage of births with month and year of birth reported (`b10`
  codes 0 "month, year and day" and 1 "month and year", the same in
  every BDHS round), overall, for living and dead children and for
  births in the last 5 and 15 years, plus the percentage of living
  children whose birth date (hence age) was partly imputed.

- `"births_by_year"` (`b3`, `v008`): births by calendar year and the
  calendar-year ratio \\100 B_x / ((B\_{x-1} + B\_{x+1}) / 2)\\, also by
  completed years before the interview. A ratio well below 100 just
  inside the five-year window (calendar year interview year - 5, or 4
  years before the interview) together with one above 100 just outside
  it suggests births were displaced out of the window that triggers the
  health questions. KR files only hold births of the last 5 years (3 in
  1993-94), so only interior years are available;
  `births_year4_vs_year3` (births 48-59 vs 36-47 months before the
  interview) then gives a window-edge check.

- `"age_at_death"` (`b6`, `b7`): share of deaths with age at death
  reported in days, months and years, or missing, and the heaping index
  at 12 months, \\100 D\_{12} / ((D\_{11} + D\_{13}) / 2)\\, where
  \\D\_{12}\\ counts deaths reported at 12 months or "1 year" (and the
  same index from the imputed months in `b7`).

- `"anthropometry"`: for children (PR `hc*`, KR/BR `hw*`) and for women
  (`ha*`) and men (`hb*`) in PR: percentage with weight or height
  missing, percentage of z-scores flagged by DHS (codes 9996-9998),
  percentage of the remaining z-scores outside the WHO plausible limits
  (height-for-age -6 to 6, weight-for-age -6 to 5, weight-for-height -5
  to 5), mean and standard deviation of the valid z-scores (WHO expects
  an SD of about 1.0-1.2; larger values point to measurement error), and
  digit preference of the terminal (decimal) digit of weight and height:
  percentage ending in 0, in 0 or 5 (expected 20) and the index of
  dissimilarity from a uniform digit distribution (0 = none, 90 = all on
  one digit).

- `"completeness"`: percentage missing (`NA`) and percentage coded as
  "don't know", "missing" or "inconsistent" (read from the value labels)
  for key variables (`vars`). `NA` includes rows skipped by design (for
  example `m14` for births outside the reference period).

All statistics are unweighted, as in the DHS data-quality tables. For PR
files the household population is the de facto population (`hv103 == 1`)
by default.

## References

Pullum, T. W. (2006). An assessment of age and date reporting in the DHS
surveys, 1985-2003. DHS Methodological Reports No. 5.

Pullum, T. W. and Staveteig, S. (2017). An assessment of the quality and
consistency of age and date reporting in DHS surveys, 2000-2015. DHS
Methodological Reports No. 19.

## See also

[`plot.bdhs_quality()`](https://armanhossen-am.github.io/bdhs/reference/plot.bdhs_quality.md)

## Examples

``` r
pr <- bdhs_simulate(2022, "PR")
q <- bdhs_quality(pr, checks = c("age_heaping", "anthropometry"))
q[q$statistic %in% c("whipple", "myers"), c("statistic", "group", "value")]
#> # A tibble: 6 × 3
#>   statistic group   value
#>   <chr>     <chr>   <dbl>
#> 1 whipple   total  101.  
#> 2 myers     total    3.70
#> 3 whipple   male   105.  
#> 4 myers     male     4.06
#> 5 whipple   female  96.4 
#> 6 myers     female   4.01
br <- bdhs_simulate(2022, "BR")
bdhs_quality(br, checks = "births_by_year")
#> # A tibble: 41 × 9
#>    survey_id recode check   statistic group value benchmark interpretation     n
#>    <chr>     <chr>  <chr>   <chr>     <chr> <dbl>     <dbl> <chr>          <dbl>
#>  1 BD2022DHS BR     births… calendar… 2001  108.        100 NA                68
#>  2 BD2022DHS BR     births… calendar… 2002   93.4       100 NA                64
#>  3 BD2022DHS BR     births… calendar… 2003   93.2       100 NA                69
#>  4 BD2022DHS BR     births… calendar… 2004  120         100 excess            84
#>  5 BD2022DHS BR     births… calendar… 2005   82.1       100 deficit           71
#>  6 BD2022DHS BR     births… calendar… 2006  115.        100 excess            89
#>  7 BD2022DHS BR     births… calendar… 2007   98.8       100 NA                84
#>  8 BD2022DHS BR     births… calendar… 2008   93.1       100 NA                81
#>  9 BD2022DHS BR     births… calendar… 2009  119.        100 excess            90
#> 10 BD2022DHS BR     births… calendar… 2010   80         100 deficit           70
#> # ℹ 31 more rows
```
