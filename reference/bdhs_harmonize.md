# Harmonize BDHS variables across survey rounds

Creates analysis-ready variables that mean the same thing in every BDHS
round, from 1993-94 to 2022. DHS codes change between rounds (new
divisions, re-ordered region codes, new answer categories, letters of
the delivery-assistance variables, ...) and some value labels in the
files are wrong; the bundled crosswalk records, for every round, which
source variables and codes to use. See
[`bdhs_harmonized_vars()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonized_vars.md)
for the catalogue.

## Usage

``` r
bdhs_harmonize(
  data,
  vars = NULL,
  survey = NULL,
  recode = NULL,
  prefix = "",
  warn = TRUE
)
```

## Arguments

- data:

  A data frame of BDHS recode data with the original DHS codes
  (haven-labelled or plain numeric), as returned by
  [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md),
  [`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md)
  or
  [`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md).

- vars:

  Names of harmonized variables to create (see
  [`bdhs_harmonized_vars()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonized_vars.md)).
  `NULL` (default) creates every variable that applies to the recode.

- survey:

  Optional survey reference for data from a single round, overriding
  detection.

- recode:

  Optional recode code (`"KR"`, `"IR"`, ...), overriding detection.

- prefix:

  A prefix for the new column names, e.g. `"h_"`.

- warn:

  If `TRUE` (default), warn about unexpected codes and label drift.

## Value

`data` with the harmonized columns added (or replaced) and its
attributes kept. The attribute `bdhs_harmonized` lists the harmonized
columns.

## Details

The data can come from one round (detected from the `bdhs_survey_id`
attribute or `v000`/`v007`) or from several rounds stacked with
[`bdhs_pool()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_pool.md)
(a `survey_id` column), in which case every row is harmonized with the
rules of its own round.

- Factors get the documented levels (in documented order); binary
  variables are integer 0/1; numeric variables are on their natural
  scale (z-scores divided by 100, weights by 1e6, ...).

- Variables whose source variables are absent from `data` are skipped
  (one message lists them).

- Rounds where a variable was not collected, or where it lives in a
  separate WI (wealth index) or HW (WHO height/weight) file, give `NA`
  with a message. Merge those files first with
  [`bdhs_merge()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_merge.md)
  and the variable is computed from them.

- Codes not covered by the rules become `NA` with a warning, and value
  labels that no longer match the expected meaning (label drift, e.g.
  data attributed to the wrong round) are reported, unless
  `warn = FALSE`. Codes whose labels mark them as missing / don't know /
  not de jure are set to `NA` silently.

Rules are written for the children's recode (KR); woman-level rules also
apply to the women's (IR) and births (BR) recodes, and variables with a
household equivalent (`household_var` in
[`bdhs_harmonized_vars()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonized_vars.md))
to the household (HR) and household member (PR) recodes.

## Examples

``` r
kr <- bdhs_simulate(2014, "KR")
h <- bdhs_harmonize(kr, vars = c("division_trend6", "residence", "child_age_months"))
table(h$division_trend6)
#> 
#>   barishal chattogram      dhaka     khulna   rajshahi     sylhet 
#>         51         56         57         31         64         27 

# Pooled rounds are harmonized round by round
pooled <- bdhs_pool(bdhs_simulate(2011, "IR"), bdhs_simulate(2022, "IR"))
#> ℹ Value labels differ between rounds for 3 columns; kept the codes and dropped
#>   the labels: `v022`, `v023`, and `v024`.
#> ℹ DHS codes are not always comparable across rounds; use `harmonize = TRUE` or
#>   `bdhs_harmonize()` for comparable variables.
h <- bdhs_harmonize(pooled, vars = c("division_surveyed", "division_trend6"))
table(h$survey_id, h$division_trend6)
#>            
#>             barishal chattogram dhaka khulna rajshahi sylhet
#>   BD2011DHS      108        114   113     54      109     53
#>   BD2022DHS      112        113   111     54      107     55
```
