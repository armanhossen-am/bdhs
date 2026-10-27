# Search variables, indicators and harmonized variables

One search box for everything bundled with the package: variable names
and labels of the DHS recode files
([bdhs_codebook](https://armanhossen-am.github.io/bdhs/reference/bdhs_codebook.md)),
official indicator ids, labels and definitions
([bdhs_indicator_catalog](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_catalog.md)),
the indicators bdhs can compute from microdata
([`bdhs_indicator_list()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_list.md),
used by
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md))
and the cross-round harmonized variables used by
[`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md).
Works offline and needs no microdata.

## Usage

``` r
bdhs_search(
  pattern,
  where = c("all", "variables", "indicators", "computable", "harmonized"),
  survey = NULL,
  recode = NULL,
  value_labels = FALSE
)
```

## Arguments

- pattern:

  A regular expression, matched case-insensitively. Invalid regular
  expressions are matched as plain text.

- where:

  Where to search: `"all"` (default), or any of `"variables"`,
  `"indicators"` (official DHS indicators), `"computable"` (indicators
  [`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
  computes) and `"harmonized"`.

- survey:

  Optional survey references (see
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md));
  keeps results available in at least one of these rounds.

- recode:

  Optional recode codes (e.g. `"IR"`, `"KR"`); restricts variables and
  harmonized variables to these recodes. Indicators are not affected.

- value_labels:

  If `TRUE`, also search the value labels of variables (e.g.
  `"pentavalent"` finds variables with such a category).

## Value

A tibble of class `bdhs_search` with columns `source` (`"variable"`,
`"indicator"`, `"computable"` or `"harmonized"`), `id` (variable name,
DHS indicator id, bdhs indicator id or harmonized name), `label`,
`recode` and `surveys` (comma separated survey ids). For computable
indicators the DHS indicator id they reproduce is appended to the label.
It prints compactly; use `print(x, n = Inf)` to see every match or
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
for a plain tibble.

## See also

[`bdhs_variable()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_variable.md)
for one variable across rounds,
[`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md)
for the official estimates of an indicator.

## Examples

``` r
bdhs_search("stunt")
#> # bdhs search for "stunt": 5 matches
#> 
#> ── Official DHS indicators (2) ─────────────────────────────────────────────────
#> CN_NUTS_C_HA3  Children severely stunted  1996-97, 1999-00, 2004, 2007, 2011, 2014, 2017-18, 2022
#> CN_NUTS_C_HA2  Children stunted           1996-97, 1999-00, 2004, 2007, 2011, 2014, 2017-18, 2022
#> 
#> ── Computable with bdhs_estimate() (2) ─────────────────────────────────────────
#> nt_stunting         Stunting (height-for-age < ...  1996-97, 1999-00, 2004, 2007, 2011, 2014, 2017-18, 2022
#> nt_stunting_severe  Severe stunting (height-for...  1996-97, 1999-00, 2004, 2007, 2011, 2014, 2017-18, 2022
#> 
#> ── Harmonized variables (1) ────────────────────────────────────────────────────
#> stunted_who  Stunted (WHO HAZ < -2)  all 9 DHS rounds
bdhs_search("region|division", where = "variables", recode = "IR")
#> # bdhs search for "region|division": 5 matches
#> 
#> ── Variables (5) ───────────────────────────────────────────────────────────────
#> v024 [IR]     division                        2017-18, 2022
#> v101 [IR]     division                        2017-18, 2022
#> v105a [IR]    division of previous residence  2017-18, 2022
#> v139 [IR]     de jure region of residence     2017-18, 2022
#> awfactr [IR]  all woman factor - regional     2017-18, 2022
bdhs_search("anemia", where = "indicators", survey = 2011)
#> # bdhs search for "anemia": 10 matches
#> 
#> ── Official DHS indicators (10) ────────────────────────────────────────────────
#> CN_ANMC_C_ANY  Children with any anemia                     2011
#> CN_ANMC_C_MLD  Children with mild anemia                    2011
#> CN_ANMC_C_MOD  Children with moderate anemia                2011
#> CN_ANMC_C_SEV  Children with severe anemia                  2011
#> AN_ANEM_W_ANY  Women with any anemia                        2011
#> AN_ANEM_W_MLD  Women with mild anemia                       2011
#> AN_ANEM_W_MOD  Women with moderate anemia                   2011
#> AN_ANEM_W_SEV  Women with severe anemia                     2011
#> CN_ANMC_C_NUM  Number of children under age 5               2011
#> CN_ANMC_C_UNW  Number of children under age 5 (unweighted)  2011
```
