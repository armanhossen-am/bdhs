# Calibrated synthetic BDHS data

Builds **synthetic** recode data with the structure of
[`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md)
whose weighted estimates reproduce the *official* DHS values of a
curated set of indicators
([`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md)),
nationally and by division, residence and (optionally) wealth quintile.
Only published aggregates are used, never microdata, so the result can
be shared, bundled with papers and used for teaching, testing and fully
reproducible worked examples whose headline numbers match the DHS
reports.

## Usage

``` r
bdhs_synthesize(
  survey = 2022,
  recode = c("KR", "IR", "PR"),
  targets = NULL,
  margins = c("division", "residence"),
  scale = 1,
  seed = 1
)
```

## Arguments

- survey:

  A survey reference understood by
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md)
  (a DHS round, 1993-94 to 2022).

- recode:

  `"KR"` (children), `"IR"` (women) or `"PR"` (household members).

- targets:

  Indicator ids to calibrate (see Details). `NULL` uses every supported
  indicator of the recode with an official value in the round.

- margins:

  Margins to calibrate in addition to the national value: any of
  `"division"`, `"residence"` and `"wealth"`. `NULL` or
  [`character()`](https://rdrr.io/r/base/character.html) calibrates
  national values only.

- scale:

  Sample size relative to the real survey (clusters are scaled;
  households per cluster are kept). Small values give fast, coarser
  data.

- seed:

  Random seed. The global random number generator state is left
  untouched.

## Value

A tibble like
[`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md)
output, with attributes `bdhs_survey_id`, `bdhs_recode`,
`bdhs_simulated = TRUE`, `bdhs_synthetic = TRUE`, `bdhs_targets` (the
official values used: one row per indicator, margin and category),
`bdhs_synthetic_design` (seed, scale, clusters, households per cluster,
reference indicator, and `inconsistent_margins`: published breakdowns
that were not calibrated because their denominators do not add up to the
national one) and `bdhs_fidelity` (the result of
[`bdhs_synthetic_fidelity()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_synthetic_fidelity.md)).

## Details

Supported targets (each depends on one source variable that the
synthesiser controls):

- KR: `rh_del_facility` (`m15`), `rh_del_csection` (`m17`),
  `rh_anc_4plus` (`m14`), `ch_vac_bcg` (`h2`), `ch_vac_dpt3`
  (`h3`/`h5`/`h7`), `ch_vac_measles` (`h9`), `ch_diarrhoea` (`h11`).
  Vaccinations are not available for 2022 and caesarean section not for
  1993-94. For 1993-94 the KR file is restricted to births in the last 3
  years, as in the real file.

- IR: `fp_cpr_any`, `fp_cpr_modern` (`v312`/`v313`), `fp_unmet_total`
  (`v626a`), `ms_married_by_18` (`v511`, all-women denominator through
  `awfactt`), and `nt_wm_thin` (`v445`) on request.

- PR: `nt_stunting` (`hc70`), `nt_wasting` (`hc72`), `nt_underweight`
  (`hc71`), `nt_wm_thin` (`ha40`), `hh_electricity` (`hv206`).

Official values use the indicator's reference period
(`by_variable_label`, e.g. births in the last 3 years). Divisions are
matched to the divisions as surveyed in the round (older rounds publish
Dhaka and Rajshahi under "Dhaka before 2015" and "Rajshahi/Rangpur"). A
margin without published values for every category is skipped for that
indicator, and so is a margin whose published denominators do not add up
(within 2%) to the national denominator, because it then refers to a
different population than the national value (this happens for the
1996-97 and 1999-2000 child anthropometry by residence).

## Method

1.  **Sample design.** The number of clusters is the real number of the
    round (from
    [bdhs_surveys](https://armanhossen-am.github.io/bdhs/reference/bdhs_surveys.md))
    times `scale`; households per cluster are chosen so the number of
    women approximates the real sample. Clusters are allocated to
    division x residence strata in proportion to the official
    *unweighted* denominators of a reference indicator (births in the
    last 3 years for KR, currently married women for IR, the de jure
    population for PR), with at least two PSUs per stratum. After
    simulation the weights are post-stratified so that the *weighted*
    reference population is distributed across strata as in the official
    weighted denominators (the joint division x residence table is the
    product of the published margins, which is the only information
    available from aggregates). Weights are normalised to the DHS
    convention (mean `v005` = 1,000,000).

2.  **Cell model.** For every target indicator the probability in cell
    \\c\\ (division x residence, x wealth quintile when requested) is
    \\p_c = \mathrm{logit}^{-1}(a + b\_{div} + c\_{res} \[+
    d\_{wq}\])\\. The coefficients are fitted by iterative proportional
    fitting on the logit scale (logit raking) so that the weighted
    synthetic margins equal the official margins exactly, using the
    synthetic weighted denominators of each cell (the indicator's own
    universe function, including all-woman factors for all-women
    indicators).

3.  **Deterministic assignment.** Within each cell the eligible records
    are ordered by a key and the first \\k_c\\ receive the outcome, with
    \\k_c\\ chosen so that the weighted cell proportion is closest to
    \\p_c\\; a greedy controlled-rounding step then moves the \\k_c\\ by
    single records to minimise the squared deviations of all calibrated
    margins (national included). Results are therefore exact up to the
    granularity of single records, not subject to Monte Carlo noise.
    Keys combine a mild wealth gradient with random noise, except for
    anthropometric targets, where the record's own z-score or BMI is the
    key and the cell's distribution is shifted (location shift) so that
    its shape is kept.

4.  **Coherence.** Source variables are rewritten consistently:
    caesarean births are facility births, delivery assistance (`m3*`)
    follows the place of delivery, modern users are a subset of users,
    unmet need only occurs among non-users, method (`v312`), method type
    (`v313`) and need status (`v626a`) agree, and electricity is a
    household attribute.

## What is and is not reproduced

Reproduced: the weighted value of every calibrated indicator nationally
and for each calibrated margin (see
[`bdhs_synthetic_fidelity()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_synthetic_fidelity.md)),
typically within a few hundredths of a percentage point at `scale = 1`
(official values are rounded to 0.1). The national value is implied by
the margins and the synthetic population shares; it can deviate slightly
when an indicator's universe is distributed across divisions differently
from the reference population.

**Not** reproduced: joint distributions beyond the additive logit model
(e.g. urban Sylhet), associations between targets or with non-target
variables (they follow the simple simulation model), every non-target
variable, fertility and mortality rates, and standard errors (the design
is realistic in size and structure but intra-cluster correlation is not
calibrated, so SEs are only indicative). Calibration is done per recode:
synthesised KR, IR and PR files share their structure but are not
calibrated jointly, so linked values (e.g. a woman's BMI in IR and PR)
can differ. The 2022 short-questionnaire design (`sqtype`) is not
reproduced.

**Synthetic data must never be used for substantive conclusions**; they
are for teaching, software testing and reproducible demonstrations.

## See also

[`bdhs_synthetic_fidelity()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_synthetic_fidelity.md),
[`bdhs_simulate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_simulate.md),
[`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md).

## Examples

``` r
# \donttest{
kr <- bdhs_synthesize(2017, "KR", scale = 0.2)
bdhs_estimate(kr, "rh_del_csection", by = "v025")
#> # BDHS estimates: 1 indicator | BD2017DHS | 95% CI
#> # A tibble: 2 × 11
#>   survey_id indicator     label v025  estimate    se ci_low ci_high n_unweighted
#>   <chr>     <chr>         <chr> <fct>    <dbl> <dbl>  <dbl>   <dbl>        <int>
#> 1 BD2017DHS rh_del_csect… Deli… urban     43.7  2.07   39.7    47.8          445
#> 2 BD2017DHS rh_del_csect… Deli… rural     28.7  1.80   25.3    32.4          710
#> # ℹ 2 more variables: n_weighted <dbl>, api_id <chr>
bdhs_official("RH_DELA_C_CSC", survey = 2017, characteristic = "Residence")
#> # A tibble: 2 × 13
#>   survey_id year_label indicator_id  indicator            characteristic_categ…¹
#>   <chr>     <chr>      <chr>         <chr>                <chr>                 
#> 1 BD2017DHS 2017-18    RH_DELA_C_CSC Delivery by cesarea… Residence             
#> 2 BD2017DHS 2017-18    RH_DELA_C_CSC Delivery by cesarea… Residence             
#> # ℹ abbreviated name: ¹​characteristic_category
#> # ℹ 8 more variables: characteristic_label <chr>, value <dbl>, ci_low <dbl>,
#> #   ci_high <dbl>, denom_weighted <dbl>, denom_unweighted <dbl>,
#> #   region_id <chr>, breakdown <chr>
bdhs_synthetic_fidelity(kr)
#> # A tibble: 77 × 9
#>    survey_id indicator       api_id  margin category official synthetic     diff
#>    <chr>     <chr>           <chr>   <chr>  <chr>       <dbl>     <dbl>    <dbl>
#>  1 BD2017DHS rh_del_facility RH_DEL… natio… Total        49.4      49.4 -1.88e-2
#>  2 BD2017DHS rh_del_facility RH_DEL… divis… barishal     38.8      38.8 -1.68e-3
#>  3 BD2017DHS rh_del_facility RH_DEL… divis… chittag…     46        46.0  1.90e-2
#>  4 BD2017DHS rh_del_facility RH_DEL… divis… dhaka        56.7      56.7  2.28e-2
#>  5 BD2017DHS rh_del_facility RH_DEL… divis… khulna       60.8      60.8  1.79e-2
#>  6 BD2017DHS rh_del_facility RH_DEL… divis… mymensi…     39        39.0 -9.79e-4
#>  7 BD2017DHS rh_del_facility RH_DEL… divis… rajshahi     52.8      52.8  2.12e-2
#>  8 BD2017DHS rh_del_facility RH_DEL… divis… rangpur      47.3      47.3  1.76e-2
#>  9 BD2017DHS rh_del_facility RH_DEL… divis… sylhet       38.1      38.1  1.41e-2
#> 10 BD2017DHS rh_del_facility RH_DEL… resid… urban        62.5      62.5 -1.95e-2
#> # ℹ 67 more rows
#> # ℹ 1 more variable: n_unweighted <int>
# }
```
