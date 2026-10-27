# Harmonizing 30 years of BDHS

``` r

library(bdhs)
```

The same DHS variable name does not always mean the same thing across
BDHS rounds:

- **Divisions** went from 5 (1993-94) to 6 (Sylhet), 7 (Rangpur, 2011)
  and 8 (Mymensingh, 2017-18). When Mymensingh became code 5, Rajshahi,
  Rangpur and Sylhet each moved up one code. Spellings changed too:
  “barisal” and “chittagong” became “barishal” and “chattogram” in 2022.
- **Answer categories** change. Places of delivery, types of provider
  and water and toilet types get new codes as the questionnaires evolve,
  and the letters of the delivery-assistance variables `m3a`-`m3n` shift
  between rounds.
- **Some content lives elsewhere.** The wealth index of 1993-94, 1996-97
  and 1999-2000 is in separate WI files, and the WHO growth-standard
  z-scores of 1996-97 to 2004 are in separate HW files.
- **Some value labels in the files are simply wrong**, for example
  religion in 1993-94, and roof material in 1999-2000.

[`bdhs_variable()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_variable.md)
shows the problem for any variable, using the bundled codebooks:

``` r

bdhs_variable("v024", recode = "KR")[, c("survey_id", "label", "value_labels", "label_drift")]
#> # A tibble: 8 × 4
#>   survey_id label    value_labels                                    label_drift
#>   <chr>     <chr>    <chr>                                           <lgl>      
#> 1 BD1994DHS region   1=barishal; 2=chittagong; 3=dhaka; 4=khulna; 5… TRUE       
#> 2 BD2000DHS division 1=barisal; 2=chittagong; 3=dhaka; 4=khulna; 5=… TRUE       
#> 3 BD2004DHS region   1=barisal; 2=chittagong; 3=dhaka; 4=khulna; 5=… TRUE       
#> 4 BD2007DHS division 1=barisal; 2=chittagong; 3=dhaka; 4=khulna; 5=… TRUE       
#> 5 BD2011DHS region   1=barisal; 2=chittagong; 3=dhaka; 4=khulna; 5=… TRUE       
#> 6 BD2014DHS division 1=barisal; 2=chittagong; 3=dhaka; 4=khulna; 5=… TRUE       
#> 7 BD2017DHS division 1=barisal; 2=chittagong; 3=dhaka; 4=khulna; 5=… TRUE       
#> 8 BD2022DHS division 1=barishal; 2=chattogram; 3=dhaka; 4=khulna; 5… FALSE
```

## The harmonized variables

bdhs ships a crosswalk with rules for 112 harmonized variables. Each
rule names the source variables and code mapping for every round, and
was checked against the real files and against official DHS estimates.

``` r

hv <- bdhs_harmonized_vars()
hv[, c("name", "label", "comparability")]
#> # A tibble: 112 × 3
#>    name              label                                         comparability
#>    <chr>             <chr>                                         <chr>        
#>  1 survey_id         DHS survey identifier                         full         
#>  2 cluster           Cluster number (v001)                         full         
#>  3 psu               Primary sampling unit (v021; equals v001 in … full         
#>  4 strata            Sampling-error strata (v022)                  full         
#>  5 wt                Women's/children's sample weight (v005/1e6)   full         
#>  6 awfact_total      All-woman factor, total (awfactt/100)         full         
#>  7 interview_cmc     Date of interview (CMC)                       full         
#>  8 interview_year    Year of interview (from CMC)                  full         
#>  9 division_surveyed Division as surveyed (v024)                   break        
#> 10 division_trend6   Division, 6-division trend-consistent (Rangp… full         
#> # ℹ 102 more rows
table(hv$comparability)
#> 
#>             break              full           partial partial (no 2022) 
#>                 2                56                44                10
```

`comparability` tells you how far a variable can be compared:

- `"full"`: same meaning in every round.
- `"partial"`: comparable, but with documented caveats, such as new
  categories or one round missing.
- `"break"`: definitions differ. The division as surveyed is one
  example, because the boundaries themselves changed.

`bdhs_harmonized_vars(survey)` shows the rules for one round, with the
source variables:

``` r

r <- bdhs_harmonized_vars(2004)
r[r$name %in% c("wealth_quintile", "facility_delivery", "haz_who"), c("name", "method", "source", "available", "note")]
#> # A tibble: 3 × 5
#>   name              method        source                         available note 
#>   <chr>             <chr>         <chr>                          <lgl>     <chr>
#> 1 wealth_quintile   map           v190                           TRUE      NA   
#> 2 facility_delivery derive        derived from delivery_place_c… TRUE      NA   
#> 3 haz_who           external_file hc70 (HW file)                 FALSE     WHO …
```

## Harmonizing a dataset

[`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)
adds the harmonized columns to your data. The original DHS columns stay
as they were.

``` r

kr <- bdhs_simulate(2017, "KR")
h <- bdhs_harmonize(kr, vars = c("division_surveyed", "division_trend6", "residence",
                                 "wealth_quintile", "facility_delivery", "csection"))
h[1:6, c("v024", "division_surveyed", "division_trend6", "residence", "facility_delivery", "csection")]
#> # A tibble: 6 × 6
#>   v024    division_surveyed division_trend6 residence facility_delivery csection
#>   <dbl+l> <fct>             <fct>           <fct>                 <int>    <int>
#> 1 1 [bar… barishal          barishal        urban                     1        1
#> 2 1 [bar… barishal          barishal        urban                     1        0
#> 3 1 [bar… barishal          barishal        urban                     0        0
#> 4 1 [bar… barishal          barishal        urban                     0        0
#> 5 1 [bar… barishal          barishal        urban                     1        1
#> 6 1 [bar… barishal          barishal        urban                     1        0
```

Without `vars`, every harmonized variable that applies to the recode is
created. Rules written for children (KR) also apply to women (IR) and
births (BR) where they concern the mother. Variables with a household
equivalent also work in the household (HR) and member (PR) files.

### Division layouts

| Variable | Layout |
|----|----|
| `division_surveyed` | Divisions as they existed at the survey (5 to 8) |
| `division_trend6` | Six divisions: Rangpur in Rajshahi, Mymensingh in Dhaka |
| `division_trend5` | Five 1993-94 divisions: Sylhet also in Chattogram |
| `division_backcast8` | 2022’s eight divisions, rebuilt from districts where possible |

The bundled `bdhs_regions` table holds the same crosswalk for every
survey, with the codes as they appear in the files:

``` r

bdhs_regions[bdhs_regions$survey_id == "BD2017DHS", c("region_code", "region_label", "division", "trend6", "trend5")]
#> # A tibble: 8 × 5
#>   region_code region_label division   trend6     trend5    
#>         <int> <chr>        <chr>      <chr>      <chr>     
#> 1           1 barisal      barishal   barishal   barishal  
#> 2           2 chittagong   chattogram chattogram chattogram
#> 3           3 dhaka        dhaka      dhaka      dhaka     
#> 4           4 khulna       khulna     khulna     khulna    
#> 5           5 mymensingh   mymensingh dhaka      dhaka     
#> 6           6 rajshahi     rajshahi   rajshahi   rajshahi  
#> 7           7 rangpur      rangpur    rajshahi   rajshahi  
#> 8           8 sylhet       sylhet     sylhet     chattogram
```

## Trends with pooled, harmonized data

Pool rounds with `harmonize = TRUE`, then analyse with the pooled
design:

``` r

years <- c(2004, 2011, 2017, 2022)
rounds <- lapply(seq_along(years), function(i) bdhs_simulate(years[i], "KR", seed = i))
pooled <- bdhs_pool(rounds, harmonize = TRUE, vars = c("division_trend6", "csection"))
#> ℹ Value labels differ between rounds for 3 columns; kept the codes and dropped
#>   the labels: `v022`, `v023`, and `v024`.
#> ℹ DHS codes are not always comparable across rounds; use `harmonize = TRUE` or
#>   `bdhs_harmonize()` for comparable variables.
#> ℹ Harmonized 2 variables in 4 rounds.
des <- bdhs_design(pooled)
survey::svyby(~csection, ~survey_id, des, survey::svymean, na.rm = TRUE)
#>           survey_id  csection         se
#> BD2004DHS BD2004DHS 0.3091721 0.02781968
#> BD2011DHS BD2011DHS 0.2664936 0.01948915
#> BD2017DHS BD2017DHS 0.3183512 0.03494818
#> BD2022DHS BD2022DHS 0.2905970 0.03195544
```

(The values are random because the data are simulated. With real data
this is the C-section trend.)

## Early rounds: wealth and z-scores from separate files

For 1993-94 to 1999-2000 the wealth index comes from a WI file, and the
WHO z-scores for 1996-97 to 2004 come from an HW file. Merge them first,
and
[`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)
picks them up:

``` r

kr <- bdhs_read(2004, "KR")
hw <- bdhs_read(2004, "HW")
kr <- bdhs_merge(kr, hw)
kr <- bdhs_harmonize(kr, vars = c("haz_who", "stunted_who"))
```

## Merging recodes

[`bdhs_merge()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_merge.md)
knows the DHS linking keys, so you don’t have to remember them:

| x          | y      | Keys                                               |
|------------|--------|----------------------------------------------------|
| IR, KR, BR | HR     | `v001`, `v002` = `hv001`, `hv002`                  |
| IR         | PR     | `v001`, `v002`, `v003` = `hv001`, `hv002`, `hvidx` |
| KR, BR     | IR     | `caseid`                                           |
| KR         | PR     | `v001`, `v002`, `b16` = `hv001`, `hv002`, `hvidx`  |
| PR         | HR     | `hhid`                                             |
| MR         | HR, PR | `mv001`, `mv002` (and `mv003` = `hvidx`)           |

``` r

ir <- bdhs_simulate(2022, "IR")
hr <- bdhs_simulate(2022, "HR")
m <- bdhs_merge(ir, hr)
#> ✔ Merged HR into IR on v001, v002 = hv001, hv002: 552 of 552 rows matched
#>   (100%).
m[1:3, c("caseid", "v001", "v002", "hv201", "hv205", "hv270")]
#> # A tibble: 3 × 6
#>   caseid             v001  v002 hv201                      hv205         hv270  
#>   <chr>             <dbl> <dbl> <dbl+lbl>                  <dbl+lbl>     <dbl+l>
#> 1 "       1   2  2"     1     2 11 [piped into dwelling]   21 [ventilat… 4 [ric…
#> 2 "       1   3  2"     1     3 21 [tube well or borehole] 22 [pit latr… 4 [ric…
#> 3 "       1   4  2"     1     4 21 [tube well or borehole] 31 [no facil… 2 [poo…
```

## Special codes

DHS files use codes like 98 (don’t know), 99 (missing) and 9996-9999
(flagged anthropometry), and recent files also use Stata’s tagged
missing values.
[`bdhs_na()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_na.md)
turns them into `NA` using the value labels:

``` r

x <- haven::labelled(c(1, 2, 8, 9), c(yes = 1, no = 2, "don't know" = 8, missing = 9))
bdhs_na(x)
#> <labelled<double>[4]>
#> [1]  1  2 NA NA
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no
bdhs_na(x, dont_know = "keep")
#> <labelled<double>[4]>
#> [1]  1  2  8 NA
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      2         no
#>      8 don't know
```

[`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)
already does this for every harmonized variable.

## Caveats

- No 1996-97 file was available while the crosswalk was built. Its rules
  were derived from the published codebook and are flagged as unverified
  in `bdhs_harmonized_vars(1997)`.
- District-based variables (`district_bbs`, `division_backcast8`) need a
  district variable, which most rounds do not include.
- Men’s files (MR, last collected in 2011) are not yet covered by the
  crosswalk.
