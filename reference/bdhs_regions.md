# Regions (divisions) of every Bangladesh survey

A crosswalk of the region variable of each survey: one row per survey
and region code, with the division it denotes, the stable geographic
unit, the DHS API region id, the corresponding boundaries and two
trend-consistent groupings.

## Usage

``` r
bdhs_regions
```

## Format

A tibble with 86 rows and 14 columns:

- survey_id:

  DHS survey identifier (all 13 surveys).

- region_var:

  Region variable of the survey's files: `"v024"` for the DHS rounds
  (`hv024` in household and household-member files, `mv024` in men's
  files, with the same codes); `"codiv"` (1999-2000 SPA), `"qhregion"`
  (2001 BMMS) and `"region"` (2014 and 2017 SPA) as given by the DHS
  Spatial Data Repository.

- region_code:

  Code of the region in `region_var`.

- region_label:

  Value label of the code as written in the survey file (e.g.
  `"barisal"`, `"rajshani"`) where the file was checked
  (`label_source == "microdata"`), otherwise the region name given by
  the DHS Spatial Data Repository.

- division:

  Division, canonical lower-case 2022 spelling: one of `"barishal"`,
  `"chattogram"`, `"dhaka"`, `"khulna"`, `"mymensingh"`, `"rajshahi"`,
  `"rangpur"`, `"sylhet"`.

- division_label:

  `division` in title case, e.g. `"Chattogram"`.

- includes:

  Later division contained in this region, if any: `"Sylhet"`
  (Chattogram in 1993-94), `"Rangpur"` (Rajshahi up to 2007),
  `"Mymensingh"` (Dhaka up to 2014); otherwise `NA`.

- unit_id:

  Stable 4-digit geographic unit: `"0001"` Barishal, `"0002"` Chattogram
  incl. Sylhet, `"0003"` Chattogram, `"0004"` Sylhet, `"0005"` Dhaka
  incl. Mymensingh, `"0006"` Khulna, `"0007"` Rajshahi incl. Rangpur,
  `"0008"` Rangpur, `"0009"` Rajshahi, `"0010"` Dhaka, `"0011"`
  Mymensingh. Regions with the same `unit_id` cover the same area in
  every survey.

- reg_id:

  DHS API region id (`region_id` in
  [bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md));
  `NA` for the 1999-2000 and 2014 SPA and the 2001 BMMS.

- trend6:

  6-division trend grouping used by DHS/NIPORT trend tables: Rangpur -\>
  Rajshahi, Mymensingh -\> Dhaka (lower case). `NA` for 1993-94
  Chittagong, which includes Sylhet and can only be split with the
  district variable (`sdistr`; see
  [`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)).

- trend5:

  5-division grouping of 1993-94: as `trend6`, plus Sylhet -\>
  Chattogram. Comparable in every round.

- code_verified:

  `TRUE` if the code and its label were checked against the value labels
  of the survey's microdata (1993-94, 1999-2000, 2004, 2007, 2011, 2014,
  2017-18 and 2022 DHS); `FALSE` for codes taken from the DHS Spatial
  Data Repository only (1996-97 DHS, SPAs, BMMS).

- label_source:

  `"microdata"` or `"sdr"`: where `region_label` comes from.

- note:

  Caveats for the row, if any.

## Source

Value labels of the survey files
([bdhs_value_labels](https://armanhossen-am.github.io/bdhs/reference/bdhs_value_labels.md));
DHS Spatial Data Repository region attributes
(<https://spatialdata.dhsprogram.com/home/>, accessed 2026-10-26); bdhs
harmonization rules. Built by `data-raw/05-build-regions.R`.

## Details

Bangladesh has gained divisions during the survey period: Sylhet (from
Chittagong, 1995), Rangpur (from Rajshahi, 2010) and Mymensingh (from
Dhaka, 2015). Codes therefore change meaning across rounds (code 5 is
Rajshahi up to 2014 and Mymensingh from 2017-18) and older regions
contain later divisions (see `includes`). Use `trend6`/`trend5` to
compare rounds.

The region codes of the 2017-18 DHS follow the microdata (1 Barisal, 2
Chittagong, 3 Dhaka, 4 Khulna, 5 Mymensingh, 6 Rajshahi, 7 Rangpur, 8
Sylhet); the DHS Spatial Data Repository codes 5-8 differently for this
survey. The 2017 SPA uses the Repository's coding, which has not been
checked against SPA microdata.

## See also

[`bdhs_boundaries()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_boundaries.md)
for the polygons (same `survey_id`, `region_code`, `unit_id` and
`reg_id`),
[`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)
for harmonized division variables,
[bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md)
for official regional estimates.

## Examples

``` r
# What does code 5 mean in each round?
bdhs_regions[bdhs_regions$region_code == 5,
  c("survey_id", "region_label", "division", "includes")]
#> # A tibble: 13 × 4
#>    survey_id region_label division   includes
#>    <chr>     <chr>        <chr>      <chr>   
#>  1 BD1994DHS rajshani     rajshahi   Rangpur 
#>  2 BD1997DHS rajashahi    rajshahi   Rangpur 
#>  3 BD1999SPA rasjhahi     rajshahi   Rangpur 
#>  4 BD2000DHS rajashahi    rajshahi   Rangpur 
#>  5 BD2001OTH rajshahi     rajshahi   Rangpur 
#>  6 BD2004DHS rajshahi     rajshahi   Rangpur 
#>  7 BD2007DHS rajshahi     rajshahi   Rangpur 
#>  8 BD2011DHS rajshahi     rajshahi   NA      
#>  9 BD2014DHS rajshahi     rajshahi   NA      
#> 10 BD2014SPA rajshahi     rajshahi   NA      
#> 11 BD2017DHS mymensingh   mymensingh NA      
#> 12 BD2017SPA rajshahi     rajshahi   NA      
#> 13 BD2022DHS mymensingh   mymensingh NA      

# Recode a 2017-18 v024 vector to the 6-division trend grouping
r17 <- bdhs_regions[bdhs_regions$survey_id == "BD2017DHS", ]
v024 <- c(1, 5, 7, 8)
r17$trend6[match(v024, r17$region_code)]
#> [1] "barishal" "dhaka"    "rajshahi" "sylhet"  
```
