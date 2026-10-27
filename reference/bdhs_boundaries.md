# Division boundaries of a survey round

The subnational (division) boundaries used for each Bangladesh survey,
as an sf object. Boundaries change between rounds as divisions were
created (Sylhet 1995, Rangpur 2010, Mymensingh 2015), so use the
boundaries of the round you are mapping.

## Usage

``` r
bdhs_boundaries(survey = 2022)
```

## Arguments

- survey:

  Survey references (see
  [`bdhs_survey_id()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_survey_id.md));
  several are allowed, and `NULL` returns every round.

## Value

An sf object (EPSG:4326) with columns `survey_id`, `region_code`,
`region_name`, `unit_id` (stable 4-digit code of the geographic unit
across rounds), `reg_id` and `geometry`.

## Details

Boundaries come from the DHS Program Spatial Data Repository
(<https://spatialdata.dhsprogram.com/home/>, ICF), licensed under the
Open Database License (ODbL) 1.0. `region_code` is the region code used
in the recode files (`v024`/`hv024`), and `reg_id` links to the
`region_id` of official subnational estimates in
[`bdhs_official()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_official.md).

## See also

[`bdhs_map()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_map.md)

## Examples

``` r
b <- bdhs_boundaries(2022)
b
#> Simple feature collection with 8 features and 5 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 88.03243 ymin: 20.59039 xmax: 92.68078 ymax: 26.63625
#> Geodetic CRS:  WGS 84
#>   survey_id region_code region_name unit_id          reg_id
#> 1 BD2022DHS           1    Barishal    0001 BDDHS2022360001
#> 2 BD2022DHS           2  Chattogram    0003 BDDHS2022360003
#> 3 BD2022DHS           3       Dhaka    0010 BDDHS2022360010
#> 4 BD2022DHS           4      Khulna    0006 BDDHS2022360006
#> 5 BD2022DHS           5  Mymensingh    0011 BDDHS2022360011
#> 6 BD2022DHS           6    Rajshahi    0009 BDDHS2022360009
#> 7 BD2022DHS           7     Rangpur    0008 BDDHS2022360008
#> 8 BD2022DHS           8      Sylhet    0004 BDDHS2022360004
#>                         geometry
#> 1 MULTIPOLYGON (((90.33083 21...
#> 2 MULTIPOLYGON (((91.85583 21...
#> 3 MULTIPOLYGON (((89.35535 23...
#> 4 MULTIPOLYGON (((89.2995 21....
#> 5 MULTIPOLYGON (((89.64786 25...
#> 6 MULTIPOLYGON (((88.7203 24....
#> 7 MULTIPOLYGON (((88.83978 25...
#> 8 MULTIPOLYGON (((90.96542 25...
table(bdhs_boundaries(NULL)$survey_id)
#> 
#> BD1994DHS BD1997DHS BD1999SPA BD2000DHS BD2001OTH BD2004DHS BD2007DHS BD2011DHS 
#>         5         6         6         6         6         6         6         7 
#> BD2014DHS BD2014SPA BD2017DHS BD2017SPA BD2022DHS 
#>         7         7         8         8         8 
```
