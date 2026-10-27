# Value labels of the BDHS recode files

The value labels (code -\> label) of every labelled variable in
[bdhs_codebook](https://armanhossen-am.github.io/bdhs/reference/bdhs_codebook.md).
Covers the same rounds and recodes as
[bdhs_codebook](https://armanhossen-am.github.io/bdhs/reference/bdhs_codebook.md).
Codes are not comparable across rounds without harmonization (for
example division code 5 is Rajshahi up to 2014 and Mymensingh from
2017-18; see
[bdhs_regions](https://armanhossen-am.github.io/bdhs/reference/bdhs_regions.md)
and
[`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)).

## Usage

``` r
bdhs_value_labels
```

## Format

A tibble with 71,973 rows (one per value label) and 5 columns:

- survey_id:

  DHS survey identifier.

- recode:

  Recode/file type.

- variable:

  Variable name (lower case).

- value:

  Numeric code.

- label:

  Label of the code, as in the file (typographic quotes made ASCII,
  original spelling kept, e.g. `"rajshani"` in 1993-94).

## Source

Metadata extracted from DHS recode files by
`data-raw/03-build-codebooks.R` (see
[bdhs_codebook](https://armanhossen-am.github.io/bdhs/reference/bdhs_codebook.md)).

## See also

[bdhs_codebook](https://armanhossen-am.github.io/bdhs/reference/bdhs_codebook.md),
[bdhs_regions](https://armanhossen-am.github.io/bdhs/reference/bdhs_regions.md).

## Examples

``` r
# Division codes across rounds
vl <- bdhs_value_labels[bdhs_value_labels$variable == "v024" &
  bdhs_value_labels$recode == "KR", ]
table(vl$label, vl$survey_id)
#>             
#>              BD1994DHS BD2000DHS BD2004DHS BD2007DHS BD2011DHS BD2014DHS
#>   barisal            0         1         1         1         1         1
#>   barishal           1         0         0         0         0         0
#>   chattogram         0         0         0         0         0         0
#>   chittagong         1         1         1         1         1         1
#>   dhaka              1         1         1         1         1         1
#>   khulna             1         1         1         1         1         1
#>   mymensingh         0         0         0         0         0         0
#>   rajashahi          0         1         0         0         0         0
#>   rajshahi           0         0         1         1         1         1
#>   rajshani           1         0         0         0         0         0
#>   rangpur            0         0         0         0         1         1
#>   sylhet             0         1         1         1         1         1
#>             
#>              BD2017DHS BD2022DHS
#>   barisal            1         0
#>   barishal           0         1
#>   chattogram         0         1
#>   chittagong         1         0
#>   dhaka              1         1
#>   khulna             1         1
#>   mymensingh         1         1
#>   rajashahi          0         0
#>   rajshahi           1         1
#>   rajshani           0         0
#>   rangpur            1         1
#>   sylhet             1         1
```
