# Codebooks of the BDHS recode files

Variable-level metadata of the BDHS recode files: name, label, type,
number of value labels and number of non-missing values, so you can see
which variables exist in which round without the data, and compare
rounds. Value labels are in
[bdhs_value_labels](https://armanhossen-am.github.io/bdhs/reference/bdhs_value_labels.md).

## Usage

``` r
bdhs_codebook
```

## Format

A tibble with 25,910 rows (one per variable per file) and 9 columns:

- survey_id:

  DHS survey identifier.

- recode:

  Recode/file type, e.g. `"KR"` (see
  [`bdhs_recodes()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_recodes.md)).

- file_stem:

  DHS file name the metadata describe, e.g. `"BDKR7RFL"`.

- position:

  Position of the variable in the file.

- variable:

  Variable name (lower case).

- label:

  Variable label as in the file (typographic quotes made ASCII).

- type:

  R class of the variable when read with haven: `"haven_labelled"`
  (numeric with value labels), `"numeric"` or `"character"`.

- n_value_labels:

  Number of value labels (rows in
  [bdhs_value_labels](https://armanhossen-am.github.io/bdhs/reference/bdhs_value_labels.md)).

- n_valid:

  Number of records with a non-missing value. DHS special codes such as
  9/99 ("missing") or 8/98 ("don't know") count as non-missing;
  system-missing values and empty strings do not. Compare with the
  number of records of the file to see if a variable was asked of
  everyone.

## Source

Metadata extracted from DHS recode files (Stata format) by
`data-raw/03-build-codebooks.R`; the files used are listed in
`data-raw/codebook-sources.csv`. Labels are the work of The DHS
Program/ICF and the survey implementing agencies.

## Details

These are **metadata only**: no data values, ranges, frequencies or
observed codes are included. They were extracted from files the
maintainer is licensed to use, so **only the following rounds and
recodes are covered**:

- KR (children under 5) for 1993-94, 1999-2000, 2004, 2007, 2011, 2014,
  2017-18 and 2022;

- every 2017-18 file: BR, FW, HR, IR, KR, PR, SQ and VA;

- IR, KR and PR for 2022.

1996-97 and the SPA and BMMS surveys are not covered. Variable names are
lower case, as returned by
[`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md).

## See also

[bdhs_value_labels](https://armanhossen-am.github.io/bdhs/reference/bdhs_value_labels.md),
[`bdhs_variable()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_variable.md),
[`bdhs_search()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_search.md),
[`bdhs_harmonized_vars()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonized_vars.md).

## Examples

``` r
# Which rounds have a KR caesarean-section variable (m17)?
bdhs_codebook[bdhs_codebook$variable == "m17" & bdhs_codebook$recode == "KR",
  c("survey_id", "label", "n_valid")]
#> # A tibble: 8 × 3
#>   survey_id label                          n_valid
#>   <chr>     <chr>                            <int>
#> 1 BD1994DHS delivery by caesarian secti-na       0
#> 2 BD2000DHS delivery by caesarian section     6812
#> 3 BD2004DHS delivery by caesarian section     6901
#> 4 BD2007DHS delivery by caesarian section     6150
#> 5 BD2011DHS delivery by caesarean section     8753
#> 6 BD2014DHS delivery by caesarean section     4493
#> 7 BD2017DHS delivery by caesarean section     5299
#> 8 BD2022DHS delivery by caesarean section     5331

# Variables of the 2022 PR about anthropometry
pr <- bdhs_codebook[bdhs_codebook$survey_id == "BD2022DHS" &
  bdhs_codebook$recode == "PR", ]
head(pr[grepl("^hc7", pr$variable), c("variable", "label")])
#> # A tibble: 4 × 2
#>   variable label                                     
#>   <chr>    <chr>                                     
#> 1 hc70     height/age standard deviation (new who)   
#> 2 hc71     weight/age standard deviation (new who)   
#> 3 hc72     weight/height standard deviation (new who)
#> 4 hc73     bmi standard deviation (new who)          
```
