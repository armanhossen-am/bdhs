# Set DHS special codes to `NA`

DHS recode files store "missing", "don't know", "not a de jure
resident", "flagged" and similar answers as ordinary codes (often 8/9,
98/99, 997/998 or 9996-9999). Left in place they silently distort means
and regressions. `bdhs_na()` turns them into `NA`:

## Usage

``` r
bdhs_na(x, cols = NULL, dont_know = c("na", "keep"))
```

## Arguments

- x:

  A vector (labelled, numeric or factor) or a data frame.

- cols:

  For data frames, the columns to clean (tidyselect, e.g.
  `c(v133, hw70)` or `starts_with("hw")`). `NULL` (default) cleans every
  column.

- dont_know:

  `"na"` (default) turns "don't know" answers into `NA`; `"keep"` keeps
  them, for indicators where DHS counts "don't know" as a valid answer
  (usually as "no").

## Value

`x` with special codes replaced by `NA`. Labelled vectors stay labelled;
the labels of the removed codes are dropped.

## Details

- labelled values whose labels say *missing*, *don't know* / *dk*,
  *not (a) de jure resident*, *flagged*, *implausible* / *out of
  plausible limits*, *inconsistent* or *not applicable*;

- anthropometry codes 9994-9999 in child and adult height, weight,
  z-score and BMI variables (`hw*`, `hc*`, `ha*`, `hb*`, `v437`-`v449`),
  which are not always labelled;

- Stata extended missing values
  ([`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html),
  e.g. `.a` in 2022 files) and SPSS user-defined missing values, which
  become ordinary `NA`.

Codes that are substantive answers keep their value, including
categories such as "flush to don't know where" (toilet type) or "other".

## Examples

``` r
x <- haven::labelled(
  c(1, 2, 8, 9, 1),
  c(yes = 1, no = 2, "don't know" = 8, missing = 9)
)
bdhs_na(x)
#> <labelled<double>[5]>
#> [1]  1  2 NA NA  1
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no
bdhs_na(x, dont_know = "keep")
#> <labelled<double>[5]>
#> [1]  1  2  8 NA  1
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      2         no
#>      8 don't know

kr <- bdhs_simulate(2022, "KR")
summary(kr$hw70)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>  -435.0  -201.5  -125.0  -126.9   -48.5   262.0      25 
summary(bdhs_na(kr, cols = hw70)$hw70)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>  -435.0  -201.5  -125.0  -126.9   -48.5   262.0      25 
```
