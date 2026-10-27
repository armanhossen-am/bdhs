# Merge two BDHS recode files on the right DHS keys

Detects which recodes `x` and `y` are (see
[`bdhs_recodes()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_recodes.md))
and joins them on the identifiers the DHS Program documents for that
pair, so you do not have to remember which line-number variable links a
child to its household roster entry. `x` is the base file: its rows
(unit of analysis) and attributes are kept, and the columns of `y` are
added.

## Usage

``` r
bdhs_merge(
  x,
  y,
  join = c("left", "inner"),
  by = NULL,
  recode_x = NULL,
  recode_y = NULL,
  suffix = NULL
)
```

## Arguments

- x, y:

  Data frames of BDHS recode data (as returned by
  [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)).
  `x` is the base file.

- join:

  `"left"` (default) keeps every row of `x`; `"inner"` keeps only rows
  of `x` with a match in `y`.

- by:

  Optional explicit key: a named character vector like
  `c(v001 = "hv001", v002 = "hv002")` (names in `x`, values in `y`),
  overriding detection.

- recode_x, recode_y:

  Optional recode codes overriding detection.

- suffix:

  Suffix for duplicated column names from `y`; defaults to `"_"`
  followed by the lower-case recode of `y` (e.g. `"_hr"`).

## Value

`x` with the columns of `y` added (one row per row of `x` when `y` has
one row per key), with the attributes of `x`.

## Details

|  |  |
|----|----|
| Files | Key (`x` side = `y` side) |
| IR, KR, BR with HR | `v001`, `v002` = `hv001`, `hv002` |
| IR with PR | `v001`, `v002`, `v003` = `hv001`, `hv002`, `hvidx` |
| KR, BR with IR | `caseid` |
| KR with BR | `caseid`, `bidx` |
| KR, BR with PR | `v001`, `v002`, `b16` = `hv001`, `hv002`, `hvidx` (child's line number; needs `b16`) |
| PR with HR | `hv001`, `hv002` (or `hhid`) |
| MR with HR / PR | `mv001`, `mv002` (, `mv003` = `hvidx`) |
| HW with KR / PR | `hwhhid`, `hwline` = household part of `caseid`, `b16` / `hhid`, `hvidx` |
| WI with HR, PR / IR, KR, BR | `whhid` = `hhid` / household part of `caseid` |

Both files must come from the same survey round (pooled data are joined
within rounds on `survey_id`). Columns of `y` that repeat an identifier
or design column of `x`, or that hold exactly the same values as the
same-named column of `x`, are dropped; other duplicated names get the
suffix `suffix`.

The keys follow the DHS Guide to DHS Statistics ("Merging files"). The
WI and HW keys for 1993-94 to 2004 follow DHS conventions and could not
be verified against the files; check the match rate that is reported.

## Examples

``` r
kr <- bdhs_simulate(2022, "KR")
hr <- bdhs_simulate(2022, "HR")
kh <- bdhs_merge(kr, hr)
#> ✔ Merged HR into KR on v001, v002 = hv001, hv002: 296 of 296 rows matched
#>   (100%).
table(kh$hv201)
#> 
#>  11  12  21  31  43  71 
#>  14  13 234  13   8  14 

# Mothers' characteristics on the births file
br <- bdhs_simulate(2022, "BR")
ir <- bdhs_simulate(2022, "IR")
nrow(bdhs_merge(br, ir)) == nrow(br)
#> ✔ Merged IR into BR on caseid: 1983 of 1983 rows matched (100%).
#> [1] TRUE
```
