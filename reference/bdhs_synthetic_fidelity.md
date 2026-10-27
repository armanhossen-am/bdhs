# Fidelity of calibrated synthetic data to the official values

Re-estimates every calibrated indicator of data made by
[`bdhs_synthesize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_synthesize.md)
with
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
(nationally and by each calibrated margin) and compares it with the
official DHS value it was calibrated to.

## Usage

``` r
bdhs_synthetic_fidelity(data)
```

## Arguments

- data:

  Data returned by
  [`bdhs_synthesize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_synthesize.md)
  (possibly modified; the comparison is recomputed from the data).

## Value

A tibble with columns `survey_id`, `indicator`, `api_id`, `margin`
(`"national"`, `"division"`, `"residence"`, `"wealth"`), `category`,
`official`, `synthetic`, `diff` (synthetic minus official, percentage
points) and `n_unweighted`.

## See also

[`bdhs_synthesize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_synthesize.md)

## Examples

``` r
# \donttest{
ir <- bdhs_synthesize(2022, "IR", scale = 0.2)
fid <- bdhs_synthetic_fidelity(ir)
max(abs(fid$diff))
#> [1] 0.03811859
# }
```
