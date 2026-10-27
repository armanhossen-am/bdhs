# Plot survey estimates

Point-and-interval plot of the estimates returned by
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md):
one panel per indicator, one row per group (when `by` was used), and one
colour per survey round when several rounds are present. Intervals are
the confidence intervals of the estimates (at the level used by
[`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)).

## Usage

``` r
# S3 method for class 'bdhs_estimate'
plot(x, ...)

# S3 method for class 'bdhs_estimate'
autoplot(object, ...)
```

## Arguments

- x, object:

  A `bdhs_estimate` tibble from
  [`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md).

- ...:

  Unused.

## Value

A ggplot object.

## Examples

``` r
est <- tibble::tibble(
  survey_id = "BD2022DHS", indicator = "anc4", label = "ANC 4+ visits",
  v025 = c("urban", "rural"), estimate = c(54.1, 37.9), se = c(2.1, 1.4),
  ci_low = c(50, 35.2), ci_high = c(58.2, 40.6), n_unweighted = c(1200L, 3400L),
  n_weighted = c(1250, 3350), api_id = "RH_ANCN_W_N4P"
)
class(est) <- c("bdhs_estimate", class(est))
plot(est)
```
