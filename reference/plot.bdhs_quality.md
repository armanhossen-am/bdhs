# Plot data-quality profiles

Draws the single-year age distribution of the household population (ages
ending in 0 or 5 highlighted) or the number of births by calendar year
(with the first year of the five-year window marked) from
[`bdhs_quality()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_quality.md)
results.

## Usage

``` r
# S3 method for class 'bdhs_quality'
plot(x, type = c("age", "births"), ...)

# S3 method for class 'bdhs_quality'
autoplot(object, type = c("age", "births"), ...)
```

## Arguments

- x, object:

  A `bdhs_quality` result.

- type:

  `"age"` (default) or `"births"`.

- ...:

  Unused.

## Value

A ggplot object.

## Examples

``` r
q <- bdhs_quality(bdhs_simulate(2022, "PR"), checks = "age_heaping")
plot(q)
```
