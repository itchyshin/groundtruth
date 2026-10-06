# Construct a fitting result

Construct a fitting result

## Usage

``` r
gt_result(
  estimates,
  intervals = NULL,
  converged = TRUE,
  message = "",
  diagnostics = list()
)
```

## Arguments

- estimates:

  Named numeric estimates for `alpha` and `beta`.

- intervals:

  Optional data frame with target, level, lower, and upper columns.

- converged:

  A non-missing scalar logical value.

- message:

  A scalar character message.

- diagnostics:

  A list of diagnostic values.

## Value

A `gt_result` list with estimates, intervals, convergence, message and
optional diagnostics.

## Details

Estimates must name `alpha` and `beta` exactly once. Missing, duplicate,
extra or nonfinite point targets reject the result. Intervals use
columns `target`, `level`, `lower`, `upper`, at levels 0.90 and 0.95.
Missing, duplicate, nonfinite or reversed intervals retain accepted
points and exclude the affected interval. Malformed or unsupported
interval labels make the interval schema unusable; no arbitrary name
alignment is performed. A fit exception or nonconvergence rejects both
coefficient targets.

## Examples

``` r
gt_result(c(alpha = 1, beta = 0.7))
#> $estimates
#> alpha  beta 
#>   1.0   0.7 
#> 
#> $intervals
#> NULL
#> 
#> $converged
#> [1] TRUE
#> 
#> $message
#> [1] ""
#> 
#> $diagnostics
#> list()
#> 
#> attr(,"class")
#> [1] "gt_result"
```
