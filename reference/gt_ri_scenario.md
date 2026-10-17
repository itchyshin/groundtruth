# Define a Gaussian random-intercept scenario

Define a Gaussian random-intercept scenario

## Usage

``` r
gt_ri_scenario(
  id,
  n_groups = 20L,
  group_sizes = 6L,
  alpha = 1,
  beta = 0.7,
  sigma = 1,
  tau = 0.5,
  x_level = "observation"
)
```

## Arguments

- id:

  A non-empty scalar character identifier.

- n_groups:

  Number of groups, at least two.

- group_sizes:

  A positive integer scalar repeated across groups, or one positive
  integer size for each group. At least one group must be replicated.

- alpha:

  Population intercept.

- beta:

  Population slope for `x`.

- sigma:

  Positive residual standard deviation.

- tau:

  Non-negative random-intercept standard deviation.

- x_level:

  Either `"observation"` (one independent standard-normal predictor per
  observation; the default) or `"group"` (one independent
  standard-normal predictor per group, expanded to that group's
  observations in canonical group order).

## Value

A `gt_scenario` for the Gaussian random-intercept model.

## Details

Predictor draws are independent of random intercepts and residual
errors. The conditional residual ICC given x is tau^2 / (tau^2 +
sigma^2). A shared group-level predictor also contributes to
unconditional same-group response correlation. The scenario records
`x_level` and uses generator revision `random-intercept-v2`.

## Examples

``` r
gt_ri_scenario("clusters", n_groups = 5L, group_sizes = 4L)
#> $id
#> [1] "clusters"
#> 
#> $n
#> [1] 20
#> 
#> $n_groups
#> [1] 5
#> 
#> $group_sizes
#> [1] 4 4 4 4 4
#> 
#> $alpha
#> [1] 1
#> 
#> $beta
#> [1] 0.7
#> 
#> $sigma
#> [1] 1
#> 
#> $tau
#> [1] 0.5
#> 
#> $x_level
#> [1] "observation"
#> 
#> $family
#> [1] "random_intercept"
#> 
#> attr(,"class")
#> [1] "gt_scenario"
```
