# Define a Gaussian simulation scenario

Define a Gaussian simulation scenario

## Usage

``` r
gt_scenario(id, n = 100L, alpha = 1, beta = 0.7, sigma = 1.2)
```

## Arguments

- id:

  A non-empty scalar character identifier.

- n:

  Number of observations, an integer greater than two.

- alpha:

  Intercept used to generate data.

- beta:

  Slope used to generate data.

- sigma:

  Positive residual standard deviation.

## Value

A `gt_scenario` object.

## Examples

``` r
gt_scenario("small", n = 20L)
#> $id
#> [1] "small"
#> 
#> $n
#> [1] 20
#> 
#> $alpha
#> [1] 1
#> 
#> $beta
#> [1] 0.7
#> 
#> $sigma
#> [1] 1.2
#> 
#> attr(,"class")
#> [1] "gt_scenario"
```
