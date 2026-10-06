# Define a simulation scenario

Define a simulation scenario

## Usage

``` r
gt_scenario(
  id,
  n = 100L,
  alpha = 1,
  beta = 0.7,
  sigma = if (identical(family, "logistic")) NULL else 1.2,
  family = "gaussian"
)
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

  Positive residual standard deviation for Gaussian scenarios; omitted
  logistic scenarios use no sigma.

- family:

  Either `"gaussian"` or `"logistic"`.

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
#> $family
#> [1] "gaussian"
#> 
#> attr(,"class")
#> [1] "gt_scenario"
gt_scenario("binary", family = "logistic")
#> $id
#> [1] "binary"
#> 
#> $n
#> [1] 100
#> 
#> $alpha
#> [1] 1
#> 
#> $beta
#> [1] 0.7
#> 
#> $sigma
#> NULL
#> 
#> $family
#> [1] "logistic"
#> 
#> attr(,"class")
#> [1] "gt_scenario"
```
