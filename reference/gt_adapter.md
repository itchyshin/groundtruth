# Define a fitting adapter

Define a fitting adapter

## Usage

``` r
gt_adapter(id = "lm", fit = NULL, metadata = list())
```

## Arguments

- id:

  A non-empty scalar character identifier.

- fit:

  A function accepting one fresh data frame with exactly `x` and `y`, or
  `NULL` for the default Gaussian linear-model adapter. Truth is
  withheld.

- metadata:

  A list of descriptive metadata.

## Value

A `gt_adapter` object with `id`, `fit` and descriptive `metadata`.

## Details

The default uses
`stats::lm(y ~ x, na.action = stats::na.fail, singular.ok = FALSE)` on
finite, equal-length, rank-two data with more than two observations.
`(Intercept)` maps to `alpha` and `x` to `beta`. Residual variance is
SSE / (n - 2). It returns two-sided 90% and 95% Student t intervals with
n - 2 degrees of freedom. Diagnostics include residual variance,
coefficient covariance, standard errors, fitted values and residuals.
Custom fitting functions use their separately allocated global R RNG
stream.

## Examples

``` r
gt_adapter()
#> $id
#> [1] "lm"
#> 
#> $fit
#> function (data) 
#> {
#>     if (!is.data.frame(data) || !identical(names(data), c("x", 
#>         "y")) || nrow(data) <= 2L || any(!is.finite(data$x)) || 
#>         any(!is.finite(data$y))) {
#>         stop("Default fitter requires finite x and y.", call. = FALSE)
#>     }
#>     fit <- stats::lm(y ~ x, data = data, na.action = stats::na.fail, 
#>         singular.ok = FALSE)
#>     co <- stats::coef(fit)
#>     if (!identical(names(co), c("(Intercept)", "x")) || any(!is.finite(co))) {
#>         stop("Rank-two fit with intercept and x required.", call. = FALSE)
#>     }
#>     co <- co[c("(Intercept)", "x")]
#>     names(co) <- c("alpha", "beta")
#>     df <- fit$df.residual
#>     sigma2 <- sum(stats::residuals(fit)^2)/df
#>     covariance <- stats::vcov(fit)
#>     dimnames(covariance) <- list(c("alpha", "beta"), c("alpha", 
#>         "beta"))
#>     se <- sqrt(diag(covariance))
#>     names(se) <- c("alpha", "beta")
#>     intervals <- do.call(rbind, lapply(c(0.9, 0.95), function(level) {
#>         q <- stats::qt((1 + level)/2, df)
#>         data.frame(target = c("alpha", "beta"), level = level, 
#>             lower = co - q * se, upper = co + q * se, stringsAsFactors = FALSE)
#>     }))
#>     gt_result(co, intervals, diagnostics = list(sigma2 = sigma2, 
#>         covariance = covariance, se = se, predictions = stats::fitted(fit), 
#>         residuals = stats::residuals(fit)))
#> }
#> <bytecode: 0x55e94b694990>
#> <environment: namespace:groundtruth>
#> 
#> $metadata
#> list()
#> 
#> attr(,"class")
#> [1] "gt_adapter"
```
