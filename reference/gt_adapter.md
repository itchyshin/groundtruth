# Define a fitting adapter

Define a fitting adapter

## Usage

``` r
gt_adapter(id = "lm", fit = NULL, metadata = list(), family = "gaussian")
```

## Arguments

- id:

  A non-empty scalar character identifier.

- fit:

  A function accepting one fresh data frame with exactly `x` and `y`, or
  `NULL` for the default fitter of the declared family.

- metadata:

  A list of descriptive metadata.

- family:

  Either `"gaussian"` or `"logistic"`.

## Value

A `gt_adapter` object with its declared family.

## Details

The default Gaussian fitter uses
[`stats::lm`](https://rdrr.io/r/stats/lm.html) with an intercept and
slope, estimates residual variance as SSE / (n - 2), and returns 90% and
95% Student t intervals. Logistic fitters must be supplied explicitly or
created with
[`gt_glm_adapter()`](https://itchyshin.github.io/groundtruth/reference/gt_glm_adapter.md).
Truth is withheld from every fitter.

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
#> <bytecode: 0x55d62dc77b60>
#> <environment: namespace:groundtruth>
#> 
#> $metadata
#> list()
#> 
#> $family
#> [1] "gaussian"
#> 
#> attr(,"class")
#> [1] "gt_adapter"
```
