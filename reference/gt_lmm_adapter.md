# Create the native Gaussian random-intercept adapter

Create the native Gaussian random-intercept adapter

## Usage

``` r
gt_lmm_adapter(id = "lmer", metadata = list())
```

## Arguments

- id:

  Adapter identifier.

- metadata:

  Descriptive metadata.

## Value

A `random_intercept` `gt_adapter` using
[`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) with ML.

## Examples

``` r
if (FALSE) gt_lmm_adapter() # \dontrun{}
```
