# Create the native Gaussian random-intercept adapter

Create the native Gaussian random-intercept adapter

## Usage

``` r
gt_lmm_adapter(
  id = "lmer",
  metadata = list(),
  criterion = "ML",
  interval_method = "normal",
  profile_timeout = 60
)
```

## Arguments

- id:

  Adapter identifier.

- metadata:

  Descriptive metadata.

- criterion:

  Either `"ML"` or `"REML"` for the variance fit.

- interval_method:

  Either `"normal"` or `"profile"` for fixed effects. Profile intervals
  are available only with ML.

- profile_timeout:

  Positive finite wall deadline, in seconds, for the isolated native
  profile helper, including helper startup.

## Value

A `random_intercept` `gt_adapter` using
[`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html).

## Details

Fixed-effect normal intervals use the independently reconstructed
covariance at the selected ML or REML variance estimates. Profile
intervals use native ML likelihood profiles. A profile failure retains
accepted estimates and records typed interval states in
[`gt_inference_details()`](https://itchyshin.github.io/groundtruth/reference/gt_inference_details.md).
Adapter metadata records the chosen method and lme4 version; names
beginning `gt_inference_` are reserved.

## Examples

``` r
if (FALSE) gt_lmm_adapter() # \dontrun{}
```
