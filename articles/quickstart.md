# Paired data, replay and honest denominators

A simulation comparison is easier to inspect when every adapter fits the
same generated data and every attempt remains visible. `groundtruth`
starts with Gaussian linear data and two coefficient targets. An
explicit logistic family supports Bernoulli-logit studies.

## Generate once, fit separately

``` r

scenario <- gt_scenario("small", n = 30, alpha = 1, beta = 2, sigma = 1)
study <- gt_runstudy(scenario, reps = 4, seed = "20261004")
head(study$data)
```

    ##   scenario rep row          x          y
    ## 1    small   1   1 -0.3928173 1.00540817
    ## 2    small   1   2 -0.5378652 0.06901535
    ## 3    small   1   3  0.8711923 3.73959457
    ## 4    small   1   4  0.4902612 2.67945766
    ## 5    small   1   5 -0.3820461 2.37741040
    ## 6    small   1   6 -0.2393622 0.04254794

``` r

study$ledger
```

    ##   scenario rep adapter  data_seed   fit_seed status message
    ## 1    small   1      lm 1793173013 2086680209     ok        
    ## 2    small   2      lm 1098587371  446531788     ok        
    ## 3    small   3      lm    8490282 1951231743     ok        
    ## 4    small   4      lm  431958131  670723386     ok

The Gaussian generator draws `x` and error independently. It constructs
`y` from the stated truth without calling an estimator. Each adapter
receives a fresh data frame containing only `x` and `y`. The runner
stores truth separately for scoring.

## Fit a logistic scenario

For Bernoulli outcomes, the model is

``` math
Y_i \sim \operatorname{Bernoulli}(p_i), \qquad
\operatorname{logit}(p_i)=\alpha+\beta x_i.
```

The coefficients are conditional log odds. Select the family and native
R adapter explicitly:

``` r

logistic <- gt_scenario("logistic-small", n = 80, alpha = -0.3, beta = 0.6,
                        family = "logistic")
logistic_study <- gt_runstudy(logistic, adapters = gt_glm_adapter(),
                              reps = 4, seed = "20261006")
head(logistic_study$ledger)
```

    ##         scenario rep adapter data_seed   fit_seed status message
    ## 1 logistic-small   1     glm 443429559  684706644     ok        
    ## 2 logistic-small   2     glm 810666579 1863808595     ok        
    ## 3 logistic-small   3     glm   6869322 1148310148     ok        
    ## 4 logistic-small   4     glm 271901768 1238444602     ok

The native adapter fits
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) with a binomial logit
link. It requires native GLM convergence, a maximum absolute
standardized score no greater than `1e-9`, and a maximum absolute
standardized Newton correction to the linear predictor no greater than
`1e-9`. Complete or quasi separation means the declared model has no
unique finite MLE. A data set with a finite MLE can still fail numerical
convergence, so the two conditions are reported separately. Gaussian
fits retain Student t intervals; logistic fits use normal Wald intervals
at 90% and 95%.

A custom logistic fitter must opt in with
`gt_adapter(..., family = "logistic")`. Family compatibility is checked
before calling the fitter. A mismatch is recorded as a failed attempt,
and the fitter is not invoked. The native adapter is requested with
[`gt_glm_adapter()`](https://itchyshin.github.io/groundtruth/reference/gt_glm_adapter.md).

## Keep points when intervals are absent

A custom point-only adapter can illustrate interval accounting:

``` r

point_only <- gt_adapter("point-only", function(data) {
  fitted <- stats::lm(y ~ x, data = data, na.action = stats::na.fail,
                      singular.ok = FALSE)
  coefficient <- stats::coef(fitted)
  gt_result(c(alpha = unname(coefficient["(Intercept)"]),
              beta = unname(coefficient["x"])))
})
paired <- gt_runstudy(scenario,
  adapters = list(gt_adapter(), point_only), reps = 4, seed = "20261004")
summary <- gt_summarize(paired)
summary[, c("adapter", "target", "level", "attempted", "successful",
            "usable_intervals", "coverage", "covered_per_attempt")]
```

    ##      adapter target level attempted successful usable_intervals coverage
    ## 1         lm  alpha  0.90         4          4                4        1
    ## 2         lm  alpha  0.95         4          4                4        1
    ## 3         lm   beta  0.90         4          4                4        1
    ## 4         lm   beta  0.95         4          4                4        1
    ## 5 point-only  alpha  0.90         4          4                0       NA
    ## 6 point-only  alpha  0.95         4          4                0       NA
    ## 7 point-only   beta  0.90         4          4                0       NA
    ## 8 point-only   beta  0.95         4          4                0       NA
    ##   covered_per_attempt
    ## 1                   1
    ## 2                   1
    ## 3                   1
    ## 4                   1
    ## 5                   0
    ## 6                   0
    ## 7                   0
    ## 8                   0

The point-only adapter still contributes to bias and RMSE. Its intervals
are unusable, so conditional coverage is `NA` and covered per attempt is
zero. A failed fit rejects both coefficient targets. Failure counts
remain in the attempt denominator.

An interval result uses these exact column names:

``` r

gt_result(c(alpha = 1, beta = 2),
          intervals = data.frame(target = c("alpha", "beta"),
                                 level = c(0.95, 0.95),
                                 lower = c(0.5, 1.5),
                                 upper = c(1.5, 2.5)))
```

    ## $estimates
    ## alpha  beta 
    ##     1     2 
    ## 
    ## $intervals
    ##   target level lower upper
    ## 1  alpha  0.95   0.5   1.5
    ## 2   beta  0.95   1.5   2.5
    ## 
    ## $converged
    ## [1] TRUE
    ## 
    ## $message
    ## [1] ""
    ## 
    ## $diagnostics
    ## list()
    ## 
    ## attr(,"class")
    ## [1] "gt_result"

Missing intervals at the other level remain unusable. A duplicate
target-level row does not select one row arbitrarily. Nonfinite or
reversed bounds also exclude that interval while retaining valid points.

## Distinguish two kinds of interval

The default Gaussian fitted-coefficient intervals use Student t
quantiles with `n - 2` degrees of freedom and residual variance
`SSE / (n - 2)`. Logistic intervals use normal Wald quantiles and final
observed information. The coverage summary uses a Wilson 95% Monte Carlo
interval for the measured coverage fraction. The first interval
describes coefficient uncertainty in one data set; the second describes
uncertainty from a finite number of repetitions.

With four repetitions, the Monte Carlo uncertainty is large. This
article demonstrates accounting and replay, not a calibration result.

## Retain replay information

``` r

replayed <- gt_replay(paired)
stopifnot(identical(paired$data, replayed$data))
stopifnot(identical(paired$targets, replayed$targets))
head(paired$seed_map)
```

    ##                                        key scenario rep         stream
    ## 1 8:20261004|5:small|1:1|14:fit:point-only    small   1 fit:point-only
    ## 2            8:20261004|5:small|1:1|4:data    small   1           data
    ## 3          8:20261004|5:small|1:1|6:fit:lm    small   1         fit:lm
    ## 4 8:20261004|5:small|1:2|14:fit:point-only    small   2 fit:point-only
    ## 5            8:20261004|5:small|1:2|4:data    small   2           data
    ## 6          8:20261004|5:small|1:2|6:fit:lm    small   2         fit:lm
    ##                                                               hash salt
    ## 1 aa74a1bf33a42fb105ad1d18c6902cc16ca160e23969d15e60cef298c0cef7bb    0
    ## 2 6ae1a6145acc5a923096d6c80e200c89be017703bda06909962d349951fadf9e    0
    ## 3 7c603690ee979f39ff561ee3f854a11c18f5a9f6bf58af2c48c31fb187f22356    0
    ## 4 8a1f59c9b8103f20393545507d7a8d3f085a36645e65856667501ce38cc86384    0
    ## 5 417b1cea412aa14bdb29acb58ce60591c49298529c2cf7beb58bc49f7510ddad    0
    ## 6 1a9d88cb8a6fd7c4e15eb7a3763fe2b570c8deaf7dfc9304b1fe146c9489b664    0
    ##         seed
    ## 1  712286658
    ## 2 1793173013
    ## 3 2086680209
    ## 4  169826764
    ## 5 1098587371
    ## 6  446531788

Replay uses the recorded RNG settings and full allocated seed map.
Stateless deterministic adapters replay exactly in the same environment.
For adapters that read external files or mutate captured state, preserve
those inputs and reset that state yourself.

``` r

path <- tempfile("groundtruth-export-")
gt_export(paired, path)
manifest <- dget(file.path(path, "manifest.R"))
list.files(path)
```

    ## [1] "attempts.csv"  "data.csv"      "manifest.R"    "scenarios.csv"
    ## [5] "seed-map.csv"  "summary.csv"   "targets.csv"

A manifest is an audit record.
[`dget()`](https://rdrr.io/r/base/dput.html) should be used only on a
trusted manifest, because it evaluates R expressions. The exported
manifest contains data rather than adapter code. It records a SHA256 for
the sole R implementation file that produced the study and a separate
exporter hash. This source identity does not include custom adapter code
or the whole documentation. Read CSV columns according to their schemas,
preserving seed strings as character.

R and Julia use distinct streams, so compare them on identical frozen
CSV inputs. The fixed-effect fixture results do not establish
calibration or package-wide parity. R 4.6.0 checked version 0.0.0.9000:
346 assertions passed with `Status: OK`; the pkgdown 2.2.0 preview
rendered. These checks support the declared fixed-effect workflow.
