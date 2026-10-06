# groundtruth

`groundtruth` runs small Gaussian linear simulation studies with a
record of every fit attempt. It separates data generation from fitting,
gives each adapter the same data, and reports the denominators behind
interval coverage.

This development version supports an intercept (`alpha`) and slope
(`beta`) under

``` text
y = alpha + beta * x + sigma * error
x and error are independent standard normal draws.
```

The default adapter fits
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) and returns 90% and 95%
Student t intervals. Custom adapters receive only a fresh
`data.frame(x, y)` and return a
[`gt_result()`](https://itchyshin.github.io/groundtruth/reference/gt_result.md).

## Start a study

``` r

library(groundtruth)

scenario <- gt_scenario("example", n = 40, alpha = 1, beta = 0.7, sigma = 1.2)
study <- gt_runstudy(scenario, reps = 5, seed = "20261004")
gt_summarize(study)
```

Five repetitions show the workflow. They provide little information
about repeated-sampling performance. A larger calibration study is a
separate task.

## Supply an adapter

``` r

mean_only <- gt_adapter("mean-only", function(data) {
  gt_result(c(alpha = mean(data$y), beta = 0))
})
comparison <- gt_runstudy(scenario,
  adapters = list(gt_adapter(), mean_only), reps = 5, seed = "20261004")
```

This adapter illustrates the return contract; it is not a recommended
estimator. It supplies no intervals, so its point estimates enter error
summaries while its intervals count as unusable.

## Read the denominators

A missing or nonfinite required coefficient, an exception, or
nonconvergence rejects both targets in that attempt. An unusable
interval preserves an accepted point estimate and excludes that interval
from conditional coverage.

- `attempted = successful + failed` for each target and interval level.
- `attempted = usable_intervals + unusable_intervals`.
- `coverage` is the fraction covered among usable intervals.
- `covered_per_attempt` is the fraction covered among all attempts.

Bias and root mean squared error (RMSE) use accepted points. Their Monte
Carlo standard errors describe uncertainty from the finite number of
repetitions. Coverage has a Wilson 95% Monte Carlo interval. These Monte
Carlo intervals describe uncertainty in the measured coverage, not
uncertainty in a fitted coefficient. All-failed bias, RMSE and
conditional coverage are `NA`; covered per attempt is zero. A singleton
standard error is unknown.

## Replay and export

``` r

replayed <- gt_replay(study)
stopifnot(identical(study$data, replayed$data))

# Choose a new destination; an existing destination is refused.
export_dir <- file.path(tempdir(), "groundtruth-example-export")
if (!file.exists(export_dir)) gt_export(study, export_dir)
```

Exported CSV files contain paired data, attempts, target rows,
summaries, scenarios and the complete seed map. `manifest.R` can be read
with [`dget()`](https://rdrr.io/r/base/dput.html). It identifies
schemas, versions, adapter metadata and SHA256 file hashes. It records
the SHA256 of the sole R implementation file that produced the study,
separately from the exporter implementation. That hash identifies R
code, not the whole documentation or a Git commit. CSV numbers retain 17
significant digits; a decimal reader may change the last binary digit.
Export does not serialize custom adapter code or closure state; retain
that code yourself.

R streams use a separately versioned SHA256 key allocation and fixed
Mersenne-Twister/Inversion/Rejection settings. Reordering the same
scenarios and adapters preserves results. Replay restores recorded
settings and seeds. Calls restore the caller’s RNG kind and seed,
including absence of a seed. An ambient Box-Muller normal generator is
refused before any RNG change because its cached normal state cannot be
restored safely. Do not expect R and Julia to generate equal random
draws from equal master seed labels. Keep Julia’s 64-bit seed labels as
character strings.

## Scope and evidence

The frozen analytic example checks coefficients, standard errors and
both Student t interval levels against an independent scalar ordinary
least squares (OLS) calculation. The retained public Julia linear data
and result rows separately check coefficients and 95% endpoints. Tests
also check replay, adapter isolation, accounting, export integrity and
deliberate wrong-answer controls.

This version covers Gaussian linear data and two fixed coefficient
targets. It does NOT cover logistic models, mixed models, Julia engine
calls, calibration campaigns or CRAN release readiness. The public Julia
reference is [GroundTruth.jl commit
3738bfe](https://github.com/itchyshin/GroundTruth.jl/tree/3738bfe340fff62ec1afb0a78f0101babe79fbd7).
The R package is a separate implementation.

The local documentation preview is prepared for review. Publishing, CI
activation and installation instructions for a public release await
final approval.

The URLs in package metadata are the intended source, issue tracker and
documentation destinations. A URL field is not evidence that this
development package or its site has been published.

For source maintenance, run
`Rscript --vanilla tools/update-source-identity.R` after changing
`R/groundtruth.R` or `DESCRIPTION`, before tests and package build. This
refreshes the implementation hash and package version bundled in the
export provenance.
