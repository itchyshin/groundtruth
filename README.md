# groundtruth

`groundtruth` runs small simulation studies with a record of every fit attempt. It
separates data generation from fitting, gives adapters paired observations, and reports
the denominators behind interval coverage. Established workflows support Gaussian linear
regression and fixed-effect logistic scenarios through a native R adapter. An optional
Gaussian random-intercept ML workflow also reports coefficient and variance targets;
its [finite-design reliability map](https://itchyshin.github.io/groundtruth/articles/random-intercept-reliability.html)
retains failures, few-group undercoverage and numerical discrepancies.

## Gaussian model

The default scenario has an intercept (`alpha`) and slope (`beta`) under

```text
y = alpha + beta * x + sigma * error
x and error are independent standard normal draws.
```

The default adapter fits `stats::lm()` and returns 90% and 95% Student t intervals.
Custom adapters receive only a fresh `data.frame(x, y)` and return a `gt_result()`.

```r
library(groundtruth)

scenario <- gt_scenario("example", n = 40, alpha = 1, beta = 0.7, sigma = 1.2)
study <- gt_runstudy(scenario, reps = 5, seed = "20261004")
gt_summarize(study)
```

Five repetitions show the workflow. They provide little information about
repeated-sampling performance. A larger calibration study is separate work.

## Logistic model

For an explicitly selected logistic scenario,

```text
y ~ Bernoulli(p)
logit(p) = alpha + beta * x
```

`alpha` and `beta` are conditional log-odds coefficients. The native adapter uses
`stats::glm()` with a binomial family and logit link. Logistic intervals are normal Wald
intervals from final observed information at 90% and 95%. Gaussian intervals remain
Student t intervals.

```r
logistic <- gt_scenario("binary", n = 200, alpha = -0.4, beta = 0.8,
                        family = "logistic")
logit_study <- gt_runstudy(logistic, adapters = gt_glm_adapter(),
                           reps = 5, seed = "20261006")
gt_summarize(logit_study)
```

A logistic `gt_adapter()` needs a supplied fitter and `family = "logistic"`;
`gt_adapter(fit = NULL, family = "logistic")` directs users to
`gt_glm_adapter()`. The runner checks family compatibility before calling a fitter.
A mismatch is recorded as a failed attempt and the fitter is not invoked. Exact
class-support checks distinguish complete separation, quasi separation and existence
of a finite MLE. Numerical convergence is checked separately: native GLM convergence
must pass, the maximum absolute standardized score must be at most `1e-9`, and the
maximum absolute standardized Newton correction to the linear predictor must be at most
`1e-9`. A failed point fit
rejects both coefficient targets; an interval-only failure preserves accepted point
estimates and omits only affected intervals.

## Supply an adapter

```r
mean_only <- gt_adapter("mean-only", function(data) {
  gt_result(c(alpha = mean(data$y), beta = 0))
})
comparison <- gt_runstudy(scenario,
  adapters = list(gt_adapter(), mean_only), reps = 5, seed = "20261004")
```

This adapter illustrates the return contract; it is not a recommended estimator. It
supplies no intervals, so its point estimates enter error summaries while its intervals
count as unusable.

## Read the denominators

A missing or nonfinite required coefficient, an exception, or nonconvergence rejects
both targets in that attempt. An unusable interval preserves an accepted point estimate
and excludes that interval from conditional coverage.

- `attempted = successful + failed` for each target and interval level.
- `attempted = usable_intervals + unusable_intervals`.
- `coverage` is the fraction covered among usable intervals.
- `covered_per_attempt` is the fraction covered among all attempts.

Bias and root mean squared error (RMSE) use accepted points. Their Monte Carlo standard
errors describe uncertainty from the finite number of repetitions. Coverage has a
Wilson 95% Monte Carlo interval. These Monte Carlo intervals describe uncertainty in
the measured coverage, not uncertainty in a fitted coefficient. All-failed bias, RMSE
and conditional coverage are `NA`; covered per attempt is zero. A singleton standard
error is unknown.

## Replay and export

```r
replayed <- gt_replay(study)
stopifnot(identical(study$data, replayed$data))

# Choose a new destination; an existing destination is refused.
export_dir <- file.path(tempdir(), "groundtruth-example-export")
if (!file.exists(export_dir)) gt_export(study, export_dir)
```

Exported CSV files contain paired data, attempts, target rows, summaries, scenarios
and the complete seed map. `manifest.R` can be read with `dget()`. It identifies
schemas, versions, adapter metadata and SHA256 file hashes. It records the SHA256 of
the sole R implementation file that produced the study, separately from the exporter
implementation. That hash identifies R code, not the whole documentation or a Git
commit. New exports use `groundtruth-r-export-v2` and include scenario and adapter
families; logistic scenarios store `NA` for numeric `sigma`. Earlier artifacts are
unchanged, and no importer is provided. CSV numbers retain 17 significant digits; a
decimal reader may change the last binary digit. Export does not serialize custom adapter code or closure state; retain
that code yourself.

R streams use a separately versioned SHA256 key allocation and fixed
Mersenne-Twister/Inversion/Rejection settings. Reordering the same scenarios and
adapters preserves results. Replay restores recorded settings and seeds. Calls restore
the caller's RNG kind and seed, including absence of a seed. An ambient Box-Muller
normal generator is refused before any RNG change because its cached normal state
cannot be restored safely. Do not expect R and Julia to generate equal random draws
from equal master seed labels. Keep Julia's 64-bit seed labels as character strings.

## Scope and evidence

The frozen Gaussian analytic example checks coefficients, standard errors and both
Student t interval levels against an independent scalar ordinary least squares (OLS)
calculation. The retained public Julia linear data and result rows separately check
coefficients and 95% endpoints. Tests also check replay, adapter isolation, accounting,
export integrity and deliberate wrong-answer controls. The paired logistic fixture gate checked 14 accepted fits across seven fixtures and
five rejected-design fixtures. Maximum paired absolute differences were 6.87e-11 for
coefficients, 7.12e-12 for standard errors, 8.28e-11 for endpoints and 9.99e-16 for
mean negative log likelihood. The comparisons cover one-predictor Bernoulli models
with unit weights and reused references, not new reference fits. They do not establish
calibration, fitted GLMM behavior or package-wide parity. In the earlier fixed-effect
package, R 4.6.0 reported 346 assertions passed with `Status: OK` and no NOTE, and the
pkgdown 2.2.0 preview rendered successfully. Those retained checks do not validate the
Gaussian random-intercept ML workflow; see its separate reliability map below.

The Julia public reference is [GroundTruth.jl commit
3738bfe](https://github.com/itchyshin/GroundTruth.jl/tree/3738bfe340fff62ec1afb0a78f0101babe79fbd7),
and its established site is [GroundTruth.jl documentation](https://itchyshin.github.io/GroundTruth.jl/).
The R package is a separate implementation. R and Julia use distinct random streams;
bounded numerical comparisons use the same frozen CSV bytes, not matched seed labels.
Agreement on those fixed-effect fixtures is not calibration evidence or package-wide
parity.

The [R documentation](https://itchyshin.github.io/groundtruth/) includes Gaussian and
logistic workflows. Source and issue links are recorded in the package metadata.

For source maintenance, run `Rscript --vanilla tools/update-source-identity.R` after
changing `R/groundtruth.R` or `DESCRIPTION`, before tests and package build. This
refreshes the implementation hash and package version bundled in export provenance.
The development package version remains 0.0.0.9000.

## Gaussian random-intercept model

The optional adapter supports Gaussian random-intercept maximum likelihood for
`y ~ 1 + x + (1 | group)`. In addition to `alpha` and `beta`, it scores residual
variance `sigma2` and group-intercept variance `tau2`. The default
`x_level = "observation"` draws an independent standard-normal predictor for each
observation. Set `x_level = "group"` to draw one standard-normal predictor per group
and repeat it for that group's observations in canonical group order (including when
group sizes differ). In either mode, predictor draws are independent of group effects
and residual errors.

```r
library(groundtruth)

scenario <- gt_ri_scenario("grouped-example", n_groups = 20L,
                           group_sizes = 6L, alpha = 1, beta = 0.7,
                           sigma = 1, tau = 0.5,
                           x_level = "observation")
group_scenario <- gt_ri_scenario("group-x-example", n_groups = 20L,
                                 group_sizes = 6L, alpha = 1, beta = 0.7,
                                 sigma = 1, tau = 0.5,
                                 x_level = "group")
study <- gt_runstudy(scenario, adapters = gt_lmm_adapter(),
                     reps = 2L, seed = "20261006")
gt_summarize(study)
```

The optional `lme4` dependency is required for `gt_lmm_adapter()` and must be installed
separately. R custom fitters receive a data frame with exactly `x`, `y`, and `group`.
The adapter uses ML (`REML = FALSE`). Coefficient intervals are approximate
normal plug-in intervals at 90% and 95%; variance targets are point-only, so their
interval coverage is unavailable. A converged boundary estimate `tau2 = 0` is valid.
Failed fits remain in the attempt ledger. Two repetitions illustrate the workflow and
are not calibration evidence. The conditional residual ICC given `x` is
`tau^2 / (tau^2 + sigma^2)`. With group-level `x`, shared predictor variation also
contributes to unconditional same-group response correlation, so a conditional ICC of
0.2 does not mean unconditional response correlation is 0.2.

The generator revision is `random-intercept-v2`. Study provenance retains the predictor
level and revision, and replay uses the recorded R seeds and RNG settings. R and Julia
keep distinct random-number algorithms; matching seed labels do not imply identical
draws. Random-intercept exports use schema v4, whose `scenarios.csv` records `x_level`;
existing v3 artifacts remain supported as artifacts. Legacy R scenarios or studies that
lack `x_level` default to `"observation"` when normalized for replay. The
[finite-design reliability map](https://itchyshin.github.io/groundtruth/articles/random-intercept-reliability.html)
reports 30,000 shared datasets and 60,000 attempts. It preserves failed fits,
numerical tolerance exceedances and poor interval coverage with few groups.
These findings do not establish package-wide parity or universal calibration.

The R diagnostic `exact_zero` uses `tau2 <= 1e-10`. The reliability map reports literal
zero and near-boundary counts separately. Its frozen campaign predates the small-response
unit correction to the R residual-degeneracy guard; the report page records both source
hashes and the distinction from a new campaign.
