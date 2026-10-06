# Summarize simulation-study performance

Summarize simulation-study performance

## Usage

``` r
gt_summarize(study)
```

## Arguments

- study:

  A `gt_study` returned by
  [`gt_runstudy()`](https://itchyshin.github.io/groundtruth/reference/gt_runstudy.md)
  or
  [`gt_replay()`](https://itchyshin.github.io/groundtruth/reference/gt_replay.md).

## Value

A data frame with keys, attempt/interval counts, bias/RMSE and their
MCSE, conditional coverage and its MCSE/Wilson bounds, and
covered_per_attempt.

## Details

Summaries group by scenario, adapter, target and interval level.
Attempted = successful + failed = usable_intervals + unusable_intervals.
Bias/RMSE use accepted points; bias MCSE is sd(errors)/sqrt(k), RMSE
MCSE is sd(errors^2)/(2\*sqrt(k)\*RMSE). Conditional coverage uses
usable intervals; covered_per_attempt uses all attempts. Coverage
uncertainty has a Wilson 95% Monte Carlo interval. All-failed
bias/RMSE/conditional coverage are NA and covered_per_attempt is zero.
Singleton MCSE is NA; zero RMSE with k \> 1 has MCSE zero. These are
Monte Carlo uncertainty summaries, not a claim of estimator calibration.

## Examples

``` r
study <- gt_runstudy(gt_scenario("example"), reps = 2L)
gt_summarize(study)
#>   scenario adapter target level attempted successful failed usable_intervals
#> 1  example      lm  alpha  0.90         2          2      0                2
#> 2  example      lm  alpha  0.95         2          2      0                2
#> 3  example      lm   beta  0.90         2          2      0                2
#> 4  example      lm   beta  0.95         2          2      0                2
#>   unusable_intervals       bias  bias_mcse      rmse  rmse_mcse coverage
#> 1                  0 -0.1342151 0.03939517 0.1398773 0.03780045        1
#> 2                  0 -0.1342151 0.03939517 0.1398773 0.03780045        1
#> 3                  0  0.1441681 0.04732368 0.1517365 0.04496324        1
#> 4                  0  0.1441681 0.04732368 0.1517365 0.04496324        1
#>   coverage_mcse coverage_mc_lower coverage_mc_upper covered_per_attempt
#> 1             0         0.3423802                 1                   1
#> 2             0         0.3423802                 1                   1
#> 3             0         0.3423802                 1                   1
#> 4             0         0.3423802                 1                   1
```
