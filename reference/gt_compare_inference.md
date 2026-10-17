# Compare two methods on paired study attempts

Compare two methods on paired study attempts

## Usage

``` r
gt_compare_inference(study, baseline, comparison)
```

## Arguments

- study:

  A study returned by
  [`gt_runstudy()`](https://itchyshin.github.io/groundtruth/reference/gt_runstudy.md).

- baseline, comparison:

  Distinct adapter IDs to compare explicitly.

## Value

A data frame of paired counts, coverage and finite-yield differences,
their Monte Carlo standard errors, and joint-finite width differences.

## Details

Differences are comparison minus baseline, separately for each scenario,
fixed-effect target and interval level. Every replication must occur
exactly once for each method. Conditional coverage uses usable finite
intervals. Finite covered-interval yield uses all scheduled attempts; a
zero contribution means no usable finite covered interval, not that an
unbounded confidence set excludes truth. Conditional-coverage MCSE uses
paired ratio influence values. Width differences use jointly usable
finite intervals. MCSEs describe asymptotic Monte Carlo uncertainty, not
calibration. Matching replication keys require a study that gave methods
the same data; combining unrelated studies by matching seed numbers is
unsupported.

## Examples

``` r
s <- gt_runstudy(gt_scenario('paired'),
  adapters = list(gt_adapter('a'), gt_adapter('b')), reps = 2L)
gt_compare_inference(s, 'a', 'b')
#>   scenario target level baseline comparison attempted schedule_verified
#> 1   paired  alpha  0.90        a          b         2              TRUE
#> 2   paired  alpha  0.95        a          b         2              TRUE
#> 3   paired   beta  0.90        a          b         2              TRUE
#> 4   paired   beta  0.95        a          b         2              TRUE
#>   baseline_usable comparison_usable joint_usable baseline_finite_hits
#> 1               2                 2            2                    2
#> 2               2                 2            2                    2
#> 3               2                 2            2                    2
#> 4               2                 2            2                    2
#>   comparison_finite_hits baseline_coverage comparison_coverage
#> 1                      2                 1                   1
#> 2                      2                 1                   1
#> 3                      2                 1                   1
#> 4                      2                 1                   1
#>   coverage_difference coverage_difference_mcse baseline_finite_yield
#> 1                   0                        0                     1
#> 2                   0                        0                     1
#> 3                   0                        0                     1
#> 4                   0                        0                     1
#>   comparison_finite_yield finite_yield_difference finite_yield_difference_mcse
#> 1                       1                       0                            0
#> 2                       1                       0                            0
#> 3                       1                       0                            0
#> 4                       1                       0                            0
#>   joint_width_difference joint_width_difference_mcse baseline_state_finite
#> 1                      0                           0                     0
#> 2                      0                           0                     0
#> 3                      0                           0                     0
#> 4                      0                           0                     0
#>   comparison_state_finite baseline_state_unbounded comparison_state_unbounded
#> 1                       0                        0                          0
#> 2                       0                        0                          0
#> 3                       0                        0                          0
#> 4                       0                        0                          0
#>   baseline_state_incomplete comparison_state_incomplete baseline_state_error
#> 1                         0                           0                    0
#> 2                         0                           0                    0
#> 3                         0                           0                    0
#> 4                         0                           0                    0
#>   comparison_state_error baseline_state_timeout comparison_state_timeout
#> 1                      0                      0                        0
#> 2                      0                      0                        0
#> 3                      0                      0                        0
#> 4                      0                      0                        0
#>   baseline_state_invalid comparison_state_invalid baseline_state_unknown
#> 1                      0                        0                      2
#> 2                      0                        0                      2
#> 3                      0                        0                      2
#> 4                      0                        0                      2
#>   comparison_state_unknown
#> 1                        2
#> 2                        2
#> 3                        2
#> 4                        2
```
