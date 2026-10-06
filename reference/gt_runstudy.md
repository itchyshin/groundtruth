# Run a reproducible Gaussian or logistic simulation study

Run a reproducible Gaussian or logistic simulation study

## Usage

``` r
gt_runstudy(
  scenarios,
  adapters = list(gt_adapter()),
  reps = 10L,
  seed = "20261004"
)
```

## Arguments

- scenarios:

  A `gt_scenario` or a non-empty list of them.

- adapters:

  A `gt_adapter` or a non-empty list of them.

- reps:

  Positive integer number of repetitions.

- seed:

  Non-negative integer master seed label. Character digit strings retain
  every digit, including labels above 2^53; numeric inputs must be exact
  integers no larger than 2^53.

## Value

A `gt_study` list containing `scenarios`, `adapters`, `reps`,
`master_seed`, `rng`, `seed_map`, `data`, `ledger`, `targets` and
`provenance`.

## Details

R streams use groundtruth-r-rng-v1: SHA256 of length-prefixed UTF8
master/scenario/rep/stream keys, allocated in canonical byte order to
distinct positive integer seeds. Salted rehash resolves collisions; the
full map is retained. Each data/fit stream sets
Mersenne-Twister/Inversion/Rejection. Calls restore caller RNG kinds and
seed (including absence) on success and error. Ambient Box-Muller is
refused before mutation because cached normal state cannot be safely
restored. Reordering the same input set is invariant; adding keys may
change rare collision allocations. R and Julia draws differ.

## Examples

``` r
study <- gt_runstudy(gt_scenario("example", n = 12L), reps = 2L)
gt_summarize(study)
#>   scenario adapter target level attempted successful failed usable_intervals
#> 1  example      lm  alpha  0.90         2          2      0                2
#> 2  example      lm  alpha  0.95         2          2      0                2
#> 3  example      lm   beta  0.90         2          2      0                2
#> 4  example      lm   beta  0.95         2          2      0                2
#>   unusable_intervals        bias  bias_mcse       rmse  rmse_mcse coverage
#> 1                  0 -0.15438558 0.08039531 0.17406410 0.07130635        1
#> 2                  0 -0.15438558 0.08039531 0.17406410 0.07130635        1
#> 3                  0  0.04182366 0.04147998 0.05890507 0.02945153        1
#> 4                  0  0.04182366 0.04147998 0.05890507 0.02945153        1
#>   coverage_mcse coverage_mc_lower coverage_mc_upper covered_per_attempt
#> 1             0         0.3423802                 1                   1
#> 2             0         0.3423802                 1                   1
#> 3             0         0.3423802                 1                   1
#> 4             0         0.3423802                 1                   1
```
