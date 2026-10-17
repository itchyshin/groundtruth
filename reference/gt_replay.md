# Replay a recorded simulation study

Replay a recorded simulation study

## Usage

``` r
gt_replay(study, adapters = study$adapters)
```

## Arguments

- study:

  A `gt_study` returned by
  [`gt_runstudy()`](https://itchyshin.github.io/groundtruth/reference/gt_runstudy.md).

- adapters:

  Adapters to use, defaulting to the recorded adapters.

## Value

A newly computed `gt_study` with the recorded random-stream map.

## Details

Only the recognized recorded RNG version/settings are accepted. Replay
validates and reuses the full map. Deterministic/stateless adapters
replay in the same environment; external inputs and closure state remain
the caller's responsibility. Random-intercept scenarios record `x_level`
and generator revision `random-intercept-v2`. Legacy R scenarios without
`x_level` normalize to observation-level predictors during replay.

## Examples

``` r
study <- gt_runstudy(gt_scenario("example"), reps = 1L)
replay <- gt_replay(study)
```
