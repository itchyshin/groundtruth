# Inspect typed inference outcomes without changing target schemas

Inspect typed inference outcomes without changing target schemas

## Usage

``` r
gt_inference_details(study)
```

## Arguments

- study:

  A study returned by
  [`gt_runstudy()`](https://itchyshin.github.io/groundtruth/reference/gt_runstudy.md).

## Value

A keyed data frame of inference details. Legacy studies without the
companion return an empty data frame; their interval states are unknown.

## Examples

``` r
gt_inference_details(gt_runstudy(gt_scenario('ordinary'), reps = 1L))
#>  [1] scenario          rep               adapter           target           
#>  [5] level             state             criterion         interval_method  
#>  [9] lower             upper             root_lr_residual  endpoint_distance
#> [13] elapsed_seconds   message          
#> <0 rows> (or 0-length row.names)
```
