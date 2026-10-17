# Export a study as CSV files and a base-R manifest

Export a study as CSV files and a base-R manifest

## Usage

``` r
gt_export(study, path)
```

## Arguments

- study:

  A `gt_study` returned by
  [`gt_runstudy()`](https://itchyshin.github.io/groundtruth/reference/gt_runstudy.md)
  or
  [`gt_replay()`](https://itchyshin.github.io/groundtruth/reference/gt_replay.md).

- path:

  A destination path that must not already exist.

## Value

The normalized destination path, invisibly.

## Details

Writes data.csv, attempts.csv, targets.csv, summary.csv, seed-map.csv,
scenarios.csv and manifest.R with versioned schemas, full RNG
settings/map, character master seed, SHA256 file hashes, adapter
metadata and provenance. Ordinary-only studies use export schema v2;
random-intercept studies use v4, which records `x_level` in
scenarios.csv and generator revision `random-intercept-v2`. Earlier v3
artifacts remain valid as artifacts. Source identity is the SHA256 of
the sole implementation file, R/groundtruth.R, recorded when the package
is prepared. It identifies the study producer separately from the
exporter. The CSV writer retains at least 17 significant digits; missing
values are empty fields. Read according to manifest column types.
[`dget()`](https://rdrr.io/r/base/dput.html) evaluates R expressions, so
read only trusted manifests. Custom adapter code/state is not
serialized. Any existing destination, including an empty directory, is
refused. Export failure removes only the newly created destination.

## Examples

``` r
if (FALSE) { # \dontrun{
study <- gt_runstudy(gt_scenario("example"), reps = 2L)
gt_export(study, tempfile("groundtruth-"))
} # }
```
