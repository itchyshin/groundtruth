# Codecov rollout (config only)

This file records what the coverage files in this repository do. It is not a
processed Codecov report, a coverage percentage, or a main baseline.

## Existing checks

`.github/workflows/R-CMD-check.yaml` still starts with the comment "Inert
proposal." That comment is stale. The file is already under `.github/workflows`
and GitHub has been executing the "R package check" workflow on pull requests
and on `main` since 2026-10-06. The last successful `main` check at
`d8261bf23453dd8cb8c85cc50722b00373078a31` is
https://github.com/itchyshin/groundtruth/actions/runs/39066555689. This rollout
does not change that workflow.

`.github/workflows/pkgdown.yaml` has the same "Inert proposal" header. It is
active as a `workflow_dispatch` job and has published the site. This rollout
does not change that workflow.

There was no coverage workflow, `codecov.yml`, or `covr` configuration in the
tree before this change. Nothing previously represented as working coverage is
being treated as working.

## What this change adds

- `test-coverage` runs `covr::package_coverage()` on the existing testthat
  suite, prints the local result, and writes `cobertura.xml`. Pull requests
  get that execution only. They do not receive Codecov OIDC credentials.
- A coverage artifact is saved only on a successful push to `main`.
- `Upload main coverage` is a separate `workflow_run` workflow. GitHub loads
  `workflow_run` from the default branch, so this job cannot run until the
  workflow file is on `main`. It uploads one Cobertura file with OIDC. It
  does not use a Codecov token. `github.token` is passed only to
  `actions/download-artifact`.
- `codecov.yml` keeps project and patch statuses informational and disables
  PR comments. It does not create a badge.

`covr` and `yaml` are CI extra packages. They are not added to DESCRIPTION, so
the existing package check still installs the same Suggests set.

## Evidence stages

| Stage | Meaning | Status of this change |
|---|---|---|
| Config | Workflows and `codecov.yml` exist in git | This pull request |
| Actual execution / % | `covr::package_coverage()` ran and printed a percentage | Only after `test-coverage` completes. A pull_request run uses GitHub's synthetic merge checkout, which is not the branch head. |
| Upload reception | Codecov accepted the single Cobertura upload | Only after an owner-authorized merge to `main`, a successful `test-coverage` push run, and a successful `Upload main coverage` run |
| Processed report | Codecov finished processing that upload | After upload reception; not implied by HTTP 200 from the configure API |
| Main baseline | Processed report for `main` | Requires the previous two stages on `main` |

A public Codecov API response of `active: false` is not an uploader failure and
is not a request for owner app or billing changes. Owner Codecov authorization
is requested only after the trusted uploader job actually fails.

No README badge is added until a processed `main` report exists.

## Out of scope

Scientific `R/` sources, results, package tests, fixtures, and parity scripts
are unchanged. This file does not merge to `main`.
