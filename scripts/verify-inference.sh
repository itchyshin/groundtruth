#!/bin/sh
set -eu
mode=${1:-}
case "$mode" in
  oracles)
    tests='analytic|inference-adapter'
    marker=INFERENCE_ORACLES_OK ;;
  methods)
    tests='inference-adapter'
    marker=INFERENCE_METHODS_OK ;;
  controls)
    tests='inference-adapter|ri-scale-guard'
    marker=INFERENCE_CONTROLS_OK ;;
  accounting)
    tests='inference-comparison'
    marker=INFERENCE_ACCOUNTING_OK ;;
  compatibility)
    tests='runner|paired|accounting|ri-xlevel|ri-scale-guard|controls'
    marker=INFERENCE_COMPAT_OK ;;
  package)
    tests='inference-adapter|inference-comparison|accounting|controls'
    marker=INFERENCE_PACKAGE_DOCS_OK ;;
  *)
    echo 'Usage: sh scripts/verify-inference.sh oracles|methods|controls|accounting|compatibility|package' >&2
    exit 2 ;;
esac
rscript_bin=${RSCRIPT:-/Library/Frameworks/R.framework/Versions/4.6/Resources/bin/Rscript}
if [ "$mode" = package ]; then
  "$rscript_bin" --vanilla -e "testthat::test_dir('tests/testthat', filter='$tests', reporter='summary', load_package='source', stop_on_failure=TRUE, stop_on_warning=FALSE)"
  "$rscript_bin" --vanilla -e 'roxygen2::roxygenise()'
  package_root=$(pwd)
  check_tmp=$(mktemp -d "${TMPDIR:-/tmp}/groundtruth-inference-check.XXXXXX")
  (cd "$check_tmp" && "$(dirname "$rscript_bin")/R" CMD build \
    --no-build-vignettes --no-manual --no-resave-data "$package_root")
  tarball="$check_tmp/groundtruth_0.0.0.9000.tar.gz"
  "$(dirname "$rscript_bin")/R" CMD check --no-install --no-tests \
    --no-examples --no-manual --no-vignettes --output="$check_tmp" "$tarball"
  test -f man/gt_lmm_adapter.Rd
  test -f man/gt_compare_inference.Rd
  test -f man/gt_inference_details.Rd
  grep -q 'export(gt_lmm_adapter)' NAMESPACE
  grep -q 'export(gt_compare_inference)' NAMESPACE
  grep -q 'export(gt_inference_details)' NAMESPACE
else
  "$rscript_bin" --vanilla -e "testthat::test_dir('tests/testthat', filter='$tests', reporter='summary', load_package='source', stop_on_failure=TRUE, stop_on_warning=FALSE)"
fi
if [ "$mode" = oracles ] || [ "$mode" = methods ]; then
  python3 scripts/inference-verify-capture.py
fi
printf '%s\n' "$marker"
