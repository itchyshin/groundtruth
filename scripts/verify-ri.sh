#!/bin/sh
set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
root=$(CDPATH= cd "$script_dir/.." && pwd)
cd "$root"
rscript_bin=${R_SCRIPT_BIN:-Rscript}
case "$rscript_bin" in
  */*) [ -x "$rscript_bin" ] || { echo "Rscript executable not found or not executable: $rscript_bin" >&2; exit 127; } ;;
  *) rscript_bin=$(command -v "$rscript_bin") || { echo "Rscript executable not found: ${R_SCRIPT_BIN:-Rscript}" >&2; exit 127; } ;;
esac

export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
export R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null

case "${1:-}" in
  red) "$rscript_bin" --vanilla -e 'testthat::test_dir("tests/testthat", filter="ri-contract", load_package="source", reporter="summary", stop_on_failure=TRUE)' ;;
  language) "$rscript_bin" --vanilla scripts/ri-language.R ;;
  core) "$rscript_bin" --vanilla -e 'testthat::test_dir("tests/testthat", load_package="source", reporter="summary", stop_on_failure=TRUE);cat("RI_R_CORE_OK\n")' ;;
  *) echo 'mode red|language|core required' >&2; exit 2 ;;
esac
