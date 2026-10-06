#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
evidence="$root/.unlazy/logistic-twins/evidence/docs-package"
build_dir="$evidence/build"
check_dir="$evidence/check"
lib_dir="$evidence/library"
mkdir -p "$build_dir" "$check_dir" "$lib_dir"

# All tools and dependencies must already be cached. The only package installation
# is R CMD check's candidate package in this private task-local check library.
if ! Rscript --vanilla -e 'stopifnot(requireNamespace("digest", quietly=TRUE), requireNamespace("roxygen2", quietly=TRUE), requireNamespace("pkgdown", quietly=TRUE), requireNamespace("pkgload", quietly=TRUE))'; then
  echo "Required cached R tools are unavailable; this script installs nothing." >&2
  exit 2
fi

# Verify the producer-refreshed identity before roxygen, build, or check.
Rscript --vanilla - "$root" > "$evidence/source-identity.log" 2>&1 <<'RS'
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1]], mustWork = TRUE)
setwd(root)
identity <- dget(file.path(root, "inst", "source-identity.R"))
version <- read.dcf(file.path(root, "DESCRIPTION"))[1, "Version"]
actual_hash <- digest::digest(file = file.path(root, "R", "groundtruth.R"),
                              algo = "sha256", serialize = FALSE)
stopifnot(identical(identity$package_version, unname(version)),
          identical(identity$source_sha256, actual_hash))
cat("SOURCE_IDENTITY_OK\n")
RS

# Generate help pages and exports from the finalized producer roxygen comments.
if ! Rscript --vanilla - "$root" > "$evidence/roxygen.log" 2>&1 <<'RS'
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1]], mustWork = TRUE)
roxygen2::roxygenise(package.dir = root, roclets = c("rd", "namespace"))
RS
then
  cat "$evidence/roxygen.log" >&2
  exit 1
fi

# Build into ignored task evidence storage; no vignettes are built into the source
# tarball. R CMD check runs the package tests against its own installed candidate.
if ! (cd "$build_dir" && R CMD build --no-build-vignettes --no-manual "$root") \
    > "$evidence/build.log" 2>&1; then
  cat "$evidence/build.log" >&2
  exit 1
fi
tarball_count="$(find "$build_dir" -maxdepth 1 -type f -name 'groundtruth_*.tar.gz' | wc -l | tr -d ' ')"
if [[ "$tarball_count" -ne 1 ]]; then
  echo "Expected one built groundtruth source tarball; found $tarball_count." >&2
  exit 1
fi
tarball="$(find "$build_dir" -maxdepth 1 -type f -name 'groundtruth_*.tar.gz' -print)"
set +e
R CMD check --no-manual --no-build-vignettes --library="$lib_dir" \
  --output="$check_dir" "$tarball" > "$evidence/check.log" 2>&1
check_status=$?
set -e
if [[ "$check_status" -ne 0 ]]; then
  cat "$evidence/check.log" >&2
  exit "$check_status"
fi

python3 - "$evidence/check.log" "$check_dir" <<'PY'
from pathlib import Path
import re, sys
stdout = Path(sys.argv[1]).read_text(errors="replace")
check = Path(sys.argv[2])
logs = [stdout, *(p.read_text(errors="replace") for p in check.rglob("00check.log"))]
log = "\n".join(logs)
status = re.findall(r"^Status:.*$", log, flags=re.M)
assert status, "R CMD check logs have no final Status line"
summary = status[-1]
assert "WARNING" not in summary and "ERROR" not in summary, summary
notes = sorted(set(line.strip() for line in log.splitlines()
                   if " NOTE" in line or line.lstrip().startswith("NOTE")))
(check / "notes-summary.txt").write_text(
    summary + "\n" + ("\n".join(notes) if notes else "No NOTE lines reported.") + "\n"
)
print("R CMD check: " + summary)
if notes:
    print("R CMD check NOTE scope (full logs are retained under .unlazy/logistic-twins/evidence/docs-package):")
    print("\n".join(notes))
PY

# Load and verify the exact current source namespace before pkgdown uses install=FALSE.
# This prevents references/articles from resolving a stale globally installed package.
if ! Rscript --vanilla - "$root" "$lib_dir" > "$evidence/pkgdown.log" 2>&1 <<'RS'
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1]], mustWork = TRUE)
lib_dir <- normalizePath(args[[2]], mustWork = TRUE)
stopifnot(identical(normalizePath(getwd(), mustWork = TRUE), root))
installed_pkg <- normalizePath(file.path(lib_dir, "groundtruth"), mustWork = TRUE)
receipt_path <- file.path(root, ".unlazy", "logistic-twins", "evidence",
                          "docs-package", "article-source-identity.dput")
unlink(receipt_path)
installed_identity <- dget(file.path(installed_pkg, "source-identity.R"))
installed_version <- read.dcf(file.path(installed_pkg, "DESCRIPTION"))[1, "Version"]
source_identity <- dget(file.path(root, "inst", "source-identity.R"))
source_version <- read.dcf(file.path(root, "DESCRIPTION"))[1, "Version"]
source_hash <- digest::digest(file = file.path(root, "R", "groundtruth.R"),
                              algo = "sha256", serialize = FALSE)
stopifnot(identical(unname(installed_version), unname(source_version)),
          identical(installed_identity$package_version, unname(source_version)),
          identical(installed_identity$source_sha256, source_hash),
          identical(source_identity$package_version, unname(source_version)),
          identical(source_identity$source_sha256, source_hash))
existing_r_libs <- Sys.getenv("R_LIBS", unset = "")
Sys.setenv(R_LIBS = paste(c(lib_dir, existing_r_libs[nzchar(existing_r_libs)]),
                          collapse = .Platform$path.sep))
.libPaths(unique(c(lib_dir, .libPaths())))
stopifnot(identical(normalizePath(find.package("groundtruth"), mustWork = TRUE),
                    installed_pkg))
Sys.setenv(GROUNDTRUTH_DOCS_EXPECTED_SOURCE_SHA = source_hash,
           GROUNDTRUTH_DOCS_EXPECTED_PACKAGE_PATH = installed_pkg,
           GROUNDTRUTH_DOCS_EXPECTED_PACKAGE_VERSION = unname(source_version),
           GROUNDTRUTH_DOCS_IDENTITY_RECEIPT = receipt_path)
pkgload::load_all(root, export_all = FALSE, reset = TRUE, quiet = TRUE)
ns <- asNamespace("groundtruth")
stopifnot(identical(normalizePath(getNamespaceInfo(ns, "path"), mustWork = TRUE), root))
stopifnot(all(c("gt_scenario", "gt_adapter", "gt_glm_adapter") %in% getNamespaceExports(ns)))
stopifnot(identical(names(formals(gt_glm_adapter)), c("id", "metadata")))
gaussian <- gt_scenario("docs-default")
logistic <- gt_scenario("docs-logistic", family = "logistic")
stopifnot(identical(gaussian$family, "gaussian"), identical(gaussian$sigma, 1.2),
          identical(logistic$family, "logistic"), is.null(logistic$sigma),
          identical(gt_adapter()$family, "gaussian"),
          identical(gt_glm_adapter()$family, "logistic"))
identity <- dget(file.path(root, "inst", "source-identity.R"))
version <- read.dcf(file.path(root, "DESCRIPTION"))[1, "Version"]
actual_hash <- digest::digest(file = file.path(root, "R", "groundtruth.R"),
                              algo = "sha256", serialize = FALSE)
stopifnot(identical(identity$package_version, unname(version)),
          identical(identity$source_sha256, actual_hash))
ignore <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)
stopifnot(all(c("^LOOP($|/)", "^\\.unlazy($|/)", "^docs($|/)", "^\\.git$") %in% ignore))
pkgdown::build_site(pkg = root, override = list(destination = "docs/site"),
                    preview = FALSE, install = FALSE, new_process = FALSE,
                    quiet = TRUE)
stopifnot(file.exists(receipt_path))
article_identity <- dget(receipt_path)
stopifnot(identical(normalizePath(article_identity$namespace_path, mustWork = TRUE),
                    installed_pkg),
          identical(article_identity$source_sha256, source_hash),
          identical(article_identity$package_version, unname(source_version)),
          identical(article_identity$checks_passed, TRUE))
cat("DOCS_ARTICLE_SOURCE_IDENTITY_OK\n")
RS
then
  cat "$evidence/pkgdown.log" >&2
  exit 1
fi

python3 - "$root" <<'PY'
from pathlib import Path
import re, sys
root = Path(sys.argv[1])
site = root / "docs/site"
assert (site / "index.html").is_file(), "pkgdown did not render docs/site/index.html"
rendered = "\n".join(path.read_text(errors="replace") for path in site.rglob("*.html"))
for url in ("https://github.com/itchyshin/groundtruth",
            "https://itchyshin.github.io/groundtruth/",
            "https://itchyshin.github.io/GroundTruth.jl/"):
    assert url in rendered, f"expected GitHub/site link missing from rendered docs: {url}"
for required in ("logistic", "conditional log odds", "student t"):
    assert required in rendered.lower(), f"required reader documentation is missing: {required}"
for page in [root / "README.md", *root.glob("vignettes/**/*.Rmd")]:
    body = page.read_text(errors="replace")
    for target in re.findall(r"\[[^\]]*\]\(([^)]+)\)", body):
        if target.startswith(("http://", "https://", "mailto:", "#")):
            continue
        path, _, anchor = target.partition("#")
        linked = (page.parent / path).resolve()
        assert linked.exists(), f"broken local source link in {page}: {target}"
        if anchor and linked.suffix.lower() in {".md", ".rmd"}:
            heading = linked.read_text(errors="replace")
            slug = re.sub(r"[^a-z0-9 -]", "", anchor.lower()).replace(" ", "-")
            assert slug in heading.lower(), f"missing local anchor in {page}: {target}"
print("LOGISTIC_DOCS_PACKAGE_OK")
PY
