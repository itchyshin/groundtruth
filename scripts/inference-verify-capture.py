#!/usr/bin/env python3
"""Rebind and independently validate the retained native inference capture."""
import csv
import hashlib
import json
import subprocess
import sys
from pathlib import Path

root = Path(__file__).resolve().parent.parent
evidence = root / ".unlazy" / "inference" / "evidence"
csv_path = evidence / "r-capture.csv"
sidecar_path = evidence / "r-capture.json"
manifest_path = root / "tests" / "testthat" / "fixtures" / "inference" / "manifest.json"
sidecar = json.loads(sidecar_path.read_text())
manifest = json.loads(manifest_path.read_text())
assert sidecar.get("complete") is True, "capture is incomplete"
source_files = sorted((root / "R").glob("*.R"))
source_hashes = {
    p.relative_to(root).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
    for p in source_files
}
composite = hashlib.sha256("\n".join(
    f"{name}\t{value}" for name, value in source_hashes.items()
).encode()).hexdigest()
assert sidecar["source_scope"] == "R/*.R"
assert sidecar["source_sha256_by_file"] == source_hashes, "R source files changed"
assert sidecar["source_sha256"] == composite, "composite R source hash mismatch"
assert sidecar["fixture_manifest_sha256"] == hashlib.sha256(manifest_path.read_bytes()).hexdigest()
fixture_hashes = {
    f["file"]: hashlib.sha256((manifest_path.parent / f["file"]).read_bytes()).hexdigest()
    for f in manifest["fixtures"]
}
assert sidecar["fixture_sha256"] == fixture_hashes, "fixture hash mismatch"
assert sidecar["csv_sha256"] == hashlib.sha256(csv_path.read_bytes()).hexdigest()
assert sidecar["physical_fit_attempts"] == 22
assert sidecar["profile_adapter_fit_attempts"] == 4
assert sidecar["invalid_fixture_rejections"] == 2
with csv_path.open(newline="") as stream:
    rows = list(csv.DictReader(stream))
assert len(rows) == 88
validator = root / "audit" / "inference" / "validate_captures.py"
if not validator.is_file():
    raise SystemExit("shared Phase I validator is unavailable beside the worktree")
result = subprocess.run(
    [sys.executable, str(validator), str(csv_path)], cwd=root,
    check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT
)
print(result.stdout, end="")
print("INFERENCE_CAPTURE_SOURCE_BOUND_OK")
