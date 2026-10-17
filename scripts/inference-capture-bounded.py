#!/usr/bin/env python3
"""Run one R native capture with a hard 30-second process-group deadline."""
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import time

root = Path(__file__).resolve().parent.parent
rscript = Path("/Library/Frameworks/R.framework/Versions/4.6/Resources/bin/Rscript")
command = [str(rscript), "--vanilla", "scripts/inference-capture.R"]
started_at = datetime.now(timezone.utc).isoformat()
started = time.monotonic()
env = os.environ.copy()
env.update(OPENBLAS_NUM_THREADS="1", OMP_NUM_THREADS="1", MKL_NUM_THREADS="1")
print("CAPTURE_START_UTC", started_at, flush=True)
print("CAPTURE_COMMAND", json.dumps(command), flush=True)
proc = subprocess.Popen(
    command, cwd=root, env=env, stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT, text=True, start_new_session=True
)
try:
    output, _ = proc.communicate(timeout=30)
    code = proc.returncode
except subprocess.TimeoutExpired:
    os.killpg(proc.pid, signal.SIGKILL)
    output, _ = proc.communicate()
    code = 124
elapsed = time.monotonic() - started
ended_at = datetime.now(timezone.utc).isoformat()
print(output, end="", flush=True)
receipt = {
    "command": command,
    "cwd": str(root),
    "started_at_utc": started_at,
    "ended_at_utc": ended_at,
    "elapsed_seconds": elapsed,
    "exit_code": code,
    "process_group_killed_on_timeout": code == 124,
}
path = root / ".unlazy" / "inference" / "evidence" / "r-capture-execution.json"
path.parent.mkdir(parents=True, exist_ok=True)
path.write_text(json.dumps(receipt, indent=2) + "\n")
print("CAPTURE_END_UTC", ended_at, "ELAPSED_SECONDS", f"{elapsed:.3f}", "EXIT", code)
sys.exit(code)
