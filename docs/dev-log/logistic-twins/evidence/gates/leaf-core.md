# Gates: language implementation
OWNS: R/**, tests/testthat/test-logistic*.R, tests/testthat/test-controls.R, inst/source-identity.R, scripts/verify.R
Scope: scientific implementation with regression, compatibility, accounting and checker controls.
- [x] G3: language logistic regression and numerical outputs
  CHECK: python3 scripts/timed_gate.py Rscript --vanilla scripts/verify.R logistic
  EXPECT: LOGISTIC_LANGUAGE_OK
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/Users/z3437171/Documents/Codex/2026-10-06/logistic-twins/groundtruth; path=8d2e6e29f6f6/39 entries; output=LOGISTIC_LANGUAGE_OK | NUMERICAL_SECONDS 1.967 CHARGED_TOTAL 230.117
- [x] G4: core and compatibility regressions
  CHECK: python3 scripts/timed_gate.py Rscript --vanilla scripts/verify.R core
  EXPECT: LOGISTIC_CORE_OK
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/Users/z3437171/Documents/Codex/2026-10-06/logistic-twins/groundtruth; path=8d2e6e29f6f6/39 entries; output=LOGISTIC_CORE_OK | NUMERICAL_SECONDS 2.171 CHARGED_TOTAL 232.288
- [x] G5: failure/accounting and wrong-answer controls
  CHECK: python3 scripts/timed_gate.py Rscript --vanilla scripts/verify.R controls
  EXPECT: LOGISTIC_CONTROLS_OK
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/Users/z3437171/Documents/Codex/2026-10-06/logistic-twins/groundtruth; path=8d2e6e29f6f6/39 entries; output=LOGISTIC_CONTROLS_OK | NUMERICAL_SECONDS 1.49 CHARGED_TOTAL 233.778
