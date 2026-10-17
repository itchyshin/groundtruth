#!/usr/bin/env Rscript
# Make this language gate exercise the package contract before producing the
# independent fixture reference. A failure here prevents the success marker.
testthat::test_dir(
  file.path("tests", "testthat"),
  filter = "ri-contract",
  load_package = "source",
  reporter = "summary",
  stop_on_failure = TRUE
)
source(file.path("R", "groundtruth.R"), local = .GlobalEnv)

fixture_dir <- file.path("tests", "testthat", "fixtures", "ri")
fixture_files <- c(
  "lmm-interior-data.csv",
  "lmm-interior-fixture-data.csv",
  "lmm-unequal-data.csv",
  "lmm-weak-data.csv",
  "lmm-zero-data.csv",
  "lmm-boundary-fixture-data.csv"
)
fields <- c(
  "scenario", "engine", "status", "alpha", "beta", "sigma2", "tau2",
  "objective", "alpha_lower90", "alpha_upper90", "beta_lower90", "beta_upper90",
  "alpha_lower95", "alpha_upper95", "beta_lower95", "beta_upper95", "message"
)
interval_value <- function(intervals, target, level, bound) {
  hit <- intervals$target == target & intervals$level == level
  if (sum(hit) != 1L) return(NA_real_)
  intervals[[bound]][hit]
}
rows <- lapply(fixture_files, function(filename) {
  scenario <- sub("-data\\.csv$", "", filename)
  dat <- utils::read.csv(file.path(fixture_dir, filename), stringsAsFactors = FALSE)
  fit <- tryCatch(gt_lmm_adapter()$fit(dat[c("x", "y", "group")]),
                  error = function(e) e)
  if (inherits(fit, "error")) {
    row <- as.list(rep(NA_real_, length(fields)))
    names(row) <- fields
    row$scenario <- scenario
    row$engine <- "lme4"
    row$status <- "fit_error"
    row$message <- conditionMessage(fit)
    return(as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE))
  }
  iv <- fit$intervals
  row <- list(
    scenario = scenario,
    engine = "lme4",
    status = "ok",
    alpha = unname(fit$estimates[["alpha"]]),
    beta = unname(fit$estimates[["beta"]]),
    sigma2 = unname(fit$estimates[["sigma2"]]),
    tau2 = unname(fit$estimates[["tau2"]]),
    objective = fit$diagnostics$objective_nll,
    alpha_lower90 = interval_value(iv, "alpha", 0.90, "lower"),
    alpha_upper90 = interval_value(iv, "alpha", 0.90, "upper"),
    beta_lower90 = interval_value(iv, "beta", 0.90, "lower"),
    beta_upper90 = interval_value(iv, "beta", 0.90, "upper"),
    alpha_lower95 = interval_value(iv, "alpha", 0.95, "lower"),
    alpha_upper95 = interval_value(iv, "alpha", 0.95, "upper"),
    beta_lower95 = interval_value(iv, "beta", 0.95, "lower"),
    beta_upper95 = interval_value(iv, "beta", 0.95, "upper"),
    message = fit$message
  )
  as.data.frame(row[fields], stringsAsFactors = FALSE, check.names = FALSE)
})
out <- do.call(rbind, rows)
out_dir <- file.path(".unlazy", "ri-twins", "evidence")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, file.path(out_dir, "r-fits.csv"), row.names = FALSE,
                 na = "")
report <- c(
  "# R random-intercept producer",
  "",
  paste0("R: ", R.version.string),
  paste0("lme4: ", as.character(utils::packageVersion("lme4"))),
  "Estimator: lme4::lmer with REML=FALSE and bobyqa; fixed-effect intervals use the independently reconstructed ML plug-in covariance.",
  "Variance targets are reported on the variance scale and have no intervals.",
  paste0("Frozen fixtures exported: ", nrow(out), "."),
  paste0("Fit errors: ", sum(out$status != "ok"), ".")
)
writeLines(report, file.path(out_dir, "r-producer.md"), useBytes = TRUE)
expected_scenarios <- sub("-data\\.csv$", "", fixture_files)
numeric_fields <- setdiff(fields, c("scenario", "engine", "status", "message"))
stopifnot(
  identical(out$scenario, expected_scenarios),
  !anyDuplicated(out$scenario),
  nrow(out) == 6L,
  all(out$status == "ok"),
  all(vapply(out[numeric_fields], function(x) all(is.finite(x)), logical(1)))
)
cat("RI_R_LANGUAGE_OK\n")
