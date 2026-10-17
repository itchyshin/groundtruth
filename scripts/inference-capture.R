#!/usr/bin/env Rscript
# Bounded, deterministic Phase I native capture. Run only under the shared
# numerical token; no data are simulated and no outputs are overwritten.
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
for (source_file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  source(source_file, local = .GlobalEnv)
}
manifest_path <- normalizePath(file.path("tests", "testthat", "fixtures", "inference", "manifest.json"),
                               winslash = "/", mustWork = TRUE)
manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
fixtures <- manifest$fixtures
out_dir <- file.path(".unlazy", "inference", "evidence")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
csv_path <- file.path(out_dir, "r-capture.csv")
sidecar_path <- file.path(out_dir, "r-capture.json")
progress_csv <- file.path(out_dir, "r-capture-progress.csv")
progress_json <- file.path(out_dir, "r-capture-progress.json")
if (any(file.exists(c(csv_path, sidecar_path, progress_csv, progress_json)))) {
  stop("Refusing to overwrite an existing native capture.", call. = FALSE)
}
fixture_dir <- dirname(manifest_path)
expected_fixture_hashes <- vapply(fixtures, `[[`, "", "sha256")
fixture_files <- file.path(fixture_dir, vapply(fixtures, `[[`, "", "file"))
actual_fixture_hashes <- vapply(fixture_files, digest::digest, "", file = TRUE,
                                algo = "sha256")
if (!identical(unname(actual_fixture_hashes), unname(expected_fixture_hashes))) {
  stop("Fixture files do not match the frozen manifest.", call. = FALSE)
}
source_files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))
source_hashes <- vapply(source_files, digest::digest, "", file = TRUE, algo = "sha256")
source_identity <- digest::digest(
  paste(paste(source_files, source_hashes, sep = "\t"), collapse = "\n"),
  algo = "sha256", serialize = FALSE
)
start <- proc.time()[["elapsed"]]
rows <- list()
base_fit_attempts <- 0L
profile_fit_attempts <- 0L
curve_points <- 0L
invalid_rejections <- 0L
completed_strategies <- character()
append_result <- function(name, result, criterion, method, status) {
  estimates <- if (!is.null(result) && inherits(result, "gt_result")) result$estimates else
    c(alpha = NA_real_, beta = NA_real_, sigma2 = NA_real_, tau2 = NA_real_)
  diagnostics <- if (!is.null(result) && inherits(result, "gt_result")) result$diagnostics else list()
  covariance <- diagnostics$covariance
  if (!is.matrix(covariance) || !identical(dim(covariance), c(2L, 2L))) covariance <- matrix(NA_real_, 2L, 2L)
  details <- diagnostics$inference_details
  for (target in c("alpha", "beta")) for (level in c(.90, .95)) {
    hit <- if (is.data.frame(details)) which(details$target == target & details$level == level) else integer()
    d <- if (length(hit) == 1L) details[hit, , drop = FALSE] else NULL
    interval <- if (!is.null(result) && inherits(result, "gt_result")) result$intervals else NULL
    ivhit <- if (is.data.frame(interval)) which(interval$target == target & interval$level == level) else integer()
    iv <- if (length(ivhit) == 1L) interval[ivhit, , drop = FALSE] else NULL
    rows[[length(rows) + 1L]] <<- data.frame(
      fixture = name, criterion = criterion, interval_method = method,
      target = target, level = level,
      estimate = unname(estimates[[target]]), sigma2 = unname(estimates[["sigma2"]]),
      tau2 = unname(estimates[["tau2"]]),
      objective = diagnostics$objective_nll %||% NA_real_,
      independent_objective = diagnostics$independent_objective_nll %||% NA_real_,
      cov11 = covariance[1L, 1L], cov12 = covariance[1L, 2L], cov22 = covariance[2L, 2L],
      lower = if (!is.null(iv)) iv$lower else NA_real_,
      upper = if (!is.null(iv)) iv$upper else NA_real_,
      state = if (!is.null(d)) d$state else "error",
      root_lr_residual = if (!is.null(d)) d$root_lr_residual else NA_real_,
      endpoint_distance = if (!is.null(d)) d$endpoint_distance else NA_real_,
      elapsed_seconds = if (!is.null(d)) d$elapsed_seconds else NA_real_,
      base_status = status, engine_version = as.character(utils::packageVersion("lme4")),
      message = if (!is.null(d)) d$message else if (inherits(result, "error")) conditionMessage(result) else "",
      stringsAsFactors = FALSE
    )
  }
}
`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
progress_receipt <- function(complete = FALSE) {
  output <- if (length(rows)) do.call(rbind, rows) else NULL
  if (!is.null(output)) utils::write.csv(output, progress_csv, row.names = FALSE, na = "")
  receipt <- list(
    schema = "groundtruth-r-inference-capture-v1",
    complete = complete,
    source_scope = "R/*.R", source_sha256 = source_identity,
    source_sha256_by_file = as.list(stats::setNames(source_hashes, source_files)),
    fixture_manifest_sha256 = digest::digest(file = manifest_path, algo = "sha256"),
    fixture_sha256 = as.list(stats::setNames(actual_fixture_hashes, vapply(fixtures, `[[`, "", "file"))),
    runtime = list(R = R.version.string, lme4 = as.character(utils::packageVersion("lme4"))),
    physical_fit_attempts = base_fit_attempts,
    profile_adapter_fit_attempts = profile_fit_attempts,
    invalid_fixture_rejections = invalid_rejections,
    native_profile_curve_points = curve_points,
    completed_strategies = as.list(completed_strategies),
    elapsed_seconds = proc.time()[["elapsed"]] - start,
    csv_sha256 = if (!is.null(output)) digest::digest(file = progress_csv, algo = "sha256") else NULL
  )
  jsonlite::write_json(receipt, progress_json, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", digits = 17)
  invisible(receipt)
}
for (i in seq_along(fixtures)) {
  fixture <- fixtures[[i]]
  name <- sub("\\.csv$", "", fixture$file)
  dat <- utils::read.csv(fixture_files[[i]], stringsAsFactors = FALSE)[c("x", "y", "group")]
  if (!is.null(fixture$invalid)) {
    result <- tryCatch(gt_lmm_adapter()$fit(dat), error = function(e) e)
    if (!inherits(result, "error")) stop("Frozen invalid fixture was accepted: ", name)
    invalid_rejections <- invalid_rejections + 1L
    completed_strategies <- c(completed_strategies, paste(name, "rejected", sep = ":"))
    progress_receipt()
    next
  }
  for (criterion in c("ML", "REML")) {
    result <- tryCatch(gt_lmm_adapter(criterion = criterion)$fit(dat), error = function(e) e)
    base_fit_attempts <- base_fit_attempts + 1L
    if (inherits(result, "error")) {
      append_result(name, result, criterion, "normal", "fit_error")
    } else {
      append_result(name, result, criterion, "normal", "ok")
    }
    completed_strategies <- c(completed_strategies, paste(name, criterion, "normal", sep = ":"))
    progress_receipt()
  }
  if (isTRUE(fixture$profile_validation)) {
    result <- tryCatch(gt_lmm_adapter(
      criterion = "ML", interval_method = "profile", profile_timeout = 60
    )$fit(dat), error = function(e) e)
    base_fit_attempts <- base_fit_attempts + 1L
    profile_fit_attempts <- profile_fit_attempts + 1L
    if (!inherits(result, "error")) {
      points <- result$diagnostics$profile_curve_points
      if (length(points) == 1L && is.finite(points)) curve_points <- curve_points + as.integer(points)
      append_result(name, result, "ML", "profile", "ok")
    } else append_result(name, result, "ML", "profile", "fit_error")
    completed_strategies <- c(completed_strategies, paste(name, "ML", "profile", sep = ":"))
    progress_receipt()
  }
}
if (base_fit_attempts > 48L || profile_fit_attempts > 24L || base_fit_attempts > 22L) {
  stop("Native capture exceeded the frozen physical-fit allowance.", call. = FALSE)
}
output <- do.call(rbind, rows)
if (nrow(output) != 88L || base_fit_attempts != 22L ||
    profile_fit_attempts != 4L || invalid_rejections != 2L) {
  stop("Native capture did not complete the frozen 88-row/22-fit/4-profile/2-rejection schedule.",
       call. = FALSE)
}
source_hashes_after <- vapply(source_files, digest::digest, "", file = TRUE, algo = "sha256")
if (!identical(source_hashes, source_hashes_after)) {
  stop("R source changed during native capture; refusing to mark the evidence complete.",
       call. = FALSE)
}
utils::write.csv(output, csv_path, row.names = FALSE, na = "")
receipt <- progress_receipt(complete = TRUE)
receipt$csv_sha256 <- digest::digest(file = csv_path, algo = "sha256")
jsonlite::write_json(receipt, sidecar_path, auto_unbox = TRUE, pretty = TRUE,
                     null = "null", digits = 17)
unlink(c(progress_csv, progress_json))
cat(sprintf("R_CAPTURE_OK fits=%d profiles=%d elapsed=%.3f\n",
            base_fit_attempts, profile_fit_attempts, receipt$elapsed_seconds))
