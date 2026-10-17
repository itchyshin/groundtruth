test_that("random-intercept inference choices validate before fitting", {
  before <- loadedNamespaces()
  adapter <- gt_lmm_adapter(
    metadata = list(source = "phase-I-fixture"),
    criterion = "ML",
    interval_method = "normal",
    profile_timeout = 60
  )

  expect_s3_class(adapter, "gt_adapter")
  expect_identical(adapter$id, "lmer")
  expect_identical(adapter$family, "random_intercept")
  expect_identical(adapter$criterion, "ML")
  expect_identical(adapter$interval_method, "normal")
  expect_identical(adapter$profile_timeout, 60)
  expect_identical(adapter$metadata$source, "phase-I-fixture")
  expect_identical(adapter$metadata$gt_inference_criterion, "ML")
  expect_identical(adapter$metadata$gt_inference_interval_method, "normal")
  expect_identical(adapter$metadata$gt_inference_levels, c(.90, .95))
  expect_identical(adapter$metadata$gt_inference_engine, "lme4::lmer")
  expect_true(is.character(adapter$metadata$gt_inference_engine_version))
  expect_length(adapter$metadata$gt_inference_engine_version, 1L)
  expect_false("lme4" %in% setdiff(loadedNamespaces(), before))
})

test_that("inference settings reject unsupported combinations and collisions", {
  expect_error(gt_lmm_adapter(criterion = "REML-ish"), "criterion.*ML.*REML")
  expect_error(gt_lmm_adapter(interval_method = "wald"), "interval_method.*normal.*profile")
  expect_error(gt_lmm_adapter(criterion = "REML", interval_method = "profile"),
               "profile.*ML")
  for (deadline in list(0, -1, Inf, NaN, NA_real_, c(1, 2), "60")) {
    expect_error(gt_lmm_adapter(profile_timeout = deadline), "profile_timeout")
  }
  expect_error(
    gt_lmm_adapter(metadata = list(gt_inference_criterion = "custom")),
    "name.*gt_inference_criterion.*reserved"
  )
})

test_that("covariance failure leaves finite accepted points intact", {
  estimates <- c(alpha = 1.2, beta = .7, sigma2 = .4, tau2 = .3)
  result <- .gt_lmm_result(
    estimates = estimates,
    covariance = NULL,
    criterion = "ML",
    interval_method = "normal",
    profile_timeout = 60,
    native_objective_nll = 12.5,
    independent_objective_nll = NA_real_
  )

  expect_s3_class(result, "gt_result")
  expect_identical(result$estimates, estimates)
  expect_null(result$intervals)
  expect_identical(result$diagnostics$objective_nll, 12.5)
  expect_true(is.na(result$diagnostics$independent_objective_nll))
  expect_identical(result$diagnostics$inference_details$state, rep("invalid", 4L))
})

test_that("profile helper failure leaves accepted points intact", {
  estimates <- c(alpha = 1.2, beta = .7, sigma2 = .4, tau2 = .3)
  result <- .gt_lmm_result(
    estimates = estimates,
    covariance = diag(c(.2, .3)),
    criterion = "ML",
    interval_method = "profile",
    profile_timeout = 60,
    native_objective_nll = 12.5,
    independent_objective_nll = 12.5,
    profile_fit = NULL
  )

  expect_identical(result$estimates, estimates)
  expect_null(result$intervals)
  expect_identical(result$diagnostics$inference_details$state, rep("error", 4L))
  expect_match(result$message, "unavailable")
})

test_that("timeout keeps an in-progress profile endpoint typed as timeout", {
  skip_on_os("windows")
  values <- seq(-3, 3, length.out = 13L)
  profile <- data.frame(
    .par = rep(c("(Intercept)", "x"), each = length(values)),
    .zeta = rep(2 * values, 2L),
    `(Intercept)` = c(values, rep(0, length(values))),
    x = c(rep(0, length(values)), values),
    check.names = FALSE
  )
  profile_fun <- function(fit, ...) profile
  calls <- 0L
  confint_fun <- function(object, parm, level) {
    calls <<- calls + 1L
    if (calls == 2L) Sys.sleep(.5)
    cutoff <- stats::qnorm((1 + level) / 2)
    matrix(c(-cutoff / 2, cutoff / 2), nrow = 1L)
  }

  result <- .gt_profile_lmm(
    fit = NULL,
    estimates = c(alpha = 0, beta = 0),
    standard_errors = c(alpha = 1, beta = 1),
    timeout = .15,
    profile_fun = profile_fun,
    confint_fun = confint_fun
  )
  states <- stats::setNames(
    result$details$state,
    paste(result$details$target, sprintf("%.2f", result$details$level), sep = ":")
  )
  expect_identical(states[["alpha:0.90"]], "finite")
  expect_identical(states[["alpha:0.95"]], "timeout")
  expect_identical(states[["beta:0.90"]], "timeout")
  expect_identical(states[["beta:0.95"]], "timeout")
})

test_that("typed details export beside unchanged legacy target rows", {
  study <- gt_runstudy(gt_scenario("details-export", n = 8L), reps = 1L)
  details <- .gt_inference_detail_rows(
    stats::setNames(rep("error", 4L), c(
      "alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95"
    )), "ML", "profile", messages = stats::setNames(rep("forced", 4L), c(
      "alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95"
    ))
  )
  study$inference_details <- data.frame(
    scenario = rep("details-export", 4L), rep = rep(1L, 4L),
    adapter = rep("lmer", 4L), details, row.names = NULL,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  destination <- tempfile("gt-details-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  gt_export(study, destination)
  manifest <- dget(file.path(destination, "manifest.R"))
  expect_identical(manifest$inference_details_schema, "inference-details-v1")
  expect_true(file.exists(file.path(destination, "inference-details.csv")))
  exported_targets <- utils::read.csv(
    file.path(destination, "targets.csv"), check.names = FALSE
  )
  expect_identical(names(exported_targets), names(study$targets))
  expect_true("inference-details.csv" %in% names(manifest$sha256))
})

test_that("export preserves producer implementation identity", {
  study <- gt_study()
  producer <- strrep("a", 64L)
  exporter <- strrep("b", 64L)
  study$provenance$implementation_scope <- "R/*.R"
  study$provenance$implementation_sha256 <- producer
  testthat::local_mocked_bindings(
    .gt_source_identity = function() list(
      package_version = "0.0.0.9000",
      source_scope = "R/groundtruth.R",
      source_sha256 = strrep("c", 64L),
      implementation_scope = "R/*.R",
      implementation_sha256 = exporter
    ),
    .package = "groundtruth"
  )

  destination <- tempfile("producer-identity-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  gt_export(study, destination)
  manifest <- dget(file.path(destination, "manifest.R"))
  expect_identical(manifest$implementation_scope, "R/*.R")
  expect_identical(manifest$implementation_sha256, producer)
  expect_identical(manifest$exporter_implementation_scope, "R/*.R")
  expect_identical(manifest$exporter_implementation_sha256, exporter)
})

test_that("run and export retain accepted-fit information-limit diagnostics", {
  warning_text <- "With two groups and group-level x, tau2 is not identified."
  adapter <- gt_lmm_adapter()
  adapter$fit <- function(data) .gt_lmm_result(
    estimates = c(alpha = 1, beta = .5, sigma2 = .8, tau2 = .2),
    covariance = diag(c(.1, .2)), criterion = "REML",
    interval_method = "normal", profile_timeout = 60,
    native_objective_nll = 10, independent_objective_nll = 10,
    diagnostics = list(
      engine = "lme4::lmer", engine_version = "2.0.1",
      optimizer = "bobyqa", optimizer_code = 0L,
      optimizer_evaluations = 17L, optimizer_messages = character(),
      warnings = "synthetic optimizer warning", converged = TRUE,
      singular = FALSE, tau2_unidentified = TRUE,
      information_limit = warning_text
    )
  )
  study <- gt_runstudy(
    gt_ri_scenario("diagnostic-export", n_groups = 2L,
                   group_sizes = 2L, x_level = "group"),
    adapter, reps = 1L, seed = "321"
  )

  expect_match(study$ledger$message[[1L]], warning_text, fixed = TRUE)
  expect_true(all(grepl(warning_text, study$inference_details$message, fixed = TRUE)))
  expect_identical(study$fit_diagnostics$engine_version, "2.0.1")
  expect_identical(study$fit_diagnostics$optimizer_code, "0")
  expect_identical(study$fit_diagnostics$warnings, "synthetic optimizer warning")
  expect_identical(study$fit_diagnostics$information_limit, warning_text)

  destination <- tempfile("fit-diagnostics-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  gt_export(study, destination)
  manifest <- dget(file.path(destination, "manifest.R"))
  expect_identical(manifest$fit_diagnostics_schema, "fit-diagnostics-v1")
  exported <- utils::read.csv(
    file.path(destination, "fit-diagnostics.csv"), stringsAsFactors = FALSE
  )
  expect_identical(exported$information_limit, warning_text)
  expect_identical(exported$engine_version, "2.0.1")
})
