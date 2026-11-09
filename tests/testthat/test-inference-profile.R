mock_profile_frame <- function(values = seq(-4, 4, length.out = 41)) {
  data.frame(
    .par = rep(c("(Intercept)", "x"), each = length(values)),
    .zeta = rep(2 * values, 2L),
    `(Intercept)` = c(values, rep(0, length(values))),
    x = c(rep(0, length(values)), values),
    check.names = FALSE
  )
}

mock_confint_bracketed <- function(object, parm, level) {
  cutoff <- stats::qnorm((1 + level) / 2)
  matrix(c(-cutoff / 2, cutoff / 2), nrow = 1L,
         dimnames = list(NULL, c(parm, parm)))
}

test_that("native profile rows bracketed roots yield four finite intervals", {
  profile <- mock_profile_frame()
  checkpoint <- tempfile("gt-profile-")
  on.exit(unlink(checkpoint), add = TRUE)

  result <- .gt_native_profile_rows(
    fit = NULL,
    estimates = c(alpha = 0, beta = 0),
    standard_errors = c(alpha = 1, beta = 1),
    checkpoint = checkpoint,
    profile_fun = function(fit, ...) profile,
    confint_fun = mock_confint_bracketed
  )

  expect_length(result$states, 4L)
  expect_identical(unname(result$states), rep("finite", 4L))
  expect_identical(nrow(result$intervals), 4L)
  expect_true(all(result$residuals <= 0.02))
  expect_true(file.exists(checkpoint))
  saved <- readRDS(checkpoint)
  expect_identical(saved$completed, names(result$states))
})

test_that("native profile rows record incomplete and invalid interval states", {
  narrow <- data.frame(
    .par = rep("(Intercept)", 5L),
    .zeta = seq(-0.4, 0.4, length.out = 5L),
    `(Intercept)` = seq(-0.4, 0.4, length.out = 5L),
    x = rep(0, 5L),
    check.names = FALSE
  )
  incomplete <- .gt_native_profile_rows(
    NULL, c(alpha = 0, beta = 0), c(1, 1), tempfile(),
    function(...) narrow,
    function(...) stop("unused")
  )
  expect_true(all(incomplete$states == "incomplete"))
  expect_null(incomplete$intervals)

  bracketed <- mock_profile_frame(seq(-3, 3, length.out = 21))
  bad_confint <- function(object, parm, level) {
    matrix(c(Inf, 1), nrow = 1L)
  }
  invalid <- .gt_native_profile_rows(
    NULL, c(alpha = 0, beta = 0), c(1, 1), tempfile(),
    function(...) bracketed,
    bad_confint
  )
  expect_identical(invalid$states[["alpha:0.90"]], "invalid")
  expect_match(invalid$messages[["alpha:0.90"]], "nonfinite|reversed|missing")

  residual_fail <- function(object, parm, level) {
    cutoff <- stats::qnorm((1 + level) / 2)
    matrix(c(-cutoff * 4, cutoff * 4), nrow = 1L)
  }
  residual <- .gt_native_profile_rows(
    NULL, c(alpha = 0, beta = 0), c(1, 1), tempfile(),
    function(...) bracketed,
    residual_fail
  )
  expect_identical(residual$states[["alpha:0.90"]], "invalid")
  expect_match(residual$messages[["alpha:0.90"]], "residual")

  generic <- .gt_native_profile_rows(
    NULL, c(alpha = 0, beta = 0), c(1, 1), tempfile(),
    function(...) bracketed,
    function(...) stop("profile confint exploded")
  )
  expect_identical(generic$states[["alpha:0.90"]], "error")
  expect_match(generic$messages[["alpha:0.90"]], "exploded")
})

test_that("profile helper success and child errors surface typed details", {
  skip_on_os("windows")
  profile_fun <- function(fit, ...) mock_profile_frame()
  confint_fun <- mock_confint_bracketed

  ok <- .gt_profile_lmm(
    NULL, c(alpha = 0, beta = 0), c(1, 1), timeout = 10,
    profile_fun = profile_fun, confint_fun = confint_fun
  )
  expect_identical(unique(ok$details$state), "finite")
  expect_identical(nrow(ok$intervals), 4L)
  expect_gt(ok$details$elapsed_seconds[[1L]], 0)

  boom <- .gt_profile_lmm(
    NULL, c(alpha = 0, beta = 0), c(1, 1), timeout = 10,
    profile_fun = function(...) stop("profile setup failed"),
    confint_fun = confint_fun
  )
  expect_identical(unique(boom$details$state), "error")
  expect_match(boom$details$message[[1L]], "profile setup failed")
})

test_that("lmm result wires successful and failing profile branches", {
  estimates <- c(alpha = 1.1, beta = 0.4, sigma2 = 0.5, tau2 = 0.2)
  cov <- diag(c(0.04, 0.09))
  profiled <- list(
    intervals = data.frame(
      target = rep(c("alpha", "beta"), each = 2L),
      level = rep(c(0.90, 0.95), 2L),
      lower = c(0.5, 0.4, 0.1, 0),
      upper = c(1.7, 1.8, 0.7, 0.8),
      stringsAsFactors = FALSE
    ),
    details = .gt_inference_detail_rows(
      stats::setNames(rep("finite", 4L), c(
        "alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95"
      )),
      "ML", "profile"
    ),
    profile_curve_points = 82L
  )

  testthat::local_mocked_bindings(
    .gt_profile_lmm = function(...) profiled,
    .package = "groundtruth"
  )
  ok <- .gt_lmm_result(
    estimates = estimates,
    covariance = cov,
    criterion = "ML",
    interval_method = "profile",
    profile_timeout = 30,
    native_objective_nll = 9,
    independent_objective_nll = 9,
    profile_fit = structure(list(), class = "merMod")
  )
  expect_identical(ok$intervals, profiled$intervals)
  expect_identical(ok$diagnostics$profile_curve_points, 82L)
  expect_identical(ok$diagnostics$inference_details$state, rep("finite", 4L))

  testthat::local_mocked_bindings(
    .gt_profile_lmm = function(...) stop("profile branch failed"),
    .package = "groundtruth"
  )
  err <- .gt_lmm_result(
    estimates = estimates,
    covariance = cov,
    criterion = "ML",
    interval_method = "profile",
    profile_timeout = 30,
    native_objective_nll = 9,
    independent_objective_nll = 9,
    profile_fit = structure(list(), class = "merMod")
  )
  expect_identical(err$diagnostics$inference_details$state, rep("error", 4L))
  expect_match(err$message, "profile branch failed")
})

test_that("failed random-intercept fits retain companion inference details", {
  adapter <- gt_lmm_adapter(id = "lmer-detail")
  bad <- gt_adapter("lmer-detail", function(data) {
    stop("synthetic RI failure for coverage")
  }, family = "random_intercept", metadata = list(note = "injected"))
  bad$criterion <- adapter$criterion
  bad$interval_method <- adapter$interval_method
  bad$profile_timeout <- adapter$profile_timeout

  study <- gt_runstudy(
    gt_ri_scenario("detail-failure", n_groups = 3L, group_sizes = 3L),
    adapters = bad,
    reps = 1L,
    seed = "404"
  )
  expect_identical(study$ledger$status, "fit_error")
  expect_gt(nrow(study$inference_details), 0L)
  expect_true(all(study$inference_details$state == "error"))
  expect_match(study$inference_details$message[[1L]], "synthetic RI failure")
  expect_identical(
    unique(study$inference_details$criterion),
    adapter$criterion
  )
})

test_that("lmm metadata helper lists reserved inference names", {
  expect_identical(
  .gt_lmm_metadata_names(),
  c(
    "gt_inference_criterion", "gt_inference_interval_method",
    "gt_inference_levels", "gt_inference_engine",
    "gt_inference_engine_version"
  )
  )
})
