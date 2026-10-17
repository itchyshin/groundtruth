ri_fixture_path <- function(name) {
  path <- testthat::test_path("fixtures", "ri", name)
  testthat::expect_true(file.exists(path), info = paste("missing RI fixture", name))
  path
}

ri_point_result <- function(estimates = c(
  alpha = 1, beta = 0.7, sigma2 = 1, tau2 = 0.25
), intervals = data.frame(
  target = rep(c("alpha", "beta"), each = 2L),
  level = rep(c(0.90, 0.95), 2L),
  lower = c(0, 0.1, -0.1, 0),
  upper = c(2, 1.3, 2.1, 1.4)
)) {
  gt_result(estimates, intervals)
}

test_that("RI scenario sizes define n and validate repeated groups", {
  expect_error(gt_scenario("bad-ri", family = "random_intercept"),
               "gt_ri_scenario")
  scalar <- gt_ri_scenario("scalar", n_groups = 3L, group_sizes = 4L)
  expect_identical(scalar$n, 12L)
  expect_identical(scalar$n_groups, 3L)
  expect_identical(scalar$group_sizes, c(4L, 4L, 4L))
  expect_identical(scalar$family, "random_intercept")

  unequal <- gt_ri_scenario("unequal", n_groups = 3L,
                            group_sizes = c(2L, 4L, 7L))
  expect_identical(unequal$n, 13L)
  expect_identical(unequal$n_groups, 3L)
  expect_error(gt_ri_scenario("too-few", n_groups = 1L), "groups")
  mixed_singletons <- gt_ri_scenario("mixed-singletons", n_groups = 3L,
                                     group_sizes = c(1L, 1L, 4L))
  expect_identical(mixed_singletons$n, 6L)
  expect_error(gt_ri_scenario("all-singletons", n_groups = 3L,
                              group_sizes = c(1L, 1L, 1L)), "At least one group")
  expect_error(gt_ri_scenario("wrong-length", n_groups = 3L,
                              group_sizes = c(2L, 3L)), "length")
  expect_error(gt_ri_scenario("fractional", group_sizes = 2.5), "integer")
  expect_error(gt_ri_scenario("bad-sigma", sigma = 0), "sigma")
  expect_error(gt_ri_scenario("bad-tau", tau = -1), "tau")
})

test_that("gt_result accepts generic nonempty named finite estimates", {
  good <- ri_point_result()
  expect_named(good$estimates, c("alpha", "beta", "sigma2", "tau2"))
  expect_named(gt_result(c(alpha = 1, beta = 2))$estimates, c("alpha", "beta"))
  expect_named(gt_result(c(alpha = 1, beta = 2, sigma2 = 1))$estimates,
               c("alpha", "beta", "sigma2"))
  expect_named(gt_result(c(alpha = 1, beta = 2, sigma2 = 1,
                           tau2 = 1, extra = 0))$estimates,
               c("alpha", "beta", "sigma2", "tau2", "extra"))
  expect_error(gt_result(c(alpha = 1, beta = 2, sigma2 = 1,
                           tau2 = Inf)), "finite named numeric")
  expect_error(gt_result(setNames(numeric(), character())), "finite named numeric")
})

test_that("RI generator and callback use deterministic canonical groups", {
  seen <- NULL
  capture <- gt_adapter("capture", function(data) {
    seen <<- data
    ri_point_result()
  }, family = "random_intercept")
  scenario <- gt_ri_scenario("groups", n_groups = 3L,
                             group_sizes = c(2L, 4L, 3L))
  first <- gt_runstudy(scenario, adapters = capture, reps = 1L, seed = "871")
  first_data <- first$data
  expect_identical(names(seen), c("x", "y", "group"))
  expect_identical(sort(unique(first_data$group)), 1:3)
  expect_identical(as.integer(table(first_data$group)), c(2L, 4L, 3L))
  expect_identical(names(first_data), c("scenario", "rep", "row", "x", "y", "group"))
  expect_identical(attr(seen, "truth"), NULL)

  second <- gt_runstudy(scenario, adapters = capture, reps = 1L, seed = "871")
  expect_identical(first_data, second$data)
})

test_that("RI native adapter uses ML and returns variance targets on variance scale", {
  testthat::skip_if_not_installed("lme4")
  path <- ri_fixture_path("lmm-interior-data.csv")
  data <- utils::read.csv(path, stringsAsFactors = FALSE)[c("x", "y", "group")]
  fit <- gt_lmm_adapter()$fit(data)
  expect_named(fit$estimates, c("alpha", "beta", "sigma2", "tau2"))
  expect_true(all(is.finite(fit$estimates)))
  expect_gt(fit$estimates[["sigma2"]], 0)
  expect_true(fit$estimates[["tau2"]] >= 0)
  expect_true(fit$converged)
  expect_identical(fit$diagnostics$optimizer, "bobyqa")
  expect_false(fit$diagnostics$REML)
  expect_setequal(unique(fit$intervals$target), c("alpha", "beta"))
  expect_setequal(unique(fit$intervals$level), c(0.90, 0.95))
  expect_false(any(c("sigma2", "tau2") %in% fit$intervals$target))
})

test_that("zero random-intercept variance is accepted as a boundary estimate", {
  testthat::skip_if_not_installed("lme4")
  path <- ri_fixture_path("lmm-boundary-fixture-data.csv")
  data <- utils::read.csv(path, stringsAsFactors = FALSE)[c("x", "y", "group")]
  fit <- gt_lmm_adapter()$fit(data)
  expect_true(fit$converged)
  expect_true(all(is.finite(fit$estimates)))
  expect_true(fit$estimates[["sigma2"]] > 0)
  expect_true(fit$estimates[["tau2"]] >= 0)
  expect_lte(fit$estimates[["tau2"]], 1e-8)
  expect_true(is.logical(fit$diagnostics$singular))
})

test_that("residual-degenerate random-intercept data fail explicitly", {
  testthat::skip_if_not_installed("lme4")
  path <- ri_fixture_path("residual-degenerate-data.csv")
  data <- utils::read.csv(path, stringsAsFactors = FALSE)[c("x", "y", "group")]
  expect_error(gt_lmm_adapter()$fit(data), "Residual-degenerate")
})

test_that("residual-degeneracy detection is stable to a large response offset", {
  exact <- data.frame(
    x = rep(c(-1, 0, 1), 3L),
    group = rep(1:3, each = 3L)
  )
  exact$y <- 2 + 0.5 * exact$x + c(-2, 0, 3)[exact$group]
  expect_true(groundtruth:::.gt_lmm_residual_degenerate(exact))

  noisy <- utils::read.csv(ri_fixture_path("lmm-interior-data.csv"),
                           stringsAsFactors = FALSE)[c("x", "y", "group")]
  noisy$y <- noisy$y + 1e14
  expect_false(groundtruth:::.gt_lmm_residual_degenerate(noisy))
})

test_that("RI runner defaults to four targets with one point-only row per variance", {
  fixed <- gt_adapter("lmer-stub", function(data) ri_point_result(),
                      family = "random_intercept")
  study <- gt_runstudy(
    gt_ri_scenario("ri", n_groups = 3L, group_sizes = 4L),
    adapters = fixed, reps = 1L, seed = "117"
  )
  expect_identical(sort(unique(study$targets$target)),
                   c("alpha", "beta", "sigma2", "tau2"))
  variance <- study$targets[study$targets$target %in% c("sigma2", "tau2"), ]
  expect_equal(nrow(variance), 2L)
  expect_true(all(is.na(variance$level)))
  expect_true(all(variance$interval_expected %in% FALSE))
  expect_true(all(is.na(variance$lower) & is.na(variance$upper)))
  expect_true(all(is.finite(variance$estimate)))
  summary <- gt_summarize(study)
  variance_summary <- summary[summary$target %in% c("sigma2", "tau2"), ]
  expect_true(all(is.na(variance_summary$coverage)))
  expect_true(all(is.na(variance_summary$covered_per_attempt)))
  expect_true(all(variance_summary$intervals_expected == 0L))
  expect_true(all(variance_summary$usable_intervals == 0L))
  expect_true(all(variance_summary$unusable_intervals == 0L))
})

test_that("RI runner rejects missing or extra family estimates as whole-fit errors", {
  bad_results <- list(
    gt_result(c(alpha = 1, beta = 0.7, sigma2 = 1)),
    gt_result(c(alpha = 1, beta = 0.7, sigma2 = 1, tau2 = 0.25,
                extra = 0))
  )
  for (i in seq_along(bad_results)) {
    adapter <- gt_adapter(paste0("bad", i), function(data) bad_results[[i]],
                          family = "random_intercept")
    study <- gt_runstudy(gt_ri_scenario("ri", n_groups = 3L,
                                       group_sizes = 4L), adapter,
                         reps = 1L, seed = as.character(700 + i))
    expect_identical(study$ledger$status, "invalid_estimate")
    expect_true(all(is.na(study$targets$estimate)))
  }
})

test_that("RI variance domains are checked after generic result construction", {
  for (estimates in list(
    c(alpha = 1, beta = 0.7, sigma2 = 0, tau2 = 0.25),
    c(alpha = 1, beta = 0.7, sigma2 = 1, tau2 = -0.25)
  )) {
    adapter <- gt_adapter("bad-variance", function(data) ri_point_result(estimates),
                          family = "random_intercept")
    study <- gt_runstudy(gt_ri_scenario("ri", n_groups = 3L,
                                       group_sizes = 4L), adapter,
                         reps = 1L, seed = "730")
    expect_identical(study$ledger$status, "invalid_estimate")
    expect_true(all(is.na(study$targets$estimate)))
  }
})

test_that("target subsets are selected per family and survive replay", {
  mixed <- gt_runstudy(
    list(gt_scenario("g", n = 12L),
         gt_ri_scenario("ri", n_groups = 3L, group_sizes = 4L)),
    adapters = list(gt_adapter("lm"),
                    gt_adapter("ri", function(data) ri_point_result(),
                               family = "random_intercept")),
    reps = 1L, seed = "211"
  )
  ordinary <- mixed$targets[mixed$targets$scenario == "g", ]
  random_intercept <- mixed$targets[mixed$targets$scenario == "ri", ]
  expect_setequal(unique(ordinary$target), c("alpha", "beta"))
  expect_setequal(unique(random_intercept$target),
                  c("alpha", "beta", "sigma2", "tau2"))
  expect_identical(gt_replay(mixed)$targets, mixed$targets)
  expect_error(gt_runstudy(
    list(gt_scenario("g", n = 12L),
         gt_ri_scenario("ri", n_groups = 3L, group_sizes = 4L)),
    adapters = list(gt_adapter("lm"),
                    gt_adapter("ri", function(data) ri_point_result(),
                               family = "random_intercept")),
    reps = 1L, seed = "211", targets = "tau2"
  ), "every scenario")
})

test_that("legacy studies without recorded target maps replay their targets", {
  study <- gt_runstudy(gt_scenario("legacy", n = 12L), reps = 2L,
                       seed = "212")
  study$target_map <- NULL
  study$target_selection <- NULL
  replay <- gt_replay(study)
  expect_identical(replay$targets, study$targets)
  expect_setequal(unique(replay$targets$target), c("alpha", "beta"))
})

test_that("family mismatches fail before custom fitters are called", {
  called <- FALSE
  ri <- gt_adapter("ri", function(data) {
    called <<- TRUE
    ri_point_result()
  }, family = "random_intercept")
  study <- gt_runstudy(gt_scenario("ordinary", n = 12L), ri,
                       reps = 1L, seed = "315")
  expect_false(called)
  expect_identical(study$ledger$status, "fit_error")
  expect_true(all(is.na(study$targets$estimate)))
})

test_that("legacy ordinary-only studies retain their schemas and export v2", {
  study <- gt_runstudy(gt_scenario("ordinary", n = 12L), reps = 1L,
                       seed = "415")
  expect_identical(names(study$data), c("scenario", "rep", "row", "x", "y"))
  expect_false("interval_expected" %in% names(study$targets))
  path <- tempfile("groundtruth-legacy-")
  expect_invisible(gt_export(study, path))
  manifest <- dget(file.path(path, "manifest.R"))
  expect_identical(manifest$schema_version, "groundtruth-r-export-v2")
  expect_identical(names(utils::read.csv(file.path(path, "data.csv"))),
                   c("scenario", "rep", "row", "x", "y"))
  unlink(path, recursive = TRUE)
})

test_that("RI and mixed exports use v4 and preserve group labels", {
  mixed <- gt_runstudy(
    list(gt_scenario("ordinary", n = 12L),
         gt_ri_scenario("ri", n_groups = 3L, group_sizes = 4L)),
    adapters = list(gt_adapter("lm"),
                    gt_adapter("ri", function(data) ri_point_result(),
                               family = "random_intercept")),
    reps = 1L, seed = "516"
  )
  expect_identical(names(mixed$data),
                   c("scenario", "rep", "row", "x", "y", "group"))
  expect_true(all(is.na(mixed$data$group[mixed$data$scenario == "ordinary"])))
  expect_setequal(unique(stats::na.omit(mixed$data$group)), 1:3)
  path <- tempfile("groundtruth-ri-")
  expect_invisible(gt_export(mixed, path))
  manifest <- dget(file.path(path, "manifest.R"))
  expect_identical(manifest$schema_version, "groundtruth-r-export-v4")
  expect_identical(manifest$generator_revision, "random-intercept-v2")
  expect_identical(manifest$rng_algorithm, "groundtruth-r-rng-v1")
  exported_scenarios <- utils::read.csv(file.path(path, "scenarios.csv"))
  expect_identical(exported_scenarios$x_level[
    exported_scenarios$family == "random_intercept"
  ], "observation")
  expect_identical(names(utils::read.csv(file.path(path, "data.csv"))),
                   names(mixed$data))
  expect_setequal(manifest$target_metadata$name,
                  c("alpha", "beta", "sigma2", "tau2"))
  unlink(path, recursive = TRUE)
})

test_that("point-only targets reject supplied intervals", {
  estimates <- c(alpha = 1, beta = 0.7, sigma2 = 1, tau2 = 0.25)
  intervals <- rbind(
    data.frame(target = rep(c("alpha", "beta"), each = 2L),
               level = rep(c(0.90, 0.95), 2L),
               lower = c(0, 0.1, -0.1, 0), upper = c(2, 1.3, 2.1, 1.4)),
    data.frame(target = "tau2", level = 0.95, lower = 0, upper = 0.5)
  )
  adapter <- gt_adapter("bad-ri-interval", function(data) {
    gt_result(estimates, intervals)
  }, family = "random_intercept")
  study <- gt_runstudy(gt_ri_scenario("ri", n_groups = 3L,
                                     group_sizes = 4L), adapter,
                       reps = 1L, seed = "617")
  expect_identical(study$ledger$status, "invalid_estimate")
  expect_true(all(is.na(study$targets$estimate)))
})

test_that("RI runner rejects intervals for undeclared targets", {
  estimates <- c(alpha = 1, beta = 0.7, sigma2 = 1, tau2 = 0.25)
  intervals <- data.frame(
    target = c("alpha", "beta", "gamma"),
    level = c(0.90, 0.90, 0.90),
    lower = c(0, 0.1, -1),
    upper = c(2, 1.3, 1)
  )
  adapter <- gt_adapter("unknown-interval-target", function(data) {
    gt_result(estimates, intervals)
  }, family = "random_intercept")
  study <- gt_runstudy(gt_ri_scenario("ri", n_groups = 3L,
                                     group_sizes = 4L), adapter,
                       reps = 1L, seed = "618")
  expect_identical(study$ledger$status, "invalid_estimate")
  expect_true(all(is.na(study$targets$estimate)))
})

test_that("coefficient interval failure retains all point estimates", {
  partial <- gt_result(
    c(alpha = 1, beta = 0.7, sigma2 = 1, tau2 = 0.25),
    intervals = data.frame(
      target = "alpha", level = 0.90, lower = 0, upper = 2
    )
  )
  adapter <- gt_adapter("partial-ci", function(data) partial,
                        family = "random_intercept")
  study <- gt_runstudy(gt_ri_scenario("ri", n_groups = 3L,
                                     group_sizes = 4L), adapter,
                       reps = 1L, seed = "718")
  expect_identical(study$ledger$status, "ok")
  expect_true(all(is.finite(study$targets$estimate)))
  expect_equal(sum(study$targets$usable_interval), 1L)
  expect_true(all(study$targets$interval_expected[
    study$targets$target %in% c("alpha", "beta")
  ]))
  expect_true(all(!study$targets$interval_expected[
    study$targets$target %in% c("sigma2", "tau2")
  ]))
})

test_that("RI summaries keep point and interval denominators separate", {
  call <- 0L
  adapter <- gt_adapter("partial-attempts", function(data) {
    call <<- call + 1L
    if (call == 4L) stop("planned fit failure")
    estimates <- c(alpha = 1, beta = 0.7, sigma2 = 1, tau2 = 0.25)
    if (call == 3L) return(gt_result(estimates))
    bounds <- if (call == 1L) {
      list(alpha = c(0, 2), beta = c(0.2, 1.2))
    } else {
      list(alpha = c(3, 4), beta = c(2, 3))
    }
    intervals <- do.call(rbind, lapply(c(0.90, 0.95), function(level) {
      do.call(rbind, lapply(c("alpha", "beta"), function(target) {
        data.frame(target = target, level = level,
                   lower = bounds[[target]][1], upper = bounds[[target]][2])
      }))
    }))
    gt_result(estimates, intervals)
  }, family = "random_intercept")
  study <- gt_runstudy(gt_ri_scenario("ri", n_groups = 3L,
                                      group_sizes = 4L), adapter,
                       reps = 4L, seed = "720")
  summary <- gt_summarize(study)
  coefficient <- summary[summary$target %in% c("alpha", "beta"), ]
  expect_equal(nrow(coefficient), 4L)
  expect_true(all(coefficient$attempted == 4L))
  expect_true(all(coefficient$successful == 3L & coefficient$failed == 1L))
  expect_true(all(coefficient$intervals_expected == 4L))
  expect_true(all(coefficient$usable_intervals == 2L))
  expect_true(all(coefficient$unusable_intervals == 2L))
  expect_true(all(coefficient$coverage == 0.5))
  expect_true(all(coefficient$covered_per_attempt == 0.25))

  variance <- summary[summary$target %in% c("sigma2", "tau2"), ]
  expect_true(all(variance$attempted == 4L))
  expect_true(all(variance$successful == 3L & variance$failed == 1L))
  expect_true(all(variance$intervals_expected == 0L))
  expect_true(all(variance$usable_intervals == 0L &
                    variance$unusable_intervals == 0L))
  expect_true(all(is.na(variance$coverage) &
                    is.na(variance$covered_per_attempt)))
})

test_that("all-failed RI fits retain point and interval denominators honestly", {
  adapter <- gt_adapter("fail", function(data) stop("fit failed"),
                        family = "random_intercept")
  study <- gt_runstudy(gt_ri_scenario("ri", n_groups = 3L,
                                     group_sizes = 4L), adapter,
                       reps = 1L, seed = "719")
  expect_identical(study$ledger$status, "fit_error")
  expect_equal(nrow(study$targets), 6L)
  expect_true(all(is.na(study$targets$estimate)))
  summary <- gt_summarize(study)
  variance <- summary[summary$target %in% c("sigma2", "tau2"), ]
  expect_true(all(variance$attempted == 1L & variance$failed == 1L))
  expect_true(all(variance$intervals_expected == 0L))
  expect_true(all(variance$unusable_intervals == 0L))
  expect_true(all(is.na(variance$coverage)))
  expect_true(all(is.na(variance$covered_per_attempt)))
})
