logistic_fixture_path <- function(name) {
  path <- testthat::test_path("fixtures", "logistic", name)
  testthat::expect_true(file.exists(path), info = paste("missing logistic fixture", name))
  path
}

test_that("logistic inputs enforce the frozen scenario and adapter contract", {
  s <- gt_scenario("legacy")
  expect_identical(s$family, "gaussian")
  expect_equal(s$sigma, 1.2)
  expect_null(gt_scenario("log", family = "logistic")$sigma)
  expect_error(gt_scenario("bad", family = "poisson"), "family")
  expect_error(gt_scenario("bad", family = "logistic", sigma = 1.2), "sigma")
  expect_error(gt_adapter(family = "logistic"), "gt_glm_adapter")
  expect_identical(gt_glm_adapter()$family, "logistic")
  legacy <- structure(list(id = "old", n = 8L, alpha = 1, beta = .7,
                          sigma = 1.2), class = "gt_scenario")
  expect_identical(groundtruth:::.gt_scenarios(legacy)[[1]]$family, "gaussian")
  old_adapter <- structure(list(id = "old", fit = gt_adapter()$fit,
                                metadata = list()), class = "gt_adapter")
  expect_identical(groundtruth:::.gt_adapters(old_adapter)[[1]]$family,
                   "gaussian")
})

test_that("native logistic MLE recovers frozen finite fixtures", {
  root <- logistic_fixture_path("analytic.csv")
  dat <- utils::read.csv(root)[c("x", "y")]
  fit <- gt_glm_adapter()$fit(dat)
  l0 <- log(0.2 / 0.8)
  l1 <- log(0.7 / 0.3)
  expected <- c(alpha = (l0 + l1) / 2, beta = (l1 - l0) / 2)
  expect_named(fit$estimates, c("alpha", "beta"))
  expect_equal(fit$estimates, expected, tolerance = 1e-10)
  expect_true(fit$converged)
  expect_identical(fit$diagnostics$inference, "final-information Wald")
  expect_true(all(c(.9, .95) %in% fit$intervals$level))
  expect_true(all(is.finite(fit$intervals$lower)))
  expect_true(all(is.finite(fit$intervals$upper)))
})

test_that("tied supports retain a finite MLE and exact score", {
  dat <- utils::read.csv(logistic_fixture_path("tied.csv"))[c("x", "y")]
  fit <- gt_glm_adapter()$fit(dat)
  expect_equal(unname(fit$estimates), c(-log(3), 0), tolerance = 1e-10)
  expect_true(max(abs(fit$diagnostics$score)) <= 1e-9)
})

test_that("shifted and reflected predictors transform the full covariance", {
  dat <- utils::read.csv(logistic_fixture_path("analytic.csv"))[c("x", "y")]
  original <- gt_glm_adapter()$fit(dat)
  shifted <- gt_glm_adapter()$fit(transform(dat, x = 3 - 100 * x))
  expect_equal(unname(shifted$estimates),
               c(unname(original$estimates["alpha"]) +
                   3 * unname(original$estimates["beta"]) / 100,
                 -unname(original$estimates["beta"]) / 100), tolerance = 1e-7)
  A <- matrix(c(1, 3 / 100, 0, -1 / 100), 2, byrow = TRUE)
  expected <- A %*% original$diagnostics$covariance %*% t(A)
  dimnames(expected) <- list(c("alpha", "beta"), c("alpha", "beta"))
  expect_identical(dimnames(shifted$diagnostics$covariance), dimnames(expected))
  expect_equal(shifted$diagnostics$covariance, expected, tolerance = 1e-7)
})

test_that("complete, quasi, constant, and one-class samples fail explicitly", {
  for (name in c("complete.csv", "quasi.csv", "constant.csv",
                 "all_zero.csv", "all_one.csv")) {
    dat <- utils::read.csv(logistic_fixture_path(name))[c("x", "y")]
    expect_error(gt_glm_adapter()$fit(dat), regexp =
      "separation|Constant predictor|All-zero outcomes|All-one outcomes")
  }
})

test_that("stable residuals and losses retain 40 and 700 logit tails", {
  eta <- c(40, -40, 700, -700)
  y <- c(1, 0, 1, 0)
  expected_residual <- c(plogis(-40), -plogis(-40),
                         plogis(-700), -plogis(-700))
  expected_loss <- c(log1p(exp(-40)), log1p(exp(-40)),
                     log1p(exp(-700)), log1p(exp(-700)))
  expect_equal(groundtruth:::.gt_logistic_residual(y, eta),
               expected_residual, tolerance = 0)
  expect_equal(groundtruth:::.gt_logistic_loss(y, eta),
               expected_loss, tolerance = 1e-15)
})

test_that("all finite frozen fixtures satisfy the final score checks", {
  for (name in c("retained.csv", "analytic.csv", "asymmetric.csv",
                 "inverted.csv", "scaled.csv", "shifted.csv", "tied.csv")) {
    dat <- utils::read.csv(logistic_fixture_path(name))[c("x", "y")]
    fit <- gt_glm_adapter()$fit(dat)
    expect_true(fit$converged, info = name)
    expect_true(all(is.finite(fit$estimates)), info = name)
    expect_true(max(abs(fit$diagnostics$score)) <= 1e-9, info = name)
    expect_true(max(abs(fit$diagnostics$linear_predictor_correction)) <= 1e-9,
                info = name)
  }
})

test_that("tiny x can retain points and alpha intervals when beta intervals overflow", {
  dat <- data.frame(
    x = c(-2, -1, 0, 1, 2) * 1e-310,
    y = c(0, 1, 0, 1, 0)
  )
  fit <- gt_glm_adapter()$fit(dat)
  expect_true(all(is.finite(fit$estimates)))
  expect_true(all(fit$intervals$target == "alpha"))
  expect_true(all(fit$intervals$level %in% c(.90, .95)))
  expect_true(all(is.finite(fit$intervals$lower)))
  expect_match(fit$message, "intervals omitted")
})

test_that("fixed iteration limit and malformed data fail clearly", {
  dat <- utils::read.csv(logistic_fixture_path("analytic.csv"))[c("x", "y")]
  expect_error(groundtruth:::.gt_glm_fit(dat, maxit = 0L), "converg|iteration")
  expect_error(groundtruth:::.gt_glm_fit(dat, maxit = 1L), "converg|iteration")
  expect_error(gt_glm_adapter()$fit(data.frame(x = 1:4, y = c(0, 1, 2, 1))),
               "0/1")
})

test_that("mixed-family matches run and mismatches fail before fitting", {
  gaussian_calls <- 0L
  gaussian_fit <- function(data) {
    gaussian_calls <<- gaussian_calls + 1L
    gt_result(c(alpha = 0, beta = 0))
  }
  fixed <- function(data) gt_result(c(alpha = 0, beta = 0))
  study <- gt_runstudy(
    list(gt_scenario("g", n = 10L), gt_scenario("b", n = 10L,
                                               family = "logistic")),
    adapters = list(gt_adapter("g-only", gaussian_fit),
                    gt_adapter("b-only", fixed, family = "logistic")),
    reps = 1L, seed = "44"
  )
  expect_equal(gaussian_calls, 1L)
  expect_equal(sum(study$ledger$status == "fit_error"), 2L)
  expect_equal(sum(study$ledger$status == "ok"), 2L)
  expect_equal(nrow(study$ledger), 4L)
  failed <- study$ledger[study$ledger$status == "fit_error", c("scenario", "adapter")]
  for (i in seq_len(nrow(failed))) {
    rows <- study$targets$scenario == failed$scenario[i] &
      study$targets$adapter == failed$adapter[i]
    expect_true(all(is.na(study$targets$estimate[rows])))
  }
  expect_equal(sum(is.finite(study$targets$estimate)), 8L)
})

test_that("logistic DGP is Bernoulli and adapter receives only x and y", {
  seen <- NULL
  capture <- gt_adapter("capture", function(data) {
    seen <<- data
    gt_result(c(alpha = 0, beta = 0))
  }, family = "logistic")
  s <- gt_scenario("log", n = 100L, alpha = 0, beta = 1,
                   family = "logistic")
  study <- gt_runstudy(s, list(capture), reps = 1L, seed = "55")
  expect_true(all(study$data$y %in% c(0, 1)))
  expect_identical(names(seen), c("x", "y"))
  expect_null(attr(seen, "family"))
  expect_identical(names(study$data), c("scenario", "rep", "row", "x", "y"))
})

test_that("mixed-family export adds metadata while retaining v2 core schemas", {
  study <- gt_runstudy(list(gt_scenario("g", n = 10L),
                            gt_scenario("b", n = 10L, family = "logistic")),
                        list(gt_adapter(), gt_glm_adapter()), reps = 1L,
                        seed = "66")
  path <- tempfile("export-v2-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  gt_export(study, path)
  manifest <- dget(file.path(path, "manifest.R"))
  expect_identical(manifest$schema_version, "groundtruth-r-export-v2")
  expect_identical(manifest$csv$data$columns,
                   c("scenario", "rep", "row", "x", "y"))
  scenarios <- utils::read.csv(file.path(path, "scenarios.csv"))
  expect_identical(scenarios$family, c("logistic", "gaussian"))
  expect_true(is.na(scenarios$sigma[scenarios$family == "logistic"]))
  glm_index <- which(vapply(manifest$adapters, function(a) identical(a$id, "glm"), logical(1)))
  expect_identical(manifest$adapters[[glm_index]]$family, "logistic")
})


test_that("logistic replay is deterministic and restores caller RNG state", {
  fixed <- gt_adapter("fixed", function(data) gt_result(c(alpha = 0, beta = 0)),
                      family = "logistic")
  s <- gt_scenario("binary", n = 20L, family = "logistic")
  set.seed(821)
  before <- .Random.seed
  study <- gt_runstudy(s, fixed, reps = 2L, seed = "821")
  expect_identical(.Random.seed, before)
  replay <- gt_replay(study)
  expect_identical(.Random.seed, before)
  expect_identical(study$data, replay$data)
  expect_identical(study$targets, replay$targets)
})
