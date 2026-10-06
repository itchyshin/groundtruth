gt_fixture_path <- function(name) {
  path <- testthat::test_path("fixtures", name)
  testthat::expect_true(
    file.exists(path),
    info = paste("missing frozen fixture", name)
  )
  path
}

gt_check_fixture_hash <- function(name) {
  json <- paste(
    readLines(gt_fixture_path("freeze.json"), warn = FALSE),
    collapse = "\n"
  )
  key <- paste0(
    '"',
    gsub("([.])", "\\\\\\1", name),
    '"[[:space:]]*:[[:space:]]*"([0-9a-f]{64})"'
  )
  hit <- regexec(key, json, perl = TRUE)
  frozen <- regmatches(json, hit)[[1L]][2L]
  testthat::expect_true(
    length(frozen) == 1L && nzchar(frozen),
    info = paste("fixture hash missing:", name)
  )
  got <- digest::digest(
    file = gt_fixture_path(name),
    algo = "sha256",
    serialize = FALSE
  )
  testthat::expect_identical(
    got,
    frozen,
    info = paste("frozen fixture changed:", name)
  )
  invisible(got)
}

gt_oracle <- function(data, levels = c(.90, .95)) {
  stopifnot(is.data.frame(data), all(c("x", "y") %in% names(data)))
  x <- as.numeric(data$x)
  y <- as.numeric(data$y)
  n <- length(x)
  stopifnot(n > 2L, length(y) == n, all(is.finite(x)), all(is.finite(y)))
  xc <- x - mean(x)
  yc <- y - mean(y)
  sxx <- sum(xc * xc)
  stopifnot(is.finite(sxx), sxx > 0)
  beta <- sum(xc * yc) / sxx
  alpha <- mean(y) - beta * mean(x)
  residuals <- y - alpha - beta * x
  sse <- sum(residuals * residuals)
  df <- n - 2L
  sigma2 <- sse / df
  covariance <- sigma2 *
    matrix(
      c(
        1 / n + mean(x)^2 / sxx,
        -mean(x) / sxx,
        -mean(x) / sxx,
        1 / sxx
      ),
      2L,
      2L,
      dimnames = list(c("alpha", "beta"), c("alpha", "beta"))
    )
  se <- sqrt(diag(covariance))
  estimates <- c(alpha = alpha, beta = beta)
  intervals <- do.call(
    rbind,
    lapply(levels, function(level) {
      q <- stats::qt((1 + level) / 2, df = df)
      data.frame(
        target = names(estimates),
        level = level,
        lower = estimates - q * se,
        upper = estimates + q * se,
        row.names = NULL
      )
    })
  )
  list(
    estimates = estimates,
    sse = sse,
    sigma2 = sigma2,
    covariance = covariance,
    se = se,
    residuals = residuals,
    predictions = alpha + beta * x,
    intervals = intervals,
    df = df
  )
}

gt_close <- function(actual, expected, atol = 1e-10, rtol = 1e-10) {
  length(actual) > 0L &&
    length(actual) == length(expected) &&
    identical(dim(actual), dim(expected)) &&
    all(is.finite(actual)) &&
    all(is.finite(expected)) &&
    all(abs(actual - expected) <= atol + rtol * abs(expected))
}

gt_assert_fit <- function(fit, data, atol = 1e-10, rtol = 1e-10) {
  oracle <- gt_oracle(data)
  testthat::expect_s3_class(fit, "gt_result")
  testthat::expect_setequal(names(fit$estimates), c("alpha", "beta"))
  testthat::expect_true(gt_close(
    unname(fit$estimates[c("alpha", "beta")]),
    unname(oracle$estimates),
    atol,
    rtol
  ))
  d <- fit$diagnostics
  testthat::expect_true(gt_close(d$sigma2, oracle$sigma2, atol, rtol))
  testthat::expect_true(gt_close(
    as.numeric(d$covariance[c("alpha", "beta"), c("alpha", "beta")]),
    as.numeric(oracle$covariance),
    atol,
    rtol
  ))
  testthat::expect_true(gt_close(
    unname(d$se[c("alpha", "beta")]),
    unname(oracle$se),
    atol,
    rtol
  ))
  got <- fit$intervals[
    order(fit$intervals$level, fit$intervals$target),
    c("target", "level", "lower", "upper")
  ]
  want <- oracle$intervals[
    order(oracle$intervals$level, oracle$intervals$target),
    c("target", "level", "lower", "upper")
  ]
  testthat::expect_identical(
    as.character(got$target),
    as.character(want$target)
  )
  testthat::expect_identical(as.numeric(got$level), as.numeric(want$level))
  testthat::expect_true(gt_close(
    as.matrix(got[, c("lower", "upper")]),
    as.matrix(want[, c("lower", "upper")]),
    atol,
    rtol
  ))
  invisible(oracle)
}

gt_study <- function(
  reps = 1L,
  seed = "20261004",
  adapters = list(groundtruth::gt_adapter()),
  scenario = groundtruth::gt_scenario(
    "s",
    n = 5L,
    alpha = 1,
    beta = 2,
    sigma = 1
  )
) {
  groundtruth::gt_runstudy(
    scenario,
    adapters = adapters,
    reps = reps,
    seed = seed
  )
}

gt_attempts <- function(study) {
  study$ledger[
    order(study$ledger$scenario, study$ledger$rep, study$ledger$adapter),
    ,
    drop = FALSE
  ]
}
