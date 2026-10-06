test_that("fixtures retain bytes and character fields", {
  for (name in c("analytic.csv", "rep1-data.csv", "rep1-julia.csv")) {
    gt_check_fixture_hash(name)
  }
  julia <- utils::read.csv(
    gt_fixture_path("rep1-julia.csv"),
    colClasses = "character",
    stringsAsFactors = FALSE
  )
  expect_identical(names(julia), c("target", "estimate", "lower", "upper"))
  expect_true(all(vapply(julia, is.character, logical(1))))
  expect_equal(julia$target, c("alpha", "beta"))
})

test_that("default fit agrees with scalar OLS and Julia", {
  data <- utils::read.csv(
    gt_fixture_path("analytic.csv"),
    stringsAsFactors = FALSE
  )
  fit <- gt_adapter()$fit(data)
  oracle <- gt_assert_fit(fit, data)
  expect_equal(unname(oracle$estimates), c(1, 2), tolerance = 1e-12)
  expect_equal(oracle$sse, 14, tolerance = 1e-12)
  expect_equal(oracle$sigma2, 14 / 3, tolerance = 1e-12)
  expect_equal(
    unname(diag(oracle$covariance)),
    c(14 / 15, 7 / 15),
    tolerance = 1e-12
  )
  retained <- utils::read.csv(
    gt_fixture_path("rep1-data.csv"),
    stringsAsFactors = FALSE
  )[c("x", "y")]
  julia <- utils::read.csv(
    gt_fixture_path("rep1-julia.csv"),
    colClasses = "character",
    stringsAsFactors = FALSE
  )
  got <- gt_adapter()$fit(retained)
  gt_assert_fit(got, retained)
  expect_true(gt_close(
    unname(got$estimates[julia$target]),
    as.numeric(julia$estimate)
  ))
  interval95 <- got$intervals[got$intervals$level == .95, ]
  interval95 <- interval95[match(julia$target, interval95$target), ]
  expect_true(gt_close(interval95$lower, as.numeric(julia$lower)))
  expect_true(gt_close(interval95$upper, as.numeric(julia$upper)))
})

test_that("OLS has specified shift scale and row-order invariances", {
  d <- utils::read.csv(gt_fixture_path("analytic.csv"))
  base <- gt_adapter()$fit(d)
  shifted <- d
  shifted$x <- shifted$x + 3
  scaled <- d
  scaled$x <- scaled$x * .01
  expect_true(gt_close(
    gt_adapter()$fit(shifted)$estimates[c("alpha", "beta")],
    c(-5, 2)
  ))
  expect_true(gt_close(
    gt_adapter()$fit(scaled)$estimates[c("alpha", "beta")],
    c(1, 200)
  ))
  expect_true(gt_close(
    gt_adapter()$fit(shifted)$diagnostics$predictions,
    base$diagnostics$predictions
  ))
  expect_true(gt_close(
    gt_adapter()$fit(d[5:1, ])$estimates[c("alpha", "beta")],
    base$estimates[c("alpha", "beta")]
  ))
})

test_that("independent oracle rejects numerical mutants", {
  d <- utils::read.csv(gt_fixture_path("analytic.csv"))
  o <- gt_oracle(d)
  swapped <- c(alpha = o$estimates[["beta"]], beta = o$estimates[["alpha"]])
  expect_false(gt_close(swapped, o$estimates))
  normal <- o$estimates +
    cbind(lower = -stats::qnorm(.975) * o$se, upper = stats::qnorm(.975) * o$se)
  t95 <- o$intervals[o$intervals$level == .95, c("lower", "upper")]
  expect_false(gt_close(as.matrix(normal), as.matrix(t95)))
  expect_false(gt_close(
    (o$sse / nrow(d)) * solve(crossprod(cbind(1, d$x))),
    o$covariance
  ))
  mutant <- gt_adapter()$fit(d)
  mutant$estimates[] <- unname(mutant$estimates[c(2L, 1L)])
  expect_error(gt_assert_fit(mutant, d))
})


test_that("fit checker detects normal and SSE/n mutants", {
  d <- utils::read.csv(gt_fixture_path("analytic.csv"))
  base <- gt_adapter()$fit(d)
  normal <- base
  for (lev in c(.9, .95)) {
    i <- normal$intervals$level == lev
    row <- normal$intervals$target[i]
    normal$intervals$lower[i] <- normal$estimates[row] -
      stats::qnorm((1 + lev) / 2) * normal$diagnostics$se[row]
    normal$intervals$upper[i] <- normal$estimates[row] +
      stats::qnorm((1 + lev) / 2) * normal$diagnostics$se[row]
  }
  expect_error(gt_assert_fit(normal, d))
  mle <- base
  ratio <- (nrow(d) - 2) / nrow(d)
  mle$diagnostics$sigma2 <- mle$diagnostics$sigma2 * ratio
  mle$diagnostics$covariance <- mle$diagnostics$covariance * ratio
  mle$diagnostics$se <- mle$diagnostics$se * sqrt(ratio)
  expect_error(gt_assert_fit(mle, d))
  permuted <- d[c(3, 5, 1, 4, 2), ]
  expect_error(gt_assert_fit(gt_adapter()$fit(permuted), permuted), NA)
  expect_false(gt_close(numeric(), 1))
})
