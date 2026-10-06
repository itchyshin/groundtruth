test_that("summary keeps point and interval denominators", {
  i <- 0L
  scripted <- gt_adapter("scripted", function(data) {
    i <<- i + 1L
    if (i == 4L) {
      return(gt_result(c(alpha = 1, beta = 2), converged = FALSE))
    }
    ints <- data.frame(
      target = rep(c("alpha", "beta"), each = 2L),
      level = rep(c(.9, .95), 2L),
      lower = c(-1, -1, 0, 0),
      upper = c(3, 3, 4, 4)
    )
    if (i == 2L) {
      ints$lower <- c(2, 2, 3, 3)
    }
    if (i == 3L) {
      ints$lower[] <- NA_real_
    }
    gt_result(
      c(alpha = 1 + c(-1, 2, 3)[i], beta = 2 + c(2, -1, 1)[i]),
      intervals = ints
    )
  })
  s <- gt_study(reps = 4L, adapters = list(scripted))
  z <- gt_summarize(s)
  expect_true(all(z$attempted == 4L))
  expect_true(all(z$successful == 3L))
  expect_true(all(z$failed == 1L))
  alpha90 <- z[z$target == "alpha" & z$level == .9, ]
  expect_equal(alpha90$usable_intervals, 2L)
  expect_equal(alpha90$unusable_intervals, 2L)
  expect_true(all(z$usable_intervals == 2L))
  expect_true(all(z$unusable_intervals == 2L))
  expect_equal(z$coverage, rep(.5, 4), tolerance = 1e-12)
  expect_equal(z$covered_per_attempt, rep(.25, 4), tolerance = 1e-12)
  for (target in c("alpha", "beta")) {
    e <- if (target == "alpha") c(-1, 2, 3) else c(2, -1, 1)
    row <- z[z$target == target, ]
    expect_equal(row$bias, rep(mean(e), 2), tolerance = 1e-12)
    expect_equal(
      row$bias_mcse,
      rep(stats::sd(e) / sqrt(3), 2),
      tolerance = 1e-12
    )
    rmse <- sqrt(mean(e^2))
    expect_equal(row$rmse, rep(rmse, 2), tolerance = 1e-12)
    expect_equal(
      row$rmse_mcse,
      rep(stats::sd(e^2) / (2 * sqrt(3) * rmse), 2),
      tolerance = 1e-12
    )
  }
  critical <- stats::qnorm(.975)
  m <- 2
  p <- .5
  center <- (p + critical^2 / (2 * m)) / (1 + critical^2 / m)
  half <- critical *
    sqrt(p * (1 - p) / m + critical^2 / (4 * m^2)) /
    (1 + critical^2 / m)
  expect_equal(z$coverage_mc_lower, rep(center - half, 4), tolerance = 1e-12)
  expect_equal(z$coverage_mc_upper, rep(center + half, 4), tolerance = 1e-12)
})

test_that("summary handles all failed singleton and zero-error cases", {
  failed <- gt_adapter("failed", function(data) stop("no"))
  sf <- gt_study(adapters = list(failed))
  zf <- gt_summarize(sf)
  expect_true(all(zf$covered_per_attempt == 0))
  expect_true(all(is.na(zf$bias)))
  expect_true(all(is.na(zf$rmse)))
  expect_true(all(is.na(zf$coverage)))
  one <- gt_adapter("one", function(data) {
    gt_result(
      c(alpha = 1, beta = 2),
      intervals = data.frame(
        target = c("alpha", "beta"),
        level = .95,
        lower = c(0, 1),
        upper = c(2, 3)
      )
    )
  })
  so <- gt_study(adapters = list(one))
  zo <- gt_summarize(so)
  expect_true(all(is.na(zo$bias_mcse)))
  expect_true(all(is.na(zo$rmse_mcse)))
  expect_true(all(is.na(zo$coverage_mcse)))
  sz <- gt_study(reps = 2L, adapters = list(one))
  zz <- gt_summarize(sz)
  expect_true(all(zz$rmse == 0))
  expect_true(all(zz$rmse_mcse == 0))
})
