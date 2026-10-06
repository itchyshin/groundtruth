test_that("runner generates once and isolates adapters", {
  seen <- list()
  mutator <- gt_adapter("mutate", function(data) {
    data$x[] <- 999
    gt_result(c(alpha = 1, beta = 2))
  })
  recorder <- gt_adapter("record", function(data) {
    seen[[length(seen) + 1L]] <<- data
    gt_adapter()$fit(data)
  })
  study <- gt_study(reps = 2L, adapters = list(mutator, recorder))
  expect_equal(nrow(study$data), 10L)
  expect_equal(nrow(study$ledger), 4L)
  expect_equal(sort(unique(study$ledger$status)), "ok")
  expect_equal(length(seen), 2L)
  expect_false(any(seen[[1]]$x == 999))
  expect_identical(as.integer(table(study$ledger$rep)), c(2L, 2L))
})

test_that("runner rejects bad scenarios data and malformed result names", {
  expect_error(gt_scenario("", n = 5L))
  expect_error(gt_scenario("x", n = 2L))
  expect_error(gt_runstudy(list(
    gt_scenario("x", n = 5L),
    gt_scenario("x", n = 5L)
  )))
  bad_data <- gt_adapter("bad-data", function(data) {
    data$y <- NA_real_
    gt_adapter()$fit(data)
  })
  study <- gt_study(adapters = list(bad_data))
  expect_identical(study$ledger$status, "fit_error")
  for (bad in list(
    c(alpha = 1, beta = 2, gamma = 3),
    c(alpha = 1),
    c(alpha = 1, beta = Inf),
    c(alpha = 1, alpha = 2)
  )) {
    a <- gt_adapter("bad", function(data) gt_result(bad))
    s <- gt_study(adapters = list(a))
    expect_identical(s$ledger$status, "fit_error")
    expect_true(all(is.na(s$targets$estimate)))
  }
})

test_that("runner records each failure type", {
  boom <- gt_adapter("boom", function(data) stop("boom"))
  nc <- gt_adapter("nc", function(data) {
    gt_result(c(alpha = 1, beta = 2), converged = FALSE)
  })
  rank1 <- gt_adapter("rank1", function(data) {
    gt_adapter()$fit(data.frame(x = 1, y = data$y))
  })
  s <- gt_study(adapters = list(boom, nc, rank1))
  expect_equal(s$ledger$status, c("fit_error", "nonconverged", "fit_error"))
  expect_equal(nrow(s$targets), 12L)
  expect_true(all(is.na(s$targets$covered)))
})


test_that("forged results reject both targets", {
  for (bad in list(
    c(alpha = 1),
    c(alpha = Inf, beta = 2),
    c(alpha = 1, alpha = 2),
    c(alpha = 1, beta = 2, gamma = 3)
  )) {
    a <- gt_adapter("forged", function(data) {
      structure(
        list(
          estimates = bad,
          intervals = NULL,
          converged = TRUE,
          message = ""
        ),
        class = "gt_result"
      )
    })
    study <- gt_study(adapters = a)
    expect_identical(study$ledger$status, "invalid_estimate")
    expect_true(all(is.na(study$targets$estimate)))
  }
})

test_that("default fit requires finite rank-two data", {
  fit <- gt_adapter()$fit
  expect_error(fit(data.frame(x = rep(1, 5), y = 1:5)))
  expect_error(fit(data.frame(x = c(1, 2, NA, 4, 5), y = 1:5)))
  expect_error(fit(data.frame(x = 1:5, y = c(1, 2, Inf, 4, 5))))
  expect_error(fit(data.frame(x = 1:2, y = 1:2)))
  expect_error(fit(data.frame(x = factor(letters[1:5]), y = 1:5)))
  expect_error(fit(data.frame(x = letters[1:5], y = 1:5)))
  matrix_x <- data.frame(y = 1:5)
  matrix_x$x <- I(matrix(1:10, 5, 2))
  matrix_x <- matrix_x[c("x", "y")]
  expect_error(fit(matrix_x))
})


test_that("Gaussian parameters and point estimates are finite real numbers", {
  expect_error(gt_scenario("complex", alpha = 1 + 1i))
  expect_error(gt_scenario("complex", beta = 1 + 1i))
  expect_error(gt_scenario("complex", sigma = 1 + 1i))
  expect_error(gt_result(c(alpha = 1 + 1i, beta = 2)))
})

test_that("generator overflow is a generation failure shared by all adapters", {
  study <- gt_runstudy(
    gt_scenario(
      "overflow",
      n = 10,
      alpha = .Machine$double.xmax,
      beta = .Machine$double.xmax,
      sigma = .Machine$double.xmax
    ),
    adapters = list(
      gt_adapter(),
      gt_adapter("point", function(data) gt_result(c(alpha = 1, beta = 2)))
    ),
    reps = 1,
    seed = "1"
  )
  expect_identical(
    study$ledger$status,
    c("generation_error", "generation_error")
  )
  expect_true(all(is.na(study$targets$estimate)))
})


test_that("point-only fits have an honest missing-interval diagnostic", {
  point <- gt_adapter("point", function(data) gt_result(c(alpha = 1, beta = 2)))
  study <- gt_study(adapters = list(point))
  expect_identical(study$ledger$status, "ok")
  expect_match(study$ledger$message, "Intervals not supplied")
  expect_false(grepl("Malformed", study$ledger$message))
})
