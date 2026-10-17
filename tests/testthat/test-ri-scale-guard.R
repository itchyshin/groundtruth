scale_guard_fixture <- function() {
  data.frame(
    x = rep(c(0, 1, 2), 3L),
    group = rep(1:3, each = 3L),
    y = rep(c(0, 1.5, 3), 3L) + rep(c(0, 4, -2), each = 3L) +
      rep(c(0, 0.25, -0.1), 3L)
  )
}

test_that("residual degeneracy tolerance is relative to response scale", {
  base <- scale_guard_fixture()
  for (response_scale in c(1e-18, 1, 1e18)) {
    dat <- base
    dat$y <- dat$y * response_scale
    expect_false(groundtruth:::.gt_lmm_residual_degenerate(dat),
                 info = paste("scale", response_scale))

    reflected <- dat
    reflected$y <- -reflected$y
    expect_false(groundtruth:::.gt_lmm_residual_degenerate(reflected),
                 info = paste("reflected scale", response_scale))

    shifted <- dat
    shifted$y <- shifted$y + 10 * response_scale
    expect_false(groundtruth:::.gt_lmm_residual_degenerate(shifted),
                 info = paste("shifted scale", response_scale))
  }
})

test_that("exact group-plus-slope response is degenerate at tiny and ordinary scales", {
  dat <- scale_guard_fixture()
  dat$y <- 3 + 0.5 * dat$x + rep(c(0, 4, -2), each = 3L)
  expect_true(groundtruth:::.gt_lmm_residual_degenerate(dat))
  tiny <- dat
  tiny$y <- tiny$y * 1e-18
  expect_true(groundtruth:::.gt_lmm_residual_degenerate(tiny))
})

test_that("nonzero within-group residuals remain nondegenerate when dx is zero", {
  dat <- data.frame(
    x = rep(c(0, 1, 2), each = 2L),
    group = rep(1:3, each = 2L),
    y = c(0, 1, 0, 1.2, 0, 0.7) * 1e-18
  )
  expect_false(groundtruth:::.gt_lmm_residual_degenerate(dat))
  dat$y <- dat$y * 1e18
  expect_false(groundtruth:::.gt_lmm_residual_degenerate(dat))
})
