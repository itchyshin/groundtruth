test_that("gt_compare_inference validates schedules against ledger and reps", {
  row <- function(rep, adapter, usable, covered) {
    data.frame(
      scenario = "s", rep = rep, adapter = adapter, target = "alpha",
      level = 0.95, truth = 0, estimate = 0,
      lower = if (usable) -1 else NA_real_,
      upper = if (usable) 1 else NA_real_,
      usable_interval = usable,
      covered = if (usable) covered else NA,
      interval_expected = TRUE,
      stringsAsFactors = FALSE
    )
  }
  uncovered <- function(r) {
    r$lower <- 1
    r$upper <- 2
    r$covered <- FALSE
    r
  }
  study <- list(
    reps = 2L,
    targets = rbind(
      row(1L, "a", TRUE, TRUE),
      uncovered(row(1L, "b", TRUE, FALSE)),
      uncovered(row(2L, "a", TRUE, FALSE)),
      row(2L, "b", TRUE, TRUE)
    ),
    ledger = data.frame(
      scenario = rep("s", 4L),
      rep = rep(1:2, each = 2L),
      adapter = rep(c("a", "b"), times = 2L),
      stringsAsFactors = FALSE
    )
  )
  out <- gt_compare_inference(study, "a", "b")
  expect_true(out$schedule_verified)
  expect_identical(out$attempted, 2L)

  expect_error(gt_compare_inference(study, "a", "a"), "distinct")
  broken_cols <- study$targets[, c("scenario", "rep", "adapter", "target", "level")]
  expect_error(gt_compare_inference(list(targets = broken_cols), "a", "b"),
               "required columns")
  expect_error(
    gt_compare_inference(
      list(targets = study$targets[study$targets$adapter != "b", ]),
      "a", "b"
    ),
    "Both adapters"
  )

  bad_ledger <- study
  bad_ledger$ledger$rep[[1L]] <- 99L
  expect_error(gt_compare_inference(bad_ledger, "a", "b"), "ledger")
})

test_that("adapter factories reject malformed metadata and missing fitters", {
  expect_error(gt_glm_adapter(metadata = "not-a-list"), "metadata")
  expect_error(gt_lmm_adapter(metadata = 7), "metadata")
  expect_error(gt_adapter(family = "logistic"), "gt_glm_adapter")
  expect_error(gt_adapter(family = "random_intercept"), "gt_lmm_adapter")
})

test_that("gt_result validates convergence and diagnostics fields", {
  expect_error(
    gt_result(c(alpha = 1, beta = 2), intervals = "not-a-frame"),
    "data frame"
  )
  expect_error(
    gt_result(c(alpha = 1, beta = 2), converged = NA),
    "converged"
  )
  expect_error(
    gt_result(c(alpha = 1, beta = 2), diagnostics = "bad"),
    "diagnostics"
  )
})

test_that("gt_replay refuses adapters with unexpected ids", {
  study <- gt_study(reps = 1L)
  alt <- gt_adapter("other")
  expect_error(gt_replay(study, adapters = alt), "recorded adapter ids")
})

test_that("gt_export reports create failures before writing files", {
  study <- gt_study()
  blocked <- tempfile("blocked-export-")
  testthat::local_mocked_bindings(
    dir.create = function(path, recursive = FALSE, showWarnings = TRUE, ...) {
      if (identical(path, blocked)) {
        return(FALSE)
      }
      base::dir.create(path, recursive = recursive, showWarnings = showWarnings)
    },
    .package = "base"
  )
  expect_error(gt_export(study, blocked), "Could not create destination")
})

test_that("scenario and adapter collectors reject malformed containers", {
  expect_error(.gt_scenarios(list()), "non-empty list")
  expect_error(.gt_adapters(list()), "non-empty list")
  expect_error(
    .gt_adapters(list(structure(list(id = "a"), class = "gt_adapter"),
                        structure(list(id = "a"), class = "gt_adapter"))),
    "unique"
  )
  bad <- structure(
    list(id = "bad", fit = "not-a-function", metadata = list(),
         family = "gaussian"),
    class = "gt_adapter"
  )
  expect_error(.gt_adapters(bad), "Malformed adapter")
})

test_that("explicit target subsets must be supported everywhere", {
  scenarios <- list(
    gt_scenario("g", n = 10L),
    gt_ri_scenario("ri", n_groups = 3L, group_sizes = 3L)
  )
  map <- .gt_resolve_targets(scenarios, c("alpha", "beta"))
  expect_identical(map$g, c("alpha", "beta"))
  expect_identical(map$ri, c("alpha", "beta"))
  expect_error(.gt_resolve_targets(scenarios, character()), "unique")
  expect_error(.gt_resolve_targets(scenarios, c("tau2")), "every scenario")
})

test_that("seed helpers reject invalid labels and hash outputs", {
  expect_error(.gt_reps(0), "positive integer")
  expect_error(.gt_master_seed("not-digits"), "integer digits")
  expect_error(
    .gt_seed_map(
      list(gt_scenario("a")),
      list(gt_adapter()),
      reps = 1L,
      master = "1",
      hash_fun = function(...) "not-a-hex-digest"
    ),
    "SHA256"
  )
  salted <- .gt_key_raw("1", "a", 1L, "data", salt = 3L)
  expect_gt(length(salted), 10L)
})

test_that("logistic generation and glm fit paths surface expected failures", {
  study <- gt_runstudy(
    gt_scenario("log-run", family = "logistic", n = 12L),
    adapters = gt_glm_adapter(),
    reps = 1L,
    seed = "501"
  )
  expect_identical(study$scenarios[[1]]$family, "logistic")
  expect_true(all(study$data$y %in% c(0, 1)))

  dat <- utils::read.csv(
    testthat::test_path("fixtures", "logistic", "analytic.csv")
  )[c("x", "y")]
  expect_error(groundtruth:::.gt_glm_fit(dat, maxit = 0L), "iteration limit is 0")
})

test_that("random-intercept fit errors are explicit without model retention", {
  skip_if_not_installed("lme4")
  expect_error(
    groundtruth:::.gt_lmm_fit(
      data.frame(x = c(1, 2, 3), y = c(1, 2, 3), group = c(1, 1, 1)),
      criterion = "ML", interval_method = "normal", profile_timeout = 1
    ),
    "two groups"
  )

  constant_x <- data.frame(
    x = rep(5, 6),
    y = c(1, 2, 2.5, 3, 4, 5),
    group = c(1, 1, 2, 2, 3, 3)
  )
  expect_error(
    groundtruth:::.gt_lmm_fit(
      constant_x,
      criterion = "ML", interval_method = "normal", profile_timeout = 1
    ),
    "rank-two"
  )
})

test_that("interval schema errors are surfaced through result checking", {
  checked <- .gt_result_checked(list(
    estimates = c(alpha = 1, beta = 2),
    intervals = matrix(1:4, 2),
    converged = TRUE,
    message = "",
    diagnostics = list()
  ))
  expect_false(checked$ok)
  expect_match(checked$message, "gt_result")

  malformed <- .gt_result_checked(gt_result(
    c(alpha = 1, beta = 2),
    intervals = data.frame(
      target = c("alpha", "beta", "gamma"),
      level = c(0.9, 0.9, 0.9),
      lower = c(0, 0, 0),
      upper = c(2, 2, 2)
    )
  ))
  expect_true(malformed$ok)
  expect_match(
    malformed$intervals[["alpha 0.9"]]$message,
    "Extra or unsupported"
  )
})

test_that("gt_inference_details rejects duplicate companion keys", {
  study <- gt_study()
  study$inference_details <- data.frame(
    scenario = "s", rep = 1L, adapter = "lm", target = "alpha",
    level = 0.95, state = "finite", criterion = "ML",
    interval_method = "normal", lower = -1, upper = 1,
    root_lr_residual = NA, endpoint_distance = NA,
    elapsed_seconds = 0, message = "",
    stringsAsFactors = FALSE
  )
  dup <- rbind(study$inference_details, study$inference_details)
  study$inference_details <- dup
  expect_error(gt_inference_details(study), "duplicate")
})

test_that("source identity failures are reported before export", {
  testthat::local_mocked_bindings(
    .gt_source_identity = function() stop("missing identity", call. = FALSE),
    .package = "groundtruth"
  )
  expect_error(gt_export(gt_study()), "missing identity")
})

test_that("validate study rejects mismatched random-intercept data columns", {
  study <- gt_runstudy(
    gt_ri_scenario("ri", n_groups = 3L, group_sizes = 3L),
    adapters = gt_lmm_adapter(),
    reps = 1L,
    seed = "600"
  )
  study$data$group[study$data$scenario == "ri"] <- NA_integer_
  expect_error(.gt_validate_study(study), "group labels")
})

test_that("ri scenario rejects pathological sizes and bad scalars", {
  expect_error(
    gt_ri_scenario(
      "huge", n_groups = 2L,
      group_sizes = as.integer(c(.Machine$integer.max, 1L))
    ),
    "too large"
  )
  expect_error(gt_ri_scenario("bad-alpha", alpha = Inf), "finite numeric scalar")
})

test_that("seed lookup fails when the recorded map is incomplete", {
  map <- .gt_seed_map(list(gt_scenario("s")), list(gt_adapter()), 1L, "1")
  expect_error(.gt_seed_for(map, "missing", 1L, "data"), "incomplete")
})

test_that("optimizer metadata capture records invalid UTF-8 safely", {
  skip_if_not(exists(".gt_optimizer_error", mode = "function"))
  bad_text <- rawToChar(as.raw(c(0xc3, 0x28)))
  err <- .gt_optimizer_error(
    "optimizer failed",
    list(message = bad_text)
  )
  expect_true(
    err$metadata$optimizer_message %in%
      c("[invalid UTF-8]", NA_character_)
  )
  expect_true(err$metadata$truncated)
})

test_that("optimizer metadata shrinks oversized evaluation maps deterministically", {
  skip_if_not(exists(".gt_optimizer_error", mode = "function"))
  opt <- list(
    feval = stats::setNames(
      as.list(1:40),
      sprintf("eval_%02d", 1:40)
    )
  )
  meta <- .gt_optimizer_error("failed", opt)$metadata
  expect_lte(length(meta$evaluations), 32L)
  expect_true(meta$truncated)
})
