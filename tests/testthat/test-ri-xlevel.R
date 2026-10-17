test_that("RI predictor level defaults to observation and validates levels", {
  scenario <- gt_ri_scenario("x-level-default")
  expect_identical(scenario$x_level, "observation")
  expect_error(gt_ri_scenario("x-level-invalid", x_level = "individual"),
               "x_level")
})

test_that("group-level x expands canonically and controls y generation", {
  scenario <- gt_ri_scenario("x-level-group", n_groups = 3L,
                             group_sizes = c(2L, 4L, 3L), x_level = "group")
  fit <- gt_adapter("capture", function(data) {
    expect_identical(data$x, rep(data$x[!duplicated(data$group)],
                                 times = c(2L, 4L, 3L)))
    ri_point_result()
  }, family = "random_intercept")
  study <- gt_runstudy(scenario, fit, reps = 1L, seed = "871")
  x <- study$data$x
  group <- study$data$group
  expect_identical(unique(group), 1:3)
  expect_identical(x, rep(x[c(1L, 3L, 7L)], times = c(2L, 4L, 3L)))
  expect_gt(length(unique(x)), 1L)
  expect_true(qr(cbind(1, x[c(1L, 3L, 7L)]))$rank == 2L)
  set.seed(study$seed_map$seed[study$seed_map$stream == "data"])
  x_group <- rnorm(3L)
  expect_identical(unique(x), x_group)
  expect_identical(x, rep(x_group, times = scenario$group_sizes))
  u <- rnorm(3L, sd = scenario$tau)
  e <- rnorm(scenario$n, sd = scenario$sigma)
  expect_equal(study$data$y, scenario$alpha + scenario$beta * rep(x_group,
               times = scenario$group_sizes) + u[group] + e)
  expect_identical(study$scenarios[[1]]$x_level, "group")
  expect_identical(study$provenance$families[[1]]$x_level, "group")
  expect_identical(study$provenance$generator_revision, "random-intercept-v2")
  expect_identical(study$seed_map$stream, c("fit:capture", "data"))
})

test_that("group-level study replay preserves generated data and metadata", {
  capture <- gt_adapter("capture", function(data) ri_point_result(),
                        family = "random_intercept")
  scenario <- gt_ri_scenario("group-replay", n_groups = 3L,
                             group_sizes = c(2L, 4L, 3L), x_level = "group")
  study <- gt_runstudy(scenario, capture, reps = 2L, seed = "872")
  replay <- gt_replay(study, capture)
  expect_identical(replay$data$group, study$data$group)
  expect_identical(replay$data$x, study$data$x)
  expect_identical(replay$data$y, study$data$y)
  expect_identical(replay$scenarios[[1]]$x_level, "group")
  expect_identical(replay$provenance$generator_revision, "random-intercept-v2")
  expect_identical(replay$provenance$families[[1]]$x_level, "group")
})

test_that("legacy RI scenarios without x_level replay as observation level", {
  capture <- gt_adapter("capture", function(data) ri_point_result(),
                        family = "random_intercept")
  scenario <- gt_ri_scenario("legacy-ri", n_groups = 3L, group_sizes = 4L)
  scenario$x_level <- NULL
  study <- gt_runstudy(scenario, capture, reps = 1L, seed = "880")
  study$scenarios[[1]]$x_level <- NULL
  replay <- gt_replay(study, capture)
  expect_identical(study$data, replay$data)
  expect_named(replay$data, c("scenario", "rep", "row", "x", "y", "group"))
  expect_identical(replay$data$group, study$data$group)
  expect_identical(replay$scenarios[[1]]$x_level, "observation")
})

test_that("observation-level generator preserves pre-amendment draws and export", {
  capture <- gt_adapter("capture", function(data) ri_point_result(),
                        family = "random_intercept")
  scenario <- gt_ri_scenario("default-golden", n_groups = 3L,
                             group_sizes = c(2L, 4L, 3L))
  expect_identical(scenario$n_groups, 3L)
  expect_identical(scenario$group_sizes, c(2L, 4L, 3L))
  expect_identical(scenario$n, 9L)
  expect_identical(scenario$alpha, 1)
  expect_identical(scenario$beta, 0.7)
  expect_identical(scenario$sigma, 1)
  expect_identical(scenario$tau, 0.5)
  study <- gt_runstudy(scenario, capture, reps = 1L, seed = "871")
  data_seed <- study$seed_map$seed[study$seed_map$stream == "data"]
  expect_identical(as.integer(data_seed), 544412446L)

  # Independent pre-amendment oracle: the observation-level stream draws x,
  # then the random intercepts, then residuals from the same fixed R RNG kinds.
  draw_reference <- function() {
    old_kind <- RNGkind()
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      do.call(RNGkind, as.list(old_kind))
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(data_seed, kind = "Mersenne-Twister",
             normal.kind = "Inversion", sample.kind = "Rejection")
    x <- stats::rnorm(scenario$n)
    group <- rep(seq_len(scenario$n_groups), times = scenario$group_sizes)
    u <- stats::rnorm(scenario$n_groups, sd = scenario$tau)
    e <- stats::rnorm(scenario$n, sd = scenario$sigma)
    list(x = x, y = scenario$alpha + scenario$beta * x + u[group] + e,
         group = group)
  }
  reference <- draw_reference()
  expected <- data.frame(
    scenario = rep(scenario$id, scenario$n), rep = rep(1L, scenario$n),
    row = seq_len(scenario$n), x = reference$x, y = reference$y,
    group = reference$group, stringsAsFactors = FALSE
  )
  expect_identical(study$data, expected)

  path <- tempfile("gt-ri-default-golden-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  gt_export(study, path)
  data_path <- file.path(path, "data.csv")
  csv_number <- function(x) trimws(formatC(x, digits = 17, format = "g",
                                             decimal.mark = "."))
  expected_lines <- c(
    paste(sprintf("\"%s\"", names(expected)), collapse = ","),
    vapply(seq_len(nrow(expected)), function(i) paste(
      paste0("\"", expected$scenario[i], "\""), expected$rep[i],
      expected$row[i], csv_number(expected$x[i]), csv_number(expected$y[i]),
      expected$group[i], sep = ","
    ), "")
  )
  expected_text <- paste0(paste(expected_lines, collapse = "\n"), "\n")
  expected_bytes <- charToRaw(expected_text)
  actual_bytes <- readBin(data_path, what = "raw", n = file.info(data_path)$size)
  expect_identical(actual_bytes, expected_bytes)

  col_classes <- c("character", "integer", "integer", "numeric", "numeric", "integer")
  parsed <- utils::read.csv(data_path, stringsAsFactors = FALSE,
                            check.names = FALSE, colClasses = col_classes)
  parsed_reference <- utils::read.csv(text = expected_text, stringsAsFactors = FALSE,
                                      check.names = FALSE, colClasses = col_classes)
  expect_identical(names(parsed), names(expected))
  expect_identical(vapply(parsed, class, ""),
                   c(scenario = "character", rep = "integer", row = "integer",
                     x = "numeric", y = "numeric", group = "integer"))
  # Compare the file readback to the exact oracle bytes parsed by the same R
  # runtime; base R's decimal parser can round a 17-digit CSV number one ULP.
  expect_identical(parsed, parsed_reference)
})

test_that("RI v4 export records x_level in scenarios and provenance", {
  capture <- gt_adapter("capture", function(data) ri_point_result(),
                        family = "random_intercept")
  study <- gt_runstudy(gt_ri_scenario("export-ri", n_groups = 3L,
                                      group_sizes = 4L, x_level = "group"),
                       capture, reps = 1L, seed = "881")
  expect_identical(groundtruth:::.gt_scenarios_frame(study$scenarios)$x_level,
                   "group")
  path <- tempfile("gt-ri-v4-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  gt_export(study, path)
  manifest <- dget(file.path(path, "manifest.R"))
  expect_identical(manifest$schema_version, "groundtruth-r-export-v4")
  expect_identical(utils::read.csv(file.path(path, "scenarios.csv"))$x_level,
                   "group")
  expect_identical(manifest$provenance$families[[1]]$x_level, "group")
})

test_that("two groups give the declared minimum group-level x rank", {
  scenario <- gt_ri_scenario("two-group-x", n_groups = 2L,
                             group_sizes = c(2L, 3L), x_level = "group")
  data <- groundtruth:::.gt_generate(scenario, 701L)
  expect_identical(data$x, rep(data$x[c(1L, 3L)], times = c(2L, 3L)))
  expect_equal(qr(cbind(1, data$x[c(1L, 3L)]))$rank, 2L)
})
