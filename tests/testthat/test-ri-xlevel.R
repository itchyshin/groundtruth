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

test_that("observation-level data CSV matches the pre-amendment generator bytes", {
  # Golden CSV generated from sealed pre-amendment R source whose public base
  # was commit 6f80a67acf0d4381a21667ddd3b8cb183835657f.
  baseline_sha256 <- "ff8ae894b6fd2eb4377f3e07420599bdf6f3d7a5ba6616d101adc0151ef0f389"
  capture <- gt_adapter("capture", function(data) ri_point_result(),
                        family = "random_intercept")
  study <- gt_runstudy(
    gt_ri_scenario("default-golden", n_groups = 3L,
                   group_sizes = c(2L, 4L, 3L)),
    capture, reps = 1L, seed = "871"
  )
  path <- tempfile("gt-ri-default-golden-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  gt_export(study, path)
  expect_identical(
    digest::digest(file = file.path(path, "data.csv"), algo = "sha256",
                   serialize = FALSE),
    baseline_sha256
  )
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
