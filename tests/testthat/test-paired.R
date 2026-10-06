test_that("data and fit streams replay across input orders", {
  a <- gt_adapter("a", function(data) gt_adapter()$fit(data))
  b <- gt_adapter("b", function(data) gt_adapter()$fit(data))
  s1 <- gt_runstudy(
    list(gt_scenario("z"), gt_scenario("a")),
    list(a, b),
    reps = 2L,
    seed = "9007199254740991"
  )
  s2 <- gt_runstudy(
    list(gt_scenario("a"), gt_scenario("z")),
    list(b, a),
    reps = 2L,
    seed = "9007199254740991"
  )
  expect_identical(s1$master_seed, "9007199254740991")
  expect_equal(
    s1$data[order(s1$data$scenario, s1$data$rep, s1$data$row), ],
    s2$data[order(s2$data$scenario, s2$data$rep, s2$data$row), ]
  )
  expect_equal(gt_attempts(s1), gt_attempts(s2))
  expect_equal(gt_replay(s1)$data, s1$data)
  expect_equal(gt_replay(s1)$targets, s1$targets)
})

test_that("caller RNG state is restored and Box-Muller refused", {
  old <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  set.seed(45)
  before <- .Random.seed
  gt_study()
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), old)
  set.seed(45)
  before <- .Random.seed
  expect_identical(
    gt_study(
      adapters = list(gt_adapter("bad", function(data) stop("x")))
    )$ledger$status,
    "fit_error"
  )
  expect_identical(.Random.seed, before)
  saved <- .Random.seed
  rm(".Random.seed", envir = .GlobalEnv)
  # nolint start: object_name_linter. R's prescribed RNG state name.
  on.exit(assign(".Random.seed", saved, envir = .GlobalEnv), add = TRUE)
  # nolint end: object_name_linter.
  gt_study()
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  RNGkind(normal.kind = "Box-Muller")
  expect_error(gt_study(), regexp = "Box-Muller|normal")
})

test_that("collisions resolve and stochastic adapters replay", {
  expect_true(
    exists(
      ".gt_seed_map",
      envir = asNamespace("groundtruth"),
      inherits = FALSE
    ),
    info = "required internal seed-map seam is absent"
  )
  seed_map <- get(".gt_seed_map", envir = asNamespace("groundtruth"))
  scenarios <- list(gt_scenario("a"), gt_scenario("b"))
  adapters <- list(gt_adapter("a"), gt_adapter("b"))
  calls <- 0L
  m <- seed_map(
    scenarios,
    adapters,
    reps = 1L,
    master = "7",
    hash_fun = function(...) {
      calls <<- calls + 1L
      paste0(
        sprintf("%08x", if (calls <= 2L) 0L else calls - 2L),
        paste(rep("00", 28L), collapse = "")
      )
    }
  )
  expect_true(anyDuplicated(m$hash) || any(m$salt > 0L))
  expect_equal(length(unique(m$seed)), nrow(m))
  stochastic <- gt_adapter("stochastic", function(data) {
    gt_result(c(alpha = 1 + stats::runif(1), beta = 2 + stats::runif(1)))
  })
  s <- gt_study(reps = 2L, adapters = list(stochastic), seed = "17")
  expect_equal(gt_replay(s)$targets, s$targets)
  broken <- s
  broken$seed_map$seed[1L] <- broken$seed_map$seed[1L] + 1L
  expect_error(gt_replay(broken), regexp = "seed|map|Seed")
})


test_that("wide seed strings retain every digit", {
  wide <- "18446744073709551615"
  study <- gt_study(seed = wide)
  expect_identical(study$master_seed, wide)
  expect_identical(gt_replay(study)$seed_map, study$seed_map)
  path <- tempfile("wide-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  gt_export(study, path)
  expect_identical(dget(file.path(path, "manifest.R"))$master_seed, wide)
  expect_error(gt_study(seed = 2^54))
})

test_that("every stream resets RNG kinds even when an adapter changes them", {
  rogue <- gt_adapter("rogue", function(data) {
    RNGkind("Super-Duper", "Box-Muller", "Rejection")
    gt_result(c(alpha = 1, beta = 2))
  })
  stochastic <- gt_adapter("random", function(data) {
    gt_result(c(alpha = stats::runif(1), beta = stats::rnorm(1)))
  })
  scenario <- gt_scenario("isolate", n = 6L)
  a <- gt_runstudy(scenario, list(rogue, stochastic), reps = 2L, seed = "12")
  b <- gt_runstudy(scenario, list(stochastic, rogue), reps = 2L, seed = "12")
  expect_identical(a$data, b$data)
  canonical <- function(x) {
    x <- x[order(x$rep, x$adapter, x$target, x$level), ]
    rownames(x) <- NULL
    x
  }
  expect_identical(canonical(a$targets), canonical(b$targets))
})

test_that("replay refuses altered RNG version or kind settings", {
  study <- gt_study()
  altered <- study
  altered$rng$algorithm <- "unknown-version"
  expect_error(gt_replay(altered), regexp = "RNG|rng|version")
  altered <- study
  altered$rng$kind[2] <- "Box-Muller"
  expect_error(gt_replay(altered), regexp = "RNG|rng|kind")
})

test_that("a top-level runner error restores present and absent caller seeds", {
  testthat::local_mocked_bindings(
    .gt_run = function(...) stop("injected runner error"),
    .package = "groundtruth"
  )
  saved_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    saved_seed <- .Random.seed
  }
  on.exit(
    {
      do.call(RNGkind, as.list(saved_kind))
      if (had_seed) {
        # nolint start: object_name_linter. R's prescribed RNG state name.
        assign(".Random.seed", saved_seed, envir = .GlobalEnv)
        # nolint end: object_name_linter.
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  RNGkind("Wichmann-Hill", "Inversion", "Rejection")
  set.seed(39)
  before <- .Random.seed
  kinds <- RNGkind()
  expect_error(gt_study(), "injected runner error")
  expect_identical(RNGkind(), kinds)
  expect_identical(.Random.seed, before)
  rm(".Random.seed", envir = .GlobalEnv)
  expect_error(gt_study(), "injected runner error")
  expect_identical(RNGkind(), kinds)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})


test_that("Box-Muller rejection leaves caller kinds and seed byte-identical", {
  oldkind <- RNGkind()
  oldseed <- .Random.seed
  on.exit(
    {
      do.call(RNGkind, as.list(oldkind))
      # nolint start: object_name_linter. R's prescribed RNG state name.
      assign(".Random.seed", oldseed, envir = .GlobalEnv)
      # nolint end: object_name_linter.
    },
    add = TRUE
  )
  RNGkind(normal.kind = "Box-Muller")
  set.seed(59)
  stats::rnorm(1)
  before <- .Random.seed
  kind <- RNGkind()
  expect_error(gt_study(), "Box-Muller")
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
})


test_that("recorded fixed RNG kinds govern every generated and fitted stream", {
  rogue <- gt_adapter("rogue", function(data) {
    RNGkind("Super-Duper", "Box-Muller", "Rejection")
    gt_result(c(alpha = 1, beta = 2))
  })
  random <- gt_adapter("zzz-random", function(data) {
    gt_result(c(alpha = stats::runif(1), beta = stats::rnorm(1)))
  })
  study <- gt_study(reps = 2L, adapters = list(rogue, random), seed = "12")
  oldkind <- RNGkind()
  oldseed <- .Random.seed
  on.exit(
    {
      do.call(RNGkind, as.list(oldkind))
      # nolint start: object_name_linter. R's prescribed RNG state name.
      assign(".Random.seed", oldseed, envir = .GlobalEnv)
      # nolint end: object_name_linter.
    },
    add = TRUE
  )
  for (rep in 1:2) {
    row <- study$seed_map$scenario == "s" & study$seed_map$rep == rep
    set.seed(
      study$seed_map$seed[row & study$seed_map$stream == "data"],
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    x <- stats::rnorm(5)
    y <- 1 + 2 * x + stats::rnorm(5)
    expect_identical(study$data$x[study$data$rep == rep], x)
    expect_identical(study$data$y[study$data$rep == rep], y)
    set.seed(
      study$seed_map$seed[row & study$seed_map$stream == "fit:zzz-random"],
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    want <- c(alpha = stats::runif(1), beta = stats::rnorm(1))
    target <- study$targets[
      study$targets$adapter == "zzz-random" &
        study$targets$rep == rep &
        study$targets$level == .95,
    ]
    expect_identical(target$estimate, unname(want[target$target]))
  }
})
