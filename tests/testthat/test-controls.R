test_that("invalid point targets are rejected", {
  expect_error(gt_result(c(alpha = 1, alpha = 2)))
  expect_error(gt_result(c(alpha = 1)))
  expect_error(gt_result(c(alpha = 1, beta = 2, gamma = 3)))
  expect_error(gt_result(c(alpha = 1, beta = NaN)))
})

test_that("interval defects retain valid points", {
  bad <- gt_adapter("intervals", function(data) {
    gt_result(
      c(alpha = 1, beta = 2),
      intervals = data.frame(
        target = c("alpha", "alpha", "beta", "beta"),
        level = c(.95, .95, .90, .95),
        lower = c(0, 0, 3, 1),
        upper = c(2, 2, 1, 3)
      )
    )
  })
  s <- gt_study(adapters = list(bad))
  expect_identical(s$ledger$status, "ok")
  expect_true(any(s$targets$usable_interval))
  expect_true(any(!s$targets$usable_interval))
})

test_that("export round trips and refuses foreign paths", {
  s <- gt_study(reps = 2L, seed = "9007199254740991")
  out <- tempfile("groundtruth-export-")
  expect_identical(normalizePath(gt_export(s, out)), normalizePath(out))
  files <- c(
    "data.csv",
    "attempts.csv",
    "targets.csv",
    "summary.csv",
    "seed-map.csv",
    "scenarios.csv",
    "manifest.R"
  )
  expect_true(all(file.exists(file.path(out, files))))
  manifest <- dget(file.path(out, "manifest.R"))
  expect_identical(as.character(manifest$master_seed), "9007199254740991")
  csv <- files[files != "manifest.R"]
  actual <- vapply(
    csv,
    function(f) {
      digest::digest(
        file = file.path(out, f),
        algo = "sha256",
        serialize = FALSE
      )
    },
    ""
  )
  expect_identical(unname(unlist(manifest$sha256)), unname(actual))
  expect_equal(
    utils::read.csv(file.path(out, "data.csv"), stringsAsFactors = FALSE),
    s$data
  )
  foreign <- tempfile("groundtruth-foreign-")
  dir.create(foreign)
  writeBin(as.raw(c(1, 2, 3)), file.path(foreign, "foreign.bin"))
  before <- readBin(file.path(foreign, "foreign.bin"), "raw", 3L)
  expect_error(gt_export(s, foreign))
  expect_identical(
    readBin(file.path(foreign, "foreign.bin"), "raw", 3L),
    before
  )
})

test_that("adapter receives data only and no truth columns", {
  got <- NULL
  a <- gt_adapter("inspect", function(data) {
    got <<- data
    gt_adapter()$fit(data)
  })
  gt_study(adapters = list(a))
  expect_identical(names(got), c("x", "y"))
})


test_that("byte checker rejects a changed fixture", {
  file <- gt_fixture_path("analytic.csv")
  original <- readBin(file, "raw", n = file.info(file)$size)
  on.exit(writeBin(original, file), add = TRUE)
  writeBin(c(original, charToRaw("\n")), file)
  expect_error(gt_check_fixture_hash("analytic.csv"))
  writeBin(original, file)
  expect_error(gt_check_fixture_hash("analytic.csv"), NA)
})


test_that("export hashes and schemas are portable", {
  study <- gt_study(reps = 2L, seed = "18446744073709551615")
  path <- tempfile("schemas-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  gt_export(study, path)
  manifest <- dget(file.path(path, "manifest.R"))
  expected_files <- c(
    "data.csv",
    "attempts.csv",
    "targets.csv",
    "summary.csv",
    "seed-map.csv",
    "scenarios.csv"
  )
  expect_setequal(names(manifest$sha256), expected_files)
  for (name in names(manifest$csv)) {
    schema <- manifest$csv[[name]]
    file <- paste0(name, ".csv")
    tab <- utils::read.csv(
      file.path(path, file),
      colClasses = unname(schema$types),
      stringsAsFactors = FALSE
    )
    expect_identical(names(tab), schema$columns)
    expect_identical(vapply(tab, function(x) class(x)[1], ""), schema$types)
    expect_identical(
      digest::digest(
        file = file.path(path, file),
        algo = "sha256",
        serialize = FALSE
      ),
      manifest$sha256[[file]]
    )
  }
  data <- utils::read.csv(
    file.path(path, "data.csv"),
    colClasses = unname(manifest$csv$data$types),
    stringsAsFactors = FALSE
  )
  expect_true(gt_close(
    data$x,
    study$data$x,
    atol = 0,
    rtol = 2 * .Machine$double.eps
  ))
  expect_true(gt_close(
    data$y,
    study$data$y,
    atol = 0,
    rtol = 2 * .Machine$double.eps
  ))
})


test_that("export identifies the R source that produced the study", {
  study <- gt_study()
  path <- tempfile("identity-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  gt_export(study, path)
  manifest <- dget(file.path(path, "manifest.R"))
  expect_type(manifest$source_sha256, "character")
  expect_length(manifest$source_sha256, 1L)
  expect_match(manifest$source_sha256, "^[0-9a-f]{64}$")
  expect_identical(manifest$source_scope, "R/groundtruth.R")
  expect_identical(manifest$source_sha256, study$provenance$source_sha256)
  identity <- dget(system.file("source-identity.R", package = "groundtruth"))
  expect_identical(identity$source_sha256, manifest$source_sha256)
  expect_identical(manifest$package_version, identity$package_version)
  source_file <- file.path("..", "..", "R", "groundtruth.R")
  if (file.exists(source_file)) {
    expect_identical(
      manifest$source_sha256,
      digest::digest(file = source_file, algo = "sha256", serialize = FALSE)
    )
  }
})
