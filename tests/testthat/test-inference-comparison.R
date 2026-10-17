test_that("standalone paired comparison regression suite runs in package tests", {
  package_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
  script <- file.path(package_root, "tests", "inference", "comparison.R")
  expect_true(file.exists(script))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(package_root)
  expect_output(source(script, local = new.env(parent = globalenv())),
                "INFERENCE_ACCOUNTING_OK")
})
