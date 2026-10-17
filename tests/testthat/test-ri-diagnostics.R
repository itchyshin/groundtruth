# These controls use synthetic optinfo and callbacks only. No model is fitted.
ri_diagnostic_mode <- getOption("groundtruth.ri.diagnostic_mode", "all")
ri_diagnostic_optinfo <- function() list(
  optimizer = "bobyqa", conv = list(opt = 3L, lme4 = list(messages = "gradient warning")),
  message = "trust region step failed", warnings = "native warning", feval = 27L,
  derivs = list(gradient = c(0.1, -0.2), Hessian = diag(c(2, 3))),
  arbitrary_model = new.env()
)
ri_diagnostic_message <- "lme4 optimizer failed with code 3."

if (ri_diagnostic_mode %in% c("all", "diagnostics")) {
  test_that("optimizer failures retain typed metadata and exact messages through RDS", {
    expect_true(exists(".gt_optimizer_error", mode = "function"))
    if (!exists(".gt_optimizer_error", mode = "function")) return(invisible(NULL))
    constructor <- .gt_optimizer_error
    # Package-like lookup proves that default-session utils attachment is not required.
    environment(constructor) <- new.env(parent = baseenv())
    error <- constructor(ri_diagnostic_message, ri_diagnostic_optinfo(), "captured warning")
    expect_identical(class(error), c("gt_optimizer_error", "error", "condition"))
    expect_identical(conditionMessage(error), ri_diagnostic_message)
    expect_null(conditionCall(error))
    metadata <- error$metadata
    expect_named(metadata, c("optimizer", "code", "optimizer_message", "warnings",
                            "convergence_messages", "evaluations", "diagnostics",
                            "truncated", "capture_error"))
    expect_identical(metadata$optimizer, "bobyqa")
    expect_identical(metadata$code, 3L)
    expect_identical(metadata$optimizer_message, "trust region step failed")
    expect_identical(metadata$warnings, c("captured warning", "native warning"))
    expect_identical(metadata$convergence_messages, "gradient warning")
    expect_identical(metadata$evaluations, list(feval = 27L))
    expect_identical(names(metadata$diagnostics), sort(names(metadata$diagnostics)))
    expect_lte(length(metadata$diagnostics), 32L)
    expect_true(all(vapply(metadata$diagnostics, function(x) {
      length(x) == 1L && (is.numeric(x) || is.logical(x))
    }, logical(1))))
    expect_false(metadata$truncated)
    expect_identical(metadata$capture_error, "")
    path <- tempfile(fileext = ".rds")
    on.exit(unlink(path), add = TRUE)
    saveRDS(error, path)
    expect_identical(readRDS(path), error)
    expect_lte(length(serialize(metadata, NULL, version = 2)), 16384L)
    expect_false(any(vapply(metadata, is.environment, logical(1))))
  })

  test_that("evaluation labels and scalar attributes cannot bypass metadata bounds", {
    if (!exists(".gt_optimizer_error", mode = "function")) {
      expect_true(FALSE, info = "missing optimizer condition constructor")
      return(invisible(NULL))
    }
    opt <- ri_diagnostic_optinfo()
    opt$optimizer <- structure("bobyqa", payload = new.env())
    opt$message <- structure("trust region step failed", payload = new.env())
    opt$feval <- setNames(list(structure(1L, payload = new.env()), 2L),
                         paste0(strrep("e", 600L), c("a", "b")))
    m <- .gt_optimizer_error(ri_diagnostic_message, opt)$metadata
    expect_true(m$truncated)
    expect_identical(m$optimizer, "bobyqa")
    expect_identical(m$optimizer_message, "trust region step failed")
    expect_length(m$evaluations, 2L)
    expect_identical(unname(unlist(m$evaluations)), c(1L, 2L))
    expect_true(all(nchar(names(m$evaluations), type = "bytes") <= 512L))
    expect_false(anyDuplicated(names(m$evaluations)) > 0L)
    expect_true(all(vapply(m$evaluations, function(x) is.null(attributes(x)), logical(1))))
    expect_lte(length(serialize(m, NULL, version = 2)), 16384L)
  })

  test_that("serialized metadata respects the cap with current and version-2 R formats", {
    if (!exists(".gt_optimizer_error", mode = "function")) {
      expect_true(FALSE, info = "missing optimizer condition constructor")
      return(invisible(NULL))
    }
    sizes <- vapply(400:512, function(bytes) {
      opt <- ri_diagnostic_optinfo()
      opt$warnings <- rep(strrep("w", bytes), 16L)
      opt$conv$lme4$messages <- rep(strrep("c", bytes), 16L)
      m <- .gt_optimizer_error(ri_diagnostic_message, opt)$metadata
      max(length(serialize(m, NULL, version = 2)), length(serialize(m, NULL, version = 3)))
    }, integer(1))
    expect_lte(max(sizes), 16384L)
  })

  test_that("oversized metadata is bounded deterministic and valid UTF-8", {
    if (!exists(".gt_optimizer_error", mode = "function")) {
      expect_true(FALSE, info = "missing optimizer condition constructor")
      return(invisible(NULL))
    }
    opt <- ri_diagnostic_optinfo()
    opt$message <- strrep("\u00e9", 20000L)
    opt$optimizer <- strrep("b", 20000L)
    opt$warnings <- rep(strrep("\u00e9", 20000L), 100L)
    opt$conv$lme4$messages <- rep(strrep("c", 20000L), 100L)
    opt$feval <- setNames(as.list(seq_len(1000L)), sprintf("%s%04d", strrep("e", 600L), 1:1000))
    opt$derivs$gradient <- rep(.Machine$double.xmax, 10000L)
    opt$derivs$Hessian <- diag(rep(.Machine$double.xmax, 100L))
    first <- .gt_optimizer_error(ri_diagnostic_message, opt)
    second <- .gt_optimizer_error(ri_diagnostic_message, opt)
    expect_identical(first, second)
    m <- first$metadata
    expect_true(m$truncated)
    expect_lte(length(serialize(m, NULL, version = 2)), 16384L)
    expect_lte(length(m$warnings), 16L)
    expect_lte(length(m$convergence_messages), 16L)
    texts <- c(m$optimizer, m$optimizer_message, m$warnings, m$convergence_messages,
               m$capture_error, names(m$evaluations), names(m$diagnostics))
    expect_true(all(nchar(texts, type = "bytes") <= 512L))
    expect_true(all(!is.na(iconv(texts, from = "UTF-8", to = "UTF-8"))))
    expect_identical(conditionMessage(first), ri_diagnostic_message)
    expect_identical(names(m$diagnostics), sort(names(m$diagnostics)))
    expect_lte(length(m$diagnostics), 32L)
    expect_true(all(vapply(m$evaluations, function(x) is.numeric(x) && length(x) == 1L, logical(1))))
  })
}

if (ri_diagnostic_mode %in% c("all", "accounting")) {
  test_that("absent fields and capture errors preserve the original optimizer failure", {
    expect_true(exists(".gt_optimizer_error", mode = "function"))
    if (!exists(".gt_optimizer_error", mode = "function")) return(invisible(NULL))
    absent <- .gt_optimizer_error(ri_diagnostic_message, list())
    expect_identical(absent$metadata$optimizer, NA_character_)
    expect_identical(absent$metadata$code, NA_integer_)
    expect_identical(absent$metadata$optimizer_message, NA_character_)
    expect_identical(absent$metadata$warnings, character())
    expect_identical(absent$metadata$convergence_messages, character())
    expect_identical(absent$metadata$evaluations, list())
    expect_identical(absent$metadata$diagnostics, list())
    failed <- .gt_optimizer_error(ri_diagnostic_message, stop(strrep("capture broke ", 1000L)))
    expect_identical(conditionMessage(failed), ri_diagnostic_message)
    expect_true(inherits(failed, "gt_optimizer_error"))
    expect_match(failed$metadata$capture_error, "capture broke")
    expect_lte(nchar(failed$metadata$capture_error, type = "bytes"), 512L)
    expect_true(failed$metadata$truncated)
    expect_lte(length(serialize(failed$metadata, NULL, version = 2)), 16384L)
  })

  test_that("synthetic callback errors preserve fit_error accounting and ledger schema", {
    if (!exists(".gt_optimizer_error", mode = "function")) {
      expect_true(FALSE, info = "missing optimizer condition constructor")
      return(invisible(NULL))
    }
    error <- .gt_optimizer_error(ri_diagnostic_message, ri_diagnostic_optinfo())
    adapter <- gt_adapter("synthetic-error", function(data) stop(error), family = "random_intercept")
    study <- gt_runstudy(gt_ri_scenario("synthetic", n_groups = 3L, group_sizes = 3L),
                         adapters = adapter, reps = 2L, seed = "9123")
    expect_named(study$ledger, c("scenario", "rep", "adapter", "data_seed", "fit_seed", "status", "message"))
    expect_identical(study$ledger$status, rep("fit_error", 2L))
    expect_identical(study$ledger$message, rep(ri_diagnostic_message, 2L))
    expect_true(all(is.na(study$targets$estimate)))
    summary <- gt_summarize(study)
    expect_true(all(summary$attempted == 2L & summary$successful == 0L & summary$failed == 2L))
    expect_true(all(is.na(summary$coverage)))
  })
}
