#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !args[[1]] %in% c("logistic", "core", "controls")) {
  stop("Usage: Rscript --vanilla scripts/verify.R logistic|core|controls")
}
mode <- args[[1]]
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
test_dir <- file.path(root, "tests", "testthat")
filters <- list(
  logistic = "logistic",
  core = "runner|paired|analytic",
  controls = "controls|accounting"
)
testthat::test_dir(test_dir, filter = filters[[mode]], reporter = "summary",
                   load_package = "source", stop_on_failure = TRUE,
                   stop_on_warning = FALSE)

if (identical(mode, "logistic")) {
  fixtures <- c("retained", "analytic", "asymmetric", "inverted", "scaled",
                "complete", "quasi", "all_zero", "all_one", "constant",
                "shifted", "tied")
  fields <- c("fixture", "engine", "status", "target", "estimate", "se",
              "lower90", "upper90", "lower95", "upper95", "nll",
              "score_alpha", "score_beta", "Iaa", "Iab", "Ibb", "message")
  rows <- list()
  for (name in fixtures) {
    path <- file.path(test_dir, "fixtures", "logistic", paste0(name, ".csv"))
    dat <- utils::read.csv(path, stringsAsFactors = FALSE)[c("x", "y")]
    result <- tryCatch(groundtruth::gt_glm_adapter()$fit(dat), error = function(e) e)
    if (inherits(result, "error")) {
      for (target in c("alpha", "beta")) {
        rows[[length(rows) + 1L]] <- as.list(c(
          fixture = name, engine = "r", status = "fit_error",
          target = target, setNames(rep(NA_character_, 12L), fields[5:16]),
          message = conditionMessage(result)
        ))
      }
      next
    }
    eta <- result$estimates[["alpha"]] + result$estimates[["beta"]] * dat$x
    p <- plogis(eta)
    w <- exp(-abs(eta)) / (1 + exp(-abs(eta)))^2
    X <- cbind(1, dat$x)
    score <- drop(crossprod(X, dat$y - p))
    info <- crossprod(X, X * w)
    iv <- result$intervals
    for (target in c("alpha", "beta")) {
      j <- match(target, c("alpha", "beta"))
      get_interval <- function(level, column) iv[iv$target == target & iv$level == level, column]
      rows[[length(rows) + 1L]] <- list(
        fixture = name, engine = "r", status = "ok",
        target = target, estimate = unname(result$estimates[[target]]),
        se = unname(result$diagnostics$se[[target]]),
        lower90 = get_interval(.90, "lower"), upper90 = get_interval(.90, "upper"),
        lower95 = get_interval(.95, "lower"), upper95 = get_interval(.95, "upper"),
        nll = result$diagnostics$mean_nll * nrow(dat), score_alpha = score[[1]],
        score_beta = score[[2]], Iaa = info[1, 1], Iab = info[1, 2], Ibb = info[2, 2],
        message = ""
      )
    }
  }
  out <- as.data.frame(do.call(rbind, lapply(rows, function(x) as.data.frame(x,
    stringsAsFactors = FALSE, check.names = FALSE))), check.names = FALSE)
  out <- out[fields]
  dir.create(file.path(root, ".unlazy", "logistic-twins", "evidence"),
             recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(out, file.path(root, ".unlazy", "logistic-twins", "evidence",
                                  "r-fits.csv"), row.names = FALSE, na = "")
  cat("LOGISTIC_LANGUAGE_OK\n")
} else if (identical(mode, "core")) {
  cat("LOGISTIC_CORE_OK\n")
} else {
  cat("LOGISTIC_CONTROLS_OK\n")
}
