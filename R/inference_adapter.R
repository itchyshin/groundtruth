.gt_lmm_metadata_names <- function() {
  c(
    "gt_inference_criterion", "gt_inference_interval_method",
    "gt_inference_levels", "gt_inference_engine",
    "gt_inference_engine_version"
  )
}

.gt_validate_lmm_inference <- function(
  criterion, interval_method, profile_timeout
) {
  if (length(criterion) != 1L || !is.character(criterion) || is.na(criterion) ||
      !criterion %in% c("ML", "REML")) {
    stop("`criterion` must be `ML` or `REML`.", call. = FALSE)
  }
  if (length(interval_method) != 1L || !is.character(interval_method) ||
      is.na(interval_method) || !interval_method %in% c("normal", "profile")) {
    stop("`interval_method` must be `normal` or `profile`.", call. = FALSE)
  }
  if (identical(criterion, "REML") && identical(interval_method, "profile")) {
    stop("`interval_method = \"profile\"` requires `criterion = \"ML\"`.",
         call. = FALSE)
  }
  if (length(profile_timeout) != 1L || !is.numeric(profile_timeout) ||
      is.na(profile_timeout) || !is.finite(profile_timeout) ||
      profile_timeout <= 0) {
    stop("`profile_timeout` must be a positive finite number of seconds.",
         call. = FALSE)
  }
  list(
    criterion = criterion,
    interval_method = interval_method,
    profile_timeout = as.numeric(profile_timeout)
  )
}

.gt_inference_detail_rows <- function(
  states, criterion, interval_method, intervals = NULL,
  residuals = NULL, distances = NULL, elapsed = NA_real_, messages = NULL
) {
  targets <- rep(c("alpha", "beta"), each = 2L)
  levels <- rep(c(.90, .95), times = 2L)
  lookup <- function(x, target, level) {
    key <- paste(target, format(level, nsmall = 2L), sep = ":")
    if (is.null(x) || is.null(names(x)) || !key %in% names(x)) return(NA_real_)
    as.numeric(x[[key]])
  }
  rows <- vector("list", length(targets))
  for (i in seq_along(targets)) {
    target <- targets[[i]]
    level <- levels[[i]]
    iv <- NULL
    if (is.data.frame(intervals)) {
      hit <- which(intervals$target == target & intervals$level == level)
      if (length(hit) == 1L) iv <- intervals[hit, , drop = FALSE]
    }
    state <- if (is.null(names(states))) states[[i]] else {
      states[[paste(target, format(level, nsmall = 2L), sep = ":")]]
    }
    message <- if (is.null(messages) || is.null(names(messages))) "" else {
      messages[[paste(target, format(level, nsmall = 2L), sep = ":")]]
    }
    rows[[i]] <- data.frame(
      target = target, level = level, state = as.character(state),
      criterion = criterion, interval_method = interval_method,
      lower = if (!is.null(iv)) as.numeric(iv$lower) else NA_real_,
      upper = if (!is.null(iv)) as.numeric(iv$upper) else NA_real_,
      root_lr_residual = lookup(residuals, target, level),
      endpoint_distance = lookup(distances, target, level),
      elapsed_seconds = as.numeric(elapsed), message = as.character(message),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

.gt_lmm_failed_details <- function(adapter, state, message) {
  if (is.null(adapter$criterion) || is.null(adapter$interval_method)) return(NULL)
  keys <- c("alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95")
  states <- stats::setNames(rep(state, length(keys)), keys)
  messages <- stats::setNames(rep(as.character(message)[[1L]], length(keys)), keys)
  .gt_inference_detail_rows(
    states, adapter$criterion, adapter$interval_method, messages = messages
  )
}

.gt_native_profile_rows <- function(
  fit, estimates, standard_errors, checkpoint,
  profile_fun = stats::profile, confint_fun = stats::confint
) {
  profile <- profile_fun(
    fit, which = "beta_", parallel = "no", maxpts = 100L
  )
  z95 <- stats::qnorm(.975)
  all_zeta <- as.numeric(profile$.zeta)
  rows <- list()
  intervals <- list()
  residuals <- distances <- messages <- numeric()
  states <- character()
  completed <- character()
  names(states) <- names(residuals) <- names(distances) <- names(messages) <- character()
  checkpoint_now <- function() {
    saveRDS(list(
      intervals = if (length(intervals)) do.call(rbind, intervals) else NULL,
      states = states, residuals = residuals, distances = distances,
      messages = messages, completed = completed
    ), checkpoint)
  }
  for (target in c("alpha", "beta")) {
    parameter <- if (identical(target, "alpha")) "(Intercept)" else "x"
    key_rows <- which(as.character(profile$.par) == parameter)
    par_values <- if (length(key_rows)) as.numeric(profile[[parameter]][key_rows]) else numeric()
    zeta <- if (length(key_rows)) all_zeta[key_rows] else numeric()
    center <- unname(estimates[[target]])
    finite_curve <- is.finite(par_values) & is.finite(zeta)
    left_bracket <- any(finite_curve & par_values < center & zeta <= -z95)
    right_bracket <- any(finite_curve & par_values > center & zeta >= z95)
    for (level in c(.90, .95)) {
      key <- paste(target, format(level, nsmall = 2L), sep = ":")
      states[[key]] <- "incomplete"
      messages[[key]] <- "Native profile did not bracket both 95% root-LR cutoffs."
      residuals[[key]] <- NA_real_
      distances[[key]] <- NA_real_
      if (!length(key_rows) || !left_bracket || !right_bracket) {
        completed <- c(completed, key)
        checkpoint_now()
        next
      }
      pair <- tryCatch({
        ci <- confint_fun(profile, parm = parameter, level = level)
        bounds <- as.numeric(ci[1L, , drop = TRUE])
        if (length(bounds) != 2L || any(!is.finite(bounds)) || bounds[[1L]] > bounds[[2L]]) {
          stop("Native profile returned missing, nonfinite, or reversed endpoints.", call. = FALSE)
        }
        cutoff <- stats::qnorm((1 + level) / 2)
        order_par <- order(par_values)
        z_at <- stats::approx(
          x = par_values[order_par], y = zeta[order_par],
          xout = bounds, ties = mean, rule = 1
        )$y
        root_values <- stats::approx(
          x = zeta[order(zeta)], y = par_values[order(zeta)],
          xout = c(-cutoff, cutoff), ties = mean, rule = 1
        )$y
        endpoint_residual <- c(abs(z_at[[1L]] + cutoff), abs(z_at[[2L]] - cutoff))
        root_residual <- if (all(is.finite(endpoint_residual))) max(endpoint_residual) else NA_real_
        endpoint_distance <- if (all(is.finite(root_values))) max(abs(bounds - root_values)) else NA_real_
        if (!is.finite(root_residual) || root_residual > .02) {
          stop("Native profile endpoint failed the frozen root-LR residual limit.", call. = FALSE)
        }
        list(bounds = bounds, root_residual = root_residual, endpoint_distance = endpoint_distance)
      }, error = function(e) e)
      if (inherits(pair, "error")) {
        states[[key]] <- if (grepl("missing|nonfinite|reversed|residual", conditionMessage(pair))) "invalid" else "error"
        messages[[key]] <- conditionMessage(pair)
        completed <- c(completed, key)
        checkpoint_now()
        next
      }
      bounds <- pair$bounds
      root_residual <- pair$root_residual
      endpoint_distance <- pair$endpoint_distance
      residuals[[key]] <- root_residual
      distances[[key]] <- endpoint_distance
      states[[key]] <- "finite"
      messages[[key]] <- ""
      intervals[[length(intervals) + 1L]] <- data.frame(
        target = target, level = level, lower = bounds[[1L]],
        upper = bounds[[2L]], stringsAsFactors = FALSE
      )
      completed <- c(completed, key)
      checkpoint_now()
    }
  }
  list(
    intervals = if (length(intervals)) do.call(rbind, intervals) else NULL,
    states = states, residuals = residuals, distances = distances,
    messages = messages, profile_curve_points = nrow(profile)
  )
}

.gt_profile_lmm <- function(
  fit, estimates, standard_errors, timeout,
  profile_fun = stats::profile, confint_fun = stats::confint
) {
  settings <- .gt_validate_lmm_inference("ML", "profile", timeout)
  if (.Platform$OS.type == "windows") {
    elapsed <- 0
    states <- stats::setNames(rep("error", 4L), c(
      "alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95"
    ))
    messages <- stats::setNames(rep(
      "Hard native-profile deadlines require POSIX fork support.", 4L
    ), names(states))
    return(list(
      intervals = NULL,
      details = .gt_inference_detail_rows(
        states, "ML", "profile", elapsed = elapsed, messages = messages
      )
    ))
  }
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package `parallel` is required for killable native profiling.", call. = FALSE)
  }
  started <- proc.time()[["elapsed"]]
  checkpoint <- tempfile("gt-profile-")
  on.exit(unlink(checkpoint), add = TRUE)
  job <- tryCatch(parallel::mcparallel(
    tryCatch(
      list(ok = TRUE, result = .gt_native_profile_rows(
        fit, estimates, standard_errors, checkpoint, profile_fun, confint_fun
      )),
      error = function(e) list(ok = FALSE, message = conditionMessage(e))
    ),
    silent = TRUE, mc.set.seed = FALSE
  ), error = function(e) NULL)
  if (is.null(job)) {
    keys <- c("alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95")
    states <- stats::setNames(rep("error", 4L), keys)
    messages <- stats::setNames(rep("Native profile helper could not start.", 4L), keys)
    return(list(intervals = NULL, details = .gt_inference_detail_rows(
      states, "ML", "profile", elapsed = proc.time()[["elapsed"]] - started,
      messages = messages
    )))
  }
  reaped <- FALSE
  on.exit({
    if (!reaped) {
      try(tools::pskill(job$pid, signal = 9L), silent = TRUE)
      suppressWarnings(try(parallel::mccollect(job, wait = TRUE), silent = TRUE))
    }
  }, add = TRUE)
  result <- NULL
  timed_out <- FALSE
  repeat {
    result <- parallel::mccollect(job, wait = FALSE)
    if (!is.null(result)) {
      reaped <- TRUE
      break
    }
    if (proc.time()[["elapsed"]] - started >= settings$profile_timeout) {
      timed_out <- TRUE
      try(tools::pskill(job$pid, signal = 9L), silent = TRUE)
      suppressWarnings(try(parallel::mccollect(job, wait = TRUE), silent = TRUE))
      reaped <- TRUE
      break
    }
    Sys.sleep(min(.01, settings$profile_timeout / 20))
  }
  elapsed <- proc.time()[["elapsed"]] - started
  keys <- c("alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95")
  if (timed_out || elapsed > settings$profile_timeout) {
    partial <- tryCatch(readRDS(checkpoint), error = function(e) NULL)
    states <- stats::setNames(rep("timeout", 4L), keys)
    messages <- stats::setNames(rep(
      sprintf("Native profile exceeded %.6g seconds.", settings$profile_timeout), 4L
    ), keys)
    if (!is.null(partial)) {
      done <- partial$completed
      states[done] <- partial$states[done]
      messages[done] <- partial$messages[done]
    }
    return(list(
      intervals = if (is.null(partial)) NULL else partial$intervals,
      details = .gt_inference_detail_rows(
        states, "ML", "profile", if (is.null(partial)) NULL else partial$intervals,
        if (is.null(partial)) NULL else partial$residuals,
        if (is.null(partial)) NULL else partial$distances,
        elapsed, messages
      )
    ))
  }
  child <- result[[1L]]
  if (is.null(child) || !is.list(child) || !isTRUE(child$ok)) {
    message <- if (is.list(child) && is.character(child$message) && length(child$message)) {
      child$message[[1L]]
    } else "Native profile helper exited without a result."
    states <- stats::setNames(rep("error", 4L), keys)
    messages <- stats::setNames(rep(message, 4L), keys)
    return(list(
      intervals = NULL,
      details = .gt_inference_detail_rows(
        states, "ML", "profile", elapsed = elapsed, messages = messages
      )
    ))
  }
  profile <- child$result
  list(
    intervals = profile$intervals,
    details = .gt_inference_detail_rows(
      profile$states, "ML", "profile", profile$intervals,
      profile$residuals, profile$distances, elapsed, profile$messages
    ),
    profile_curve_points = profile$profile_curve_points
  )
}

.gt_lmm_result <- function(
  estimates, covariance, criterion, interval_method, profile_timeout,
  native_objective_nll, independent_objective_nll, diagnostics = list(),
  profile_fit = NULL
) {
  settings <- .gt_validate_lmm_inference(criterion, interval_method, profile_timeout)
  if (!is.list(diagnostics)) stop("`diagnostics` must be a list.", call. = FALSE)
  valid_covariance <- is.matrix(covariance) && is.numeric(covariance) &&
    identical(dim(covariance), c(2L, 2L)) && all(is.finite(covariance)) &&
    all(diag(covariance) >= 0)
  standard_errors <- if (valid_covariance) {
    setNames(sqrt(diag(covariance)), c("alpha", "beta"))
  } else {
    c(alpha = NA_real_, beta = NA_real_)
  }
  intervals <- NULL
  if (identical(settings$interval_method, "normal")) {
    if (valid_covariance) {
      intervals <- do.call(rbind, lapply(c(.90, .95), function(level) {
        q <- stats::qnorm((1 + level) / 2)
        data.frame(
          target = c("alpha", "beta"), level = level,
          lower = estimates[c("alpha", "beta")] - q * standard_errors,
          upper = estimates[c("alpha", "beta")] + q * standard_errors,
          stringsAsFactors = FALSE
        )
      }))
      states <- stats::setNames(rep("finite", 4L), c(
        "alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95"
      ))
      details <- .gt_inference_detail_rows(
        states, settings$criterion, settings$interval_method, intervals
      )
      interval_message <- ""
    } else {
      states <- stats::setNames(rep("invalid", 4L), c(
        "alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95"
      ))
      interval_message <- "Independent fixed-effect covariance unavailable; intervals omitted."
      details <- .gt_inference_detail_rows(
        states, settings$criterion, settings$interval_method,
        messages = stats::setNames(rep(interval_message, 4L), names(states))
      )
    }
  } else {
    if (is.null(profile_fit)) {
      states <- stats::setNames(rep("error", 4L), c(
        "alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95"
      ))
      message <- "A fitted model was unavailable for native profile intervals."
      details <- .gt_inference_detail_rows(
        states, settings$criterion, settings$interval_method,
        messages = stats::setNames(rep(message, 4L), names(states))
      )
      interval_message <- message
      profiled <- NULL
    } else {
      profiled <- tryCatch(
        .gt_profile_lmm(
          profile_fit, estimates, standard_errors, settings$profile_timeout
        ),
        error = function(e) e
      )
      if (inherits(profiled, "error")) {
        states <- stats::setNames(rep("error", 4L), c(
          "alpha:0.90", "alpha:0.95", "beta:0.90", "beta:0.95"
        ))
        message <- conditionMessage(profiled)
        details <- .gt_inference_detail_rows(
          states, settings$criterion, settings$interval_method,
          messages = stats::setNames(rep(message, 4L), names(states))
        )
        interval_message <- message
      } else {
        intervals <- profiled$intervals
        details <- profiled$details
        interval_message <- paste(
          unique(details$message[nzchar(details$message)]), collapse = " "
        )
      }
    }
  }
  information_limit <- diagnostics$information_limit
  if (is.character(information_limit) && length(information_limit) == 1L &&
      !is.na(information_limit) && nzchar(information_limit)) {
    details$message <- ifelse(
      nzchar(details$message),
      paste(details$message, information_limit),
      information_limit
    )
    interval_message <- paste(
      c(interval_message[nzchar(interval_message)], information_limit),
      collapse = " "
    )
  }
  diagnostics$criterion <- settings$criterion
  diagnostics$interval_method <- settings$interval_method
  diagnostics$profile_timeout <- settings$profile_timeout
  diagnostics$objective_nll <- as.numeric(native_objective_nll)
  diagnostics$independent_objective_nll <- as.numeric(independent_objective_nll)
  diagnostics$covariance <- covariance
  diagnostics$se <- standard_errors
  diagnostics$inference_details <- details
  if (exists("profiled", inherits = FALSE) && is.list(profiled)) {
    diagnostics$profile_curve_points <- profiled$profile_curve_points
  }
  gt_result(estimates, intervals, message = interval_message, diagnostics = diagnostics)
}
