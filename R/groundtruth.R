#' Define a simulation scenario
#'
#' @param id A non-empty scalar character identifier.
#' @param n Number of observations, an integer greater than two.
#' @param alpha Intercept used to generate data.
#' @param beta Slope used to generate data.
#' @param sigma Positive residual standard deviation for Gaussian scenarios;
#'   omitted logistic scenarios use no sigma.
#' @param family Either `"gaussian"` or `"logistic"`.
#' @return A `gt_scenario` object.
#' @examples
#' gt_scenario("small", n = 20L)
#' gt_scenario("binary", family = "logistic")
#' @export
gt_scenario <- function(
  id, n = 100L, alpha = 1, beta = 0.7,
  sigma = if (identical(family, "logistic")) NULL else 1.2,
  family = "gaussian"
) {
  .gt_scalar_character(id, "id", nonempty = TRUE)
  .gt_family(family)
  if (
    length(n) != 1L || is.na(n) || !is.numeric(n) || !is.finite(n) ||
      n != floor(n) || n > .Machine$integer.max || n <= 2L
  ) stop("`n` must be an integer greater than 2.", call. = FALSE)
  for (nm in c("alpha", "beta")) {
    value <- get(nm, inherits = FALSE)
    if (length(value) != 1L || !is.numeric(value) || is.na(value) ||
        !is.finite(value)) {
      stop(sprintf("`%s` must be a finite numeric scalar.", nm), call. = FALSE)
    }
  }
  if (identical(family, "gaussian")) {
    if (length(sigma) != 1L || !is.numeric(sigma) || is.na(sigma) ||
        !is.finite(sigma) || sigma <= 0) {
      stop("`sigma` must be a positive finite scalar for Gaussian scenarios.",
           call. = FALSE)
    }
    sigma <- as.numeric(sigma)
  } else if (!is.null(sigma)) {
    stop("`sigma` must be NULL for logistic scenarios.", call. = FALSE)
  }
  structure(list(id = id, n = as.integer(n), alpha = as.numeric(alpha),
                 beta = as.numeric(beta), sigma = sigma, family = family),
            class = "gt_scenario")
}

#' Define a fitting adapter
#'
#' @param id A non-empty scalar character identifier.
#' @param fit A function accepting one fresh data frame with exactly `x` and
#'   `y`, or `NULL` for the default fitter of the declared family.
#' @param metadata A list of descriptive metadata.
#' @param family Either `"gaussian"` or `"logistic"`.
#' @details The default Gaussian fitter uses `stats::lm` with an intercept and
#'   slope, estimates residual variance as SSE / (n - 2), and returns 90% and
#'   95% Student t intervals. Logistic fitters must be supplied explicitly or
#'   created with [gt_glm_adapter()]. Truth is withheld from every fitter.
#' @return A `gt_adapter` object with its declared family.
#' @examples
#' gt_adapter()
#' @export
gt_adapter <- function(id = "lm", fit = NULL, metadata = list(), family = "gaussian") {
  .gt_make_adapter(id, fit, metadata, family, .gt_lm_fit)
}

#' Create the native fixed-effect binomial-logit adapter
#'
#' @param id Adapter identifier.
#' @param metadata Descriptive metadata.
#' @return A logistic `gt_adapter` using stats::glm with 200 iterations.
#' @export
gt_glm_adapter <- function(id = "glm", metadata = list()) {
  .gt_scalar_character(id, "id", nonempty = TRUE)
  if (!is.list(metadata)) stop("`metadata` must be a list.", call. = FALSE)
  fit <- function(data) .gt_glm_fit(data, maxit = 200L)
  structure(list(id = id, fit = fit, metadata = metadata, family = "logistic"),
            class = "gt_adapter")
}

.gt_make_adapter <- function(id, fit, metadata, family, default_fit) {
  .gt_scalar_character(id, "id", nonempty = TRUE)
  .gt_family(family)
  if (!is.null(fit) && !is.function(fit)) stop("`fit` must be a function or NULL.", call. = FALSE)
  if (!is.list(metadata)) stop("`metadata` must be a list.", call. = FALSE)
  if (is.null(fit)) {
    if (identical(family, "logistic")) {
      stop("Supply a logistic fitter or use `gt_glm_adapter()`.", call. = FALSE)
    }
    fit <- default_fit
  }
  structure(list(id = id, fit = fit, metadata = metadata, family = family),
            class = "gt_adapter")
}

.gt_family <- function(x) {
  if (length(x) != 1L || !is.character(x) || is.na(x) ||
      !x %in% c("gaussian", "logistic")) {
    stop("`family` must be `gaussian` or `logistic`.", call. = FALSE)
  }
  x
}

#' Construct a fitting result
#'
#' @param estimates Named numeric estimates for `alpha` and `beta`.
#' @param intervals Optional data frame with target, level, lower, and upper
#'   columns.
#' @param converged A non-missing scalar logical value.
#' @param message A scalar character message.
#' @param diagnostics A list of diagnostic values.
#' @details Estimates must name `alpha` and `beta` exactly once. Missing,
#' duplicate, extra or nonfinite point targets reject the result. Intervals
#' use columns `target`, `level`, `lower`, `upper`, at levels 0.90 and 0.95.
#' Missing, duplicate, nonfinite or reversed intervals retain accepted points
#' and exclude the affected interval. Malformed or unsupported interval labels
#' make the interval schema unusable; no arbitrary name alignment is performed.
#' A fit exception or nonconvergence rejects both coefficient targets.
#' @return A `gt_result` list with estimates, intervals, convergence, message
#' and optional diagnostics.
#' @examples
#' gt_result(c(alpha = 1, beta = 0.7))
#' @export
gt_result <- function(
  estimates,
  intervals = NULL,
  converged = TRUE,
  message = "",
  diagnostics = list()
) {
  .gt_validate_estimates(estimates)
  if (!is.null(intervals) && !is.data.frame(intervals)) {
    stop("`intervals` must be NULL or a data frame.", call. = FALSE)
  }
  if (length(converged) != 1L || !is.logical(converged) || is.na(converged)) {
    stop(
      "`converged` must be a non-missing scalar logical value.",
      call. = FALSE
    )
  }
  .gt_scalar_character(message, "message")
  if (!is.list(diagnostics)) {
    stop("`diagnostics` must be a list.", call. = FALSE)
  }
  structure(
    list(
      estimates = estimates,
      intervals = intervals,
      converged = converged,
      message = message,
      diagnostics = diagnostics
    ),
    names = c("estimates", "intervals", "converged", "message", "diagnostics"),
    class = "gt_result"
  )
}

#' Run a reproducible Gaussian or logistic simulation study
#'
#' @param scenarios A `gt_scenario` or a non-empty list of them.
#' @param adapters A `gt_adapter` or a non-empty list of them.
#' @param reps Positive integer number of repetitions.
#' @param seed Non-negative integer master seed label. Character digit strings
#' retain every digit, including labels above 2^53; numeric inputs must be
#'   exact
#'   integers no larger than 2^53.
#' @details R streams use groundtruth-r-rng-v1: SHA256 of length-prefixed UTF8
#' master/scenario/rep/stream keys, allocated in canonical byte order to
#' distinct
#' positive integer seeds. Salted rehash resolves collisions; the full map is
#' retained. Each data/fit stream sets Mersenne-Twister/Inversion/Rejection.
#' Calls restore caller RNG kinds and seed (including absence) on success and
#' error. Ambient Box-Muller is refused before mutation because cached normal
#' state cannot be safely restored. Reordering the same input set is invariant;
#' adding keys may change rare collision allocations. R and Julia draws differ.
#' @return A `gt_study` list containing `scenarios`, `adapters`, `reps`,
#' `master_seed`, `rng`, `seed_map`, `data`, `ledger`, `targets` and
#' `provenance`.
#' @importFrom stats rnorm
#' @importFrom digest digest
#' @examples
#' study <- gt_runstudy(gt_scenario("example", n = 12L), reps = 2L)
#' gt_summarize(study)
#' @export
gt_runstudy <- function(
  scenarios,
  adapters = list(gt_adapter()),
  reps = 10L,
  seed = "20261004"
) {
  scenarios <- .gt_scenarios(scenarios)
  adapters <- .gt_adapters(adapters)
  reps <- .gt_reps(reps)
  master <- .gt_master_seed(seed)
  .gt_with_rng_state(.gt_run(
    scenarios,
    adapters,
    reps,
    master,
    .gt_seed_map(scenarios, adapters, reps, master)
  ))
}

#' Replay a recorded simulation study
#'
#' @param study A `gt_study` returned by [gt_runstudy()].
#' @param adapters Adapters to use, defaulting to the recorded adapters.
#' @details Only the recognized recorded RNG version/settings are accepted.
#' Replay validates and reuses the full map. Deterministic/stateless adapters
#' replay in the same environment; external inputs and closure state remain
#' the caller's responsibility.
#' @return A newly computed `gt_study` with the recorded random-stream map.
#' @examples
#' study <- gt_runstudy(gt_scenario("example"), reps = 1L)
#' replay <- gt_replay(study)
#' @export
gt_replay <- function(study, adapters = study$adapters) {
  .gt_validate_study(study)
  adapters <- .gt_adapters(adapters)
  if (
    !identical(
      sort(vapply(adapters, `[[`, "", "id")),
      sort(vapply(study$adapters, `[[`, "", "id"))
    )
  ) {
    stop("Replay adapters must have the recorded adapter ids.", call. = FALSE)
  }
  expected <- .gt_seed_map(
    study$scenarios,
    adapters,
    study$reps,
    study$master_seed
  )
  .gt_validate_seed_map(study$seed_map, expected)
  .gt_with_rng_state(.gt_run(
    study$scenarios,
    adapters,
    study$reps,
    study$master_seed,
    study$seed_map
  ))
}

#' Summarize simulation-study performance
#'
#' @param study A `gt_study` returned by [gt_runstudy()] or [gt_replay()].
#' @details Summaries group by scenario, adapter, target and interval level.
#' Attempted = successful + failed = usable_intervals + unusable_intervals.
#' Bias/RMSE use accepted points; bias MCSE is sd(errors)/sqrt(k), RMSE MCSE
#' is sd(errors^2)/(2*sqrt(k)*RMSE). Conditional coverage uses usable intervals;
#' covered_per_attempt uses all attempts. Coverage uncertainty has a Wilson 95%
#' Monte Carlo interval. All-failed bias/RMSE/conditional coverage are NA and
#' covered_per_attempt is zero. Singleton MCSE is NA; zero RMSE with k > 1
#' has MCSE zero. These are Monte Carlo uncertainty summaries, not a claim
#' of estimator calibration.
#' @return A data frame with keys, attempt/interval counts, bias/RMSE and their
#' MCSE, conditional coverage and its MCSE/Wilson bounds, and
#' covered_per_attempt.
#' @examples
#' study <- gt_runstudy(gt_scenario("example"), reps = 2L)
#' gt_summarize(study)
#' @export
gt_summarize <- function(study) {
  .gt_validate_study(study)
  target <- study$targets
  keys <- unique(target[c("scenario", "adapter", "target", "level")])
  keys <- keys[do.call(order, keys), , drop = FALSE]
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    z <- target[
      target$scenario == keys$scenario[i] &
        target$adapter == keys$adapter[i] &
        target$target == keys$target[i] &
        target$level == keys$level[i],
      ,
      drop = FALSE
    ]
    ok <- is.finite(z$estimate)
    errors <- z$estimate[ok] - z$truth[ok]
    k <- length(errors)
    attempted <- nrow(z)
    failed <- attempted - k
    bias <- if (k) mean(errors) else NA_real_
    bias_mcse <- if (k > 1L) stats::sd(errors) / sqrt(k) else NA_real_
    sq <- errors^2
    rmse <- if (k) sqrt(mean(sq)) else NA_real_
    rmse_mcse <- if (k > 1L) {
      if (rmse == 0) 0 else stats::sd(sq) / (2 * sqrt(k) * rmse)
    } else {
      NA_real_
    }
    usable <- z$usable_interval %in% TRUE
    m <- sum(usable)
    hits <- sum(z$covered[usable] %in% TRUE)
    coverage <- if (m) hits / m else NA_real_
    coverage_mcse <- if (m > 1L) {
      sqrt(coverage * (1 - coverage) / m)
    } else {
      NA_real_
    }
    wilson <- .gt_wilson(hits, m)
    data.frame(
      scenario = keys$scenario[i],
      adapter = keys$adapter[i],
      target = keys$target[i],
      level = keys$level[i],
      attempted = attempted,
      successful = k,
      failed = failed,
      usable_intervals = m,
      unusable_intervals = attempted - m,
      bias = bias,
      bias_mcse = bias_mcse,
      rmse = rmse,
      rmse_mcse = rmse_mcse,
      coverage = coverage,
      coverage_mcse = coverage_mcse,
      coverage_mc_lower = wilson[1],
      coverage_mc_upper = wilson[2],
      covered_per_attempt = hits / attempted,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Export a study as CSV files and a base-R manifest
#'
#' @param study A `gt_study` returned by [gt_runstudy()] or [gt_replay()].
#' @param path A destination path that must not already exist.
#' @details Writes data.csv, attempts.csv, targets.csv, summary.csv,
#' seed-map.csv, scenarios.csv and manifest.R with versioned schemas, full
#' RNG settings/map, character master seed, SHA256 file hashes, adapter metadata
#' and provenance. Source identity is the SHA256 of the sole implementation
#' file, R/groundtruth.R, recorded when the package is prepared. It identifies
#' the study producer separately from the exporter.
#' The CSV writer retains at least 17 significant digits;
#' missing values
#' are empty fields. Read according to manifest column types. `dget()` evaluates
#' R expressions, so read only trusted manifests. Custom adapter code/state is
#' not serialized. Any existing destination, including an empty directory, is
#' refused. Export failure removes only the newly created destination.
#' @return The normalized destination path, invisibly.
#' @examples
#' \dontrun{
#' study <- gt_runstudy(gt_scenario("example"), reps = 2L)
#' gt_export(study, tempfile("groundtruth-"))
#' }
#' @export
gt_export <- function(study, path) {
  .gt_validate_study(study)
  .gt_scalar_character(path, "path", nonempty = TRUE)
  if (file.exists(path) || dir.exists(path)) {
    stop("Destination already exists.", call. = FALSE)
  }
  made <- FALSE
  on.exit(
    if (made && dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  if (!dir.create(path, recursive = FALSE)) {
    stop("Could not create destination.", call. = FALSE)
  }
  made <- TRUE
  files <- list(
    data = study$data,
    attempts = study$ledger,
    targets = study$targets,
    summary = gt_summarize(study),
    `seed-map` = study$seed_map,
    scenarios = .gt_scenarios_frame(study$scenarios)
  )
  csv_names <- paste0(names(files), ".csv")
  for (i in seq_along(files)) {
    .gt_write_csv(files[[i]], file.path(path, csv_names[i]))
  }
  hashes <- vapply(
    file.path(path, csv_names),
    digest::digest,
    "",
    file = TRUE,
    algo = "sha256"
  )
  names(hashes) <- csv_names
  manifest <- list(
    schema_version = "groundtruth-r-export-v2",
    rng_algorithm = "groundtruth-r-rng-v1",
    master_seed = study$master_seed,
    csv = lapply(files, .gt_schema),
    sha256 = as.list(hashes),
    rng = list(
      kind = c("Mersenne-Twister", "Inversion", "Rejection"),
      seed_map = study$seed_map
    ),
    r_version = R.version.string,
    package_version = .gt_source_identity()$package_version,
    source_sha256 = study$provenance$source_sha256,
    source_scope = study$provenance$source_scope,
    exporter_source_sha256 = .gt_source_identity()$source_sha256,
    digest_version = as.character(utils::packageVersion("digest")),
    adapters = lapply(study$adapters, function(a) {
      list(id = a$id, family = a$family, metadata = a$metadata)
    }),
    provenance = study$provenance
  )
  writeLines(
    utils::capture.output(dput(manifest)),
    file.path(path, "manifest.R"),
    useBytes = TRUE
  )
  made <- FALSE
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

.gt_scalar_character <- function(x, name, nonempty = FALSE) {
  if (
    length(x) != 1L || !is.character(x) || is.na(x) || (nonempty && !nzchar(x))
  ) {
    stop(
      sprintf(
        "`%s` must be %sa scalar character value.",
        name,
        if (nonempty) "a non-empty " else ""
      ),
      call. = FALSE
    )
  }
  invisible(x)
}
.gt_validate_estimates <- function(x) {
  if (
    !is.numeric(x) ||
      length(x) != 2L ||
      is.null(names(x)) ||
      anyDuplicated(names(x)) ||
      !setequal(names(x), c("alpha", "beta")) ||
      any(!is.finite(x))
  ) {
    stop(
      paste(
        "`estimates` must be finite named numeric",
        "alpha and beta values exactly once."
      ),
      call. = FALSE
    )
  }
  invisible(x)
}
.gt_scenarios <- function(x) {
  if (inherits(x, "gt_scenario")) {
    x <- list(x)
  }
  if (
    !is.list(x) ||
      !length(x) ||
      !all(vapply(x, inherits, logical(1), what = "gt_scenario"))
  ) {
    stop(
      "`scenarios` must be a scenario or non-empty list of scenarios.",
      call. = FALSE
    )
  }
  ids <- vapply(x, `[[`, "", "id")
  if (anyDuplicated(ids)) {
    stop("Scenario ids must be unique.", call. = FALSE)
  }
  x <- lapply(x, function(s) {
    family <- if (is.null(s$family)) "gaussian" else s$family
    sigma <- if (identical(family, "gaussian") && is.null(s$sigma)) 1.2 else s$sigma
    gt_scenario(s$id, s$n, s$alpha, s$beta, sigma, family)
  })
  x[order(.gt_byte_order(ids))]
}
.gt_adapters <- function(x) {
  if (inherits(x, "gt_adapter")) {
    x <- list(x)
  }
  if (
    !is.list(x) ||
      !length(x) ||
      !all(vapply(x, inherits, logical(1), what = "gt_adapter"))
  ) {
    stop(
      "`adapters` must be an adapter or non-empty list of adapters.",
      call. = FALSE
    )
  }
  ids <- vapply(x, `[[`, "", "id")
  if (anyDuplicated(ids)) {
    stop("Adapter ids must be unique.", call. = FALSE)
  }
  x <- lapply(x, function(a) {
    .gt_scalar_character(a$id, "adapter id", nonempty = TRUE)
    if (!is.function(a$fit) || !is.list(a$metadata)) stop("Malformed adapter.", call. = FALSE)
    if (is.null(a$family)) a$family <- "gaussian"
    .gt_family(a$family)
    a
  })
  x[order(.gt_byte_order(ids))]
}
.gt_reps <- function(x) {
  if (
    length(x) != 1L ||
      !is.numeric(x) ||
      is.na(x) ||
      !is.finite(x) ||
      x != floor(x) ||
      x > .Machine$integer.max ||
      x < 1L
  ) {
    stop("`reps` must be a positive integer.", call. = FALSE)
  }
  as.integer(x)
}
.gt_master_seed <- function(x) {
  if (is.character(x)) {
    .gt_scalar_character(x, "seed", nonempty = TRUE)
    if (!grepl("^[0-9]+$", x)) {
      stop(
        "Character `seed` must contain non-negative integer digits.",
        call. = FALSE
      )
    }
    return(x)
  }
  if (
    length(x) != 1L ||
      !is.numeric(x) ||
      is.na(x) ||
      !is.finite(x) ||
      x < 0 ||
      x > 9007199254740992 ||
      x != floor(x)
  ) {
    stop(
      "`seed` must be a non-negative exact integer no larger than 2^53.",
      call. = FALSE
    )
  }
  format(x, scientific = FALSE, trim = TRUE, digits = 17)
}

.gt_raw_part <- function(x) {
  z <- charToRaw(enc2utf8(as.character(x)))
  c(charToRaw(as.character(length(z))), as.raw(58L), z)
}
.gt_key_text <- function(master, scenario, rep, stream) {
  one <- function(x) {
    raw <- charToRaw(enc2utf8(as.character(x)))
    paste0(length(raw), ":", enc2utf8(as.character(x)))
  }
  paste(vapply(list(master, scenario, rep, stream), one, ""), collapse = "|")
}
.gt_key_raw <- function(master, scenario, rep, stream, salt = NULL) {
  parts <- list(master, scenario, as.character(rep), stream)
  if (!is.null(salt)) {
    parts <- c(parts, as.character(salt))
  }
  Reduce(function(a, b) c(a, charToRaw("|"), b), lapply(parts, .gt_raw_part))
}
.gt_sha256 <- function(raw) {
  digest::digest(raw, algo = "sha256", serialize = FALSE)
}
.gt_hex_seed <- function(hex) {
  bytes <- substring(hex, seq(1L, 7L, by = 2L), seq(2L, 8L, by = 2L))
  value <- 0
  # Convert only two hexadecimal characters at a time: never parse a 32-bit
  # value with `strtoi()`, whose signed range would overflow for many digests.
  for (b in bytes) {
    value <- value * 256 + strtoi(b, base = 16L)
  }
  as.integer((value %% 2147483646) + 1)
}
.gt_byte_order <- function(x) {
  vapply(
    x,
    function(z) {
      paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(z)))), collapse = "")
    },
    ""
  )
}

# Internal seam: `hash_fun` is deliberately injectable for collision tests.
.gt_seed_map <- function(
  scenarios,
  adapters,
  reps,
  master,
  hash_fun = .gt_sha256
) {
  keys <- character()
  scenario <- character()
  rep <- integer()
  stream <- character()
  for (s in scenarios) {
    for (r in seq_len(reps)) {
      keys <- c(keys, .gt_key_text(master, s$id, r, "data"))
      scenario <- c(scenario, s$id)
      rep <- c(rep, r)
      stream <- c(stream, "data")
      for (a in adapters) {
        keys <- c(keys, .gt_key_text(master, s$id, r, paste0("fit:", a$id)))
        scenario <- c(scenario, s$id)
        rep <- c(rep, r)
        stream <- c(stream, paste0("fit:", a$id))
      }
    }
  }
  ord <- order(.gt_byte_order(keys))
  keys <- keys[ord]
  scenario <- scenario[ord]
  rep <- rep[ord]
  stream <- stream[ord]
  out_hash <- character(length(keys))
  salt <- integer(length(keys))
  seeds <- integer(length(keys))
  for (i in seq_along(keys)) {
    k <- 0L
    repeat {
      h <- hash_fun(.gt_key_raw(
        master,
        scenario[i],
        rep[i],
        stream[i],
        if (k) k else NULL
      ))
      if (
        !is.character(h) || length(h) != 1L || !grepl("^[0-9A-Fa-f]{64}$", h)
      ) {
        stop("Hash function must return one SHA256 hex digest.", call. = FALSE)
      }
      candidate <- .gt_hex_seed(h)
      if (!(candidate %in% seeds[seq_len(i - 1L)])) {
        break
      }
      k <- k + 1L
      if (k > 100000L) {
        stop("Could not resolve a seed collision.", call. = FALSE)
      }
    }
    out_hash[i] <- h
    salt[i] <- k
    seeds[i] <- candidate
  }
  data.frame(
    key = keys,
    scenario = scenario,
    rep = rep,
    stream = stream,
    hash = out_hash,
    salt = salt,
    seed = seeds,
    stringsAsFactors = FALSE
  )
}

.gt_with_rng_state <- function(expr) {
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (identical(old_kind[2], "Box-Muller")) {
    stop("Box-Muller RNG state is not supported.", call. = FALSE)
  }
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit(
    {
      do.call(RNGkind, as.list(old_kind))
      if (had_seed) {
        # nolint start: object_name_linter. R's prescribed RNG state name.
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
        # nolint end: object_name_linter.
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  do.call(RNGkind, list("Mersenne-Twister", "Inversion", "Rejection"))
  force(expr)
}
.gt_generate <- function(s, seed) {
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  x <- stats::rnorm(s$n)
  if (identical(s$family, "gaussian")) {
    e <- stats::rnorm(s$n)
    y <- s$alpha + s$beta * x + s$sigma * e
  } else {
    y <- stats::rbinom(s$n, size = 1L, prob = stats::plogis(s$alpha + s$beta * x))
  }
  if (any(!is.finite(x)) || any(!is.finite(y))) stop("Generated nonfinite data.", call. = FALSE)
  data.frame(x = x, y = y, stringsAsFactors = FALSE)
}
.gt_lm_fit <- function(data) {
  if (
    !is.data.frame(data) ||
      !identical(names(data), c("x", "y")) ||
      nrow(data) <= 2L ||
      any(!is.finite(data$x)) ||
      any(!is.finite(data$y))
  ) {
    stop("Default fitter requires finite x and y.", call. = FALSE)
  }
  fit <- stats::lm(
    y ~ x,
    data = data,
    na.action = stats::na.fail,
    singular.ok = FALSE
  )
  co <- stats::coef(fit)
  if (!identical(names(co), c("(Intercept)", "x")) || any(!is.finite(co))) {
    stop("Rank-two fit with intercept and x required.", call. = FALSE)
  }
  co <- co[c("(Intercept)", "x")]
  names(co) <- c("alpha", "beta")
  df <- fit$df.residual
  sigma2 <- sum(stats::residuals(fit)^2) / df
  covariance <- stats::vcov(fit)
  dimnames(covariance) <- list(c("alpha", "beta"), c("alpha", "beta"))
  se <- sqrt(diag(covariance))
  names(se) <- c("alpha", "beta")
  intervals <- do.call(
    rbind,
    lapply(c(0.90, 0.95), function(level) {
      q <- stats::qt((1 + level) / 2, df)
      data.frame(
        target = c("alpha", "beta"),
        level = level,
        lower = co - q * se,
        upper = co + q * se,
        stringsAsFactors = FALSE
      )
    })
  )
  gt_result(
    co,
    intervals,
    diagnostics = list(
      sigma2 = sigma2,
      covariance = covariance,
      se = se,
      predictions = stats::fitted(fit),
      residuals = stats::residuals(fit)
    )
  )
}

.gt_logistic_residual <- function(y, eta) {
  residual <- numeric(length(y))
  success <- y == 1
  residual[success] <- stats::plogis(-eta[success])
  residual[!success] <- -stats::plogis(eta[!success])
  residual
}

.gt_logistic_loss <- function(y, eta) {
  loss <- numeric(length(y))
  positive_eta <- eta >= 0
  success <- y == 1
  idx <- positive_eta & success
  loss[idx] <- log1p(exp(-eta[idx]))
  idx <- positive_eta & !success
  loss[idx] <- eta[idx] + log1p(exp(-eta[idx]))
  idx <- !positive_eta & success
  loss[idx] <- -eta[idx] + log1p(exp(eta[idx]))
  idx <- !positive_eta & !success
  loss[idx] <- log1p(exp(eta[idx]))
  loss
}

.gt_glm_fit <- function(data, maxit = 200L) {
  if (!is.data.frame(data) || !identical(names(data), c("x", "y")) ||
      nrow(data) <= 2L || !is.numeric(data$x) || !is.numeric(data$y) ||
      any(!is.finite(data$x)) || any(!is.finite(data$y)) ||
      any(!(data$y %in% c(0, 1)))) {
    stop("Logistic fitter requires finite numeric x and numeric 0/1 y.", call. = FALSE)
  }
  x <- data$x; y <- data$y
  if (all(y == 0)) stop("All-zero outcomes have no finite logistic MLE.", call. = FALSE)
  if (all(y == 1)) stop("All-one outcomes have no finite logistic MLE.", call. = FALSE)
  if (length(unique(x)) < 2L) stop("Constant predictor: a rank-two design is required.", call. = FALSE)
  a0 <- range(x[y == 0]); a1 <- range(x[y == 1])
  if (a0[2] < a1[1] || a1[2] < a0[1]) stop("Complete separation: no finite MLE.", call. = FALSE)
  if (a0[2] == a1[1] || a1[2] == a0[1]) stop("Quasi separation: no finite MLE.", call. = FALSE)
  center <- min(x) / 2 + max(x) / 2
  scale <- max(abs(x - center))
  if (!is.finite(center) || !is.finite(scale) || scale <= 0) stop("Predictor standardization failed.", call. = FALSE)
  z <- (x - center) / scale
  design <- cbind(`(Intercept)` = 1, x = z)
  if (any(!is.finite(design))) stop("Standardized design is nonfinite.", call. = FALSE)
  if (maxit == 0L) stop("Native glm iteration limit is 0; fit did not converge.", call. = FALSE)
  warnings <- character()
  fit <- withCallingHandlers(
    stats::glm(y ~ z, data = data.frame(y = y, z = z),
               family = stats::binomial(link = "logit"),
               na.action = stats::na.fail, singular.ok = FALSE,
               control = stats::glm.control(epsilon = 1e-12, maxit = maxit)),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  if (!isTRUE(fit$converged)) stop(sprintf("Native glm did not converge (iterations=%d).", fit$iter), call. = FALSE)
  gamma <- stats::coef(fit)
  if (length(gamma) != 2L || any(!is.finite(gamma))) stop("Native glm returned nonfinite coefficients.", call. = FALSE)
  eta <- drop(design %*% gamma)
  p <- stats::plogis(eta)
  residual <- .gt_logistic_residual(y, eta)
  w <- exp(-abs(eta)) / (1 + exp(-abs(eta)))^2
  score <- drop(crossprod(design, residual)) / length(y)
  information <- crossprod(design, design * w) / length(y)
  correction <- tryCatch(solve(information, score), error = function(e) NULL)
  correction_eta <- if (is.null(correction)) NULL else drop(design %*% correction)
  if (is.null(correction) || any(!is.finite(correction_eta)) ||
      max(abs(score)) > 1e-9 || max(abs(correction_eta)) > 1e-9) {
    stop("Native glm failed the independent standardized final score check.", call. = FALSE)
  }
  x_ratio <- center / scale
  gcoef <- c(alpha = gamma[[1]] - x_ratio * gamma[[2]],
             beta = gamma[[2]] / scale)
  native_vcov <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (!is.null(native_vcov) && all(is.finite(native_vcov))) {
    dimnames(native_vcov) <- list(c("gamma0", "gamma1"), c("gamma0", "gamma1"))
  } else {
    native_vcov <- NULL
  }
  final_covariance <- tryCatch(solve(crossprod(design, design * w)), error = function(e) NULL)
  covariance <- matrix(NA_real_, 2L, 2L,
                       dimnames = list(c("alpha", "beta"), c("alpha", "beta")))
  se <- c(alpha = NA_real_, beta = NA_real_)
  intervals <- list()
  interval_message <- ""
  if (!is.null(final_covariance) && all(is.finite(final_covariance))) {
    # Transform each element separately: a nonfinite slope variance must not
    # contaminate the otherwise usable intercept variance through 0 * Inf.
    alpha_variance <- final_covariance[1, 1] -
      2 * x_ratio * final_covariance[1, 2] +
      x_ratio^2 * final_covariance[2, 2]
    beta_se <- sqrt(final_covariance[2, 2]) / abs(scale)
    alpha_beta <- (final_covariance[1, 2] - x_ratio * final_covariance[2, 2]) / scale
    if (is.finite(alpha_variance) && alpha_variance >= 0) {
      covariance[1, 1] <- alpha_variance
      se["alpha"] <- sqrt(alpha_variance)
    }
    covariance[2, 2] <- beta_se^2
    covariance[1, 2] <- covariance[2, 1] <- alpha_beta
    if (is.finite(beta_se)) se["beta"] <- beta_se
    for (target in c("alpha", "beta")) {
      if (!is.finite(se[[target]])) next
      for (level in c(.90, .95)) {
        q <- stats::qnorm((1 + level) / 2)
        lower <- gcoef[[target]] - q * se[[target]]
        upper <- gcoef[[target]] + q * se[[target]]
        if (is.finite(lower) && is.finite(upper)) {
          intervals[[length(intervals) + 1L]] <- data.frame(
            target = target, level = level, lower = lower, upper = upper,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (length(intervals)) intervals <- do.call(rbind, intervals) else intervals <- NULL
    if (is.null(intervals) || nrow(intervals) < 4L) {
      interval_message <- "One or more final-information Wald intervals omitted."
    }
  } else {
    intervals <- NULL
    interval_message <- "Final information matrix is unavailable; intervals omitted."
  }
  loss <- .gt_logistic_loss(y, eta)
  nll <- mean(loss)
  gt_result(gcoef, intervals, message = interval_message, diagnostics = list(
    se = se, covariance = covariance, native_vcov = native_vcov,
    native_warnings = warnings, iterations = fit$iter, native_converged = fit$converged,
    inference = "final-information Wald", score = score,
    information = information, correction = correction,
    linear_predictor_correction = correction_eta, mean_nll = nll,
    predictions = p
  ))
}

.gt_result_checked <- function(x) {
  if (!inherits(x, "gt_result")) {
    return(list(ok = FALSE, message = "Fitter did not return gt_result."))
  }
  .gt_scalar_character(x$message, "result message")
  point <- tryCatch(.gt_validate_estimates(x$estimates), error = function(e) e)
  if (inherits(point, "error")) {
    return(list(ok = FALSE, message = conditionMessage(point)))
  }
  if (
    length(x$converged) != 1L || !is.logical(x$converged) || is.na(x$converged)
  ) {
    return(list(ok = FALSE, message = "Invalid convergence flag."))
  }
  intervals <- .gt_intervals(x$intervals)
  interval_messages <- unique(vapply(intervals, `[[`, "", "message"))
  interval_messages <- interval_messages[nzchar(interval_messages)]
  message <- paste(c(x$message, interval_messages), collapse = " ")
  list(
    ok = TRUE,
    estimates = x$estimates,
    intervals = intervals,
    converged = x$converged,
    message = message
  )
}
.gt_intervals <- function(x) {
  ans <- list()
  levels <- c(0.90, 0.95)
  targets <- c("alpha", "beta")
  valid_schema <- is.data.frame(x) &&
    identical(names(x), c("target", "level", "lower", "upper")) &&
    is.character(x$target) &&
    is.numeric(x$level) &&
    is.numeric(x$lower) &&
    is.numeric(x$upper)
  for (tar in targets) {
    for (lev in levels) {
      ok <- FALSE
      lo <- hi <- NA_real_
      msg <- "Interval unavailable."
      if (is.null(x)) {
        msg <- "Intervals not supplied."
      } else if (valid_schema) {
        hit <- which(x$target == tar & x$level == lev)
        if (
          length(hit) == 1L &&
            is.finite(x$lower[hit]) &&
            is.finite(x$upper[hit]) &&
            x$lower[hit] <= x$upper[hit]
        ) {
          ok <- TRUE
          lo <- x$lower[hit]
          hi <- x$upper[hit]
          msg <- ""
        } else {
          msg <- "Missing, duplicate, or invalid interval."
        }
        if (any(!(x$target %in% targets)) || any(!(x$level %in% levels))) {
          ok <- FALSE
          lo <- hi <- NA_real_
          msg <- "Extra or unsupported interval labels."
        }
      } else {
        msg <- "Malformed interval schema."
      }
      ans[[paste(tar, lev)]] <- list(
        ok = ok,
        lower = lo,
        upper = hi,
        message = msg
      )
    }
  }
  ans
}

.gt_seed_for <- function(map, scenario, rep, stream) {
  hit <- map$seed[
    map$scenario == scenario & map$rep == rep & map$stream == stream
  ]
  if (length(hit) != 1L) {
    stop("Recorded seed map is incomplete.", call. = FALSE)
  }
  hit
}
.gt_empty_target <- function(scenario, rep, adapter, truth, message) {
  do.call(
    rbind,
    lapply(c("alpha", "beta"), function(target) {
      do.call(
        rbind,
        lapply(c(0.90, 0.95), function(level) {
          data.frame(
            scenario = scenario,
            rep = rep,
            adapter = adapter,
            target = target,
            level = level,
            truth = truth[[target]],
            estimate = NA_real_,
            lower = NA_real_,
            upper = NA_real_,
            usable_interval = FALSE,
            covered = NA,
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )
}
.gt_target_rows <- function(
  scenario,
  rep,
  adapter,
  truth,
  result,
  accepted,
  message
) {
  if (!accepted) {
    return(.gt_empty_target(scenario, rep, adapter, truth, message))
  }
  do.call(
    rbind,
    lapply(c("alpha", "beta"), function(target) {
      do.call(
        rbind,
        lapply(c(0.90, 0.95), function(level) {
          iv <- result$intervals[[paste(target, level)]]
          estimate <- unname(result$estimates[target])
          usable <- isTRUE(iv$ok)
          data.frame(
            scenario = scenario,
            rep = rep,
            adapter = adapter,
            target = target,
            level = level,
            truth = truth[[target]],
            estimate = estimate,
            lower = if (usable) iv$lower else NA_real_,
            upper = if (usable) iv$upper else NA_real_,
            usable_interval = usable,
            covered = if (usable) {
              (iv$lower <= truth[[target]] && truth[[target]] <= iv$upper)
            } else {
              NA
            },
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )
}
.gt_run <- function(scenarios, adapters, reps, master, seed_map) {
  data_rows <- list()
  ledger_rows <- list()
  target_rows <- list()
  di <- li <- ti <- 0L
  for (s in scenarios) {
    for (r in seq_len(reps)) {
      dseed <- .gt_seed_for(seed_map, s$id, r, "data")
      generated <- tryCatch(.gt_generate(s, dseed), error = function(e) e)
      truth <- list(alpha = s$alpha, beta = s$beta)
      if (!inherits(generated, "error")) {
        di <- di + 1L
        data_rows[[di]] <- data.frame(
          scenario = s$id,
          rep = r,
          row = seq_len(nrow(generated)),
          x = generated$x,
          y = generated$y,
          stringsAsFactors = FALSE
        )
      }
      for (a in adapters) {
        fseed <- .gt_seed_for(seed_map, s$id, r, paste0("fit:", a$id))
        status <- "ok"
        message <- ""
        checked <- NULL
        if (inherits(generated, "error")) {
          status <- "generation_error"
          message <- conditionMessage(generated)
        } else if (!identical(s$family, a$family)) {
          status <- "fit_error"
          message <- sprintf("Scenario family '%s' does not match adapter family '%s'.", s$family, a$family)
        } else {
          set.seed(
            fseed,
            kind = "Mersenne-Twister",
            normal.kind = "Inversion",
            sample.kind = "Rejection"
          )
          # Deliberately construct a new two-column object for each adapter.
          supplied <- data.frame(
            x = generated$x,
            y = generated$y,
            stringsAsFactors = FALSE
          )
          raw <- tryCatch(a$fit(supplied), error = function(e) e)
          if (inherits(raw, "error")) {
            status <- "fit_error"
            message <- conditionMessage(raw)
          } else {
            checked <- tryCatch(.gt_result_checked(raw), error = function(e) {
              list(ok = FALSE, message = conditionMessage(e))
            })
            if (!checked$ok) {
              status <- "invalid_estimate"
              message <- checked$message
            } else if (!isTRUE(checked$converged)) {
              status <- "nonconverged"
              message <- checked$message
            } else {
              message <- checked$message
            }
          }
        }
        li <- li + 1L
        ledger_rows[[li]] <- data.frame(
          scenario = s$id,
          rep = r,
          adapter = a$id,
          data_seed = dseed,
          fit_seed = fseed,
          status = status,
          message = message,
          stringsAsFactors = FALSE
        )
        ti <- ti + 1L
        target_rows[[ti]] <- .gt_target_rows(
          s$id,
          r,
          a$id,
          truth,
          checked,
          identical(status, "ok"),
          message
        )
      }
    }
  }
  data <- if (length(data_rows)) {
    do.call(rbind, data_rows)
  } else {
    data.frame(
      scenario = character(),
      rep = integer(),
      row = integer(),
      x = numeric(),
      y = numeric()
    )
  }
  ledger <- do.call(rbind, ledger_rows)
  targets <- do.call(rbind, target_rows)
  structure(
    list(
      scenarios = scenarios,
      adapters = adapters,
      reps = reps,
      master_seed = master,
      rng = list(
        algorithm = "groundtruth-r-rng-v1",
        kind = c("Mersenne-Twister", "Inversion", "Rejection")
      ),
      seed_map = seed_map,
      data = data,
      ledger = ledger,
      targets = targets,
      provenance = list(
        package = "groundtruth",
        version = .gt_source_identity()$package_version,
        source_sha256 = .gt_source_identity()$source_sha256,
        source_scope = .gt_source_identity()$source_scope,
        data_source = "independent R family-specific generator v1",
        families = stats::setNames(lapply(scenarios, function(s) list(
          family = s$family,
          model = if (s$family == "gaussian") "y=alpha+beta*x+sigma*error; independent standard normal x,error" else "y~Bernoulli(plogis(alpha+beta*x)); independent standard normal x"
        )), vapply(scenarios, `[[`, "", "id")),
        model = "family-specific declared scenario model",
        r_version = R.version.string,
        reference = "https://github.com/itchyshin/GroundTruth.jl",
        reference_commit = "3738bfe340fff62ec1afb0a78f0101babe79fbd7",
        agreement_scope = paste(
          "paired public Gaussian linear fixtures;",
          "no calibration claim"
        )
      )
    ),
    class = "gt_study"
  )
}

.gt_wilson <- function(hits, n) {
  if (!n) {
    return(c(NA_real_, NA_real_))
  }
  z <- stats::qnorm(0.975)
  p <- hits / n
  den <- 1 + z^2 / n
  mid <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(max(0, mid - half), min(1, mid + half))
}
.gt_validate_seed_map <- function(map, expected) {
  required <- c("key", "scenario", "rep", "stream", "hash", "salt", "seed")
  if (
    !is.data.frame(map) ||
      !identical(names(map), required) ||
      nrow(map) != nrow(expected)
  ) {
    stop("Invalid recorded seed map schema.", call. = FALSE)
  }
  a <- map[order(map$key), required, drop = FALSE]
  b <- expected[order(expected$key), required, drop = FALSE]
  rownames(a) <- rownames(b) <- NULL
  if (!identical(a, b)) {
    stop("Recorded seed map does not match study settings.", call. = FALSE)
  }
  invisible(TRUE)
}
.gt_validate_study <- function(study) {
  if (!inherits(study, "gt_study") || !is.list(study)) {
    stop("`study` must be a gt_study object.", call. = FALSE)
  }
  if (
    !is.list(study$rng) ||
      !identical(study$rng$algorithm, "groundtruth-r-rng-v1") ||
      !identical(
        study$rng$kind,
        c("Mersenne-Twister", "Inversion", "Rejection")
      )
  ) {
    stop("Unsupported recorded RNG version or kind settings.", call. = FALSE)
  }
  scenarios <- .gt_scenarios(study$scenarios)
  adapters <- .gt_adapters(study$adapters)
  reps <- .gt_reps(study$reps)
  master <- .gt_master_seed(study$master_seed)
  .gt_validate_seed_map(
    study$seed_map,
    .gt_seed_map(scenarios, adapters, reps, master)
  )
  schemas <- list(
    data = c("scenario", "rep", "row", "x", "y"),
    ledger = c(
      "scenario",
      "rep",
      "adapter",
      "data_seed",
      "fit_seed",
      "status",
      "message"
    ),
    targets = c(
      "scenario",
      "rep",
      "adapter",
      "target",
      "level",
      "truth",
      "estimate",
      "lower",
      "upper",
      "usable_interval",
      "covered"
    )
  )
  for (nm in names(schemas)) {
    if (
      !is.data.frame(study[[nm]]) ||
        !identical(names(study[[nm]]), schemas[[nm]])
    ) {
      stop(sprintf("Study has invalid %s schema.", nm), call. = FALSE)
    }
  }
  invisible(TRUE)
}
.gt_scenarios_frame <- function(scenarios) {
  data.frame(
    id = vapply(scenarios, `[[`, "", "id"),
    n = vapply(scenarios, `[[`, integer(1), "n"),
    alpha = vapply(scenarios, `[[`, numeric(1), "alpha"),
    beta = vapply(scenarios, `[[`, numeric(1), "beta"),
    sigma = vapply(scenarios, function(s) if (is.null(s$sigma)) NA_real_ else s$sigma, numeric(1)),
    family = vapply(scenarios, `[[`, "", "family"),
    stringsAsFactors = FALSE
  )
}
.gt_csv_value <- function(x) {
  if (is.integer(x)) {
    return(ifelse(is.na(x), "", as.character(x)))
  }
  if (is.numeric(x)) {
    return(ifelse(
      is.na(x),
      "",
      trimws(formatC(x, digits = 17, format = "g", decimal.mark = "."))
    ))
  }
  if (is.logical(x)) {
    return(ifelse(is.na(x), "", ifelse(x, "TRUE", "FALSE")))
  }
  text <- enc2utf8(as.character(x))
  ifelse(is.na(x), "", paste0('"', gsub('"', '""', text, fixed = TRUE), '"'))
}
.gt_write_csv <- function(x, path) {
  # Numeric fields stay unquoted so scan(colClasses=integer/numeric) is usable.
  # Seventeen significant digits retain machine precision on decimal reading.
  columns <- lapply(x, .gt_csv_value)
  header <- paste(.gt_csv_value(names(x)), collapse = ",")
  rows <- if (nrow(x)) do.call(paste, c(columns, sep = ",")) else character()
  writeLines(c(header, rows), path, useBytes = TRUE)
}
.gt_schema <- function(x) {
  list(columns = names(x), types = vapply(x, function(z) class(z)[1], ""))
}

.gt_source_identity <- function() {
  file <- system.file("source-identity.R", package = "groundtruth")
  if (!nzchar(file)) {
    stop(
      "Package source identity is missing; rebuild the package.",
      call. = FALSE
    )
  }
  identity <- dget(file)
  if (
    !is.list(identity) ||
      !identical(identity$source_scope, "R/groundtruth.R") ||
      length(identity$source_sha256) != 1L ||
      !is.character(identity$source_sha256) ||
      is.na(identity$source_sha256) ||
      !grepl("^[0-9a-f]{64}$", identity$source_sha256)
  ) {
    stop("Invalid package source identity.", call. = FALSE)
  }
  identity
}
