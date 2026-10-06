# Run from the source root before tests/build after editing R or DESCRIPTION.
stopifnot(file.exists("R/groundtruth.R"), file.exists("DESCRIPTION"))
desc <- read.dcf("DESCRIPTION")
stopifnot(identical(unname(desc[1, "Package"]), "groundtruth"))
identity <- list(
  schema = "groundtruth-r-source-v1",
  package_version = unname(desc[1, "Version"]),
  source_scope = "R/groundtruth.R",
  source_sha256 = digest::digest(
    file = "R/groundtruth.R",
    algo = "sha256",
    serialize = FALSE
  )
)
quote_r <- function(x) encodeString(x, quote = "\"")
writeLines(
  c(
    "list(",
    paste0("  schema = ", quote_r(identity$schema), ","),
    paste0("  package_version = ", quote_r(identity$package_version), ","),
    paste0("  source_scope = ", quote_r(identity$source_scope), ","),
    "  source_sha256 =",
    paste0("    ", quote_r(identity$source_sha256)),
    ")"
  ),
  "inst/source-identity.R"
)
