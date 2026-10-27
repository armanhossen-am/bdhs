#!/usr/bin/env Rscript
# =============================================================================
# 04-build-crosswalk.R -- build inst/extdata/crosswalk.rds from the
# hand-curated rules in data-raw/inputs/crosswalk.json.
#
# The JSON is the source of truth. It was produced from METADATA ONLY
# (variable names, value labels, code frequencies) of the maintainer's local
# BDHS files and validated with weighted aggregates against the DHS API; it
# contains no respondent-level data.
#
# The .rds is the plain parse (`jsonlite::read_json(simplifyVector = FALSE)`):
# a list with `meta`, `lookups` and `variables` (one element per harmonized
# variable). The package normalises it once per session (R/crosswalk.R,
# cw_normalize()). This script also runs that normaliser so that a broken rule
# (unknown method, a map value that is not a documented level, an expression
# that does not parse, ...) fails here rather than at run time.
#
# Usage (from the package root): Rscript data-raw/04-build-crosswalk.R
# =============================================================================

json <- file.path("data-raw", "inputs", "crosswalk.json")
out <- file.path("inst", "extdata", "crosswalk.rds")
stopifnot(file.exists(json))

cw <- jsonlite::read_json(json, simplifyVector = FALSE)
stopifnot(
  identical(sort(names(cw)), c("lookups", "meta", "variables")),
  length(cw$variables) > 0
)

# Validate with the package's own normaliser.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
norm <- bdhs:::cw_normalize(cw)
problems <- bdhs:::cw_check(norm)
if (length(problems)) {
  stop("Crosswalk problems:\n", paste("-", problems, collapse = "\n"), call. = FALSE)
}

saveRDS(cw, out, version = 2, compress = "xz")

n_rules <- sum(vapply(norm$vars, function(v) length(v$rounds), integer(1)))
message(sprintf(
  "Wrote %s: %d variables, %d round rules, crosswalk version %s.",
  out, length(norm$vars), n_rules, cw$meta$version
))
