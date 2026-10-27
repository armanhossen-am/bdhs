# Report top-level objects defined in more than one R/ file. A later file
# (in collation order) silently overrides an earlier one, which has bitten
# this package before (.design_vars). Run: Rscript tools/check-duplicate-defs.R
defs <- do.call(rbind, lapply(list.files("R", pattern = "[.]R$", full.names = TRUE), function(f) {
  ex <- parse(f, keep.source = FALSE)
  nm <- vapply(ex, function(e) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])) as.character(e[[2]]) else NA_character_
  }, "")
  nm <- nm[!is.na(nm)]
  if (length(nm)) data.frame(file = basename(f), name = nm) else NULL
}))
dup <- defs[defs$name %in% defs$name[duplicated(defs$name)], ]
if (nrow(dup)) {
  print(dup[order(dup$name), ], row.names = FALSE)
  quit(status = 1)
}
cat("No duplicated top-level definitions among", nrow(defs), "objects.\n")
