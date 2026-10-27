# Rebuilds the validation record in R/indicator-defs-validated.R: national
# estimates computed by bdhs from the maintainer's own BDHS microdata,
# compared with the official DHS Program API values. Only aggregate
# statistics are written; the microdata are only read. From the package root:
#
#   BDHS_LOCAL_DIR=~/Documents/MU/Research Rscript data-raw/07-build-validation.R

devtools::load_all()

dir <- path.expand(Sys.getenv("BDHS_LOCAL_DIR", "~/Documents/MU/Research"))
files <- list.files(dir, pattern = "[.]dta$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
ids <- do.call(rbind, lapply(files, function(f) tryCatch(bdhs_identify_file(f), error = function(e) NULL)))
ids <- ids[!is.na(ids$survey_id) & !ids$derived & ids$recode %in% c("IR", "KR", "BR", "PR", "HR"), ]
ids <- ids[!duplicated(paste(ids$survey_id, ids$recode)), ]

res <- lapply(seq_len(nrow(ids)), function(i) {
  message("Validating ", ids$survey_id[i], " ", ids$recode[i])
  inds <- applicable_indicators(ids$recode[i], ids$survey_id[i], with_api = TRUE)
  if (!length(inds)) {
    return(NULL)
  }
  nm <- names(haven::read_dta(ids$path[i], n_max = 0))
  cols <- indicator_columns(inds, tolower(nm))
  d <- bdhs_read_file(ids$path[i], vars = nm[tolower(nm) %in% cols])
  v <- suppressMessages(suppressWarnings(bdhs_validate(d)))
  v <- v[!is.na(v$pass), ]
  data.frame(survey_id = v$survey_id, recode = ids$recode[i], indicator = v$indicator,
             computed = round(v$computed, 3), official = v$official, pass = v$pass)
})
out <- do.call(rbind, res)
out <- out[order(out$survey_id, out$recode, out$indicator), ]

rows <- sprintf('  "%s", "%s", "%s", %.3f, %.2f, %s,', out$survey_id, out$recode, out$indicator,
                out$computed, out$official, ifelse(out$pass, "TRUE", "FALSE"))
rows[length(rows)] <- sub(",$", "", rows[length(rows)])
code <- c(
  "# Local validation record ---------------------------------------------------------",
  "#",
  "# National aggregates computed by bdhs_validate() from the maintainer's own",
  "# copies of the BDHS recode files, compared with the DHS Program API values",
  "# bundled in bdhs_indicators. Only aggregate numbers are stored (no microdata).",
  paste0("# Regenerate with data-raw/07-build-validation.R. Last run: ", format(Sys.Date()), "."),
  "",
  ".indicator_validation <- tibble::tribble(",
  "  ~survey_id, ~recode, ~indicator, ~computed, ~official, ~pass,",
  rows,
  ")"
)
writeLines(code, "R/indicator-defs-validated.R")
message(sum(out$pass), " of ", nrow(out), " comparisons pass.")
