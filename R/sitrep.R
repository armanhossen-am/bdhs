# Situation report ----------------------------------------------------------------------------

#' Report on your bdhs set-up
#'
#' Prints where your data store is, whether it is writable and stored safely
#' (not inside a git repository or a cloud-synchronised folder), which
#' datasets are registered, the versions of the packages used to read and
#' store them, and reminders of the DHS Program's conditions of use.
#'
#' @return A list with elements `data_dir`, `data_dir_source`, `exists`,
#'   `writable`, `git_root`, `sync_service`, `registry` (one row per
#'   survey with its registered recodes) and `packages`, invisibly.
#' @family data store
#' @export
#' @examples
#' bdhs_sitrep()
bdhs_sitrep <- function() {
  dir <- store_dir()
  exists <- dir.exists(dir)
  writable <- dir_writable(dir)
  git <- path_git_root(dir)
  sync <- path_sync_service(dir)
  reg <- registry_read(dir)
  pkgs <- c("haven", "arrow", "httr2", "keyring", "rdhs")
  versions <- vapply(pkgs, function(p) {
    if (rlang::is_installed(p)) as.character(utils::packageVersion(p)) else NA_character_
  }, character(1))

  cli::cli_h1("bdhs situation report")
  cli::cli_h2("Data directory")
  cli::cli_bullets(c(
    "*" = "{.path {dir}} ({store_dir_source()})",
    if (exists) c("v" = "Exists") else c("i" = "Does not exist yet; it is created on the first {.fn bdhs_import}."),
    if (writable) c("v" = "Writable") else c("x" = "Not writable"),
    if (is.null(git)) c("v" = "Not inside a git repository") else c("x" = "Inside the git repository {.path {git}}: never commit DHS data."),
    if (is.null(sync)) c("v" = "Not inside a known cloud-sync folder") else c("x" = "Inside a {sync} folder: DHS data must not be uploaded to third-party services.")
  ))

  cli::cli_h2("Registered datasets")
  summary <- NULL
  if (!nrow(reg)) {
    cli::cli_bullets(c("i" = "None yet. Download your files from the DHS Program and run {.code bdhs_import(\"path\")}."))
  } else {
    paths <- registry_abs_path(reg, dir)
    reg$ok <- file.exists(paths)
    summary <- tibble::tibble(
      survey_id = unique(reg$survey_id),
      recodes = vapply(unique(reg$survey_id), function(s) {
        r <- reg[reg$survey_id == s, ]
        paste0(r$recode, ifelse(r$linked, "*", ""), ifelse(r$ok, "", "!"), collapse = " ")
      }, character(1), USE.NAMES = FALSE)
    )
    lines <- paste0(summary$survey_id, " (", survey_label(summary$survey_id), "): ", summary$recodes)
    cli::cli_bullets(stats::setNames(cli_escape(lines), rep("*", length(lines))))
    cli::cli_text("{nrow(reg)} dataset{?s}; formats: {paste(names(table(reg$format)), table(reg$format), sep = ' x', collapse = ', ')}.")
    if (any(reg$linked)) cli::cli_text("{.emph *} registered in place (original file not copied).")
    if (any(!reg$ok)) cli::cli_text("{.emph !} file missing: re-import it with {.fn bdhs_import}.")
  }

  cli::cli_h2("Packages")
  cli::cli_bullets(c(
    "*" = "bdhs {bdhs_version()}",
    "*" = "haven {versions[['haven']]}",
    "*" = if (is.na(versions[["arrow"]])) "arrow not installed: copies are stored in their original format (install arrow for fast Parquet storage)" else "arrow {versions[['arrow']]} (Parquet storage)",
    "*" = if (is.na(versions[["httr2"]])) "httr2 not installed: {.fn bdhs_download} is unavailable" else "httr2 {versions[['httr2']]} (for the experimental {.fn bdhs_download})",
    "*" = if (is.na(versions[["keyring"]])) "keyring not installed: DHS password is read from {.envvar DHS_PASSWORD} or asked for" else "keyring {versions[['keyring']]} (DHS password store)"
  ))

  cli::cli_h2("Conditions of use")
  cli::cli_bullets(c(
    "!" = "DHS microdata are licensed to you for your registered research project only. Do not share or redistribute them, directly or within any tool, package, repository or supplementary file.",
    "!" = "Keep the files secure, and cite the survey (NIPORT and ICF) in every publication.",
    "!" = "Bangladesh condition: send a copy of every publication that uses BDHS data to the NIPORT library ({.email niportru@citechco.net}).",
    "i" = "Full terms: {.url https://dhsprogram.com/data/terms-of-use.cfm}"
  ))

  invisible(list(
    data_dir = dir,
    data_dir_source = store_dir_source(),
    exists = exists,
    writable = writable,
    git_root = git,
    sync_service = sync,
    registry = summary %||% tibble::tibble(survey_id = character(), recodes = character()),
    packages = versions
  ))
}

# Can we write to `dir` (or create it)?
dir_writable <- function(dir) {
  d <- normalize_path(dir)
  while (!dir.exists(d)) {
    parent <- dirname(d)
    if (identical(parent, d)) {
      return(FALSE)
    }
    d <- parent
  }
  unname(file.access(d, 2) == 0)
}
