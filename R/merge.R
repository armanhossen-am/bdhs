# Linking recode files ------------------------------------------------------------------

#' Merge two BDHS recode files on the right DHS keys
#'
#' Detects which recodes `x` and `y` are (see [bdhs_recodes()]) and joins them
#' on the identifiers the DHS Program documents for that pair, so you do not
#' have to remember which line-number variable links a child to its household
#' roster entry. `x` is the base file: its rows (unit of analysis) and
#' attributes are kept, and the columns of `y` are added.
#'
#' | Files | Key (`x` side = `y` side) |
#' |---|---|
#' | IR, KR, BR with HR | `v001`, `v002` = `hv001`, `hv002` |
#' | IR with PR | `v001`, `v002`, `v003` = `hv001`, `hv002`, `hvidx` |
#' | KR, BR with IR | `caseid` |
#' | KR with BR | `caseid`, `bidx` |
#' | KR, BR with PR | `v001`, `v002`, `b16` = `hv001`, `hv002`, `hvidx` (child's line number; needs `b16`) |
#' | PR with HR | `hv001`, `hv002` (or `hhid`) |
#' | MR with HR / PR | `mv001`, `mv002` (, `mv003` = `hvidx`) |
#' | HW with KR / PR | `hwhhid`, `hwline` = household part of `caseid`, `b16` / `hhid`, `hvidx` |
#' | WI with HR, PR / IR, KR, BR | `whhid` = `hhid` / household part of `caseid` |
#'
#' Both files must come from the same survey round (pooled data are joined
#' within rounds on `survey_id`). Columns of `y` that repeat an identifier or
#' design column of `x`, or that hold exactly the same values as the
#' same-named column of `x`, are dropped; other duplicated names get the
#' suffix `suffix`.
#'
#' The keys follow the DHS Guide to DHS Statistics ("Merging files"). The
#' WI and HW keys for 1993-94 to 2004 follow DHS conventions and could not be
#' verified against the files; check the match rate that is reported.
#'
#' @param x,y Data frames of BDHS recode data (as returned by [bdhs_read()]).
#'   `x` is the base file.
#' @param join `"left"` (default) keeps every row of `x`; `"inner"` keeps
#'   only rows of `x` with a match in `y`.
#' @param by Optional explicit key: a named character vector like
#'   `c(v001 = "hv001", v002 = "hv002")` (names in `x`, values in `y`),
#'   overriding detection.
#' @param recode_x,recode_y Optional recode codes overriding detection.
#' @param suffix Suffix for duplicated column names from `y`; defaults to
#'   `"_"` followed by the lower-case recode of `y` (e.g. `"_hr"`).
#' @return `x` with the columns of `y` added (one row per row of `x` when
#'   `y` has one row per key), with the attributes of `x`.
#' @export
#' @examples
#' kr <- bdhs_simulate(2022, "KR")
#' hr <- bdhs_simulate(2022, "HR")
#' kh <- bdhs_merge(kr, hr)
#' table(kh$hv201)
#'
#' # Mothers' characteristics on the births file
#' br <- bdhs_simulate(2022, "BR")
#' ir <- bdhs_simulate(2022, "IR")
#' nrow(bdhs_merge(br, ir)) == nrow(br)
bdhs_merge <- function(x, y, join = c("left", "inner"), by = NULL, recode_x = NULL, recode_y = NULL,
                       suffix = NULL) {
  join <- rlang::arg_match(join)
  if (!is.data.frame(x) || !is.data.frame(y)) {
    cli::cli_abort("{.arg x} and {.arg y} must be data frames.")
  }
  rx <- merge_recode(x, recode_x, "x")
  ry <- merge_recode(y, recode_y, "y")
  suffix <- suffix %||% paste0("_", tolower(ry))

  # Same survey, and survey_id as an extra key for pooled data.
  sx <- merge_survey_rows(x)
  sy <- merge_survey_rows(y)
  if (!is.null(sx) && !is.null(sy)) {
    ux <- unique(sx)
    uy <- unique(sy)
    if (!length(intersect(ux, uy))) {
      cli::cli_abort(c(
        "{.arg x} and {.arg y} come from different surveys.",
        "i" = "{.arg x}: {.val {ux}}; {.arg y}: {.val {uy}}."
      ))
    }
  }
  pooled <- (!is.null(sx) && length(unique(sx)) > 1L) || (!is.null(sy) && length(unique(sy)) > 1L)

  spec <- if (is.null(by)) {
    merge_key_spec(rx, ry, x, y)
  } else {
    if (is.null(names(by)) || any(!nzchar(names(by)))) {
      cli::cli_abort("{.arg by} must be a named character vector, e.g. {.code c(v001 = \"hv001\")}.")
    }
    list(
      x = lapply(names(by), function(nm) key_col(x, nm)), y = lapply(unname(by), function(nm) key_col(y, nm)),
      x_cols = names(by), y_cols = unname(by), desc = paste0(names(by), " = ", by, collapse = ", "),
      same_unit = FALSE
    )
  }
  kx <- spec$x
  ky <- spec$y
  if (pooled) {
    if (is.null(sx) || is.null(sy)) {
      cli::cli_abort("Pooled data can only be merged when both files identify their survey round.")
    }
    kx <- c(kx, list(rep_len(sx, nrow(x))))
    ky <- c(ky, list(rep_len(sy, nrow(y))))
  }
  key_x <- composite_key(kx)
  key_y <- composite_key(ky)

  dup_y <- anyDuplicated(key_y[!is.na(key_y)]) > 0L
  if (!dup_y) {
    idx <- match(key_x, key_y, incomparables = NA)
    rows_x <- seq_len(nrow(x))
    if (join == "inner") {
      rows_x <- which(!is.na(idx))
      idx <- idx[rows_x]
    }
  } else {
    pairs <- dplyr::left_join(
      tibble::tibble(.key = key_x, .rx = seq_along(key_x)),
      tibble::tibble(.key = key_y[!is.na(key_y)], .ry = which(!is.na(key_y))),
      by = ".key", relationship = "many-to-many", na_matches = "never"
    )
    if (join == "inner") {
      pairs <- pairs[!is.na(pairs$.ry), ]
    }
    rows_x <- pairs$.rx
    idx <- pairs$.ry
    cli::cli_inform(c(
      "!" = "Several rows of {.arg y} ({ry}) match some rows of {.arg x} ({rx}); those rows of {.arg x} are repeated.",
      "i" = "For one row per {rx} record, use the lower-level file as {.arg x}."
    ))
  }
  n_match <- sum(!is.na(idx))
  cli::cli_inform(c(
    "v" = "Merged {ry} into {rx} on {spec$desc}{if (pooled) ', survey_id' else ''}: {n_match} of {length(idx)} row{?s} matched ({round(100 * n_match / max(1, length(idx)), 1)}%)."
  ))

  # Columns from y.
  y_names <- names(y)
  drop <- unique(c(spec$y_cols, "survey_id"))
  shared <- intersect(setdiff(y_names, drop), names(x))
  design <- shared[is_id_design_col(shared)]
  same <- character()
  for (nm in setdiff(shared, design)) {
    if (spec$same_unit || same_values(x[[nm]][rows_x], y[[nm]][idx])) same <- c(same, nm)
  }
  renamed <- setdiff(shared, c(design, same))
  keep_y <- setdiff(y_names, c(drop, design, same))
  if (length(renamed)) {
    cli::cli_inform(c(
      "i" = "{length(renamed)} column{?s} of {ry} also in {.arg x} with different values got the suffix {.val {suffix}}: {.var {renamed}}."
    ))
  }

  out <- if (identical(rows_x, seq_len(nrow(x)))) x else x[rows_x, , drop = FALSE]
  add <- lapply(keep_y, function(nm) {
    col <- y[[nm]]
    col[idx]
  })
  new_names <- ifelse(keep_y %in% renamed, paste0(keep_y, suffix), keep_y)
  names(add) <- new_names
  out <- add_columns(out, add)
  restore_attrs(out, x)
}

merge_recode <- function(data, recode, arg) {
  if (!is.null(recode)) {
    return(check_recode(recode))
  }
  rec <- attr(data, "bdhs_recode", exact = TRUE)
  if (!is.null(rec)) {
    return(rec)
  }
  nm <- tolower(names(data))
  if ("whhid" %in% nm) {
    return("WI")
  }
  if ("hwhhid" %in% nm) {
    return("HW")
  }
  tryCatch(data_recode(data), error = function(e) {
    cli::cli_abort(c("Can't tell which recode {.arg {arg}} is.", "i" = "Supply {.arg recode_{arg}}."), parent = e)
  })
}

# Survey id per row, or NULL when it can't be determined (WI/HW files).
merge_survey_rows <- function(data) {
  if ("survey_id" %in% names(data)) {
    s <- as.character(zap_all(data$survey_id))
    ids <- bdhs_survey_id(unique(s), error = FALSE)
    return(ids[match(s, unique(s))])
  }
  id <- tryCatch(data_survey_id(data), error = function(e) NULL)
  if (is.null(id) || length(id) != 1L) NULL else id
}

key_col <- function(data, nm, call = rlang::caller_env()) {
  if (!nm %in% names(data)) {
    cli::cli_abort("Key variable {.field {nm}} not found.", call = call)
  }
  x <- data[[nm]]
  if (is.character(x)) squish(x) else as.numeric(zap_all(x))
}

squish <- function(x) gsub("\\s+", " ", trimws(as.character(x)))

# Household part of a DHS caseid (caseid = hhid + 3-character line number).
caseid_hhid <- function(caseid) {
  caseid <- as.character(caseid)
  squish(substr(caseid, 1L, pmax(nchar(caseid) - 3L, 0L)))
}

composite_key <- function(keys) {
  keys <- lapply(keys, function(k) if (is.numeric(k)) format(k, scientific = FALSE, trim = TRUE) else k)
  out <- do.call(paste, c(keys, sep = "\r"))
  bad <- Reduce(`|`, lapply(keys, function(k) is.na(k) | k == "NA"))
  out[bad] <- NA_character_
  out
}

# Key specification for a pair of recodes.
merge_key_spec <- function(rx, ry, x, y, call = rlang::caller_env()) {
  hh_of <- function(r) switch(r, HR = , PR = c("hv001", "hv002"), MR = c("mv001", "mv002"), c("v001", "v002"))
  person_of <- function(r) switch(r, PR = "hvidx", MR = "mv003", IR = "v003", KR = , BR = "b16", NULL)
  women <- c("IR", "KR", "BR")
  hh_files <- c("HR", "PR")
  num <- function(d, cols) lapply(cols, function(nm) key_col(d, nm, call = call))
  spec <- function(xc, yc, kx = num(x, xc), ky = num(y, yc), same_unit = FALSE) {
    list(
      x = kx, y = ky, x_cols = xc, y_cols = yc, same_unit = same_unit,
      desc = paste0(paste(xc, collapse = ", "), if (!identical(xc, yc)) paste0(" = ", paste(yc, collapse = ", ")))
    )
  }
  pair <- paste(rx, ry)
  # Same recode family: a woman's record in KR/BR <-> IR, a birth KR <-> BR.
  if (rx %in% c("KR", "BR") && ry == "IR" || rx == "IR" && ry %in% c("KR", "BR")) {
    return(if (all(c("caseid") %in% c(names(x))) && "caseid" %in% names(y)) {
      spec("caseid", "caseid", same_unit = TRUE)
    } else {
      spec(c("v001", "v002", "v003"), c("v001", "v002", "v003"), same_unit = TRUE)
    })
  }
  if (setequal(c(rx, ry), c("KR", "BR")) && rx != ry) {
    return(spec(c("caseid", "bidx"), c("caseid", "bidx"), same_unit = TRUE))
  }
  if (rx %in% c(women, "MR") && ry == "HR" || rx == "HR" && ry %in% c(women, "MR")) {
    return(spec(hh_of(rx), hh_of(ry)))
  }
  if (rx == "PR" && ry == "HR" || rx == "HR" && ry == "PR") {
    return(spec(c("hv001", "hv002"), c("hv001", "hv002")))
  }
  if (rx %in% c("IR", "MR") && ry == "PR" || rx == "PR" && ry %in% c("IR", "MR")) {
    return(spec(c(hh_of(rx), person_of(rx)), c(hh_of(ry), person_of(ry))))
  }
  if (rx %in% c("KR", "BR") && ry == "PR" || rx == "PR" && ry %in% c("KR", "BR")) {
    kid <- if (rx == "PR") y else x
    if (!"b16" %in% names(kid)) {
      cli::cli_abort(c(
        "The {.field b16} variable (child's line number in the household) is needed to link children to {.val PR}.",
        "i" = "It is missing in this file (it does not exist before the 2004 BDHS)."
      ), call = call)
    }
    return(spec(c(hh_of(rx), person_of(rx)), c(hh_of(ry), person_of(ry))))
  }
  if ("HW" %in% c(rx, ry)) {
    other <- setdiff(c(rx, ry), "HW")
    hw <- if (rx == "HW") x else y
    od <- if (rx == "HW") y else x
    hw_key <- list(squish(hw$hwhhid), as.numeric(zap_all(hw$hwline)))
    od_key <- switch(other,
      PR = list(squish(od$hhid), as.numeric(zap_all(od$hvidx))),
      KR = ,
      BR = {
        if (!"b16" %in% names(od)) {
          cli::cli_abort("The {.field b16} variable is needed to link children to the HW file.", call = call)
        }
        list(caseid_hhid(od$caseid), as.numeric(zap_all(od$b16)))
      },
      cli::cli_abort("The HW file can be merged with {.val KR}, {.val BR} or {.val PR}, not {.val {other}}.", call = call)
    )
    od_cols <- if (other == "PR") c("hhid", "hvidx") else c("caseid", "b16")
    return(if (rx == "HW") {
      spec(c("hwhhid", "hwline"), od_cols, kx = hw_key, ky = od_key)
    } else {
      spec(od_cols, c("hwhhid", "hwline"), kx = od_key, ky = hw_key)
    })
  }
  if ("WI" %in% c(rx, ry)) {
    other <- setdiff(c(rx, ry), "WI")
    wi <- if (rx == "WI") x else y
    od <- if (rx == "WI") y else x
    od_key <- if (other %in% hh_files) {
      list(squish(od$hhid))
    } else if (other %in% women) {
      list(caseid_hhid(od$caseid))
    } else {
      cli::cli_abort("The WI file can be merged with household, women's, children's or births files, not {.val {other}}.", call = call)
    }
    od_cols <- if (other %in% hh_files) "hhid" else "caseid"
    wi_key <- list(squish(wi$whhid))
    return(if (rx == "WI") {
      spec("whhid", od_cols, kx = wi_key, ky = od_key)
    } else {
      spec(od_cols, "whhid", kx = od_key, ky = wi_key)
    })
  }
  cli::cli_abort(c(
    "Don't know how to link {.val {rx}} and {.val {ry}} files.",
    "i" = "Supply the key with {.arg by}, e.g. {.code by = c(v001 = \"hv001\", v002 = \"hv002\")}."
  ), call = call)
}

# Identifier and survey-design columns (never duplicated from y).
is_id_design_col <- function(nm) {
  grepl("^(caseid|hhid|whhid|hwhhid|hwline|bidx|midx|hvidx|survey_id|pool_wt|awfact[a-z]*|(h|m)?v0(0[0-8]|2[0-5]))$", nm)
}

same_values <- function(a, b) {
  if (length(a) != length(b)) {
    return(FALSE)
  }
  ok <- !is.na(b)
  if (!any(ok)) {
    return(TRUE)
  }
  pa <- if (is.factor(a) || is.character(a)) as.character(a) else as.numeric(zap_all(a))
  pb <- if (is.factor(b) || is.character(b)) as.character(b) else as.numeric(zap_all(b))
  isTRUE(all(pa[ok] == pb[ok] | (is.na(pa[ok]) & is.na(pb[ok])), na.rm = FALSE)) ||
    identical(pa[ok], pb[ok])
}

# Copy the non-structural attributes of `from` onto `to`.
restore_attrs <- function(to, from) {
  a <- attributes(from)
  a <- a[setdiff(names(a), c("names", "row.names", "class"))]
  for (nm in names(a)) attr(to, nm) <- a[[nm]]
  to
}
