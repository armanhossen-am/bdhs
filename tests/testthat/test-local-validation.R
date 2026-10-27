# Validation against official DHS values with the maintainer's own microdata.
# Skipped unless BDHS_LOCAL_TESTS=true. Files are read read-only and only
# aggregate estimates are compared; nothing is written.

local_files <- tibble::tribble(
  ~survey_id, ~recode, ~path,
  "BD1994DHS", "KR", "BDHS Caeserean/Data/BDHS1993KR.DTA",
  "BD2000DHS", "KR", "BDHS Caeserean/Data/BDHS1999KR.DTA",
  "BD2004DHS", "KR", "BDHS Caeserean/Data/BDHS2004KR.DTA",
  "BD2007DHS", "KR", "BDHS Caeserean/Data/BDHS2007KR.DTA",
  "BD2011DHS", "KR", "BDHS Caeserean/Data/BDHS2011KR.DTA",
  "BD2014DHS", "KR", "BDHS Caeserean/Data/BDHS2014KR.DTA",
  "BD2017DHS", "KR", "BDHS NPD/BDKR7RDT/BDKR7RFL.DTA",
  "BD2022DHS", "KR", "BDHS Caeserean/Data/BDHS2022KR.DTA",
  "BD2017DHS", "IR", "BDHS NPD/BDIR7RDT/BDIR7RFL.DTA",
  "BD2022DHS", "IR", "BDHS Data/BDIR81FL.DTA",
  "BD2017DHS", "PR", "BDHS NPD/BDPR7RDT/BDPR7RFL.DTA",
  "BD2022DHS", "PR", "BDHS Pre-hypertension/BDPR81FL.DTA",
  "BD2017DHS", "BR", "BDHS NPD/BDBR7RDT/BDBR7RFL.DTA"
)

# Read only the columns the indicators need, with lower-case names and the
# attributes bdhs_read() sets.
read_local_recode <- function(path, survey_id, recode) {
  nm <- names(haven::read_dta(path, n_max = 0))
  cols <- indicator_columns(applicable_indicators(recode, survey_id), tolower(nm))
  d <- haven::read_dta(path, col_select = dplyr::all_of(nm[tolower(nm) %in% cols]))
  names(d) <- tolower(names(d))
  attr(d, "bdhs_survey_id") <- survey_id
  attr(d, "bdhs_recode") <- recode
  d
}

test_that("local microdata reproduce the official DHS values", {
  dir <- skip_if_no_local_data()
  record <- indicator_validation_status()
  checked <- 0L
  for (i in seq_len(nrow(local_files))) {
    f <- file.path(dir, local_files$path[i])
    if (!file.exists(f)) next
    s <- local_files$survey_id[i]
    r <- local_files$recode[i]
    d <- read_local_recode(f, s, r)
    v <- suppressMessages(suppressWarnings(bdhs_validate(d)))
    expect_gt(nrow(v), 0)
    expected <- record$indicator[record$survey_id == s & record$recode == r & record$pass]
    got <- v$indicator[v$pass]
    expect_true(all(expected %in% got), info = paste(s, r, "failed:", paste(setdiff(expected, got), collapse = ", ")))
    # At least 90% of the comparable indicators pass in every file.
    expect_gte(mean(v$pass), 0.9)
    checked <- checked + 1L
  }
  expect_gt(checked, 0)
})
