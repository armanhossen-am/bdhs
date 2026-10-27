# Synthetic DHS-structured data ---------------------------------------------------

#' Simulate synthetic BDHS-like recode data
#'
#' Generates **fake** data with the structure of a BDHS recode file: the same
#' variable names, codes, value labels, identifiers and complex survey design
#' variables (clusters, strata, weights, all-women factors) as the real files.
#' Every value is randomly generated, so the output can be shared freely. It
#' powers the package examples, tests and vignettes, and lets you write and
#' test analysis code before you have DHS access.
#'
#' All recodes simulated with the same `survey` and `seed` describe the same
#' synthetic population, so they can be linked with [bdhs_merge()] exactly like
#' real files: women in `"IR"` are household members in `"PR"`, children in
#' `"KR"` are births in `"BR"`, and so on.
#'
#' The data are *structurally* faithful: region codes follow the division
#' layout of the requested round (5 divisions in 1993-94, 8 from 2017-18), and
#' the core variables use DHS-7 coding. They are not *statistically* faithful,
#' and round-specific coding changes in less common variables are not
#' reproduced. Never use simulated data for substantive conclusions.
#'
#' @param survey A survey reference understood by [bdhs_survey_id()]. Only
#'   the standard DHS rounds are supported.
#' @param recode One of `"IR"` (women), `"KR"` (children under 5), `"BR"`
#'   (births), `"HR"` (households) or `"PR"` (household members).
#' @param n_clusters Number of clusters (primary sampling units) to simulate.
#' @param households Households per cluster.
#' @param seed Random seed. The same seed always gives the same population;
#'   the global random number generator state is left untouched.
#'
#' @return A tibble with lower-case DHS variable names and haven-labelled
#'   columns, carrying the attributes `bdhs_survey_id`, `bdhs_recode` and
#'   `bdhs_simulated = TRUE`.
#' @export
#' @examples
#' ir <- bdhs_simulate(2022, "IR")
#' ir[1:5, c("caseid", "v001", "v005", "v022", "v024", "v025", "v012", "v313")]
#'
#' # The same synthetic population in household-member form
#' pr <- bdhs_simulate(2022, "PR")
#' nrow(pr)
bdhs_simulate <- function(survey = 2022, recode = c("IR", "KR", "BR", "HR", "PR"),
                          n_clusters = 40, households = 15, seed = 1) {
  survey_id <- bdhs_survey_id(survey, type = "DHS")
  if (length(survey_id) != 1L) {
    cli::cli_abort("{.arg survey} must refer to a single survey round.")
  }
  recode <- toupper(recode)
  recode <- rlang::arg_match(recode, c("IR", "KR", "BR", "HR", "PR"))
  stopifnot(n_clusters >= 2, households >= 1)

  world <- withr_seed(seed, sim_world(survey_id, n_clusters, households))
  out <- switch(recode,
    HR = world$hr,
    PR = world$pr,
    IR = world$ir,
    BR = world$br,
    KR = world$kr
  )
  out <- tibble::as_tibble(out)
  attr(out, "bdhs_survey_id") <- survey_id
  attr(out, "bdhs_recode") <- recode
  attr(out, "bdhs_simulated") <- TRUE
  out
}

# Run `code` with a fixed seed, restoring the caller's RNG state afterwards.
withr_seed <- function(seed, code) {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    if (is.null(old)) {
      rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old, envir = globalenv())
    }
  })
  set.seed(seed)
  code
}

# Round-specific facts used by the simulator.
sim_round_info <- function(survey_id) {
  divisions <- switch(survey_id,
    BD1994DHS = c("barishal", "chittagong", "dhaka", "khulna", "rajshahi"),
    BD1997DHS = ,
    BD2000DHS = ,
    BD2004DHS = ,
    BD2007DHS = c("barishal", "chittagong", "dhaka", "khulna", "rajshahi", "sylhet"),
    BD2011DHS = ,
    BD2014DHS = c("barishal", "chittagong", "dhaka", "khulna", "rajshahi", "rangpur", "sylhet"),
    BD2017DHS = c("barishal", "chittagong", "dhaka", "khulna", "mymensingh", "rajshahi", "rangpur", "sylhet"),
    BD2022DHS = c("barishal", "chattogram", "dhaka", "khulna", "mymensingh", "rajshahi", "rangpur", "sylhet")
  )
  # Interview months (first, last) as calendar year + month.
  fieldwork <- switch(survey_id,
    BD1994DHS = c(1993, 11, 1994, 3),
    BD1997DHS = c(1996, 11, 1997, 3),
    BD2000DHS = c(1999, 11, 2000, 3),
    BD2004DHS = c(2004, 1, 2004, 5),
    BD2007DHS = c(2007, 3, 2007, 8),
    BD2011DHS = c(2011, 7, 2011, 12),
    BD2014DHS = c(2014, 6, 2014, 11),
    BD2017DHS = c(2017, 10, 2018, 3),
    BD2022DHS = c(2022, 8, 2022, 12)
  )
  code <- survey_file_code(survey_id)
  list(
    survey_id = survey_id,
    divisions = divisions,
    cmc_first = (fieldwork[[1]] - 1900) * 12 + fieldwork[[2]],
    cmc_last = (fieldwork[[3]] - 1900) * 12 + fieldwork[[4]],
    v000 = paste0("BD", substr(code, 1, 1)),
    two_digit_year = survey_id %in% c("BD1994DHS", "BD1997DHS")
  )
}

lbl <- function(x, labels, label) {
  x <- haven::labelled(x, labels = labels)
  attr(x, "label") <- label
  x
}

num <- function(x, label) {
  attr(x, "label") <- label
  x
}

sim_world <- function(survey_id, n_clusters, households) {
  info <- sim_round_info(survey_id)
  n_div <- length(info$divisions)

  # Clusters: division, residence, stratum and a cluster-level weight.
  # Clusters are allocated to division x residence strata in pairs, so that
  # every stratum that is used has at least two PSUs, as in the real designs.
  cl <- data.frame(v001 = seq_len(n_clusters))
  n_strata <- 2L * n_div
  cl$strat <- ((cl$v001 - 1L) %/% 2L) %% n_strata + 1L
  if (n_clusters %% 2L == 1L) {
    cl$strat[[n_clusters]] <- cl$strat[[n_clusters - 1L]]
  }
  cl$div <- (cl$strat - 1L) %/% 2L + 1L
  cl$res <- (cl$strat - 1L) %% 2L + 1L
  w <- stats::rlnorm(n_clusters, 0, 0.35)
  cl$wt <- round(w / mean(w) * 1e6)
  cl$cmc <- sample(info$cmc_first:info$cmc_last, n_clusters, replace = TRUE)

  # Households.
  hh <- cl[rep(seq_len(n_clusters), each = households), ]
  hh$hv002 <- rep(seq_len(households), times = n_clusters)
  nh <- nrow(hh)
  hh$hhid <- sprintf("%8d%4d", hh$v001, hh$hv002)
  hh$wscore <- stats::rnorm(nh, ifelse(hh$res == 1L, 0.8, -0.3), 1)
  brk <- stats::quantile(hh$wscore, c(0.2, 0.4, 0.6, 0.8))
  hh$wq <- findInterval(hh$wscore, brk) + 1L
  hh$religion <- sample(1:4, nh, replace = TRUE, prob = c(0.90, 0.08, 0.01, 0.01))
  hh$elec <- as.integer(stats::runif(nh) < stats::plogis(1 + hh$wscore))
  hh$water <- sample(c(11L, 12L, 21L, 31L, 43L, 71L), nh, replace = TRUE,
                     prob = c(0.05, 0.05, 0.80, 0.04, 0.03, 0.03))
  hh$toilet <- sample(c(11L, 12L, 15L, 21L, 22L, 23L, 31L), nh, replace = TRUE,
                      prob = c(0.10, 0.15, 0.05, 0.35, 0.20, 0.10, 0.05))
  hh$fuel <- sample(c(2L, 6L, 8L, 9L, 11L), nh, replace = TRUE,
                    prob = c(0.25, 0.25, 0.20, 0.20, 0.10))

  # One ever-married woman (15-49) in most households, plus her husband.
  hh$has_woman <- stats::runif(nh) < 0.92
  wm <- hh[hh$has_woman, ]
  nw <- nrow(wm)
  wm$age <- sample(15:49, nw, replace = TRUE)
  wm$v008 <- wm$cmc
  wm$v011 <- wm$v008 - wm$age * 12L - sample(0:11, nw, replace = TRUE)
  wm$afm <- pmin(wm$age, pmax(12L, round(stats::rnorm(nw, 16.5, 2.2))))
  wm$edu <- pmin(3L, pmax(0L, round(stats::rnorm(nw, 1.3 + 0.35 * wm$wscore - 0.02 * (wm$age - 30), 0.8))))
  wm$edyears <- ifelse(wm$edu == 0L, 0L, ifelse(wm$edu == 1L, sample(1:5, nw, TRUE),
    ifelse(wm$edu == 2L, sample(6:10, nw, TRUE), sample(11:16, nw, TRUE))))
  wm$marital <- sample(c(1L, 3L, 4L, 5L), nw, replace = TRUE, prob = c(0.93, 0.04, 0.015, 0.015))
  wm$line <- 2L # line 1 is the household head (husband)

  # Birth histories.
  births <- sim_births(wm)
  births$b16 <- NA_integer_
  alive <- which(births$b5 == 1L)
  ord <- alive[order(births$caseid_key[alive], -births$bidx[alive])]
  births$b16[ord] <- 2L + stats::ave(births$bidx[ord], births$caseid_key[ord], FUN = seq_along)
  wm$ceb <- tabulate(match(births$caseid_key, wm$hhid), nbins = nw)

  married <- wm$marital == 1L
  wm$method <- ifelse(!married, 0L, sample(c(0L, 1L, 2L, 3L, 5L, 6L, 8L, 9L, 11L), nw, TRUE,
    prob = c(0.36, 0.25, 0.03, 0.11, 0.07, 0.05, 0.05, 0.05, 0.03)))
  wm$v313 <- ifelse(wm$method == 0L, 0L, ifelse(wm$method %in% c(8L, 9L), 2L, 3L))
  wm$pregnant <- as.integer(married & wm$age < 40 & stats::runif(nw) < 0.05)
  wm$v626a <- ifelse(!married, 8L, ifelse(wm$v313 > 0L, sample(3:4, nw, TRUE),
    ifelse(wm$age > 45, 9L, sample(c(1L, 2L, 7L), nw, TRUE, prob = c(0.08, 0.08, 0.84)))))
  wm$bmi <- round(stats::rnorm(nw, 2250 + 120 * wm$wscore, 380))
  wm$work <- as.integer(stats::runif(nw) < 0.35)
  aw_base <- ifelse(wm$age < 20, 230, ifelse(wm$age < 25, 135, ifelse(wm$age < 30, 108, 101)))
  wm$awfactt <- round(aw_base * stats::runif(nw, 0.95, 1.05))

  list(
    hr = sim_hr(hh, info),
    pr = sim_pr(hh, wm, births, info),
    ir = sim_ir(wm, births, info),
    br = sim_br(wm, births, info),
    kr = sim_kr(wm, births, info)
  )
}

sim_births <- function(wm) {
  rows <- lapply(seq_len(nrow(wm)), function(i) {
    w <- wm[i, ]
    first_cmc <- w$v011 + w$afm * 12L + sample(10:30, 1)
    if (first_cmc >= w$v008) {
      return(NULL)
    }
    gaps <- sample(18:48, 12, replace = TRUE)
    cmcs <- cumsum(c(first_cmc, gaps))
    cmcs <- cmcs[cmcs < w$v008 - 1L]
    n <- min(length(cmcs), stats::rpois(1, 1 + 0.12 * (w$age - w$afm)) + 1L)
    if (n == 0L) {
      return(NULL)
    }
    cmcs <- rev(cmcs[seq_len(n)]) # DHS orders births most recent first
    data.frame(caseid_key = w$hhid, b3 = cmcs, bidx = seq_len(n), v008 = w$v008)
  })
  b <- do.call(rbind, rows)
  nb <- nrow(b)
  b$b4 <- sample(1:2, nb, replace = TRUE)
  b$b0 <- 0L
  dies <- stats::runif(nb) < 0.06
  age_mo <- b$v008 - b$b3
  b$b7 <- ifelse(dies, pmin(age_mo, sample(c(0L, 0L, 0L, 1L, 2L, 5L, 9L, 14L, 30L), nb, TRUE)), NA)
  b$b5 <- ifelse(dies, 0L, 1L)
  b$age_mo <- age_mo
  b
}

sim_design_cols <- function(d, info, prefix = "v") {
  div_lab <- stats::setNames(seq_along(info$divisions), info$divisions)
  strata_lab <- stats::setNames(
    seq_len(2L * length(info$divisions)),
    paste(rep(info$divisions, each = 2), c("urban", "rural"))
  )
  yr <- 1900L + (d$cmc - 1L) %/% 12L
  out <- list()
  out[[paste0(prefix, "000")]] <- num(rep(info$v000, nrow(d)), "country code and phase")
  out[[paste0(prefix, "001")]] <- num(as.numeric(d$v001), "cluster number")
  out[[paste0(prefix, "002")]] <- num(as.numeric(d$hv002), "household number")
  out[[paste0(prefix, "005")]] <- num(as.numeric(d$wt), if (prefix == "v") "women's individual sample weight (6 decimals)" else "household sample weight (6 decimals)")
  out[[paste0(prefix, "007")]] <- num(as.numeric(if (info$two_digit_year) yr - 1900L else yr), "year of interview")
  out[[paste0(prefix, "021")]] <- num(as.numeric(d$v001), "primary sampling unit")
  out[[paste0(prefix, "022")]] <- lbl(as.numeric(d$strat), strata_lab, "sample strata for sampling errors")
  out[[paste0(prefix, "023")]] <- lbl(as.numeric(d$strat), strata_lab, "stratification used in sample design")
  out[[paste0(prefix, "024")]] <- lbl(as.numeric(d$div), div_lab, if (prefix == "v") "region" else "region")
  out[[paste0(prefix, "025")]] <- lbl(as.numeric(d$res), c(urban = 1, rural = 2), "type of place of residence")
  out
}

sim_hr <- function(hh, info) {
  d <- sim_design_cols(hh, info, prefix = "hv")
  tibble::as_tibble(c(
    list(hhid = num(hh$hhid, "case identification")),
    d,
    list(
      hv009 = num(as.numeric(2L + hh$has_woman * 2L + sample(0:3, nrow(hh), TRUE)), "number of household members"),
      hv201 = lbl(as.numeric(hh$water), c("piped into dwelling" = 11, "piped to yard/plot" = 12,
        "tube well or borehole" = 21, "protected well" = 31, "surface water" = 43, "bottled water" = 71),
        "source of drinking water"),
      hv205 = lbl(as.numeric(hh$toilet), c("flush to piped sewer system" = 11, "flush to septic tank" = 12,
        "flush to don't know where" = 15, "ventilated improved pit latrine (vip)" = 21,
        "pit latrine with slab" = 22, "pit latrine without slab/open pit" = 23, "no facility/bush/field" = 31),
        "type of toilet facility"),
      hv206 = lbl(as.numeric(hh$elec), c(no = 0, yes = 1), "has electricity"),
      hv226 = lbl(as.numeric(hh$fuel), c("lpg" = 2, "wood" = 8, "straw/shrubs/grass" = 9,
        "agricultural crop" = 11, "natural gas" = 6), "type of cooking fuel"),
      hv270 = lbl(as.numeric(hh$wq), c(poorest = 1, poorer = 2, middle = 3, richer = 4, richest = 5), "wealth index combined"),
      hv271 = num(round(hh$wscore * 1e5), "wealth index factor score combined (5 decimals)")
    )
  ))
}

sim_pr <- function(hh, wm, births, info) {
  # Members: head (husband, line 1), the woman (line 2), her living children
  # (from line 3), or a single head when the household has no woman.
  heads <- hh
  heads$hvidx <- 1L
  heads$sex <- ifelse(heads$has_woman, 1L, sample(1:2, nrow(heads), TRUE))
  heads$age <- sample(25:70, nrow(heads), TRUE)
  heads$mother_line <- 0L
  heads$bmi <- NA_real_
  heads$preg <- NA_integer_
  women <- wm
  women$preg <- wm$pregnant
  women$hvidx <- 2L
  women$sex <- 2L
  women$mother_line <- 0L
  kids <- births[births$b5 == 1L, ]
  kids$hvidx <- kids$b16
  kh <- hh[match(kids$caseid_key, hh$hhid), ]
  kh$hvidx <- kids$hvidx
  kh$sex <- kids$b4
  kh$age <- kids$age_mo %/% 12L
  kh$age_mo <- kids$age_mo
  kh$mother_line <- 2L
  kh$bmi <- NA_real_
  kh$preg <- NA_integer_
  cols <- names(hh)
  all <- rbind(
    cbind(heads[, cols], heads[, c("hvidx", "sex", "age", "mother_line", "bmi", "preg")], age_mo = NA),
    cbind(women[, cols], women[, c("hvidx", "sex", "age", "mother_line", "bmi", "preg")], age_mo = NA),
    cbind(kh[, cols], kh[, c("hvidx", "sex", "age", "mother_line", "bmi", "preg", "age_mo")])
  )
  all <- all[order(all$v001, all$hv002, all$hvidx), ]
  n <- nrow(all)
  d <- sim_design_cols(all, info, prefix = "hv")
  under5 <- !is.na(all$age_mo) & all$age_mo < 60
  measured <- under5 & stats::runif(n) < 0.92
  haz <- ifelse(measured, round(stats::rnorm(n, -130 + 25 * all$wscore, 120)), NA)
  wm_measured <- all$sex == 2L & all$age >= 15 & all$age <= 49 & !is.na(all$bmi) & stats::runif(n) < 0.95
  haz[measured & stats::runif(n) < 0.01] <- 9996
  tibble::as_tibble(c(
    list(hhid = num(all$hhid, "case identification"),
         hvidx = num(as.numeric(all$hvidx), "line number")),
    d,
    list(
      hv101 = lbl(as.numeric(ifelse(all$hvidx == 1L, 1L, ifelse(all$hvidx == 2L, 2L, 3L))),
        c(head = 1, "wife or husband" = 2, "son/daughter" = 3), "relationship to head"),
      hv102 = lbl(rep(1, n), c(no = 0, yes = 1), "usual resident"),
      hv103 = lbl(as.numeric(stats::runif(n) < 0.97), c(no = 0, yes = 1), "slept last night"),
      hv104 = lbl(as.numeric(all$sex), c(male = 1, female = 2), "sex of household member"),
      hv105 = num(as.numeric(all$age), "age of household members"),
      hv112 = lbl(as.numeric(all$mother_line), c("mother not in household" = 0), "mother's line number"),
      hv027 = lbl(as.numeric(as.integer(all$v001 %% 2 == 0)), c("not selected" = 0, selected = 1),
        "selection for male/husband interview"),
      hv270 = lbl(as.numeric(all$wq), c(poorest = 1, poorer = 2, middle = 3, richer = 4, richest = 5), "wealth index combined"),
      hc1 = num(ifelse(under5, as.numeric(all$age_mo), NA), "child's age in months"),
      hc70 = num(as.numeric(haz), "height/age standard deviation (new who)"),
      hc71 = num(ifelse(measured, round(stats::rnorm(n, -120 + 20 * all$wscore, 100)), NA), "weight/age standard deviation (new who)"),
      hc72 = num(ifelse(measured, round(stats::rnorm(n, -60, 100)), NA), "weight/height standard deviation (new who)"),
      ha3 = num(ifelse(wm_measured, round(stats::rnorm(n, 1510, 55)), NA), "woman's height in centimeters (1 decimal)"),
      ha40 = num(ifelse(wm_measured, all$bmi, NA), "body mass index"),
      ha54 = lbl(ifelse(wm_measured, as.numeric(all$preg), NA), c("no/don't know" = 0, yes = 1), "currently pregnant")
    )
  ))
}

sim_ir <- function(wm, births, info) {
  n <- nrow(wm)
  d <- sim_design_cols(wm, info, prefix = "v")
  # Birth history in wide form (b3_01 ... b3_20 etc.), most recent birth first.
  wide <- function(col, label, labels = NULL) {
    out <- lapply(1:20, function(k) {
      i <- births$bidx == k
      x <- rep(NA_real_, n)
      x[match(births$caseid_key[i], wm$hhid)] <- births[[col]][i]
      if (is.null(labels)) num(x, label) else lbl(x, labels, label)
    })
    stats::setNames(out, sprintf("%s_%02d", col, 1:20))
  }
  b3_wide <- c(
    wide("b3", "date of birth (cmc)"),
    wide("b4", "sex of child", c(male = 1, female = 2)),
    wide("b5", "child is alive", c(no = 0, yes = 1)),
    wide("b7", "age at death (months, imputed)")
  )
  agegrp <- pmin(7L, (wm$age - 15L) %/% 5L + 1L)
  tibble::as_tibble(c(
    list(caseid = num(sprintf("%12s%3d", wm$hhid, wm$line), "case identification")),
    d[1:4],
    list(v003 = num(as.numeric(wm$line), "respondent's line number")),
    d[-(1:4)],
    list(
      v008 = num(as.numeric(wm$v008), "date of interview (cmc)"),
      v011 = num(as.numeric(wm$v011), "date of birth (cmc)"),
      v012 = num(as.numeric(wm$age), "respondent's current age"),
      v013 = lbl(as.numeric(agegrp), c("15-19" = 1, "20-24" = 2, "25-29" = 3, "30-34" = 4,
        "35-39" = 5, "40-44" = 6, "45-49" = 7), "age in 5-year groups"),
      v106 = lbl(as.numeric(wm$edu), c("no education" = 0, primary = 1, secondary = 2, higher = 3),
        "highest educational level"),
      v133 = num(as.numeric(wm$edyears), "education in single years"),
      v130 = lbl(as.numeric(wm$religion), c(islam = 1, hinduism = 2, buddhism = 3, christianity = 4), "religion"),
      v190 = lbl(as.numeric(wm$wq), c(poorest = 1, poorer = 2, middle = 3, richer = 4, richest = 5), "wealth index combined"),
      v191 = num(round(wm$wscore * 1e5), "wealth index factor score combined (5 decimals)"),
      v201 = num(as.numeric(wm$ceb), "total children ever born"),
      v213 = lbl(as.numeric(wm$pregnant), c("no or unsure" = 0, yes = 1), "currently pregnant"),
      v312 = lbl(as.numeric(wm$method), c("not using" = 0, pill = 1, iud = 2, injections = 3,
        condom = 5, "female sterilization" = 6, "periodic abstinence" = 8, withdrawal = 9, implants = 11),
        "current contraceptive method"),
      v313 = lbl(as.numeric(wm$v313), c("no method" = 0, "folkloric method" = 1, "traditional method" = 2,
        "modern method" = 3), "current use by method type"),
      v445 = num(as.numeric(wm$bmi), "body mass index"),
      v501 = lbl(as.numeric(wm$marital), c("never in union" = 0, married = 1, "living with partner" = 2,
        widowed = 3, divorced = 4, "no longer living together/separated" = 5), "current marital status"),
      v502 = lbl(as.numeric(ifelse(wm$marital == 1L, 1L, 2L)), c("never in union" = 0,
        "currently in union/living with a man" = 1, "formerly in union/living with a man" = 2),
        "currently/formerly/never in union"),
      v511 = num(as.numeric(wm$afm), "age at first cohabitation"),
      v626a = lbl(as.numeric(wm$v626a), c("unmet need for spacing" = 1, "unmet need for limiting" = 2,
        "using for spacing" = 3, "using for limiting" = 4, "no unmet need" = 7,
        "not married and no sex in last 30 days" = 8, "infecund, menopausal" = 9),
        "unmet need for contraception (definition 3)"),
      v714 = lbl(as.numeric(wm$work), c(no = 0, yes = 1), "respondent currently working"),
      awfactt = num(as.numeric(wm$awfactt), "all woman factor - total"),
      awfactu = num(as.numeric(wm$awfactt), "all woman factor - urban/rural"),
      awfactr = num(as.numeric(wm$awfactt), "all woman factor - region"),
      awfacte = num(as.numeric(wm$awfactt), "all woman factor - education"),
      awfactw = num(as.numeric(wm$awfactt), "all woman factor - wealth index")
    ),
    b3_wide
  ))
}

sim_births_long <- function(wm, births, info) {
  w <- wm[match(births$caseid_key, wm$hhid), ]
  list(w = w, b = births)
}

sim_br <- function(wm, births, info) {
  x <- sim_births_long(wm, births, info)
  w <- x$w
  b <- x$b
  d <- sim_design_cols(w, info, prefix = "v")
  tibble::as_tibble(c(
    list(caseid = num(sprintf("%12s%3d", w$hhid, w$line), "case identification"),
         bidx = num(as.numeric(b$bidx), "birth column number")),
    d[1:4],
    list(v003 = num(as.numeric(w$line), "respondent's line number")),
    d[-(1:4)],
    list(
      v008 = num(as.numeric(w$v008), "date of interview (cmc)"),
      v011 = num(as.numeric(w$v011), "date of birth (cmc)"),
      v012 = num(as.numeric(w$age), "respondent's current age"),
      v106 = lbl(as.numeric(w$edu), c("no education" = 0, primary = 1, secondary = 2, higher = 3),
        "highest educational level"),
      v190 = lbl(as.numeric(w$wq), c(poorest = 1, poorer = 2, middle = 3, richer = 4, richest = 5), "wealth index combined"),
      b0 = lbl(as.numeric(b$b0), c("single birth" = 0, "1st of multiple" = 1), "child is twin"),
      b3 = num(as.numeric(b$b3), "date of birth (cmc)"),
      b4 = lbl(as.numeric(b$b4), c(male = 1, female = 2), "sex of child"),
      b5 = lbl(as.numeric(b$b5), c(no = 0, yes = 1), "child is alive"),
      b7 = num(as.numeric(b$b7), "age at death (months, imputed)"),
      b8 = num(ifelse(b$b5 == 1L, as.numeric(b$age_mo %/% 12L), NA), "current age of child"),
      b19 = num(as.numeric(b$age_mo), "current age of child in months (months since birth for dead children)"),
      b16 = num(as.numeric(b$b16), "child's line number in household"),
      awfactt = num(as.numeric(w$awfactt), "all woman factor - total")
    )
  ))
}

sim_kr <- function(wm, births, info) {
  x <- sim_births_long(wm, births, info)
  keep <- x$b$age_mo < 60
  w <- x$w[keep, ]
  b <- x$b[keep, ]
  n <- nrow(b)
  d <- sim_design_cols(w, info, prefix = "v")
  facility <- stats::runif(n) < stats::plogis(-0.2 + 0.8 * w$wscore)
  m15 <- ifelse(facility, sample(c(21L, 22L, 31L, 41L), n, TRUE, prob = c(0.3, 0.1, 0.5, 0.1)),
    sample(c(11L, 12L), n, TRUE, prob = c(0.9, 0.1)))
  csec <- as.integer(facility & stats::runif(n) < 0.6)
  doctor <- as.integer(facility & stats::runif(n) < 0.75)
  vacc <- function(p) {
    out <- ifelse(stats::runif(n) < p, sample(c(1L, 3L, 2L), n, TRUE, prob = c(0.5, 0.2, 0.3)), 0L)
    ifelse(b$b5 == 1L, out, NA)
  }
  vlab <- c(no = 0, "vaccination date on card" = 1, "reported by mother" = 2,
            "vaccination marked on card" = 3, "don't know" = 8)
  symp_lab <- c(no = 0, "yes, last 24 hours" = 1, "yes, last two weeks" = 2, "don't know" = 8)
  alive <- b$b5 == 1L
  symptom <- function(p, yes = 2L) ifelse(alive, ifelse(stats::runif(n) < p, yes, 0L), NA)
  tibble::as_tibble(c(
    list(caseid = num(sprintf("%12s%3d", w$hhid, w$line), "case identification"),
         midx = num(as.numeric(b$bidx), "index to birth history"),
         bidx = num(as.numeric(b$bidx), "birth column number")),
    d[1:4],
    list(v003 = num(as.numeric(w$line), "respondent's line number")),
    d[-(1:4)],
    list(
      v008 = num(as.numeric(w$v008), "date of interview (cmc)"),
      v012 = num(as.numeric(w$age), "respondent's current age"),
      v106 = lbl(as.numeric(w$edu), c("no education" = 0, primary = 1, secondary = 2, higher = 3),
        "highest educational level"),
      v190 = lbl(as.numeric(w$wq), c(poorest = 1, poorer = 2, middle = 3, richer = 4, richest = 5), "wealth index combined"),
      b3 = num(as.numeric(b$b3), "date of birth (cmc)"),
      b4 = lbl(as.numeric(b$b4), c(male = 1, female = 2), "sex of child"),
      b5 = lbl(as.numeric(b$b5), c(no = 0, yes = 1), "child is alive"),
      b19 = num(as.numeric(b$age_mo), "current age of child in months (months since birth for dead children)"),
      b16 = num(as.numeric(b$b16), "child's line number in household"),
      m14 = num(as.numeric(ifelse(b$bidx == 1L, pmin(20L, stats::rpois(n, 2 + 1.5 * facility)), NA)),
        "number of antenatal visits during pregnancy"),
      m15 = lbl(as.numeric(m15), c("respondent's home" = 11, "other home" = 12, "government hospital" = 21,
        "upazila health complex" = 22, "private hospital/clinic" = 31, "ngo static clinic" = 41),
        "place of delivery"),
      m17 = lbl(as.numeric(csec), c(no = 0, yes = 1), "delivery by caesarean section")
    ),
    sim_m3(info$survey_id, facility, doctor),
    list(
      h2 = lbl(as.numeric(vacc(0.97)), vlab, "received bcg"),
      h3 = lbl(as.numeric(vacc(0.95)), vlab, "received pentavalent 1"),
      h5 = lbl(as.numeric(vacc(0.93)), vlab, "received pentavalent 2"),
      h7 = lbl(as.numeric(vacc(0.90)), vlab, "received pentavalent 3"),
      h9 = lbl(as.numeric(vacc(0.88)), vlab, "received measles 1"),
      h11 = lbl(as.numeric(symptom(0.05)), symp_lab, "had diarrhea recently"),
      h22 = lbl(as.numeric(symptom(0.30, yes = 1L)), c(no = 0, yes = 1, "don't know" = 8), "had fever in last two weeks"),
      h31 = lbl(as.numeric(symptom(0.25)), symp_lab, "had cough in last two weeks"),
      hw70 = num(as.numeric(ifelse(alive, round(stats::rnorm(n, -130 + 25 * w$wscore, 120)), NA)),
        "ht/a standard deviation (new who)"),
      hw71 = num(as.numeric(ifelse(alive, round(stats::rnorm(n, -120 + 20 * w$wscore, 100)), NA)),
        "wt/a standard deviation (new who)"),
      hw72 = num(as.numeric(ifelse(alive, round(stats::rnorm(n, -60, 100)), NA)),
        "wt/ht standard deviation (new who)"),
      awfactt = num(as.numeric(w$awfactt), "all woman factor - total")
    )
  ))
}

# Delivery assistance (m3a-m3n). The letters of the providers changed between
# rounds (e.g. the family welfare visitor is m3d up to 2007 and m3c later), so
# the variables are laid out with the round's crosswalk rules.
sim_m3 <- function(survey_id, facility, doctor) {
  n <- length(facility)
  roles <- list(
    assist_doctor = list("assistance: qualified doctor", doctor),
    assist_nurse_midwife = list("assistance: nurse/midwife/paramedic", as.integer(facility & !doctor)),
    assist_fwv = list("assistance: family welfare visitor", as.integer(facility & stats::runif(n) < 0.03)),
    assist_csba = list("assistance: community skilled birth attendant", as.integer(!facility & stats::runif(n) < 0.05)),
    assist_sacmo = list("assistance: sacmo", as.integer(facility & stats::runif(n) < 0.02)),
    assist_tba_trained = list("assistance: trained traditional birth attendant", as.integer(!facility & stats::runif(n) < 0.3)),
    assist_tba_untrained = list("assistance: untrained traditional birth attendant", as.integer(!facility & stats::runif(n) < 0.5)),
    assist_relative_friend = list("assistance: relative/friend/neighbor", as.integer(!facility & stats::runif(n) < 0.15)),
    assist_unqualified_doctor = list("assistance: unqualified doctor", as.integer(!facility & stats::runif(n) < 0.05)),
    assist_none = list("assistance: no one", as.integer(!facility & stats::runif(n) < 0.1))
  )
  cw <- crosswalk()$vars
  out <- list()
  for (role in names(roles)) {
    rule <- cw[[role]]$rounds[[survey_id]]
    if (is.null(rule) || !identical(rule$method, "any_of") || !length(rule$vars)) {
      next
    }
    for (j in seq_along(rule$vars)) {
      # A role spread over several letters is recorded in the first one.
      x <- if (j == 1L) roles[[role]][[2]] else rep(0L, n)
      out[[rule$vars[[j]]]] <- lbl(as.numeric(x), c(no = 0, yes = 1), roles[[role]][[1]])
    }
  }
  out[order(names(out))]
}
