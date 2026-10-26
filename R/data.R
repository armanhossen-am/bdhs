# Documentation of the bundled datasets. The data are built by the scripts in
# data-raw/ (see the header of each script); licences are in LICENSE.note.

#' Bangladesh surveys of the DHS Program
#'
#' Catalogue of the 13 Bangladesh surveys distributed by the DHS Program: the
#' nine Demographic and Health Surveys (1993-94 to 2022), the three Service
#' Provision Assessments (1999-2000, 2014, 2017) and the 2001 Maternal Health
#' Services and Maternal Mortality Survey (BMMS, survey type `"OTH"`).
#'
#' Every BDHS women's sample is a sample of **ever-married** women; see
#' `women_universe`. [bdhs_rounds()] gives a compact version of this table
#' that does not depend on the bundled data.
#'
#' @format A tibble with 13 rows (one per survey) and 29 columns:
#' \describe{
#'   \item{survey_id}{DHS survey identifier, e.g. `"BD2017DHS"`; the key used
#'     throughout the package (see [bdhs_survey_id()]).}
#'   \item{survey_type}{`"DHS"`, `"SPA"` or `"OTH"` (BMMS 2001).}
#'   \item{survey_year}{Survey year as used in the survey id (end year of the
#'     fieldwork for surveys spanning two years).}
#'   \item{survey_label}{Year label used by the DHS Program, e.g. `"2017-18"`.}
#'   \item{survey_num}{Numeric DHS survey number (`SurveyNum`); also the
#'     survey key of the DHS Spatial Data Repository.}
#'   \item{recode_version}{Two-character version code of the survey's recode
#'     files, e.g. `"7R"` in `BDIR7RFL`.}
#'   \item{dhs_phase}{DHS phase (first digit of `recode_version`).}
#'   \item{has_indicator_data}{Whether the DHS API publishes indicator values
#'     for the survey (only the nine DHS rounds; see [bdhs_indicators]).}
#'   \item{status}{Survey status in the DHS API.}
#'   \item{fieldwork_start, fieldwork_end}{First and last month of fieldwork
#'     (dates set to the first day of the month).}
#'   \item{publication_date, release_date}{Dates of the final report and of the
#'     data release.}
#'   \item{implementing_org}{Implementing organization(s).}
#'   \item{n_clusters}{Number of sample points (clusters, PSUs); for SPAs, the
#'     number of facilities in the sample.}
#'   \item{n_households}{Number of households interviewed.}
#'   \item{women_universe}{Population of the women's questionnaire (always
#'     `"Ever Married Women"` for the DHS rounds).}
#'   \item{n_women}{Number of women interviewed.}
#'   \item{women_age_min, women_age_max}{Age range of eligible women (10-49 in
#'     1993-94 to 2004 and 12-49 in 2011; DHS indicators refer to women
#'     15-49).}
#'   \item{men_universe}{Population of the men's questionnaire, if any.}
#'   \item{n_men}{Number of men interviewed.}
#'   \item{men_age_min, men_age_max}{Age range of eligible men.}
#'   \item{n_facilities}{Number of facilities: SPA facilities, or for the
#'     1993-94 and 1996-97 DHS the facilities of the service availability
#'     survey.}
#'   \item{footnotes}{DHS API footnotes about the survey.}
#'   \item{characteristic_ids}{List column of DHS survey-characteristic ids
#'     (topics covered, e.g. anthropometry, biomarkers).}
#'   \item{report_url}{URL of the survey's final report (PDF).}
#'   \item{indicators_updated}{Date of the most recent revision of the
#'     survey's indicator values in the DHS API, if any was announced.}
#' }
#' @source The DHS Program Indicator Data API, <https://api.dhsprogram.com>
#'   (`surveys`, `datasets`, `publications`, `surveycharacteristics` and
#'   `dataupdates` endpoints), API version 8.1, retrieved 2026-10-26. Built by
#'   `data-raw/01-fetch-dhs-api.R` and `data-raw/02-build-api-datasets.R`.
#' @seealso [bdhs_rounds()], [bdhs_survey_id()], [bdhs_datasets],
#'   [bdhs_indicators].
#' @examples
#' bdhs_surveys[, c("survey_id", "survey_label", "n_clusters", "n_women")]
#'
#' # Rounds with official indicator values
#' bdhs_surveys$survey_id[bdhs_surveys$has_indicator_data]
"bdhs_surveys"

#' Data files of the Bangladesh surveys
#'
#' Catalogue of every data file the DHS Program distributes for Bangladesh:
#' each recode (IR, KR, BR, HR, PR, ...) of each survey in each format, plus
#' the SPA, BMMS and GPS files. bdhs uses it to recognise files you have
#' downloaded and to know which recodes exist for a round.
#'
#' The files themselves are **not** included: the DHS Program licenses
#' microdata to registered users only.
#'
#' @format A tibble with 441 rows (one per file) and 14 columns:
#' \describe{
#'   \item{survey_id}{DHS survey identifier (see [bdhs_surveys]).}
#'   \item{survey_type}{`"DHS"`, `"SPA"` or `"OTH"`.}
#'   \item{survey_year}{Survey year.}
#'   \item{file_name}{Name of the zip file as distributed, e.g.
#'     `"BDIR7RFL.ZIP"`.}
#'   \item{file_stem}{Upper-case file name without extension, e.g.
#'     `"BDIR7RFL"`.}
#'   \item{file_type_code}{Two-letter recode/file type code (characters 3-4
#'     of the name): IR women, KR children under 5, BR births, HR households,
#'     PR household members, MR men, CR couples, GR/NR pregnancies (2022), WI
#'     wealth index and HW height/weight (early rounds), FW fieldworkers, SQ
#'     service availability, VA verbal autopsy, HH/IQ BMMS household and
#'     individual, FC/PV/SL/CO SPA facility, provider, staff listing and
#'     community, GE/GC GPS coordinates and geospatial covariates.}
#'   \item{file_type}{File type as described by the DHS Program.}
#'   \item{recode_version}{Two-character version code (characters 5-6).}
#'   \item{format_code}{File format code (characters 7-8): `"DT"` Stata,
#'     `"FL"` flat ASCII, `"SD"` SAS, `"SV"` SPSS; `NA` for files whose name
#'     has no format code.}
#'   \item{file_format}{File format as described by the DHS Program.}
#'   \item{spa_variant}{For SPA files, `"SP"` or `"SR"` (characters 9-10);
#'     otherwise `NA`.}
#'   \item{dataset_type}{`"Survey Datasets"`, `"SPA Datasets"` or
#'     `"GPS Datasets"`.}
#'   \item{file_size}{Size of the zip file in bytes.}
#'   \item{last_modified}{Date the file was last modified by the DHS Program.}
#' }
#' @source The DHS Program Indicator Data API, `datasets` endpoint,
#'   <https://api.dhsprogram.com>, retrieved 2026-10-26.
#' @seealso [bdhs_surveys], [bdhs_recodes()], [bdhs_import()].
#' @examples
#' # Stata files of the 2022 BDHS
#' bdhs_datasets[bdhs_datasets$survey_id == "BD2022DHS" &
#'   bdhs_datasets$format_code %in% "DT", c("file_name", "file_type")]
"bdhs_datasets"

#' Official DHS indicator estimates for Bangladesh
#'
#' Every indicator value the DHS Program publishes for the nine Bangladesh
#' DHS rounds (1993-94 to 2022), nationally and by region and background
#' characteristics, as released through the DHS Program Indicator Data API.
#' These are the official estimates of the survey final reports and STATcompiler;
#' use them directly or to validate your own estimates.
#'
#' One row is one value: survey x indicator x characteristic (e.g. "Urban",
#' "Rajshahi", "Highest wealth quintile") x by-variable (e.g. reference
#' period). Where an indicator is published several ways, `is_preferred`
#' flags the headline value; `default_view` keeps exactly what a user should
#' see by default.
#'
#' The API returned 321,440 rows on 2026-10-26, of which 10,912 were exact
#' duplicates (every 2022 region row twice, an artefact of the API's
#' geometry table); they have been removed. Non-preferred rows are kept.
#'
#' @format A tibble with 310,528 rows and 25 columns:
#' \describe{
#'   \item{survey_id}{DHS survey identifier, e.g. `"BD2022DHS"`.}
#'   \item{survey_year}{Survey year.}
#'   \item{survey_label}{Year label, e.g. `"2017-18"`.}
#'   \item{indicator_id}{DHS indicator id, e.g. `"FE_FRTR_W_TFR"` (total
#'     fertility rate). A few ids are reused for two indicators; the pair
#'     (`indicator_id`, `indicator_order`) is unique (see
#'     [bdhs_indicator_catalog]).}
#'   \item{indicator_order}{DHS sort order of the indicator; together with
#'     `indicator_id` it identifies the indicator.}
#'   \item{indicator}{Indicator label.}
#'   \item{indicator_type}{Type of value: `"I"` indicator, `"D"` weighted
#'     denominator, `"U"` unweighted denominator, `"T"` distribution total
#'     (100 percent), `"S"` special answers (don't know/missing), `"E"`
#'     sampling error, `"C"` confidence interval, `"N"` number of cases (e.g. women
#'     interviewed).}
#'   \item{breakdown}{`"national"` (the total, `is_total`), `"subnational"`
#'     (by region) or `"background"` (any other characteristic). The DHS API
#'     also returns subnational rows for `breakdown=background`.}
#'   \item{characteristic_id}{DHS characteristic id (1000 = total).}
#'   \item{characteristic_category}{Characteristic category, e.g.
#'     `"Residence"`, `"Region"`, `"Wealth quintile"`, `"Total"`.}
#'   \item{characteristic_label}{Characteristic, e.g. `"Urban"`. Region labels
#'     use the current spellings; labels starting with `".."` are subregions
#'     of the region above (e.g. `"..Rangpur"` within `"Rajshahi/Rangpur"`),
#'     so trend-consistent aggregates and the regions as surveyed are both
#'     present.}
#'   \item{characteristic_order}{DHS sort order of the characteristic.}
#'   \item{by_variable_id}{Id of a further disaggregation (0 = none).}
#'   \item{by_variable_label}{Label of the further disaggregation, e.g. a
#'     reference period such as `"Five years preceding the survey"`, or an
#'     age group; `NA` if none.}
#'   \item{region_id}{DHS API region id for regional rows (e.g.
#'     `"BDDHS2022360004"`); joins to `reg_id` in [bdhs_regions] and in
#'     [bdhs_boundaries()]. `NA` for other rows.}
#'   \item{level_rank}{Administrative level of a regional row (1 = the
#'     regions as surveyed; `NA` for trend aggregates and non-regional rows).}
#'   \item{value}{The estimate, in the units of the indicator (percent, rate
#'     per 1,000, mean, number, ...).}
#'   \item{precision}{Number of decimals with which the DHS Program presents
#'     the value.}
#'   \item{denom_weighted, denom_unweighted}{Weighted and unweighted
#'     denominators (number of cases), where published.}
#'   \item{ci_low, ci_high}{Lower and upper bounds of the 95 percent
#'     confidence interval; published only for a few indicators (e.g. maternal
#'     mortality).}
#'   \item{is_total}{`TRUE` for the national total.}
#'   \item{is_preferred}{`TRUE` for the DHS Program's preferred (headline)
#'     value when an indicator is published several ways (e.g. for several
#'     reference periods).}
#'   \item{default_view}{`TRUE` for the preferred row of a survey x indicator
#'     x characteristic cell, or for every row of a cell that has no preferred
#'     row (e.g. mortality by wealth, published only for the 10-year period).}
#' }
#' @source The DHS Program Indicator Data API, `data` endpoint
#'   (`breakdown=all`), <https://api.dhsprogram.com>, API version 8.1,
#'   retrieved 2026-10-26: "The DHS Program Indicator Data API, The
#'   Demographic and Health Surveys (DHS) Program. ICF. Originally funded by
#'   the United States Agency for International Development (USAID).
#'   Available from api.dhsprogram.com. \[Accessed 2026-10-26\]". Built by
#'   `data-raw/01-fetch-dhs-api.R` and `data-raw/02-build-api-datasets.R`.
#' @seealso [bdhs_indicator_catalog] for definitions, [bdhs_official()] to
#'   query these values, [bdhs_estimate()] and [bdhs_validate()] to reproduce
#'   them from microdata.
#' @examples
#' # Total fertility rate, national, every round
#' tfr <- bdhs_indicators[bdhs_indicators$indicator_id == "FE_FRTR_W_TFR" &
#'   bdhs_indicators$is_total & bdhs_indicators$is_preferred, ]
#' tfr[, c("survey_label", "value")]
#'
#' # Under-5 mortality by division, 2022 (regions as surveyed)
#' u5 <- bdhs_indicators[bdhs_indicators$indicator_id == "CM_ECMR_C_U5M" &
#'   bdhs_indicators$survey_id == "BD2022DHS" &
#'   bdhs_indicators$breakdown == "subnational" &
#'   bdhs_indicators$level_rank %in% 1 & bdhs_indicators$default_view, ]
#' u5[, c("characteristic_label", "by_variable_label", "value")]
"bdhs_indicators"

#' Definitions of the DHS indicators published for Bangladesh
#'
#' One row per indicator in [bdhs_indicators]: label, definition,
#' denominator, topic, the surveys it is available for and more.
#'
#' The DHS API's country-filtered indicator list misses 31 indicators that do
#' have Bangladesh values; their definitions were taken from the global list.
#' A few indicator ids are used for two different indicators (different
#' `indicator_order`), flagged by `id_is_shared`.
#'
#' @format A tibble with 2,607 rows and 29 columns:
#' \describe{
#'   \item{indicator_id}{DHS indicator id.}
#'   \item{indicator_order}{DHS sort order; (`indicator_id`,
#'     `indicator_order`) is the key.}
#'   \item{label}{Indicator label.}
#'   \item{short_name}{Short label.}
#'   \item{definition}{Full definition.}
#'   \item{denominator}{Description of the denominator.}
#'   \item{measurement_type}{`"Percent"`, `"Rate"`, `"Mean"`, `"Median"`,
#'     `"Number"` or `"Ratio"`.}
#'   \item{indicator_type}{Type code, as in [bdhs_indicators].}
#'   \item{topic}{Subject area (DHS level 1), e.g. `"Fertility"`.}
#'   \item{subtopic}{Topic within the subject area (DHS level 2).}
#'   \item{population}{Universe (DHS level 3), e.g. `"Women"`, `"Children"`.}
#'   \item{precision}{Number of decimals with which values are presented.}
#'   \item{tag_ids}{List column of DHS tag ids.}
#'   \item{is_quick_stat}{`TRUE` for DHS "Quickstats" (the most used
#'     indicators).}
#'   \item{quick_stat_order}{Sort order among Quickstats.}
#'   \item{denom_weighted_id, denom_unweighted_id}{Indicator ids of the
#'     weighted and unweighted denominators.}
#'   \item{special1_id, special2_id}{Indicator ids of related special answers
#'     (e.g. don't know/missing).}
#'   \item{old_id}{Former DHS indicator id.}
#'   \item{id_is_shared}{`TRUE` if `indicator_id` is used for more than one
#'     indicator.}
#'   \item{tags}{Names of the tags (e.g. `"SDGs; MDGs"`), separated by `"; "`.}
#'   \item{n_surveys}{Number of Bangladesh surveys with values.}
#'   \item{surveys}{Comma-separated survey ids with values.}
#'   \item{first_year, last_year}{First and last survey year with values.}
#'   \item{has_subnational}{Whether regional values are published.}
#'   \item{has_ci}{Whether confidence intervals are published.}
#'   \item{n_rows}{Number of rows in [bdhs_indicators].}
#' }
#' @source The DHS Program Indicator Data API, `indicators` and `tags`
#'   endpoints, <https://api.dhsprogram.com>, retrieved 2026-10-26.
#' @seealso [bdhs_indicators], [bdhs_search()], [bdhs_official()].
#' @examples
#' # Quickstat indicators available for every round
#' qs <- bdhs_indicator_catalog[bdhs_indicator_catalog$is_quick_stat &
#'   bdhs_indicator_catalog$n_surveys == 9, ]
#' head(qs[, c("indicator_id", "label")])
#'
#' # Search definitions
#' cat_ <- bdhs_indicator_catalog
#' cat_[grepl("stunt", cat_$label, ignore.case = TRUE), c("indicator_id", "label")]
"bdhs_indicator_catalog"

#' Codebooks of the BDHS recode files
#'
#' Variable-level metadata of the BDHS recode files: name, label, type, number
#' of value labels and number of non-missing values, so you can see which
#' variables exist in which round without the data, and compare rounds. Value
#' labels are in [bdhs_value_labels].
#'
#' These are **metadata only**: no data values, ranges, frequencies or
#' observed codes are included. They were extracted from files the maintainer
#' is licensed to use, so **only the following rounds and recodes are
#' covered**:
#' * KR (children under 5) for 1993-94, 1999-2000, 2004, 2007, 2011, 2014,
#'   2017-18 and 2022;
#' * every 2017-18 file: BR, FW, HR, IR, KR, PR, SQ and VA;
#' * IR, KR and PR for 2022.
#'
#' 1996-97 and the SPA and BMMS surveys are not covered. Variable names are
#' lower case, as returned by [bdhs_read()].
#'
#' @format A tibble with 25,910 rows (one per variable per file) and 9 columns:
#' \describe{
#'   \item{survey_id}{DHS survey identifier.}
#'   \item{recode}{Recode/file type, e.g. `"KR"` (see [bdhs_recodes()]).}
#'   \item{file_stem}{DHS file name the metadata describe, e.g. `"BDKR7RFL"`.}
#'   \item{position}{Position of the variable in the file.}
#'   \item{variable}{Variable name (lower case).}
#'   \item{label}{Variable label as in the file (typographic quotes made
#'     ASCII).}
#'   \item{type}{R class of the variable when read with haven:
#'     `"haven_labelled"` (numeric with value labels), `"numeric"` or
#'     `"character"`.}
#'   \item{n_value_labels}{Number of value labels (rows in
#'     [bdhs_value_labels]).}
#'   \item{n_valid}{Number of records with a non-missing value. DHS special
#'     codes such as 9/99 ("missing") or 8/98 ("don't know") count as
#'     non-missing; system-missing values and empty strings do not. Compare
#'     with the number of records of the file to see if a variable was asked
#'     of everyone.}
#' }
#' @source Metadata extracted from DHS recode files (Stata format) by
#'   `data-raw/03-build-codebooks.R`; the files used are listed in
#'   `data-raw/codebook-sources.csv`. Labels are the work of The DHS
#'   Program/ICF and the survey implementing agencies.
#' @seealso [bdhs_value_labels], [bdhs_variable()], [bdhs_search()],
#'   [bdhs_harmonized_vars()].
#' @examples
#' # Which rounds have a KR caesarean-section variable (m17)?
#' bdhs_codebook[bdhs_codebook$variable == "m17" & bdhs_codebook$recode == "KR",
#'   c("survey_id", "label", "n_valid")]
#'
#' # Variables of the 2022 PR about anthropometry
#' pr <- bdhs_codebook[bdhs_codebook$survey_id == "BD2022DHS" &
#'   bdhs_codebook$recode == "PR", ]
#' head(pr[grepl("^hc7", pr$variable), c("variable", "label")])
"bdhs_codebook"

#' Value labels of the BDHS recode files
#'
#' The value labels (code -> label) of every labelled variable in
#' [bdhs_codebook]. Covers the same rounds and recodes as [bdhs_codebook].
#' Codes are not comparable across rounds without harmonization (for example
#' division code 5 is Rajshahi up to 2014 and Mymensingh from 2017-18; see
#' [bdhs_regions] and [bdhs_harmonize()]).
#'
#' @format A tibble with 71,973 rows (one per value label) and 5 columns:
#' \describe{
#'   \item{survey_id}{DHS survey identifier.}
#'   \item{recode}{Recode/file type.}
#'   \item{variable}{Variable name (lower case).}
#'   \item{value}{Numeric code.}
#'   \item{label}{Label of the code, as in the file (typographic quotes made
#'     ASCII, original spelling kept, e.g. `"rajshani"` in 1993-94).}
#' }
#' @source Metadata extracted from DHS recode files by
#'   `data-raw/03-build-codebooks.R` (see [bdhs_codebook]).
#' @seealso [bdhs_codebook], [bdhs_regions].
#' @examples
#' # Division codes across rounds
#' vl <- bdhs_value_labels[bdhs_value_labels$variable == "v024" &
#'   bdhs_value_labels$recode == "KR", ]
#' table(vl$label, vl$survey_id)
"bdhs_value_labels"

#' Regions (divisions) of every Bangladesh survey
#'
#' A crosswalk of the region variable of each survey: one row per survey and
#' region code, with the division it denotes, the stable geographic unit, the
#' DHS API region id, the corresponding boundaries and two trend-consistent
#' groupings.
#'
#' Bangladesh has gained divisions during the survey period: Sylhet (from
#' Chittagong, 1995), Rangpur (from Rajshahi, 2010) and Mymensingh (from Dhaka,
#' 2015). Codes therefore change meaning across rounds (code 5 is Rajshahi up
#' to 2014 and Mymensingh from 2017-18) and older regions contain later
#' divisions (see `includes`). Use `trend6`/`trend5` to compare rounds.
#'
#' The region codes of the 2017-18 DHS follow the microdata (1 Barisal,
#' 2 Chittagong, 3 Dhaka, 4 Khulna, 5 Mymensingh, 6 Rajshahi, 7 Rangpur,
#' 8 Sylhet); the DHS Spatial Data Repository codes 5-8 differently for this
#' survey. The 2017 SPA uses the Repository's coding, which has not been
#' checked against SPA microdata.
#'
#' @format A tibble with 86 rows and 14 columns:
#' \describe{
#'   \item{survey_id}{DHS survey identifier (all 13 surveys).}
#'   \item{region_var}{Region variable of the survey's files: `"v024"` for the
#'     DHS rounds (`hv024` in household and household-member files, `mv024`
#'     in men's files, with the same codes); `"codiv"` (1999-2000 SPA),
#'     `"qhregion"` (2001 BMMS) and `"region"` (2014 and 2017 SPA) as given
#'     by the DHS Spatial Data Repository.}
#'   \item{region_code}{Code of the region in `region_var`.}
#'   \item{region_label}{Value label of the code as written in the survey file
#'     (e.g. `"barisal"`, `"rajshani"`) where the file was checked
#'     (`label_source == "microdata"`), otherwise the region name given by the
#'     DHS Spatial Data Repository.}
#'   \item{division}{Division, canonical lower-case 2022 spelling: one of
#'     `"barishal"`, `"chattogram"`, `"dhaka"`, `"khulna"`, `"mymensingh"`,
#'     `"rajshahi"`, `"rangpur"`, `"sylhet"`.}
#'   \item{division_label}{`division` in title case, e.g. `"Chattogram"`.}
#'   \item{includes}{Later division contained in this region, if any:
#'     `"Sylhet"` (Chattogram in 1993-94), `"Rangpur"` (Rajshahi up to 2007),
#'     `"Mymensingh"` (Dhaka up to 2014); otherwise `NA`.}
#'   \item{unit_id}{Stable 4-digit geographic unit: `"0001"` Barishal,
#'     `"0002"` Chattogram incl. Sylhet, `"0003"` Chattogram, `"0004"` Sylhet,
#'     `"0005"` Dhaka incl. Mymensingh, `"0006"` Khulna, `"0007"` Rajshahi
#'     incl. Rangpur, `"0008"` Rangpur, `"0009"` Rajshahi, `"0010"` Dhaka,
#'     `"0011"` Mymensingh. Regions with the same `unit_id` cover the same
#'     area in every survey.}
#'   \item{reg_id}{DHS API region id (`region_id` in [bdhs_indicators]); `NA`
#'     for the 1999-2000 and 2014 SPA and the 2001 BMMS.}
#'   \item{trend6}{6-division trend grouping used by DHS/NIPORT trend tables:
#'     Rangpur -> Rajshahi, Mymensingh -> Dhaka (lower case). `NA` for
#'     1993-94 Chittagong, which includes Sylhet and can only be split with
#'     the district variable (`sdistr`; see [bdhs_harmonize()]).}
#'   \item{trend5}{5-division grouping of 1993-94: as `trend6`, plus Sylhet ->
#'     Chattogram. Comparable in every round.}
#'   \item{code_verified}{`TRUE` if the code and its label were checked
#'     against the value labels of the survey's microdata (1993-94, 1999-2000,
#'     2004, 2007, 2011, 2014, 2017-18 and 2022 DHS); `FALSE` for codes taken
#'     from the DHS Spatial Data Repository only (1996-97 DHS, SPAs, BMMS).}
#'   \item{label_source}{`"microdata"` or `"sdr"`: where `region_label` comes
#'     from.}
#'   \item{note}{Caveats for the row, if any.}
#' }
#' @source Value labels of the survey files ([bdhs_value_labels]); DHS
#'   Spatial Data Repository region attributes
#'   (<https://spatialdata.dhsprogram.com>, accessed 2026-10-26); bdhs
#'   harmonization rules. Built by `data-raw/05-build-regions.R`.
#' @seealso [bdhs_boundaries()] for the polygons (same `survey_id`,
#'   `region_code`, `unit_id` and `reg_id`), [bdhs_harmonize()] for
#'   harmonized division variables, [bdhs_indicators] for official regional
#'   estimates.
#' @examples
#' # What does code 5 mean in each round?
#' bdhs_regions[bdhs_regions$region_code == 5,
#'   c("survey_id", "region_label", "division", "includes")]
#'
#' # Recode a 2017-18 v024 vector to the 6-division trend grouping
#' r17 <- bdhs_regions[bdhs_regions$survey_id == "BD2017DHS", ]
#' v024 <- c(1, 5, 7, 8)
#' r17$trend6[match(v024, r17$region_code)]
"bdhs_regions"

#' Population represented by each BDHS round, for pooling
#'
#' The national female population aged 15-49 at the reference date of each
#' DHS round and the number of women interviewed. DHS weights are normalised
#' so that they sum to the sample size of each round; when rounds are pooled,
#' [bdhs_pool()] multiplies the weights of each round by
#' `female_15_49 / n_women_sample` so that each round counts in proportion to
#' the population it represents rather than to its sample size.
#'
#' BDHS women's samples are ever-married women; `p_ever_married` gives the
#' share of women 15-49 who were ever married, should you prefer to scale to
#' the ever-married population.
#'
#' @format A tibble with 9 rows (one per DHS round) and 9 columns:
#' \describe{
#'   \item{survey_id}{DHS survey identifier.}
#'   \item{survey_label}{Year label, e.g. `"2017-18"`.}
#'   \item{ref_year}{Reference date as a decimal year: the mean interview date
#'     where the microdata were available to the maintainer, otherwise the
#'     midpoint of fieldwork (1996-97).}
#'   \item{female_15_49}{Female population aged 15-49 (persons): UN World
#'     Population Prospects 2024 estimate, linearly interpolated between the
#'     1 July estimates bracketing `ref_year`.}
#'   \item{n_women_sample}{Number of (ever-married) women interviewed, from the
#'     DHS API.}
#'   \item{women_age_min, women_age_max}{Age range of eligible women.}
#'   \item{p_ever_married}{Proportion of women 15-49 ever married: 1 minus the
#'     DHS percentage never married (indicator `MA_MSTA_W_NMA`) / 100.}
#'   \item{source}{Sources of `female_15_49` and `n_women_sample`.}
#' }
#' @source United Nations, Department of Economic and Social Affairs,
#'   Population Division (2024). World Population Prospects 2024,
#'   <https://population.un.org/wpp/> (CC BY 3.0 IGO); The DHS Program
#'   Indicator Data API, retrieved 2026-10-26. Built by
#'   `data-raw/06-build-pool-population.R`.
#' @seealso [bdhs_pool()], [bdhs_surveys].
#' @examples
#' bdhs_pool_population[, c("survey_id", "ref_year", "female_15_49", "n_women_sample")]
#'
#' # Pooling weight multipliers
#' with(bdhs_pool_population, setNames(female_15_49 / n_women_sample, survey_id))
"bdhs_pool_population"
