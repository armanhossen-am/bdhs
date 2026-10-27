---
title: 'bdhs: An R package for reproducible analysis of the Bangladesh Demographic and Health Survey'
tags:
  - R
  - demography
  - global health
  - survey statistics
  - Bangladesh
  - Demographic and Health Surveys
authors:
  - name: Arman Hossen
    affiliation: 1
affiliations:
  - name: Department of Statistics, Miami University, Oxford, Ohio, United States
    index: 1
date: 27 October 2026
bibliography: paper.bib
---

# Summary

The Bangladesh Demographic and Health Survey (BDHS) is the country's main
source of population and health statistics. Nine nationally representative
rounds have been fielded between 1993-94 and 2022 [@bdhs2022], and hundreds of
studies use them every year. Working with the BDHS is, however, harder than it
looks. Variable codes change between rounds: Bangladesh went from five
divisions to eight, and the codes were renumbered when Mymensingh division was
created. Every women's sample is restricted to ever-married women, so
population-level indicators need adjustment factors. The 2022 round asked a
third of women a short questionnaire, and some rounds keep the wealth index or
the WHO growth-standard z-scores in separate files. Each of these traps leads
to biased estimates when overlooked.

`bdhs` is an R package that makes BDHS analysis correct by default and
reproducible. It bundles everything the DHS Program allows to be
redistributed: all 310,528 official estimates published for Bangladesh
through the DHS Program API [@dhsapi], definitions of 2,607 indicators,
catalogues of the surveys and their 441 data files, cross-round codebooks,
division crosswalks and survey-specific boundaries [@dhssdr]. For the
respondent-level data, which the DHS Program licenses to registered users and
does not allow to be redistributed, `bdhs` provides one-line access to a
user's own downloads, harmonization of 112 variables across all rounds,
correct complex survey designs [@lumley2004], pooling of rounds, and a library
of 97 indicators. The indicators include fertility and childhood mortality
rates with jackknife standard errors, and are validated against the official
estimates.

# Statement of need

General tools for DHS data exist. `rdhs` [@rdhs] queries the DHS API and
downloads datasets, `DHS.rates` [@dhsrates] computes fertility and mortality
rates, and the DHS Program publishes reference code for its indicators
[@dhsindicators]. None of them solves the problems that make cross-round BDHS
work error-prone: changing codes and labels, ever-married samples,
round-specific questionnaire changes, and checking results against published
figures. Researchers therefore re-implement these steps in every project,
often differently, which makes published BDHS estimates hard to compare and
reproduce.

`bdhs` is designed for health and population researchers, students and
analysts in Bangladesh and elsewhere. It encodes the decisions once, documents
them, and tests them against the official numbers:

* `bdhs_import()` and `bdhs_read()` identify DHS files by name or, for renamed
  files, by content. They store a fast column-oriented copy and read any
  round and recode with the identifiers and design variables needed
  downstream. Microdata never leave the user's machine. The package checks
  that the data store is not inside a public repository or a cloud-sync
  folder.
* `bdhs_harmonize()` applies a crosswalk of 1,008 round-specific rules,
  covering divisions in three comparable layouts, residence, education,
  wealth, delivery care and anthropometry. Each rule is checked against value
  labels, so a mislabelled file is reported instead of being silently
  miscoded.
* `bdhs_design()` and `bdhs_pool()` build survey designs with the DHS
  sampling-error strata. Pooled designs keep each round's strata and clusters
  separate, and offer normalized, de-normalized and equal weights.
* `bdhs_estimate()`, `bdhs_fertility()` and `bdhs_mortality()` compute
  indicators with Taylor-linearized or jackknife standard errors, using the
  all-woman factors where the DHS Program does.
* `bdhs_official()`, `bdhs_plot_trend()` and `bdhs_map()` make the bundled
  official estimates available offline, with trend plots and division maps.
* `bdhs_simulate()` generates synthetic data with the exact structure of each
  round's recode files. All examples, tests and tutorials run on it, so the
  package can be taught and tested without access to the microdata.

# Validation

Every indicator with an official value was recomputed from the original
recode files of all rounds available to the author and compared with the
published value. 353 of 355 comparisons agree within half a percentage point,
or the equivalent for rates, across eight rounds. The two remaining
differences are documented and explained: women's anthropometry in 2022 is
published from the household-member file, not the women's file. The
validation uncovered several pitfalls that affect published work. These
include the 2022 short questionnaire, which understates unmet need for family
planning by a third if ignored. Others are the use of the total all-woman
factor in the official child-marriage tables, births to mothers under 15 in
the general fertility rate, and the exclusion of country-specific liquids from
exclusive breastfeeding. The validation record ships with the package
(`bdhs_validation_record()`), and the scripts that rebuild it are in the
repository.

# Acknowledgements

The BDHS is implemented by the National Institute of Population Research and
Training (NIPORT) with ICF through The DHS Program. `bdhs` is not affiliated
with or endorsed by NIPORT, ICF or The DHS Program.

# References
