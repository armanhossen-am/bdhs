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
    orcid: 0009-0003-4346-1026
    affiliation: 1
affiliations:
  - name: Department of Statistics, Miami University, Oxford, Ohio, United States
    index: 1
date: 29 October 2026
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
of 97 indicators validated against the official estimates. On top of these it
offers tools for inequality, decomposition, data quality, progress towards
the Sustainable Development Goals and synthetic data.

# Statement of need

Most BDHS analyses are written from scratch. Each project re-implements the
same decisions about weights, strata, sample restrictions and recoding, often
differently and rarely checked against the published figures. This makes
estimates hard to compare across studies and hard to reproduce. `bdhs` is
designed for health and population researchers, students and analysts in
Bangladesh and elsewhere. It encodes these decisions once, documents them,
and tests them against the official numbers, so that a correct analysis takes
a few lines of code.

# State of the field

General tools for DHS data exist. `rdhs` [@rdhs] queries the DHS API and
downloads datasets for any country, `DHS.rates` [@dhsrates] computes fertility
and mortality rates, the DHS Program publishes reference code for its
indicators [@dhsindicators], and IPUMS-DHS [@ipumsdhs] offers harmonized
extracts of selected variables. `survey` [@lumley2004] provides the
design-based estimation that all of these rely on. None of them handles the
problems specific to cross-round work with one country's surveys: changing
division codes and labels, ever-married samples and their all-woman factors,
round-specific questionnaire changes, and systematic checking of results
against the published figures. `bdhs` builds on `survey` and complements
`rdhs`: it focuses on one survey programme in depth rather than many in
breadth.

# Software design

The package has two layers. The first works for every user without any
download: `bdhs_official()`, `bdhs_search()`, `bdhs_plot_trend()` and
`bdhs_map()` make the bundled official estimates available offline, with
trend plots and division maps. The second works with the user's own
microdata:

* `bdhs_import()` and `bdhs_read()` identify DHS files by name or, for renamed
  files, by content. They store a fast column-oriented copy and read any
  round and recode with the identifiers and design variables needed
  downstream. Microdata never leave the user's machine, and the package
  checks that its data store is not inside a public repository or a
  cloud-sync folder.
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
  all-woman factors where the DHS Program does [@croft2018].
* `bdhs_compare()` tests changes between rounds. `bdhs_inequality()` computes
  absolute and relative gaps, the slope and relative indices of inequality and
  concentration indices [@wagstaff1991; @erreygers2009].
  `bdhs_decompose()` splits a change into composition and rate components
  [@kitagawa1955]. `bdhs_sdg()` reports annual rates of change and whether
  the 2030 targets are on track.
* `bdhs_quality()` assesses age and date reporting [@pullum2006],
  completeness, and the plausibility of height and weight measurements.

Two functions make the package usable without microdata access.
`bdhs_simulate()` generates data with the exact structure of each round's
recode files, and all examples, tests and tutorials run on it.
`bdhs_synthesize()` goes further and calibrates the synthetic data to the
published estimates by iterative proportional fitting on the logit scale
[@deming1940], using aggregates only, in the spirit of synthetic data for
disclosure limitation [@rubin1993; @raghunathan2003]. Across 27 survey-recode
combinations, the synthetic files reproduce 1,278 official national and
subnational values with a maximum difference of 0.21 percentage points
(`bdhs_synthetic_fidelity()`). Students can therefore learn, test and share a
complete analysis before their data request is approved.

# Validation

Every indicator with an official value was recomputed from the original
recode files of the eight rounds available to the author and compared with
the published value. 353 of 355 comparisons agree within half a percentage
point, or the equivalent for rates, with a median absolute difference of 0.03.
The two remaining differences are documented and explained: women's
anthropometry in 2022 is published from the household-member file, not the
women's file. The validation uncovered several pitfalls that affect published
work. Ignoring the 2022 short questionnaire understates unmet need for family
planning by a third (6.7% instead of 10.0%). Treating the ever-married sample
as all women overstates marriage before age 18 by about ten percentage points
(68.8% instead of 58.9% in 2017-18). Others are the use of the total all-woman
factor in the official child-marriage tables, births to mothers under 15 in
the general fertility rate, and the exclusion of country-specific liquids from
exclusive breastfeeding. Design effects for key indicators in 2022 range from
1.6 to 3.1. The validation record ships with the package
(`bdhs_validation_record()`), and the scripts that rebuild it are in the
repository. The package is tested with `testthat` on macOS, Windows and Linux.

# Research impact statement

`bdhs` lets researchers reproduce official BDHS figures before extending them,
which makes errors visible early and makes studies comparable. The validation
results, the quantified pitfalls, the data-quality assessment and the
synthetic-data method are the subject of a companion research article. The
synthetic data also make BDHS methods teachable in courses where students
cannot yet access the microdata.

# AI usage disclosure

An AI coding assistant (Claude, Anthropic) was used to help write code, tests,
documentation and the text of this paper. The author designed the package,
directed the work, ran the validation against the original data files, and
reviewed and verified all code, results and text.

# Acknowledgements

The BDHS is implemented by the National Institute of Population Research and
Training (NIPORT) with ICF through The DHS Program. `bdhs` is not affiliated
with or endorsed by NIPORT, ICF or The DHS Program. This work received no
specific funding.

# References
