# JOSS submission

JOSS does not use a cover letter. You submit through the web form at
https://joss.theoj.org/papers/new (sign in with your ORCID), and the paper
itself (`paper/paper.md` and `paper/paper.bib`) must already be on GitHub.
Review then happens openly in a GitHub issue, where reviewers install and test
the package.

## Form fields

- **Title:** bdhs: An R package for reproducible analysis of the Bangladesh Demographic and Health Survey
- **Repository:** https://github.com/armanhossen-am/bdhs
- **Branch with the paper:** main
- **Software version:** v0.2.0
- **Main subject / domain:** Public health, demography, survey statistics
- **Programming language:** R

## Message to the editors (paste into the description box)

I am submitting bdhs, an open-source R package (MIT licence) for reproducible
analysis of all nine rounds of the Bangladesh Demographic and Health Survey,
1993-94 to 2022.

Analyses of DHS microdata are rarely checked against the official estimates,
and the BDHS has several features that are easy to get wrong: women's samples
restricted to ever-married women, division codes that changed between rounds,
and a shortened women's questionnaire in 2022. bdhs handles these by default.
It bundles the 310,528 official estimates the DHS Program publishes for
Bangladesh, reads and harmonizes users' own microdata, builds the correct
complex survey designs, and computes 97 indicators that reproduce 353 of 355
official national values (median absolute difference 0.03 percentage points).
It also provides tools for inequality, decomposition, data quality and SDG
tracking, and synthetic data calibrated to the published aggregates, so that
every example, test and tutorial runs without restricted data.

The package has unit tests, continuous integration on macOS, Windows and
Linux, a documentation website with tutorials
(https://armanhossen-am.github.io/bdhs/), and passes R CMD check with no
errors or warnings. No DHS microdata are included in the repository.

A companion research article on the validation results, analytic pitfalls,
data quality and the synthetic-data method is being submitted separately to a
research journal. The JOSS paper describes the software only.

The use of an AI coding assistant is disclosed in the paper; I have reviewed
and verified all code, analyses and text.

Thank you for considering this submission.

Arman Hossen
Department of Statistics, Miami University
hossena@miamioh.edu | ORCID 0009-0003-4346-1026

## Before you submit

1. Push `paper/paper.md` and `paper/paper.bib` to GitHub (main branch).
2. Preview the PDF: open the repository's Actions tab, or use
   https://github.com/openjournals/inara (the JOSS paper builder).
3. Check the current JOSS rules at https://joss.readthedocs.io (scope,
   word limit, AI policy and how much development history they expect).
4. Suggest 3 to 5 reviewers from https://reviewers.joss.theoj.org who work
   with R, survey statistics or DHS data.
5. If the BMC manuscript is still under review, mention the JOSS paper to BMC
   as a related software paper.
