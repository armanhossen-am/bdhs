# Contributing to bdhs

Thank you for helping improve bdhs. Bug reports, questions, ideas and pull
requests are all welcome.

## Never share DHS microdata

The DHS Program does not allow respondent-level data to be redistributed.
Please do not attach `.dta`, `.sav`, `.zip` or other data files, or paste rows
of microdata, into issues or pull requests. To show a problem, use synthetic
data from `bdhs_simulate()` or `bdhs_synthesize()`, or describe the file
(survey, recode, file name) and the variables involved.

## Reporting a bug

Open an issue at <https://github.com/armanhossen-am/bdhs/issues> with:

- what you ran, ideally as a small reproducible example (the
  [reprex](https://reprex.tidyverse.org) package helps);
- what you expected and what happened instead, including the full error
  message;
- the output of `sessionInfo()` and `packageVersion("bdhs")`.

If an estimate does not match the official DHS value, give the indicator,
survey round and both numbers.

## Asking for help or suggesting a feature

Open an issue and describe what you are trying to do. The tutorials at
<https://armanhossen-am.github.io/bdhs/> cover most tasks. You can also email
the maintainer, Arman Hossen, at hossena@miamioh.edu.

## Contributing code

1. Fork the repository and create a branch for your change.
2. Follow the style of the existing code and document new functions with
   roxygen2 comments.
3. Add tests with testthat. Tests must run on synthetic data, not on real
   microdata.
4. Run `devtools::document()` and `devtools::check()` and make sure there are
   no errors or warnings.
5. Open a pull request that explains what the change does and why.

`DEVELOPMENT.md` in the repository describes how the package is organised and
the rules it follows.

## Code of conduct

Please be respectful and constructive. Harassment or personal attacks are not
accepted in issues, pull requests or any other project space.
