# bdhs: Tools and Data for the Bangladesh Demographic and Health Survey

`bdhs` covers every round of the Bangladesh Demographic and Health
Survey (1993-94, 1996-97, 1999-2000, 2004, 2007, 2011, 2014, 2017-18 and
2022) together with the Bangladesh Service Provision Assessments and the
2001 Maternal Health Services and Maternal Mortality Survey.

The package has two halves:

- **Bundled, ready-to-use data.** Official estimates for every DHS
  indicator published for Bangladesh
  ([bdhs_indicators](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicators.md)),
  the indicator definitions
  ([bdhs_indicator_catalog](https://armanhossen-am.github.io/bdhs/reference/bdhs_indicator_catalog.md)),
  survey and dataset catalogues
  ([bdhs_surveys](https://armanhossen-am.github.io/bdhs/reference/bdhs_surveys.md),
  [bdhs_datasets](https://armanhossen-am.github.io/bdhs/reference/bdhs_datasets.md)),
  cross-round codebooks
  ([bdhs_codebook](https://armanhossen-am.github.io/bdhs/reference/bdhs_codebook.md)),
  division crosswalks
  ([bdhs_regions](https://armanhossen-am.github.io/bdhs/reference/bdhs_regions.md))
  and subnational boundaries
  ([`bdhs_boundaries()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_boundaries.md)).

- **Your own microdata, made easy.** The DHS Program licenses
  respondent-level data to registered users only and does not allow it
  to be redistributed, so it is not shipped with the package. Once you
  have downloaded it,
  [`bdhs_import()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_import.md)
  registers it in a fast local store,
  [`bdhs_read()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_read.md)
  loads any round and recode in one line,
  [`bdhs_harmonize()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_harmonize.md)
  makes variables comparable across rounds,
  [`bdhs_design()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_design.md)
  builds the correct complex survey design and
  [`bdhs_estimate()`](https://armanhossen-am.github.io/bdhs/reference/bdhs_estimate.md)
  computes indicators that are validated against the official DHS
  values.

## Getting access to the microdata

Register at <https://dhsprogram.com/data/new-user-registration.cfm>,
request access to the Bangladesh surveys, download the files (Stata
format is recommended) and run `bdhs_import("path/to/downloads")`.

## See also

Useful links:

- <https://github.com/armanhossen-am/bdhs>

- <https://armanhossen-am.github.io/bdhs/>

- Report bugs at <https://github.com/armanhossen-am/bdhs/issues>

## Author

**Maintainer**: Arman Hossen <hossena@miamioh.edu> \[copyright holder\]

Authors:

- Arman Hossen <hossena@miamioh.edu> \[copyright holder\]
