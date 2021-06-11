
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `tidyBF`: Tidy Wrapper for `BayesFactor` Package

[![CRAN\_Release\_Badge](https://www.r-pkg.org/badges/version-ago/tidyBF)](https://CRAN.R-project.org/package=tidyBF)
[![CRAN
Checks](https://cranchecks.info/badges/summary/tidyBF)](https://cran.r-project.org/web/checks/check_results_tidyBF.html)
[![R build
status](https://github.com/IndrajeetPatil/tidyBF/workflows/R-CMD-check/badge.svg)](https://github.com/IndrajeetPatil/tidyBF)
[![pkgdown](https://github.com/IndrajeetPatil/tidyBF/workflows/pkgdown/badge.svg)](https://github.com/IndrajeetPatil/tidyBF/actions)

# Retirement notice

------------------------------------------------------------------------

This package is no longer being maintained and might be removed from
`CRAN` in future. All its functionality and development has now moved to
`statsExpressions` package.

Please see: <https://indrajeetpatil.github.io/statsExpressions/>

------------------------------------------------------------------------

# Overview <img src="man/figures/logo.png" align="right" width="240" />

`tidyBF` package is a tidy wrapper around the `BayesFactor` package that
always expects the data to be in the tidy format and return a tibble
containing Bayes Factor values. Additionally, it provides a more
consistent syntax and by default returns a dataframe with rich details.
These functions can also return expressions containing results from
Bayes Factor tests that can then be displayed in custom plots.

# Installation

| Type        | Source | Command                                            |
|-------------|--------|----------------------------------------------------|
| Release     | CRAN   | `install.packages("tidyBF")`                       |
| Development | GitHub | `remotes::install_github("IndrajeetPatil/tidyBF")` |

# Acknowledgments

The hexsticker was generously designed by Sarah Otterstetter (Max Planck
Institute for Human Development, Berlin).
