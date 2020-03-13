
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyBF: Grouped statistical analysis in a tidy way

[![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/tidyBF)](https://CRAN.R-project.org/package=tidyBF)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.0.9000-orange.svg?style=flat-square)](https://github.com/IndrajeetPatil/tidyBF/commits/master)
[![Daily downloads
badge](https://cranlogs.r-pkg.org/badges/last-day/tidyBF?color=blue)](https://CRAN.R-project.org/package=tidyBF)
[![Weekly downloads
badge](https://cranlogs.r-pkg.org/badges/last-week/tidyBF?color=blue)](https://CRAN.R-project.org/package=tidyBF)
[![Monthly downloads
badge](https://cranlogs.r-pkg.org/badges/last-month/tidyBF?color=blue)](https://CRAN.R-project.org/package=tidyBF)
[![Total downloads
badge](https://cranlogs.r-pkg.org/badges/grand-total/tidyBF?color=blue)](https://CRAN.R-project.org/package=tidyBF)
[![Travis Build
Status](https://travis-ci.org/IndrajeetPatil/tidyBF.svg?branch=master)](https://travis-ci.org/IndrajeetPatil/tidyBF)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/IndrajeetPatil/tidyBF?branch=master&svg=true)](https://ci.appveyor.com/project/IndrajeetPatil/tidyBF)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![DOI](https://zenodo.org/badge/126624251.svg)](https://zenodo.org/badge/latestdoi/126624251)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--03--13-yellowgreen.svg)](https://github.com/IndrajeetPatil/tidyBF/commits/master)
[![lifecycle](https://img.shields.io/badge/lifecycle-retired-orange.svg)](https://www.tidyverse.org/lifecycle/#retired)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![Coverage
Status](https://img.shields.io/codecov/c/github/IndrajeetPatil/tidyBF/master.svg)](https://codecov.io/github/IndrajeetPatil/tidyBF?branch=master)
[![Coverage
Status](https://coveralls.io/repos/github/IndrajeetPatil/tidyBF/badge.svg?branch=master)](https://coveralls.io/github/IndrajeetPatil/tidyBF?branch=master)
[![status](https://tinyverse.netlify.com/badge/tidyBF)](https://CRAN.R-project.org/package=tidyBF)

# Overview

`tidyBF` package is a tidy wrapper around `BayesFactor` package.

# Installation

To get the latest, stable `CRAN` release:

``` r
install.packages(pkgs = "tidyBF")
```

You can get the **development** version of the package from `GitHub`. To
see what new changes (and bug fixes) have been made to the package since
the last release on `CRAN`, you can check the detailed log of changes
here: <https://indrajeetpatil.github.io/tidyBF/news/index.html>

If you are in hurry and want to reduce the time of installation, prefer-

``` r
# needed package to download from GitHub repo
install.packages(pkgs = "remotes")

remotes::install_github(
  repo = "IndrajeetPatil/tidyBF", # package path on GitHub
  quick = TRUE
) # skips docs, demos, and vignettes
```

If time is not a constraint-

``` r
remotes::install_github(
  repo = "IndrajeetPatil/tidyBF", # package path on GitHub
  dependencies = TRUE, # installs packages which tidyBF depends on
  upgrade_dependencies = TRUE # updates any out of date dependencies
)
```

## Code of Conduct

Please note that the `tidyBF` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
