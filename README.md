
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `tidyBF`: Tidy Wrapper for `BayesFactor` Package

| Package                                                                                                                                               | Status                                                                                                                                                                             | Usage                                                                                                                                   | GitHub                                                                                                                                                 | References                                                                                                                                                 |
|-------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [![CRAN\_Release\_Badge](https://www.r-pkg.org/badges/version-ago/tidyBF)](https://CRAN.R-project.org/package=tidyBF)                                 | [![Travis Build Status](https://travis-ci.org/IndrajeetPatil/tidyBF.svg?branch=master)](https://travis-ci.org/IndrajeetPatil/tidyBF)                                               | [![Daily downloads badge](https://cranlogs.r-pkg.org/badges/last-day/tidyBF?color=blue)](https://CRAN.R-project.org/package=tidyBF)     | [![GitHub version](https://img.shields.io/badge/GitHub-0.4.3-orange.svg?style=flat-square)](https://github.com/IndrajeetPatil/tidyBF/)                 | [![Website](https://img.shields.io/badge/website-tidyBF-orange.svg?colorB=E91E63)](https://indrajeetpatil.github.io/tidyBF/)                               |
| [![CRAN Checks](https://cranchecks.info/badges/summary/tidyBF)](https://cran.r-project.org/web/checks/check_results_tidyBF.html)                      | [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/IndrajeetPatil/tidyBF?branch=master&svg=true)](https://ci.appveyor.com/project/IndrajeetPatil/tidyBF) | [![Weekly downloads badge](https://cranlogs.r-pkg.org/badges/last-week/tidyBF?color=blue)](https://CRAN.R-project.org/package=tidyBF)   | [![Forks](https://img.shields.io/badge/forks-2-blue.svg)](https://github.com/IndrajeetPatil/tidyBF/)                                                   | [![Features](https://img.shields.io/badge/features-tidyBF-orange.svg?colorB=2196F3)](https://indrajeetpatil.github.io/tidyBF/reference/index.html)         |
| [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.6.0-6666ff.svg)](https://cran.r-project.org/)                                            | [![lifecycle](https://img.shields.io/badge/lifecycle-retired-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)                                                        | [![Monthly downloads badge](https://cranlogs.r-pkg.org/badges/last-month/tidyBF?color=blue)](https://CRAN.R-project.org/package=tidyBF) | [![Github Issues](https://img.shields.io/badge/issues-0-red.svg)](https://github.com/IndrajeetPatil/tidyBF/issues)                                     | [![vignettes](https://img.shields.io/badge/vignettes-0.4.3-orange.svg?colorB=FF5722)](https://indrajeetpatil.github.io/statsExpressions/articles/)         |
| [![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/IndrajeetPatil/tidyBF.svg)](https://github.com/IndrajeetPatil/tidyBF) | [![Coverage Status](https://coveralls.io/repos/github/IndrajeetPatil/tidyBF/badge.svg?branch=master)](https://coveralls.io/github/IndrajeetPatil/tidyBF?branch=master)             | [![Total downloads badge](https://cranlogs.r-pkg.org/badges/grand-total/tidyBF?color=blue)](https://CRAN.R-project.org/package=tidyBF)  | [![Github Stars](https://img.shields.io/github/stars/IndrajeetPatil/tidyBF.svg?style=social&label=Github)](https://github.com/IndrajeetPatil/tidyBF)   | [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2074621.svg)](https://doi.org/10.5281/zenodo.2074621)                                                  |
| [![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)                                      | [![Codecov test coverage](https://codecov.io/gh/IndrajeetPatil/tidyBF/branch/master/graph/badge.svg)](https://codecov.io/gh/IndrajeetPatil/tidyBF?branch=master)                   | [![HitCount](https://hits.dwyl.com/IndrajeetPatil/tidyBF.svg)](https://hits.dwyl.com/IndrajeetPatil/tidyBF)                             | [![Last-changedate](https://img.shields.io/badge/last%20change-2021--03--04-yellowgreen.svg)](https://github.com/IndrajeetPatil/tidyBF/commits/master) | [![GitHub last commit](https://img.shields.io/github/last-commit/IndrajeetPatil/tidyBF.svg)](https://github.com/IndrajeetPatil/tidyBF/commits/master)      |
| [![status](https://tinyverse.netlify.com/badge/tidyBF)](https://CRAN.R-project.org/package=tidyBF)                                                    | [![R build status](https://github.com/IndrajeetPatil/tidyBF/workflows/R-CMD-check/badge.svg)](https://github.com/IndrajeetPatil/tidyBF)                                            | [![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/tidyBF/community)                                      | [![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)                                           | [![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/IndrajeetPatil/tidyBF/issues) |

# Retirement notice

This package is no longer being maintained and might be removed from
`CRAN` in future. All its functionality has now moved to
`statsExpressions` package.

Please see: <https://indrajeetpatil.github.io/statsExpressions/>

# Overview <img src="man/figures/logo.png" align="right" width="240" />

`tidyBF` package is a tidy wrapper around the `BayesFactor` package that
always expects the data to be in the tidy format and return a tibble
containing Bayes Factor values. Additionally, it provides a more
consistent syntax and by default returns a dataframe with rich details.
These functions can also return expressions containing results from
Bayes Factor tests that can then be displayed in custom plots.

# Installation

To get the latest, stable `CRAN` release:

``` r
install.packages("tidyBF")
```

You can get the **development** version of the package from `GitHub`. To
see what new changes (and bug fixes) have been made to the package since
the last release on `CRAN`, you can check the detailed log of changes
here: <https://indrajeetpatil.github.io/tidyBF/news/index.html>

If you are in hurry and want to reduce the time of installation, prefer-

``` r
# needed package to download from GitHub repo
install.packages("remotes")

remotes::install_github(
  repo = "IndrajeetPatil/tidyBF", # package path on GitHub
  quick = TRUE # skips docs, demos, and vignettes
)
```

If time is not a constraint-

``` r
remotes::install_github(
  repo = "IndrajeetPatil/tidyBF", # package path on GitHub
  dependencies = TRUE, # installs packages which `tidyBF` depends on
  upgrade_dependencies = TRUE # updates any out of date dependencies
)
```

# Citation

This package is one component of the
[`ggstatsplot`](https://indrajeetpatil.github.io/ggstatsplot/) package.

If you want to cite this package in a scientific journal or in any other
context, run the following code in your `R` console:

``` r
citation("tidyBF")
#> 
#>   Patil, I. (2018). ggstatsplot: 'ggplot2' Based Plots with Statistical
#>   Details. CRAN. Retrieved from
#>   https://cran.r-project.org/web/packages/ggstatsplot/index.html
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {{ggstatsplot}: 'ggplot2' Based Plots with Statistical Details},
#>     author = {Indrajeet Patil},
#>     year = {2018},
#>     journal = {CRAN},
#>     url = {https://CRAN.R-project.org/package=ggstatsplot},
#>   }
```

# Summary of available tests

Behind the curtains, `tidyBF` provides an easier syntax to marry
functionalities provided by the following two packages in a unified
framework:

-   [`BayesFactor`](https://richarddmorey.github.io/BayesFactor/) : for
    hypothesis testing

-   [`bayestestR`](https://easystats.github.io/bayestestR/): for
    posterior estimation

| Analysis                        | Function             | Hypothesis testing             | Estimation                     | Function                                                        |
|---------------------------------|----------------------|--------------------------------|--------------------------------|-----------------------------------------------------------------|
| (one/two-sample) t-test         | `bf_ttest`           | <font color="green">Yes</font> | <font color="green">Yes</font> | `BayesFactor::ttestBF` + `bayestestR::describe_posterior`       |
| one-way ANOVA                   | `bf_oneway_anova`    | <font color="green">Yes</font> | <font color="green">Yes</font> | `BayesFactor::anovaBF` + `performance::r2_bayes`                |
| correlation                     | `bf_corr_test`       | <font color="green">Yes</font> | <font color="green">Yes</font> | `BayesFactor::correlationBF` + `bayestestR::describe_posterior` |
| (one/two-way) contingency table | `bf_contingency_tab` | <font color="green">Yes</font> | <font color="green">Yes</font> | `BayesFactor::contingencyTableBF` + `effectsize::effectsize`    |
| random-effects meta-analysis    | `bf_meta_random`     | <font color="green">Yes</font> | <font color="green">Yes</font> | `metaBMA::meta_random`                                          |

# Notation

The results are always displayed as a Bayes Factor in favor of the
**null** hypothesis over the **alternative** hypothesis. Additionally,
the values are logged to avoid huge numbers. Therefore, the notation is:
![log\_{e}(BF\_{01})](https://chart.apis.google.com/chart?cht=tx&chl=log_%7Be%7D%28BF_%7B01%7D%29 "log_{e}(BF_{01})").

Also, please note that this makes flipping the evidence easy:
![log\_{e}(BF\_{10})](https://chart.apis.google.com/chart?cht=tx&chl=log_%7Be%7D%28BF_%7B10%7D%29 "log_{e}(BF_{10})")
= -
![log\_{e}(BF\_{01})](https://chart.apis.google.com/chart?cht=tx&chl=log_%7Be%7D%28BF_%7B01%7D%29 "log_{e}(BF_{01})")

# Benefits

Below are few concrete examples of where `tidyBF` wrapper might provide
a more friendly way to access output from or write functions around
`BayesFactor`.

## Syntax consistency

`BayesFactor` is inconsistent with its formula interface. `tidyBF`
avoids this as it doesn’t provide the formula interface for any of the
functions.

``` r
# setup
set.seed(123)

# with `BayesFactor` ----------------------------------------
suppressPackageStartupMessages(library(BayesFactor))
data(sleep)

# independent t-test: accepts formula interface
ttestBF(formula = wt ~ am, data = mtcars)
#> Bayes factor analysis
#> --------------
#> [1] Alt., r=0.707 : 1383.367 ±0%
#> 
#> Against denominator:
#>   Null, mu1-mu2 = 0 
#> ---
#> Bayes factor type: BFindepSample, JZS

# paired t-test: doesn't accept formula interface
ttestBF(formula = extra ~ group, data = sleep, paired = TRUE)
#> Error in ttestBF(formula = extra ~ group, data = sleep, paired = TRUE): Cannot use 'paired' with formula.

# with `tidyBF` ----------------------------------------
library(tidyBF)
#> This package is no longer being maintained and might be removed from CRAN in future.
#>     All its functionality has now moved to `statsExpressions` package.
#>     Please see: https://indrajeetpatil.github.io/statsExpressions/

# independent t-test
bf_ttest(data = mtcars, x = am, y = wt)
#> # A tibble: 2 x 13
#>   term       estimate conf.level conf.low conf.high    pd rope.percentage
#>   <chr>         <dbl>      <dbl>    <dbl>     <dbl> <dbl>           <dbl>
#> 1 Difference    -1.26       0.95   -1.79     -0.722     1               0
#> 2 Cohens_d       1.72       0.95    0.831     2.56      1               0
#>   prior.distribution prior.location prior.scale  bf10 method          log_e_bf10
#>   <chr>                       <dbl>       <dbl> <dbl> <chr>                <dbl>
#> 1 cauchy                          0       0.707 1383. Bayesian t-test       7.23
#> 2 cauchy                          0       0.707 1383. Bayesian t-test       7.23

# paired t-test
bf_ttest(data = sleep, x = group, y = extra, paired = TRUE, subject.id = ID)
#> # A tibble: 2 x 13
#>   term       estimate conf.level conf.low conf.high    pd rope.percentage
#>   <chr>         <dbl>      <dbl>    <dbl>     <dbl> <dbl>           <dbl>
#> 1 Difference     1.42       0.95    0.481     2.25  0.998               0
#> 2 Cohens_d      -1.08       0.95   -1.88     -0.219 0.998               0
#>   prior.distribution prior.location prior.scale  bf10 method          log_e_bf10
#>   <chr>                       <dbl>       <dbl> <dbl> <chr>                <dbl>
#> 1 cauchy                          0       0.707  17.3 Bayesian t-test       2.85
#> 2 cauchy                          0       0.707  17.3 Bayesian t-test       2.85
```

## Expressions for plots

Although all functions default to returning a dataframe, you can also
use it to extract expressions that can be displayed in plots.

### t-test

``` r
# setup
set.seed(123)
library(ggplot2)

# using the expression to display details in a plot
ggplot(ToothGrowth, aes(supp, len)) +
  geom_boxplot() + # two-sample t-test results in an expression
  labs(subtitle = bf_ttest(ToothGrowth, supp, len, output = "expression"))
```

<img src="man/figures/README-expr_plot-1.png" width="100%" />

### anova

``` r
# setup
set.seed(123)
library(ggplot2)
library(ggforce)
library(tidyBF)

# plot with subtitle
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_violin() +
  geom_sina() +
  labs(subtitle = bf_oneway_anova(iris, Species, Sepal.Length, output = "expression"))
```

<img src="man/figures/README-expr_plot2-1.png" width="100%" />

### correlation test

``` r
# setup
set.seed(123)
library(ggplot2)
library(tidyBF)

# using the expression to display details in a plot
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(subtitle = bf_corr_test(mtcars, wt, mpg, output = "expression"))
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="man/figures/README-expr_plot3-1.png" width="100%" />

### contingency tabs analysis

``` r
# setup
set.seed(123)
library(ggplot2)
library(tidyBF)

# basic pie chart
ggplot(as.data.frame(table(mpg$class)), aes(x = "", y = Freq, fill = factor(Var1))) +
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank()) +
  # cleaning up the chart and adding results from one-sample proportion test
  coord_polar(theta = "y", start = 0) +
  labs(
    fill = "Class",
    x = NULL,
    y = NULL,
    title = "Pie Chart of class (type of car)",
    subtitle = bf_contingency_tab(as.data.frame(table(mpg$class)), Var1, counts = Freq, output = "h1")
  )
```

<img src="man/figures/README-expr_plot4-1.png" width="100%" />

### meta-analysis

``` r
# setup
set.seed(123)
library(metaviz)
library(ggplot2)

# meta-analysis forest plot with results random-effects meta-analysis
viz_forest(
  x = mozart[, c("d", "se")],
  study_labels = mozart[, "study_name"],
  xlab = "Cohen's d",
  variant = "thick",
  type = "cumulative"
) +
  labs(
    title = "Meta-analysis of Pietschnig, Voracek, and Formann (2010) on the Mozart effect",
    subtitle = bf_meta_random(
      data = dplyr::rename(mozart, estimate = d, std.error = se),
      output = "expression",
      metaBMA.args = list(rscale_discrete = 0.880),
      conf.level = 0.99
    )
  ) +
  theme(text = element_text(size = 12))
```

<img src="man/figures/README-expr_plot5-1.png" width="100%" />

## Convenient way to extract detailed output from `BayesFactor` objects

The package provides `bf_extractor` function to conveniently extract
important details from these objects:

``` r
# setup
set.seed(123)
library(tidyBF)
library(BayesFactor)
data(puzzles)

# model
result <-
  anovaBF(
    RT ~ shape * color + ID,
    data = puzzles,
    whichRandom = "ID",
    whichModels = "top",
    progress = FALSE
  )

# extract details
bf_extractor(result)
#> # A tibble: 21 x 21
#>    term                estimate conf.level conf.low conf.high    pd
#>    <chr>                  <dbl>      <dbl>    <dbl>     <dbl> <dbl>
#>  1 mu                    45.0         0.95  43.7      46.4    1    
#>  2 shape-round            0.429       0.95   0.0643    0.801  0.992
#>  3 shape-square          -0.429       0.95  -0.801    -0.0643 0.992
#>  4 color-color           -0.426       0.95  -0.799    -0.0461 0.990
#>  5 color-monochromatic    0.426       0.95   0.0461    0.799  0.990
#>  6 ID-1                   2.47        0.95   0.783     4.37   0.995
#>  7 ID-2                   0.439       0.95  -1.21      2.20   0.698
#>  8 ID-3                   0.907       0.95  -0.849     2.66   0.848
#>  9 ID-4                   0.466       0.95  -1.47      2.20   0.704
#> 10 ID-5                   3.17        0.95   1.38      5.00   0.999
#>    rope.percentage prior.distribution prior.location prior.scale effect
#>              <dbl> <chr>                       <dbl>       <dbl> <chr> 
#>  1           0     cauchy                          0         0.5 fixed 
#>  2           0.141 cauchy                          0         0.5 fixed 
#>  3           0.141 cauchy                          0         0.5 fixed 
#>  4           0.162 cauchy                          0         0.5 fixed 
#>  5           0.162 cauchy                          0         0.5 fixed 
#>  6           0     cauchy                          0         1   random
#>  7           0.231 cauchy                          0         1   random
#>  8           0.156 cauchy                          0         1   random
#>  9           0.218 cauchy                          0         1   random
#> 10           0     cauchy                          0         1   random
#>    component    bf10 method                          log_e_bf10    r2 std.dev
#>    <chr>       <dbl> <chr>                                <dbl> <dbl>   <dbl>
#>  1 extra       2.65  Bayes factors for linear models      0.974 0.733  0.0518
#>  2 conditional 0.233 Bayes factors for linear models     -1.45  0.733  0.0518
#>  3 conditional 0.239 Bayes factors for linear models     -1.43  0.733  0.0518
#>  4 conditional 2.65  Bayes factors for linear models      0.974 0.733  0.0518
#>  5 conditional 0.233 Bayes factors for linear models     -1.45  0.733  0.0518
#>  6 conditional 0.239 Bayes factors for linear models     -1.43  0.733  0.0518
#>  7 conditional 2.65  Bayes factors for linear models      0.974 0.733  0.0518
#>  8 conditional 0.233 Bayes factors for linear models     -1.45  0.733  0.0518
#>  9 conditional 0.239 Bayes factors for linear models     -1.43  0.733  0.0518
#> 10 conditional 2.65  Bayes factors for linear models      0.974 0.733  0.0518
#>    r2.conf.level r2.conf.low r2.conf.high r2.component
#>            <dbl>       <dbl>        <dbl> <chr>       
#>  1          0.95       0.605        0.810 conditional 
#>  2          0.95       0.605        0.810 conditional 
#>  3          0.95       0.605        0.810 conditional 
#>  4          0.95       0.605        0.810 conditional 
#>  5          0.95       0.605        0.810 conditional 
#>  6          0.95       0.605        0.810 conditional 
#>  7          0.95       0.605        0.810 conditional 
#>  8          0.95       0.605        0.810 conditional 
#>  9          0.95       0.605        0.810 conditional 
#> 10          0.95       0.605        0.810 conditional 
#> # ... with 11 more rows
```

## Using in `for` loops

Here is an example about how to use these functions in the context of
`for` loops:

``` r
# setup
set.seed(123)
library(rlang)

# data
df <- dplyr::select(mtcars, am, wt, mpg)
col.name <- colnames(df)

# in the loop
set.seed(123)
for (i in 2:length(col.name)) {
  print(bf_ttest(
    data = mtcars,
    x = am,
    y = !!col.name[i]
  ))
}
#> # A tibble: 2 x 13
#>   term       estimate conf.level conf.low conf.high    pd rope.percentage
#>   <chr>         <dbl>      <dbl>    <dbl>     <dbl> <dbl>           <dbl>
#> 1 Difference    -1.26       0.95   -1.79     -0.722     1               0
#> 2 Cohens_d       1.72       0.95    0.831     2.56      1               0
#>   prior.distribution prior.location prior.scale  bf10 method          log_e_bf10
#>   <chr>                       <dbl>       <dbl> <dbl> <chr>                <dbl>
#> 1 cauchy                          0       0.707 1383. Bayesian t-test       7.23
#> 2 cauchy                          0       0.707 1383. Bayesian t-test       7.23
#> # A tibble: 2 x 13
#>   term       estimate conf.level conf.low conf.high    pd rope.percentage
#>   <chr>         <dbl>      <dbl>    <dbl>     <dbl> <dbl>           <dbl>
#> 1 Difference     6.45       0.95     2.60     9.91   1.00               0
#> 2 Cohens_d      -1.31       0.95    -2.09    -0.485  1.00               0
#>   prior.distribution prior.location prior.scale  bf10 method          log_e_bf10
#>   <chr>                       <dbl>       <dbl> <dbl> <chr>                <dbl>
#> 1 cauchy                          0       0.707  86.6 Bayesian t-test       4.46
#> 2 cauchy                          0       0.707  86.6 Bayesian t-test       4.46
```

# Acknowledgments

The hexsticker was generously designed by Sarah Otterstetter (Max Planck
Institute for Human Development, Berlin).

# Code of Conduct

Please note that the `tidyBF` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
