## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

  - Adapts to changes made to `metaBMA`.

## Reverse dependency check

  - `statsExpressions` tests are failing, but this has nothing to do with
    update to `tidyBF` package. I maintain `statsExpressions` as well and will be
    submitting a newer version of it soon.
