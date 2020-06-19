# tidyBF 0.2.1

MAJOR CHANGES

  - Keeping up with the rest of the packages in `ggstatsplot`-verse, the minimum
    R version is bumped to `3.6.0`.

MINOR CHANGES

  - `bf_contingency_tab` function no longer includes sampling plan information
    in the expression.
    
  - Internal refactoring to improve data wrangling using `ipmisc`.

# tidyBF 0.2.0

MAJOR CHANGES

  - `bf_meta` deprecates `messages` argument. If users want to see the full
    output from `metaBMA`, they are advised to run it independently. 

  - All relevant functions gain a `hypothesis.text` argument to remove the
    redundant pretext before the expression containing the results.

MINOR CHANGES

  - Thanks to Sarah, the package has a hexsticker. :)

  - Re-exports `metaBMA::prior()` for `statsExpressions`.
  
  - Removed unnecessary dependence on `broomExtra`.

# tidyBF 0.1.0

  - Initial release of the package.
