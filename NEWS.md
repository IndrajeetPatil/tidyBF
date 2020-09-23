# tidyBF 0.3.0.9000

BREAKING CHANGES

  - Removes the unnecessary aliases for certain tests: `bf_one_sample_ttest`,
    `bf_two_sample_ttest`, and `bf_onesample_proptest`.

MAJOR CHANGES

  - Replaces the heavier `broomExtra` dependency with `parameters`.

# tidyBF 0.3.0

BREAKING CHANGES
    
  - `bf_expr` now expects not a dataframe, but rather the `BayesFactor` object.

MAJOR CHANGES

  - `bf_extractor` now internally uses `parameters::model_parameters` to extract
    as many details from the `BayesFactor` as possible. So the returned
    dataframe will contain additional details now.
    
  - The redundant text related to whether the results in favor of the null or
    the alternative hypothesis has now been removed, since this information is
    already present in `BF` subscripts (`01`: null, `10`: alternative).

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
