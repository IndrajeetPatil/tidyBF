# tidyBF 0.4.0.9000

BREAKING CHANGES

  - `bf_contingency_tabs` no longer drops unused factor levels (#12).

  - `bf_meta_random` retires `d` and `tau` arguments, since the idea here is to
    rely on `metaBMA`'s defaults. To change the defaults, use `metaBMA.args`
    argument.

# tidyBF 0.4.0

BREAKING CHANGES

  - The biggest breaking change is that now all the functions always return an
    expression containing log value of Bayes Factor in favor of the null
    hypothesis ($log(BF_{01})$). This simplifies the code and also reduces
    confusion. The evidence in favor of alternative is of course just negative
    of this value.

  - Removes the unnecessary aliases for certain tests: `bf_one_sample_ttest`,
    `bf_two_sample_ttest`, and `bf_onesample_proptest`.

  - The output dataframe now only contains results for the alternative
    hypothesis (`BF10`) and its `log` value to avoid cluttered dataframe since
    all the other columns were essentially mathematical transformations of it
    and not really relevant.

  - The `output` argument's default value has been changed from too generic
    `"results"` to more specific and informative `"dataframe"`.

  - The argument `caption` has been renamed to `top.text` since caption doesn't
    really have much sense outside of the plotting-context of `ggstatsplot` in
    which it was originally conceived.

  - The *t*-test and ANOVA tests get `subject.id` argument relevant for repeated
    measures design.

  - Renames `bf_meta` to `bf_meta_random`. The previous alias is retained for
    now but will be retired in future releases.

MAJOR CHANGES

  - Replaces the heavier `broomExtra` dependency with `parameters`.

  - `bf_oneway_anova` now returns model-averaged R-squared posterior as a
    measure of effect size.

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

