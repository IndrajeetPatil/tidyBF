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
