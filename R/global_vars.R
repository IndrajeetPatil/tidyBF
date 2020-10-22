# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    "bf10",
    "rowid",
    "bf.prior",
    "component",
    "estimate",
    "estimate.LB",
    "estimate.UB",
    "term"
  ),
  package = "tidyBF",
  add = FALSE
)
