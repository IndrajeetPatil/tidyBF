# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(".", "bf10", "rowid"),
  package = "tidyBF",
  add = FALSE
)
