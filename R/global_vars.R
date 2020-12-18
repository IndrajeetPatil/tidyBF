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
    "term",
    "conf.level",
    "hpd95_lower",
    "hpd95_upper",
    "prior.parameter",
    "prior.scale",
    ".counts"
  ),
  package = "tidyBF",
  add = FALSE
)
