#' @title Bayes Factor for correlation test
#' @name bf_corr_test
#'
#' @param x The column in `data` containing the explanatory variable to be
#'   plotted on the `x`-axis. Can be entered either as a character string (e.g.,
#'   `"x"`) or as a bare expression (e.g, `x`).
#' @param y The column in `data` containing the response (outcome) variable to
#'   be plotted on the `y`-axis. Can be entered either as a character string
#'   (e.g., `"y"`) or as a bare expression (e.g, `y`).
#' @param bf.prior A number between `0.5` and `2` (default `0.707`), the prior
#'   width to use in calculating Bayes factors.
#' @inheritParams bf_ttest
#' @inheritDotParams bf_extractor -bf.object
#'
#' @importFrom BayesFactor correlationBF
#' @importFrom dplyr pull
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_ttest}}
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(tidyBF)
#'
#' # to get dataframe
#' bf_corr_test(
#'   data = anscombe,
#'   x = x1,
#'   y = y4,
#'   bf.prior = 0.3,
#'   output = "dataframe"
#' )
#'
#' # to get expression
#' bf_corr_test(
#'   data = anscombe,
#'   x = x1,
#'   y = y4,
#'   bf.prior = 0.8,
#'   output = "expression"
#' )
#' @export

# function body
bf_corr_test <- function(data,
                         x,
                         y,
                         bf.prior = 0.707,
                         ...) {

  # extracting results from Bayesian test and creating a dataframe
  bf_object <-
    BayesFactor::correlationBF(
      x = data %>% dplyr::pull({{ x }}),
      y = data %>% dplyr::pull({{ y }}),
      rscale = bf.prior
    )

  # final return
  bf_extractor(bf_object, ...)
}
