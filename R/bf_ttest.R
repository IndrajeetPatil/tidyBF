#' @title Bayes Factor for *t*-test
#' @rdname bf_ttest
#'
#' @details If `y` is `NULL`, a one-sample *t*-test will be carried out,
#'   otherwise a two-sample *t*-test will be carried out.
#'
#' @param x Either the grouping variable from the dataframe `data` if it's a
#'   two-sample *t*-test or a numeric variable if it's a one-sample *t*-test.
#' @inheritParams bf_corr_test
#' @inheritParams bf_expr
#' @inheritParams bf_oneway_anova
#' @param test.value A number specifying the value of the null hypothesis
#'   (Default: `0`).
#'
#' @importFrom BayesFactor ttestBF
#' @importFrom rlang quo_is_null new_formula ensym enquo is_null
#' @importFrom stats na.omit
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_oneway_anova}}
#'
#' @examples
#'
#' # ------------------- two-samples tests -----------------------------------
#'
#' # for reproducibility
#' set.seed(123)
#' library(tidyBF)
#'
#' # to get dataframe
#' bf_ttest(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE,
#'   bf.prior = 0.880
#' )
#'
#' # ------------------- one-samples test -----------------------------------
#'
#' # to get dataframe
#' bf_ttest(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5.85
#' )
#' @export

# function body
bf_ttest <- function(data,
                     x,
                     y = NULL,
                     test.value = 0,
                     paired = FALSE,
                     bf.prior = 0.707,
                     caption = NULL,
                     output = "results",
                     k = 2L,
                     ...) {

  # make sure both quoted and unquoted arguments are allowed
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)

  # -------------------------- two-sample tests ------------------------------

  if (!rlang::quo_is_null(rlang::enquo(y))) {
    # have a proper cleanup with NA removal
    data %<>%
      long_to_wide_converter(
        data = .,
        x = {{ x }},
        y = {{ y }},
        paired = paired,
        spread = paired
      )

    # within-subjects design
    if (isTRUE(paired)) {
      # change names for convenience
      colnames(data) <- c("rowid", "col1", "col2")

      # extracting results from Bayesian test and creating a dataframe
      bf_object <-
        BayesFactor::ttestBF(
          x = data$col1,
          y = data$col2,
          rscale = bf.prior,
          paired = TRUE,
          progress = FALSE
        )
    }

    # between-subjects design
    if (isFALSE(paired)) {
      # extracting results from Bayesian test and creating a dataframe
      bf_object <-
        BayesFactor::ttestBF(
          formula = rlang::new_formula({{ y }}, {{ x }}),
          data = as.data.frame(data),
          rscale = bf.prior,
          paired = FALSE,
          progress = FALSE
        )
    }
  }

  # -------------------------- one-sample tests ------------------------------

  if (rlang::quo_is_null(rlang::enquo(y))) {
    bf_object <-
      BayesFactor::ttestBF(
        x = stats::na.omit(data %>% dplyr::pull({{ x }})),
        rscale = bf.prior,
        mu = test.value
      )
  }

  # ============================ return ==================================

  # return the text results or the dataframe with results
  switch(
    EXPR = output,
    "results" = bf_extractor(bf_object),
    bf_expr(bf_object, k = k, output = output, caption = caption, ...)
  )
}
