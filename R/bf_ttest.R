#' @title Bayes Factor for *t*-test
#' @rdname bf_ttest
#'
#' @details If `y` is `NULL`, a one-sample *t*-test will be carried out,
#'   otherwise a two-sample *t*-test will be carried out.
#'
#' @param x Either the grouping variable from the dataframe `data` if it's a
#'   two-sample *t*-test or a numeric variable if it's a one-sample *t*-test.
#' @inheritParams bf_corr_test
#' @inheritParams bf_oneway_anova
#' @param test.value A number specifying the value of the null hypothesis
#'   (Default: `0`).
#' @inheritParams ipmisc::long_to_wide_converter
#' @inheritDotParams bf_extractor -bf.object
#'
#' @importFrom BayesFactor ttestBF
#' @importFrom rlang quo_is_null new_formula ensym enquo
#' @importFrom stats na.omit
#' @importFrom dplyr pull
#' @importFrom ipmisc long_to_wide_converter
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
#' # to get dataframe (between-subjects)
#' bf_ttest(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE,
#'   bf.prior = 0.880,
#'   output = "dataframe"
#' )
#'
#' # to get expression (within-subjects)
#' bf_ttest(
#'   data = dplyr::filter(bugs_long, condition %in% c("LDLF", "LDHF")),
#'   x = condition,
#'   y = desire,
#'   subject.id = subject,
#'   paired = TRUE,
#'   bf.prior = 0.880,
#'   output = "dataframe"
#' )
#'
#' # ------------------- one-samples test -----------------------------------
#'
#' # to get expression
#' bf_ttest(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5.85,
#'   output = "expression"
#' )
#' @export

# function body
bf_ttest <- function(data,
                     x,
                     y = NULL,
                     subject.id = NULL,
                     paired = FALSE,
                     test.value = 0,
                     bf.prior = 0.707,
                     ...) {

  # make sure both quoted and unquoted arguments are allowed
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)

  # -------------------------- one-sample tests ------------------------------

  if (rlang::quo_is_null(rlang::enquo(y))) {
    bf_object <-
      BayesFactor::ttestBF(
        x = stats::na.omit(data %>% dplyr::pull({{ x }})),
        rscale = bf.prior,
        mu = test.value
      )
  }

  # -------------------------- two-sample tests ------------------------------

  if (!rlang::quo_is_null(rlang::enquo(y))) {
    # have a proper cleanup with NA removal
    data %<>%
      ipmisc::long_to_wide_converter(
        data = .,
        x = {{ x }},
        y = {{ y }},
        subject.id = {{ subject.id }},
        paired = paired,
        spread = paired
      )

    # within-subjects design
    if (isTRUE(paired)) {
      # extracting results from Bayesian test and creating a dataframe
      bf_object <-
        BayesFactor::ttestBF(
          x = data[[2]],
          y = data[[3]],
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

  # final return
  bf_extractor(bf_object, ...)
}
