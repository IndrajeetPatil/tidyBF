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
#' @importFrom rlang quo_is_null new_formula ensym enquo exec !!!
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
  # one-way or two-way table?
  test <- ifelse(!rlang::quo_is_null(rlang::enquo(y)), "two.way", "one.way")

  # -------------------------- one-sample tests ------------------------------

  if (test == "one.way") {
    bf_object <-
      BayesFactor::ttestBF(
        x = stats::na.omit(data %>% dplyr::pull({{ x }})),
        rscale = bf.prior,
        mu = test.value
      )
  }

  # -------------------------- two-sample tests ------------------------------

  if (test == "two.way") {
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

    # relevant arguments
    if (isTRUE(paired)) bf.args <- list(x = data[[2]], y = data[[3]])
    if (isFALSE(paired)) bf.args <- list(formula = rlang::new_formula(rlang::ensym(y), rlang::ensym(x)))

    # creating a BayesFactor object
    bf_object <-
      rlang::exec(
        .fn = BayesFactor::ttestBF,
        rscale = bf.prior,
        paired = paired,
        data = as.data.frame(data),
        !!!bf.args
      )
  }

  # final return
  bf_extractor(bf_object, ...)
}
