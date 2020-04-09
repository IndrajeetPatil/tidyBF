#' @title Bayes Factor for *t*-test
#' @details If `y` is `NULL`, a one-sample *t*-test will be carried out,
#'   otherwise a two-sample *t*-test will be carried out.
#'
#' @param x Either the grouping variable from the dataframe `data` if it's a
#'   two-sample *t*-test or a numeric variable if it's a one-sample *t*-test.
#' @inheritParams bf_corr_test
#' @inheritParams bf_expr
#' @param test.value A number specifying the value of the null hypothesis
#'   (Default: `0`).
#' @inheritParams BayesFactor::ttestBF
#'
#' @importFrom BayesFactor ttestBF
#' @importFrom rlang !! quo_is_null new_formula ensym enquo
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
                     hypothesis.text = TRUE,
                     k = 2,
                     ...) {

  # make sure both quoted and unquoted arguments are allowed
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)

  # -------------------------- two-sample tests ------------------------------

  if (!rlang::quo_is_null(rlang::enquo(y))) {
    # dropping unused factor levels from `x` variable
    data %<>% dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }})))

    # within-subjects design
    if (isTRUE(paired)) {
      # the data needs to be in wide format
      data_wide <- long_to_wide_converter(data = data, x = {{ x }}, y = {{ y }})

      # change names for convenience
      colnames(data_wide) <- c("rowid", "col1", "col2")

      # extracting results from Bayesian test and creating a dataframe
      bf_object <-
        BayesFactor::ttestBF(
          x = data_wide$col1,
          y = data_wide$col2,
          rscale = bf.prior,
          paired = TRUE,
          progress = FALSE,
          ...
        )
    }

    # between-subjects design
    if (isFALSE(paired)) {
      # removing NAs
      data %<>% dplyr::filter(.data = ., !is.na({{ x }}), !is.na({{ y }}))

      # extracting results from Bayesian test and creating a dataframe
      bf_object <-
        BayesFactor::ttestBF(
          formula = rlang::new_formula({{ y }}, {{ x }}),
          data = as.data.frame(data),
          rscale = bf.prior,
          paired = FALSE,
          progress = FALSE,
          ...
        )
    }
  }

  # -------------------------- one-sample tests ------------------------------

  if (rlang::quo_is_null(rlang::enquo(y))) {
    bf_object <-
      BayesFactor::ttestBF(
        x = data %>% dplyr::pull({{ x }}),
        rscale = bf.prior,
        mu = test.value,
        nullInterval = NULL,
        ...
      )
  }

  # extracting the Bayes factors
  bf.df <-
    bf_extractor(bf_object) %>%
    dplyr::mutate(.data = ., bf.prior = bf.prior)

  # ============================ return ==================================

  # prepare the Bayes factor message
  if (output != "results") {
    bf_message <-
      bf_expr(
        bf.df = bf.df,
        output = output,
        hypothesis.text = hypothesis.text,
        k = k,
        caption = caption
      )
  }

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf.df,
    bf_message
  ))
}

#' @rdname bf_ttest
#' @aliases bf_ttest
#' @export

bf_one_sample_ttest <- bf_ttest

#' @rdname bf_ttest
#' @aliases bf_ttest
#' @export

bf_two_sample_ttest <- bf_ttest

#' @noRd
#'
#' @importFrom rlang :=
#'
#' @keywords internal

df_cleanup_paired <- function(data, x, y) {
  data %<>%
    long_to_wide_converter(data = ., x = {{ x }}, y = {{ y }}) %>%
    tidyr::gather(data = ., key, value, -rowid) %>%
    dplyr::arrange(.data = ., rowid) %>%
    dplyr::rename(.data = ., {{ x }} := key, {{ y }} := value) %>%
    dplyr::mutate(.data = ., {{ x }} := factor({{ x }}))
}
