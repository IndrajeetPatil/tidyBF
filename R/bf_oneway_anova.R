#' @title Bayesian one-way analysis of variance
#' @name bf_oneway_anova
#'
#' @importFrom BayesFactor anovaBF
#'
#' @param data A dataframe (or a tibble) from which variables specified are to
#'   be taken. A matrix or tables will **not** be accepted.
#' @param x The grouping variable from the dataframe `data`.
#' @param y The response (a.k.a. outcome or dependent) variable from the
#'   dataframe `data`.
#' @inheritParams bf_ttest
#' @inheritParams bf_corr_test
#' @param ... Additional arguments.
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_ttest}}
#'
#' @examples
#' \donttest{
#' # setup
#' set.seed(123)
#'
#' # between-subjects -------------------------------------------------------
#' bf_oneway_anova(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   bf.prior = 0.8
#' )
#'
#' # within-subjects -------------------------------------------------------
#' bf_oneway_anova(
#'   data = bugs_long,
#'   x = condition,
#'   y = desire,
#'   paired = TRUE
#' )
#' }
#' @export

# function body
bf_oneway_anova <- function(data,
                            x,
                            y,
                            bf.prior = 0.707,
                            caption = NULL,
                            output = "results",
                            hypothesis.text = TRUE,
                            paired = FALSE,
                            k = 2,
                            ...) {

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # ============================ data preparation ==========================

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    as_tibble(.)

  # ========================= within-subjects design ==========================

  if (isTRUE(paired)) {
    # converting to long format and then getting it back in wide so that the
    # rowid variable can be used as the block variable
    data %<>%
      df_cleanup_paired(data = ., x = {{ x }}, y = {{ y }}) %>%
      dplyr::mutate(.data = ., rowid = as.factor(rowid))

    # extracting results from Bayesian test (`y ~ x + id`) and creating a dataframe
    bf.df <-
      bf_extractor(BayesFactor::anovaBF(
        formula = rlang::new_formula(
          {{ rlang::enexpr(y) }}, rlang::expr(!!rlang::enexpr(x) + rowid)
        ),
        data = as.data.frame(data),
        whichRandom = "rowid",
        rscaleFixed = bf.prior,
        progress = FALSE,
        rscaleRandom = 1,
        ...
      )) %>%
      dplyr::mutate(.data = ., bf.prior = bf.prior)
  }

  # ========================= between-subjects design =========================

  if (isFALSE(paired)) {
    # remove NAs listwise for between-subjects design
    data %<>% tidyr::drop_na(.)

    # extracting results from Bayesian test and creating a dataframe
    bf.df <-
      bf_extractor(
        BayesFactor::anovaBF(
          formula = rlang::new_formula({{ y }}, {{ x }}),
          data = as.data.frame(data),
          rscaleFixed = bf.prior,
          progress = FALSE,
          ...
        )
      ) %>%
      dplyr::mutate(.data = ., bf.prior = bf.prior)
  }

  # ============================ return ==================================

  # prepare the Bayes Factor message
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
