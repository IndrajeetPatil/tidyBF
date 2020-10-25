#' @title Bayes Factor for one-way analysis of variance (ANOVA)
#' @name bf_oneway_anova
#'
#' @importFrom BayesFactor anovaBF
#' @importFrom dplyr mutate
#' @importFrom rlang new_formula enexpr expr
#' @importFrom ipmisc long_to_wide_converter
#' @importFrom lme4 nobars findbars
#'
#' @param data A dataframe (or a tibble) from which variables specified are to
#'   be taken. A matrix or tables will **not** be accepted.
#' @param paired Decides whether the design is repeated measures or not
#'   (Default: `FALSE`).
#' @param x The grouping variable from the dataframe `data`.
#' @param y The response (a.k.a. outcome or dependent) variable from the
#'   dataframe `data`.
#' @inheritParams bf_ttest
#' @inheritParams bf_corr_test
#' @inheritParams bf_expr
#' @inheritParams ipmisc::long_to_wide_converter
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_ttest}}
#'
#' @examples
#' # setup
#' set.seed(123)
#'
#' # between-subjects -------------------------------------------------------
#'
#' # to get dataframe
#' bf_oneway_anova(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   bf.prior = 0.8,
#'   output = "dataframe"
#' )
#'
#' # within-subjects -------------------------------------------------------
#'
#' # to get expression
#' bf_oneway_anova(
#'   data = bugs_long,
#'   x = condition,
#'   y = desire,
#'   subject.id = subject,
#'   paired = TRUE,
#'   output = "expression"
#' )
#' @export

# function body
bf_oneway_anova <- function(data,
                            x,
                            y,
                            subject.id = NULL,
                            paired = FALSE,
                            bf.prior = 0.707,
                            top.text = NULL,
                            output = "dataframe",
                            k = 2L,
                            ...) {
  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # ============================ data preparation ==========================

  # have a proper cleanup with NA removal
  data %<>%
    ipmisc::long_to_wide_converter(
      data = .,
      x = {{ x }},
      y = {{ y }},
      subject.id = {{ subject.id }},
      paired = paired,
      spread = FALSE
    )

  # ========================= within-subjects design ==========================

  if (isTRUE(paired)) {
    # remove NAs
    data %<>% dplyr::mutate(.data = ., rowid = as.factor(rowid))

    # extracting results from Bayesian test (`y ~ x + id`) and creating a dataframe
    bf_object <-
      BayesFactor::anovaBF(
        formula = rlang::new_formula(
          {{ rlang::enexpr(y) }}, rlang::expr(!!rlang::enexpr(x) + rowid)
        ),
        data = as.data.frame(data),
        whichRandom = "rowid",
        rscaleFixed = bf.prior,
        progress = FALSE,
        rscaleRandom = 1
      )
  }

  # ========================= between-subjects design =========================

  if (isFALSE(paired)) {
    # extracting results from Bayesian test and creating a dataframe
    bf_object <-
      BayesFactor::anovaBF(
        formula = rlang::new_formula({{ y }}, {{ x }}),
        data = as.data.frame(data),
        rscaleFixed = bf.prior,
        progress = FALSE
      )
  }

  # ============================ return ==================================

  # return the text results or the dataframe with results
  switch(
    EXPR = output,
    "dataframe" = bf_extractor(bf_object),
    bf_expr(bf_object, k = k, top.text = top.text, anova.design = TRUE, ...)
  )
}
