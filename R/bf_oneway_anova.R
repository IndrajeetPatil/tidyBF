#' @title Bayes Factor for one-way analysis of variance (ANOVA)
#' @name bf_oneway_anova
#'
#' @note If you want to set `paired = TRUE`, you are going to need to install
#'   the development version of `BayesFactor` (`0.9.12-4.3`).
#'   You can download it by running:
#' `remotes::install_github("richarddmorey/BayesFactor/pkg/BayesFactor")`.
#'
#' @importFrom BayesFactor anovaBF
#' @importFrom dplyr mutate
#' @importFrom rlang new_formula enexpr expr exec !!!
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
#' @inheritParams ipmisc::long_to_wide_converter
#' @inheritDotParams bf_extractor -bf.object
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
#' # to get expression (needs `BayesFactor 0.9.12-4.3`)
#' if (utils::packageVersion("BayesFactor") >= package_version("0.9.12-4.3")) {
#'   bf_oneway_anova(
#'     data = bugs_long,
#'     x = condition,
#'     y = desire,
#'     subject.id = subject,
#'     paired = TRUE,
#'     output = "expression"
#'   )
#' }
#' @export

# function body
bf_oneway_anova <- function(data,
                            x,
                            y,
                            subject.id = NULL,
                            paired = FALSE,
                            bf.prior = 0.707,
                            ...) {
  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # have a proper cleanup with NA removal
  data %<>%
    ipmisc::long_to_wide_converter(
      data = .,
      x = {{ x }},
      y = {{ y }},
      subject.id = {{ subject.id }},
      paired = paired,
      spread = FALSE
    ) %>%
    dplyr::mutate(.data = ., rowid = as.factor(rowid))

  # relevant arguments
  if (isTRUE(paired)) {
    bf.args <- list(
      formula = rlang::new_formula({{ rlang::enexpr(y) }}, rlang::expr(!!rlang::enexpr(x) + rowid)),
      whichRandom = "rowid",
      rscaleRandom = 1
    )
  }
  if (isFALSE(paired)) bf.args <- list(formula = rlang::new_formula({{ y }}, {{ x }}))

  # creating a BayesFactor object
  bf_object <-
    rlang::exec(
      .fn = BayesFactor::anovaBF,
      data = as.data.frame(data),
      rscaleFixed = bf.prior,
      progress = FALSE,
      !!!bf.args
    )

  # final return
  bf_extractor(bf_object, ...)
}
