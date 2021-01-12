#' @title Bayes Factor for *t*-test and one-way ANOVA
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
#' @importFrom BayesFactor ttestBF anovaBF
#' @importFrom rlang quo_is_null new_formula ensym enquo exec !!!
#' @importFrom stats na.omit
#' @importFrom dplyr pull
#' @importFrom ipmisc long_to_wide_converter
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_oneway_anova}}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(tidyBF)
#'
#' # ----------------------- one-way ANOVA -----------------------------------
#'
#' # to get dataframe (between-subjects)
#' bf_oneway_anova(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   bf.prior = 0.8,
#'   output = "dataframe"
#' )
#'
#' # to get expression (within-subjects) (needs `BayesFactor 0.9.12-4.3` or above)
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
#'
#' # ------------------- two-samples tests -----------------------------------
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
#' }
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
  # one-sample or two-sample t-test?
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
    # make sure both quoted and unquoted arguments are allowed
    c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

    # figure out which test to run based on the no. of levels of the IV
    test.type <- ifelse(nlevels(as.factor(data %>% dplyr::pull({{ x }}))) < 3L, "t", "anova")

    # should the data be spread?
    if (test.type == "t") spread <- paired else spread <- FALSE

    # have a proper cleanup with NA removal
    data %<>%
      ipmisc::long_to_wide_converter(
        data = .,
        x = {{ x }},
        y = {{ y }},
        subject.id = {{ subject.id }},
        paired = paired,
        spread = spread
      ) %>%
      dplyr::mutate(rowid = as.factor(rowid))

    # relevant arguments for `BayesFactor` t-test
    if (test.type == "t") {
      .f <- BayesFactor::ttestBF
      if (paired) .f.args <- list(x = data[[2]], y = data[[3]], rscale = bf.prior, paired = paired)
      if (!paired) .f.args <- list(formula = rlang::new_formula(y, x), rscale = bf.prior, paired = paired)
    }

    # relevant arguments for `BayesFactor` one-way ANOVA
    if (test.type == "anova") {
      .f <- BayesFactor::anovaBF
      if (paired) {
        .f.args <- list(
          formula = rlang::new_formula(rlang::enexpr(y), rlang::expr(!!rlang::enexpr(x) + rowid)),
          whichRandom = "rowid",
          rscaleFixed = bf.prior,
          rscaleRandom = 1
        )
      }
      if (!paired) .f.args <- list(formula = rlang::new_formula(y, x), rscaleFixed = bf.prior)
    }

    # creating a `BayesFactor` object
    bf_object <-
      rlang::exec(
        .fn = .f,
        data = as.data.frame(data),
        progress = FALSE,
        !!!.f.args
      )
  }

  # final return
  bf_extractor(bf_object, ...)
}


#' @rdname bf_ttest
#' @aliases bf_ttest
#' @export

bf_oneway_anova <- bf_ttest
