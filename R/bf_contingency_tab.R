#' @title Bayesian contingency table analysis
#' @name bf_contingency_tab
#'
#' @inheritParams BayesFactor::contingencyTableBF
#' @param x The variable to use as the **rows** in the contingency table.
#' @param y The variable to use as the **columns** in the contingency
#'   table. Default is `NULL`. If `NULL`, one-sample proportion test (a goodness
#'   of fit test) will be run for the `main` variable. Otherwise an appropriate
#'   association test will be run.
#' @inheritParams bf_corr_test
#' @param sampling.plan Character describing the sampling plan. Possible options
#'   are `"indepMulti"` (independent multinomial; default), `"poisson"`,
#'   `"jointMulti"` (joint multinomial), `"hypergeom"` (hypergeometric). For
#'   more, see `?BayesFactor::contingencyTableBF()`.
#' @param fixed.margin For the independent multinomial sampling plan, which
#'   margin is fixed (`"rows"` or `"cols"`). Defaults to `"rows"`.
#' @param prior.concentration Specifies the prior concentration parameter, set
#'   to `1` by default. It indexes the expected deviation from the null
#'   hypothesis under the alternative, and corresponds to Gunel and Dickey's
#'   (1974) `"a"` parameter.
#' @param ratio A vector of proportions: the expected proportions for the
#'   proportion test (should sum to 1). Default is `NULL`, which means the null
#'   is equal theoretical proportions across the levels of the nominal variable.
#'   This means if there are two levels this will be `ratio = c(0.5,0.5)` or if
#'   there are four levels this will be `ratio = c(0.25,0.25,0.25,0.25)`, etc.
#' @param counts A string naming a variable in data containing counts, or `NULL`
#'   if each row represents a single observation (Default).
#'
#' @importFrom BayesFactor contingencyTableBF logMeanExpLogs
#' @importFrom dplyr pull select rename mutate tibble
#' @importFrom tidyr uncount drop_na
#'
#' @seealso \code{\link{bf_corr_test}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_ttest}}
#'
#' @note Bayes Factor for goodness of fit test is based on gist provided by
#'   Richard Morey:
#'   \url{https://gist.github.com/richarddmorey/a4cd3a2051f373db917550d67131dba4}.
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(tidyBF)
#'
#' # ------------------ association tests --------------------------------
#'
#' # to get caption (in favor of null)
#' bf_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   fixed.margin = "cols"
#' )
#'
#' # to see results
#' bf_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   sampling.plan = "jointMulti",
#'   fixed.margin = "rows",
#'   prior.concentration = 1
#' )
#'
#' # ------------------ goodness of fit tests --------------------------------
#'
#' bf_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   prior.concentration = 10
#' )
#' @export

# function body
bf_contingency_tab <- function(data,
                               x,
                               y = NULL,
                               counts = NULL,
                               ratio = NULL,
                               sampling.plan = "indepMulti",
                               fixed.margin = "rows",
                               prior.concentration = 1,
                               caption = NULL,
                               output = "results",
                               k = 2L,
                               ...) {

  # ensure the variables work quoted or unquoted
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)
  counts <- if (!rlang::quo_is_null(rlang::enquo(counts))) rlang::ensym(counts)

  # =============================== dataframe ================================

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, {{ counts }}) %>%
    tidyr::drop_na(.) %>%
    as_tibble(.)

  # untable the dataframe based on the count for each observation
  if (!rlang::quo_is_null(rlang::enquo(counts))) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = {{ counts }},
        .remove = TRUE,
        .id = "id"
      )
  }

  # x
  data %<>% dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }})))

  # ========================= contingency tabs =============================

  if (!rlang::quo_is_null(rlang::enquo(y))) {
    # dropping unused levels
    data %<>% dplyr::mutate(.data = ., {{ y }} := droplevels(as.factor({{ y }})))

    # detailed text of sample plan
    sampling_plan_text <-
      switch(
        EXPR = sampling.plan,
        "jointMulti" = "joint multinomial",
        "poisson" = "poisson",
        "indepMulti" = "independent multinomial",
        "hypergeom" = "hypergeometric"
      )

    # extracting results from Bayesian test and creating a dataframe
    bf.df <-
      bf_extractor(
        BayesFactor::contingencyTableBF(
          x = table(data %>% dplyr::pull({{ x }}), data %>% dplyr::pull({{ y }})),
          sampleType = sampling.plan,
          fixedMargin = fixed.margin,
          priorConcentration = prior.concentration
        )
      ) %>%
      dplyr::mutate(
        .data = .,
        sampling.plan = sampling_plan_text,
        fixed.margin = fixed.margin,
        prior.concentration = prior.concentration
      )
  }

  # ========================= goodness of fit =============================

  if (rlang::quo_is_null(rlang::enquo(y))) {
    # ratio
    if (is.null(ratio)) {
      x_length <- length(table(data %>% dplyr::pull({{ x }})))
      ratio <- rep(1 / x_length, x_length)
    }

    # no. of levels in `x` variable
    n_levels <- length(as.vector(table(data %>% dplyr::pull({{ x }}))))

    # probability can't be exactly 0 or 1
    if (1 / n_levels == 0 || 1 / n_levels == 1) {
      return(NULL)
    }

    # one sample goodness of fit test for equal proportions
    x_vec <- as.matrix(table(data %>% dplyr::pull({{ x }})))

    # (log) prob of data under null
    pr_y_h0 <- stats::dmultinom(x = x_vec, prob = ratio, log = TRUE)

    # estimate log prob of data under null with Monte Carlo
    M <- 100000

    # `rdirichlet` function from `MCMCpack`
    rdirichlet_int <- function(n, alpha) {
      l <- length(alpha)
      x <- matrix(stats::rgamma(l * n, alpha), ncol = l, byrow = TRUE)
      sm <- x %*% rep(1, l)
      return(x / as.vector(sm))
    }

    # use it
    p1s <- rdirichlet_int(n = M, alpha = prior.concentration * ratio)

    # prob
    tmp_pr_h1 <-
      sapply(
        X = 1:M,
        FUN = function(i) stats::dmultinom(x = x_vec, prob = p1s[i, ], log = TRUE)
      )

    # estimate log prob of data under alternative
    pr_y_h1 <- BayesFactor::logMeanExpLogs(tmp_pr_h1)

    # computing Bayes Factor and formatting the results
    bf.df <-
      tibble(bf10 = exp(pr_y_h1 - pr_y_h0)) %>%
      bf_formatter(.) %>%
      dplyr::mutate(.data = ., prior.concentration = prior.concentration)
  }

  # ========================= caption preparation =============================

  # changing aspects of the caption based on what output is needed
  if (output %in% c("null", "caption", "H0", "h0")) {
    hypothesis.text <- "In favor of null: "
    bf.value <- bf.df$log_e_bf01[[1]]
    bf.subscript <- "01"
  } else {
    hypothesis.text <- "In favor of alternative: "
    bf.value <- -bf.df$log_e_bf01[[1]]
    bf.subscript <- "10"
  }

  # final expression
  bf_message <-
    substitute(
      atop(
        displaystyle(top.text),
        expr = paste(
          hypothesis.text,
          "log"["e"],
          "(BF"[bf.subscript],
          ") = ",
          bf,
          ", ",
          italic("a"),
          " = ",
          a
        )
      ),
      env = list(
        hypothesis.text = hypothesis.text,
        top.text = caption,
        bf.subscript = bf.subscript,
        bf = specify_decimal_p(x = bf.value, k = k),
        a = specify_decimal_p(x = bf.df$prior.concentration[[1]], k = k)
      )
    )

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf.df,
    bf_message
  ))
}


#' @rdname bf_contingency_tab
#' @aliases bf_contingency_tab
#' @export

bf_onesample_proptest <- bf_contingency_tab
