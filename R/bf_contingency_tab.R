#' @title Bayes Factor for contingency table analysis
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
#'   if each row represents a single observation.
#' @inheritParams bf_extractor
#' @inheritDotParams bf_extractor -bf.object
#'
#' @importFrom BayesFactor contingencyTableBF logMeanExpLogs
#' @importFrom dplyr pull select rename mutate
#' @importFrom tidyr uncount drop_na
#' @importFrom stats dmultinom rgamma
#'
#' @seealso \code{\link{bf_corr_test}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_ttest}}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(tidyBF)
#'
#' # ------------------ association tests --------------------------------
#'
#' # to get dataframe
#' bf_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   output = "dataframe"
#' )
#'
#' # ------------------ goodness of fit tests --------------------------------
#'
#' # to get expression
#' bf_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   prior.concentration = 10,
#'   output = "expression"
#' )
#' }
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
                               top.text = NULL,
                               output = "dataframe",
                               k = 2L,
                               ...) {

  # one-way or two-way table?
  test <- ifelse(!rlang::quo_is_null(rlang::enquo(y)), "two.way", "one.way")

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, .counts = {{ counts }}) %>%
    tidyr::drop_na(.) %>%
    as_tibble(.)

  # untable the dataframe based on the count for each observation
  if (".counts" %in% names(data)) data %<>% tidyr::uncount(data = ., weights = .counts)

  # ---------------------------- contingency tabs ----------------------------

  if (test == "two.way") {
    # Bayes Factor object
    bf_object <-
      BayesFactor::contingencyTableBF(
        table(data %>% dplyr::pull({{ x }}), data %>% dplyr::pull({{ y }})),
        sampleType = sampling.plan,
        fixedMargin = fixed.margin,
        priorConcentration = prior.concentration
      )

    # Bayes Factor expression
    return(bf_extractor(bf_object, k = k, top.text = top.text, output = output, ...))
  }

  # ---------------------------- goodness of fit ----------------------------

  if (test == "one.way") {
    # one-way table
    xtab <- table(data %>% dplyr::pull({{ x }}))

    # ratio
    if (is.null(ratio)) ratio <- rep(1 / length(xtab), length(xtab))

    # probability can't be exactly 0 or 1
    if (1 / length(as.vector(xtab)) == 0 || 1 / length(as.vector(xtab)) == 1) {
      return(NULL)
    }

    # estimate log prob of data under null with Monte Carlo
    # `rdirichlet` function from `MCMCpack`
    rdirichlet_int <- function(n, alpha) {
      l <- length(alpha)
      x <- matrix(stats::rgamma(l * n, alpha), ncol = l, byrow = TRUE)
      sm <- x %*% rep(1, l)
      return(x / as.vector(sm))
    }

    # use it
    p1s <- rdirichlet_int(n = 100000, alpha = prior.concentration * ratio)

    # prob
    tmp_pr_h1 <-
      sapply(
        X = 1:100000,
        FUN = function(i) stats::dmultinom(x = as.matrix(xtab), prob = p1s[i, ], log = TRUE)
      )

    # BF = (log) prob of data under alternative - (log) prob of data under null
    bf <-
      BayesFactor::logMeanExpLogs(tmp_pr_h1) -
      stats::dmultinom(as.matrix(xtab), prob = ratio, log = TRUE)

    # computing Bayes Factor and formatting the results
    df <-
      tibble(bf10 = exp(bf)) %>%
      dplyr::mutate(log_e_bf10 = log(bf10), prior.scale = prior.concentration)

    # final expression
    bf01_expr <-
      substitute(
        atop(
          displaystyle(top.text),
          expr = paste(
            "log"["e"],
            "(BF"["01"],
            ") = ",
            bf,
            ", ",
            italic("a")["Gunel-Dickey"],
            " = ",
            a
          )
        ),
        env = list(
          top.text = top.text,
          bf = format_num(-log(df$bf10[[1]]), k = k),
          a = format_num(df$prior.scale[[1]], k = k)
        )
      )

    # the final expression
    if (is.null(top.text)) bf01_expr <- bf01_expr$expr

    # return the expression or the dataframe
    return(switch(output, "dataframe" = df, bf01_expr))
  }
}
