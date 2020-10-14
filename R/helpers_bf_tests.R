#' @title Extract Bayes Factors from `BayesFactor` model object.
#' @name bf_extractor
#'
#' @param bf.object An object from `BayesFactor` package.
#' @param ... Additional arguments passed to
#'   [parameters::model_parameters.BFBayesFactor()].
#'
#' @importFrom BayesFactor extractBF
#' @importFrom dplyr select mutate
#' @importFrom insight standardize_names
#' @importFrom parameters model_parameters
#'
#' @note *Important*: don't enter `1/bf_obj` to extract results for null
#'   hypothesis; doing so will return wrong results.
#'
#' @examples
#' set.seed(123)
#'
#' # creating a `BayesFactor` object
#' bf_obj <-
#'   BayesFactor::anovaBF(
#'     formula = Sepal.Length ~ Species,
#'     data = iris,
#'     progress = FALSE
#'   )
#'
#' # extracting Bayes Factors in a dataframe
#' bf_extractor(bf_obj)
#' @export

# function body
bf_extractor <- function(bf.object, ...) {
  # extract needed info
  df <- tryCatch(
    suppressWarnings(suppressMessages(parameters::model_parameters(bf.object, ...) %>%
      insight::standardize_names(data = ., style = "broom"))),
    error = function(e) NULL
  )

  if (rlang::is_null(df)) df <- as_tibble(bf.object)

  # cleanup
  dplyr::rename_all(.tbl = df, .funs = dplyr::recode, "bf" = "bf10", "bayes.factor" = "bf10") %>%
    dplyr::mutate(.data = ., log_e_bf10 = log(bf10)) %>%
    as_tibble(.)
}

#' @title Prepare caption with expression for Bayes Factor results
#' @name bf_expr
#' @description Convenience function to create an expression with Bayes
#'   Factor results.
#'
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2L`).
#' @param centrality The point-estimates (centrality indices) to compute.
#'   Character (vector) or list with one or more of these options: `"median"`,
#'   `"mean"`, `"MAP"` or `"all"`.
#' @param conf.level Value or vector of probability of the CI (between 0 and 1)
#'   to be estimated. Default to `0.95` (95%).
#' @param conf.method The type of index used for Credible Interval. Can be
#'   \code{"hdi"} (default, see [bayestestR::hdi()]), \code{"eti"} (see
#'   [bayestestR::eti()]) or \code{"si"} (see [bayestestR::si()]).
#' @param caption Text to display as caption (will be displayed on top of the
#'   Bayes Factor caption/message).
#' @param output Can either be `"null"` (or `"caption"` or `"H0"` or `"h0"`),
#'   which will return expression with evidence in favor of the null hypothesis,
#'   or `"alternative"` (or `"title"` or `"H1"` or `"h1"`), which will return
#'   expression with evidence in favor of the alternative hypothesis, or
#'   `"results"`, which will return a dataframe with results all the details).
#' @param anova.design Whether the object is from `BayesFactor::anovaBF`
#'   (default: `FALSE`). The expression is different for anova designs because
#'   not all details are available.
#' @inheritParams bf_extractor
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(tidyBF)
#'
#' # creating caption (for null)
#' bf_expr(
#'   BayesFactor::correlationBF(
#'     x = iris$Sepal.Length,
#'     y = iris$Petal.Length
#'   ),
#'   output = "null",
#'   k = 3,
#'   caption = "Note: Iris dataset"
#' )
#' @export

# function body
bf_expr <- function(bf.object,
                    k = 2L,
                    conf.level = 0.95,
                    conf.method = "hdi",
                    centrality = "median",
                    output = "null",
                    caption = NULL,
                    anova.design = FALSE,
                    ...) {
  # extract a dataframe with BF and posterior estimates (if available)
  df <-
    bf_extractor(
      bf.object = bf.object,
      ci = conf.level,
      ci_method = conf.method,
      ...
    )

  # changing aspects of the caption based on what output is needed
  if (output %in% c("null", "caption", "H0", "h0")) {
    # bf-related text
    bf.value <- -log(df$bf10[[1]])
    bf.sub <- "01"
  } else {
    # bf-related text
    bf.value <- log(df$bf10[[1]])
    bf.sub <- "10"
  }

  # for anova designs
  if (isTRUE(anova.design)) {
    # prepare the Bayes Factor message
    bf_message <-
      substitute(
        atop(displaystyle(top.text),
          expr = paste("log"["e"], "(BF"[bf.sub], ") = ", bf)
        ),
        env = list(
          top.text = caption,
          bf.sub = bf.sub,
          bf = specify_decimal_p(x = bf.value, k = k)
        )
      )
  }

  # for non-anova tests
  if (isFALSE(anova.design)) {
    # t-test or correlation
    estimate.type <- ifelse(df$term[[1]] == "Difference", quote(d), quote(rho))

    # prepare the Bayes Factor message
    bf_message <-
      substitute(
        atop(displaystyle(top.text),
          expr = paste(
            "log"["e"],
            "(BF"[bf.sub],
            ") = ",
            bf,
            ", ",
            widehat(italic(estimate.type))[centrality]^"posterior",
            " = ",
            estimate,
            ", CI"[conf.level]^conf.method,
            " [",
            estimate.LB,
            ", ",
            estimate.UB,
            "]",
            ", ",
            italic("r")["Cauchy"]^"JZS",
            " = ",
            bf_prior
          )
        ),
        env = list(
          top.text = caption,
          bf.sub = bf.sub,
          estimate.type = estimate.type,
          centrality = centrality,
          conf.level = paste0(conf.level * 100, "%"),
          conf.method = toupper(conf.method),
          bf = specify_decimal_p(x = bf.value, k = k),
          estimate = specify_decimal_p(x = df$estimate[[1]], k = k),
          estimate.LB = specify_decimal_p(x = df$conf.low[[1]], k = k),
          estimate.UB = specify_decimal_p(x = df$conf.high[[1]], k = k),
          bf_prior = specify_decimal_p(x = df$prior.scale[[1]], k = k)
        )
      )
  }

  # return the final expression
  return(bf_message)
}

#' @name meta_data_check
#' @title Helper function to check column names for meta-analysis.
#'
#' @inheritParams bf_meta
#'
#' @importFrom ipmisc red blue
#'
#' @export

meta_data_check <- function(data) {
  # check if the two columns needed are present
  if (sum(c("estimate", "std.error") %in% names(data)) != 2) {
    # inform the user that skipping labels for the same reason
    stop(message(cat(
      ipmisc::red("Error"),
      ipmisc::blue(": The dataframe must contain the following two columns:\n"),
      ipmisc::blue("`estimate` and `std.error`."),
      sep = ""
    )),
    call. = FALSE
    )
  }
}
