#' @title Extract Bayes Factors from `BayesFactor` model object.
#' @name bf_extractor
#'
#' @param bf.object An object from `BayesFactor` package.
#' @param ... Additional arguments passed to
#'   [parameters::model_parameters.BFBayesFactor()].
#'
#' @importFrom BayesFactor extractBF
#' @importFrom dplyr mutate rename_all recode
#' @importFrom insight standardize_names
#' @importFrom parameters model_parameters
#'
#' @note *Important*: don't enter `1/bf.object` to extract results for null
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
    suppressMessages(parameters::model_parameters(bf.object, verbose = FALSE, ...)),
    error = function(e) NULL
  )

  # this is mostly for contingency tabs; currently not supported by `parameters`
  if (is.null(df)) {
    df <- as_tibble(bf.object)
  } else {
    df %<>% insight::standardize_names(data = ., style = "broom")
  }

  # cleanup
  dplyr::rename_all(.tbl = df, .funs = dplyr::recode, "bf" = "bf10", "bayes.factor" = "bf10") %>%
    dplyr::mutate(.data = ., log_e_bf10 = log(bf10)) %>%
    as_tibble(.)
}

#' @title Prepare expression for Bayes Factor results
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
#' @param top.text Text to display as top.text (will be displayed on top of the
#'   Bayes Factor top.text/message).
#' @param anova.design Whether the object is from `BayesFactor::anovaBF`
#'   (default: `FALSE`). The expression is different for anova designs because
#'   not all details are available.
#' @inheritParams bf_extractor
#'
#' @importFrom ipmisc specify_decimal_p
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(tidyBF)
#'
#' # creating expression
#' bf_expr(
#'   bf.object = BayesFactor::correlationBF(
#'     x = iris$Sepal.Length,
#'     y = iris$Petal.Length
#'   ),
#'   k = 3,
#'   top.text = "Note: Iris dataset"
#' )
#' @export

# function body
bf_expr <- function(bf.object,
                    k = 2L,
                    conf.level = 0.95,
                    conf.method = "hdi",
                    centrality = "median",
                    top.text = NULL,
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

  # for anova designs
  if (isTRUE(anova.design)) {
    # prepare the Bayes Factor message
    bf01_expr <-
      substitute(
        atop(displaystyle(top.text), expr = paste("log"["e"], "(BF"["01"], ") = ", bf)),
        env = list(
          top.text = top.text,
          bf = specify_decimal_p(x = -log(df$bf10[[1]]), k = k)
        )
      )
  }

  # for non-anova tests
  if (isFALSE(anova.design)) {
    # t-test or correlation
    estimate.type <- ifelse(df$term[[1]] == "Difference", quote(delta), quote(rho))

    # prepare the Bayes Factor message
    bf01_expr <-
      substitute(
        atop(displaystyle(top.text),
          expr = paste(
            "log"["e"],
            "(BF"["01"],
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
          top.text = top.text,
          estimate.type = estimate.type,
          centrality = centrality,
          conf.level = paste0(conf.level * 100, "%"),
          conf.method = toupper(conf.method),
          bf = specify_decimal_p(x = -log(df$bf10[[1]]), k = k),
          estimate = specify_decimal_p(x = df$estimate[[1]], k = k),
          estimate.LB = specify_decimal_p(x = df$conf.low[[1]], k = k),
          estimate.UB = specify_decimal_p(x = df$conf.high[[1]], k = k),
          bf_prior = specify_decimal_p(x = df$prior.scale[[1]], k = k)
        )
      )
  }

  # return the final expression
  if (is.null(top.text)) bf01_expr$expr else bf01_expr
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
