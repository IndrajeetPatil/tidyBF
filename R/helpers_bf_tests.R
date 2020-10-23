#' @title Extract Bayes Factors from `BayesFactor` model object.
#' @name bf_extractor
#'
#' @param bf.object An object from `BayesFactor` package.
#' @param ... Additional arguments passed to
#'   [parameters::model_parameters.BFBayesFactor()].
#'
#' @importFrom dplyr mutate rename
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
  suppressMessages(parameters::model_parameters(bf.object, verbose = FALSE, ...)) %>%
    insight::standardize_names(data = ., style = "broom") %>%
    as_tibble(.) %>%
    dplyr::rename(.data = ., "bf10" = "bayes.factor") %>%
    dplyr::mutate(.data = ., log_e_bf10 = log(bf10))
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
#' @importFrom performance r2_bayes
#' @importFrom bayestestR describe_prior
#' @importFrom dplyr rename_with mutate select bind_cols starts_with
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

  # for non-anova tests
  if (isFALSE(anova.design)) {
    # t-test or correlation
    estimate.type <- ifelse(df$term[[1]] == "Difference", quote(delta), quote(rho))

    # values
    c(estimate, estimate.LB, estimate.UB, bf.prior) %<-%
      c(df$estimate[[1]], df$conf.low[[1]], df$conf.high[[1]], df$prior.scale[[1]])
  } else {
    # dataframe with prior information
    df_prior <-
      bayestestR::describe_prior(bf.object) %>%
      insight::standardize_names(., style = "broom") %>%
      dplyr::filter(., term == "fixed")

    # dataframe with posterior estimates for R-squared
    df_r2 <-
      performance::r2_bayes(bf.object, average = TRUE, ci = conf.level) %>%
      as_tibble(.) %>%
      insight::standardize_names(., style = "broom") %>%
      dplyr::rename_with(.fn = ~ paste0("r2.", .x), .cols = dplyr::starts_with("conf"))

    # for within-subjects design, retain only marginal component
    if ("component" %in% names(df_r2)) {
      df_r2 %<>%
        dplyr::filter(.data = ., component == "conditional") %>%
        dplyr::rename(.data = ., "r2.component" = "component")
    }

    # combine everything
    df %<>% dplyr::bind_cols(., df_r2)

    # for expression
    c(centrality, conf.method) %<-% c("median", "hdi")
    estimate.type <- quote(R^"2")

    # values
    c(estimate, estimate.LB, estimate.UB, bf.prior) %<-%
      c(df$r2[[1]], df$r2.conf.low[[1]], df$r2.conf.high[[1]], df_prior$prior.scale[[1]])
  }

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
          bf.prior
        )
      ),
      env = list(
        top.text = top.text,
        estimate.type = estimate.type,
        centrality = centrality,
        conf.level = paste0(conf.level * 100, "%"),
        conf.method = toupper(conf.method),
        bf = specify_decimal_p(x = -log(df$bf10[[1]]), k = k),
        estimate = specify_decimal_p(x = estimate, k = k),
        estimate.LB = specify_decimal_p(x = estimate.LB, k = k),
        estimate.UB = specify_decimal_p(x = estimate.UB, k = k),
        bf.prior = specify_decimal_p(x = bf.prior, k = k)
      )
    )

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
