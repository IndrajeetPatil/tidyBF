#' @title Extract Bayes Factors from `BayesFactor` model object.
#' @name bf_extractor
#'
#' @param bf.object An object from `BayesFactor` package.
#' @param conf.level Confidence/Credible Interval (CI) level. Default to `0.95`
#'   (`95%`).
#' @param centrality The point-estimates (centrality indices) to compute.
#'   Character (vector) or list with one or more of these options: `"median"`,
#'   `"mean"`, `"MAP"` or `"all"`.
#' @param conf.method The type of index used for Credible Interval. Can be
#'   \code{"hdi"} (default, see [bayestestR::hdi()]), \code{"eti"} (see
#'   [bayestestR::eti()]) or \code{"si"} (see [bayestestR::si()]).
#' @param ... Additional arguments passed to
#'   [parameters::model_parameters.BFBayesFactor()].
#'
#' @importFrom dplyr mutate rename
#' @importFrom insight standardize_names
#' @importFrom performance r2_bayes
#' @importFrom effectsize effectsize
#' @importFrom parameters model_parameters
#'
#' @note *Important*: don't enter `1/bf.object` to extract results for null
#'   hypothesis; doing so will return wrong results.
#'
#' @examples
#' # setup
#' library(tidyBF)
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
bf_extractor <- function(bf.object,
                         conf.method = "hdi",
                         centrality = "median",
                         conf.level = 0.95,
                         ...) {

  # ------------------------ parameters --------------------------------

  if (class(bf.object) == "meta_random") {
    # creating a dataframe with posterior estimates
    df <-
      as_tibble(bf.object$estimates, rownames = "term") %>%
      dplyr::mutate(
        bf10 = bf.object$BF["random_H1", "random_H0"],
        prior.scale = bf.object$jzs$rscale_discrete[[1]],
        log_e_bf10 = log(bf10)
      ) %>%
      dplyr::rename(estimate = mean, conf.low = hpd95_lower, conf.high = hpd95_upper)
  } else {
    df <-
      suppressMessages(parameters::model_parameters(
        model = bf.object,
        ci = conf.level,
        ci_method = conf.method,
        centrality = centrality,
        verbose = FALSE,
        ...
      )) %>%
      insight::standardize_names(data = ., style = "broom") %>%
      as_tibble(.) %>%
      dplyr::rename(.data = ., "bf10" = "bayes.factor") %>%
      dplyr::mutate(.data = ., log_e_bf10 = log(bf10))

    # ------------------------ anova designs ------------------------------

    if (class(bf.object@denominator)[[1]] == "BFlinearModel") {
      # dataframe with posterior estimates for R-squared
      df_r2 <-
        performance::r2_bayes(bf.object, average = TRUE, ci = conf.level) %>%
        as_tibble(.) %>%
        insight::standardize_names(data = ., style = "broom") %>%
        dplyr::rename_with(.fn = ~ paste0("r2.", .x), .cols = dplyr::starts_with("conf"))

      # for within-subjects design, retain only marginal component
      if ("component" %in% names(df_r2)) {
        df_r2 %<>%
          dplyr::filter(.data = ., component == "conditional") %>%
          dplyr::rename(.data = ., "r2.component" = "component")
      }

      # combine everything
      df %<>% dplyr::bind_cols(., df_r2)
    }

    # ------------------------ contingency tabs ------------------------------

    if (class(bf.object@denominator)[[1]] == "BFcontingencyTable") {
      df %<>%
        dplyr::bind_cols(
          .,
          effectsize::effectsize(
            model = bf.object,
            ci = conf.level,
            ci_method = conf.method,
            centrality = centrality,
            ...
          ) %>%
            as_tibble(.) %>%
            insight::standardize_names(data = ., style = "broom")
        ) %>%
        dplyr::mutate(prior.scale = bf.object@denominator@prior$a[[1]])
    }
  }

  # final dataframe
  return(df)
}

#' @title Prepare expression for Bayes Factor results
#' @name bf_expr
#' @description Convenience function to create an expression with Bayes
#'   Factor results.
#'
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2L`).
#' @param top.text Text to display as top.text (will be displayed on top of the
#'   Bayes Factor top.text/message).
#' @param anova.design Whether the object is from `BayesFactor::anovaBF`
#'   (default: `FALSE`). The expression is different for anova designs because
#'   not all details are available.
#' @inheritParams bf_extractor
#'
#' @importFrom ipmisc specify_decimal_p
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
      conf.level = conf.level,
      conf.method = conf.method,
      centrality = centrality,
      ...
    )

  # default
  prior.type <- quote(italic("r")["Cauchy"]^"JZS")

  # for non-anova tests
  if (isFALSE(anova.design)) {
    # which test was run decides the estimate type
    if (df$term[[1]] %in% c("d", "Difference")) {
      estimate.type <- quote(delta)
    } else if (df$term[[1]] == "Cramers_v") {
      estimate.type <- quote(widehat(italic("V"))["Cramer"])
      prior.type <- quote(italic("a")["Gunel-Dickey"])
    } else {
      estimate.type <- quote(rho)
    }

    # for metaBMA
    if ("n_eff" %in% names(df)) c(centrality, conf.method) %<-% c("mean", "hdi")

    # for expression
    bf.prior <- df$prior.scale[[1]]
  } else {
    # dataframe with prior information
    df_prior <-
      bayestestR::describe_prior(bf.object) %>%
      insight::standardize_names(., style = "broom") %>%
      dplyr::filter(., term == "fixed")

    # for expression
    c(centrality, conf.method) %<-% c("median", "hdi")
    estimate.type <- quote(R^"2")
    bf.prior <- df_prior$prior.scale[[1]]
  }

  # Bayes Factor expression
  bf_expr_template(
    top.text = top.text,
    bf.prior = bf.prior,
    prior.type = prior.type,
    estimate.type = estimate.type,
    estimate.df = df,
    centrality = centrality,
    conf.level = conf.level,
    conf.method = conf.method,
    k = k
  )
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
