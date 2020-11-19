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
#'   `"hdi"` (default), `"eti"`, or `"si"` (see `si()`, `hdi()`, `eti()`
#'   functions from `bayestestR` package).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2L`).
#' @param top.text Text to display as top.text (will be displayed on top of the
#'   Bayes Factor top.text/message).
#' @param output If `"expression"`, will return expression with statistical
#'   details, while `"dataframe"` will return a dataframe containing the
#'   results.
#' @param ... Additional arguments passed to
#'   [parameters::model_parameters.BFBayesFactor()].
#'
#' @importFrom dplyr mutate rename rename_with starts_with
#' @importFrom insight standardize_names get_priors
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
                         k = 2L,
                         top.text = NULL,
                         output = "dataframe",
                         ...) {

  # ------------------------ parameters --------------------------------

  # basic parameters dataframe
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

  # expression parameter defaults
  prior.type <- quote(italic("r")["Cauchy"]^"JZS")
  estimate.type <- quote(italic(delta))

  # ------------------------ BayesFactor ---------------------------------

  if (grepl("BFBayesFactor", class(bf.object)[[1]], fixed = TRUE)) {

    # ------------------------ ANOVA designs ------------------------------

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

      # for expression
      c(centrality, conf.method) %<-% c("median", "hdi")
      estimate.type <- quote(italic(R^"2"))

      # prior
      df_prior <-
        insight::get_priors(bf.object) %>%
        dplyr::rename_with(.fn = ~ paste0("Prior_", .x), .cols = dplyr::everything()) %>%
        insight::standardize_names(., style = "broom") %>%
        dplyr::filter(.data = ., prior.parameter == "fixed")

      # merge the parameters dataframe with prior dataframe
      df <-
        dplyr::bind_cols(dplyr::select(.data = df, -dplyr::contains("prior.")), df_prior)
    }

    # ------------------------ correlation ------------------------------

    if (class(bf.object@denominator)[[1]] == "BFcorrelation") {
      estimate.type <- quote(italic(rho))
    }

    # ------------------------ contingency tabs ------------------------------

    if (class(bf.object@denominator)[[1]] == "BFcontingencyTable") {
      # dataframe cleanup
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

      # for expression
      c(estimate.type, prior.type) %<-% c(quote(italic("V")), quote(italic("a")["Gunel-Dickey"]))
    }
  }

  # ------------------------ metaBMA -------------------------------------

  if (!grepl("BFBayesFactor", class(bf.object)[[1]], fixed = TRUE)) {
    # dataframe cleanup
    df %<>%
      dplyr::filter(.data = ., term %in% c("Overall", "tau")) %>%
      dplyr::mutate(.data = ., prior.scale = bf.object$jzs$rscale_discrete[[1]])

    # for expression
    c(centrality, conf.method) %<-% c("mean", "hdi")
  }

  # Bayes Factor expression
  bf_expr_01 <-
    bf_expr_template(
      top.text = top.text,
      prior.type = prior.type,
      estimate.type = estimate.type,
      estimate.df = df,
      centrality = centrality,
      conf.level = conf.level,
      conf.method = conf.method,
      k = k
    )

  # return the text results or the dataframe with results
  switch(
    EXPR = output,
    "dataframe" = df,
    bf_expr_01
  )
}

#' @name meta_data_check
#' @title Helper function to check column names for meta-analysis.
#'
#' @inheritParams bf_meta_random
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
