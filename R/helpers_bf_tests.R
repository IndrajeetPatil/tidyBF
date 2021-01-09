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
#' @param top.text Text to display on top of the Bayes Factor message. This is
#'   mostly relevant in the context of `ggstatsplot` functions.
#' @param output If `"expression"`, will return expression with statistical
#'   details, while `"dataframe"` will return a dataframe containing the
#'   results.
#' @param ... Additional arguments passed to
#'   [parameters::model_parameters.BFBayesFactor()].
#'
#' @importFrom dplyr mutate filter rename rename_with starts_with
#' @importFrom insight standardize_names
#' @importFrom performance r2_bayes
#' @importFrom tidyr fill
#' @importFrom parameters model_parameters
#' @importFrom effectsize effectsize
#'
#' @note *Important*: don't enter `1/bf.object` to extract results for null
#'   hypothesis; doing so will return wrong results.
#'
#' @examples
#' \donttest{
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
#' }
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

  # basic parameters dataframe
  df <-
    suppressMessages(parameters::model_parameters(
      model = bf.object,
      ci = conf.level,
      ci_method = conf.method,
      centrality = centrality,
      verbose = FALSE,
      include_studies = FALSE,
      ...
    )) %>%
    insight::standardize_names(data = ., style = "broom") %>%
    dplyr::rename("bf10" = "bayes.factor") %>%
    tidyr::fill(data = ., dplyr::matches("^prior|^bf"), .direction = "updown") %>%
    dplyr::mutate(log_e_bf10 = log(bf10))

  # expression parameter defaults
  c(prior.type, estimate.type) %<-% c(quote(italic("r")["Cauchy"]^"JZS"), quote(italic(delta)))

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
          dplyr::filter(component == "conditional") %>%
          dplyr::rename("r2.component" = "component")
      }

      # combine everything
      df %<>% dplyr::bind_cols(., df_r2)

      # for expression
      c(centrality, conf.method, estimate.type) %<-% c("median", "hdi", quote(italic(R^"2")))
    }

    # ------------------------ correlation ------------------------------

    if (class(bf.object@denominator)[[1]] == "BFcorrelation") {
      estimate.type <- quote(italic(rho))
    }

    # ------------------------ contingency tabs ------------------------------

    if (class(bf.object@denominator)[[1]] == "BFcontingencyTable") {
      df %<>% dplyr::filter(grepl("cramer", term, TRUE))
      c(estimate.type, prior.type) %<-% c(quote(italic("V")), quote(italic("a")["Gunel-Dickey"]))
    }
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
  switch(output, "dataframe" = as_tibble(df), bf_expr_01)
}
