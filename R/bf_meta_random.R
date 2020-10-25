#' @title Bayes factor for random-effects meta-analysis
#' @name bf_meta_random
#'
#' @param data A dataframe. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used for `y`  and `SE` arguments in
#'   `metaBMA::meta_random`.
#' @inheritParams bf_expr
#' @inheritParams bf_ttest
#' @inheritParams metaBMA::meta_random
#' @inheritDotParams metaBMA::meta_random -y -SE -ci
#'
#' @importFrom metaBMA meta_random prior
#'
#' @examples
#'
#' \donttest{
#' # setup
#' set.seed(123)
#' library(metaBMA)
#'
#' # creating a dataframe
#' (df <-
#'   structure(
#'     .Data = list(
#'       study = c("1", "2", "3", "4", "5"),
#'       estimate = c(
#'         0.382047603321706,
#'         0.780783111514665,
#'         0.425607573765058,
#'         0.558365541235078,
#'         0.956473848429961
#'       ),
#'       std.error = c(
#'         0.0465576338644502,
#'         0.0330218199731529,
#'         0.0362834986178494,
#'         0.0480571500648261,
#'         0.062215818388157
#'       )
#'     ),
#'     row.names = c(NA, -5L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   ))
#'
#' # to get dataframe
#' bf_meta_random(
#'   data = df,
#'   k = 3,
#'   iter = 1500,
#'   # customizing analysis with additional arguments
#'   control = list(max_treedepth = 15),
#'   output = "dataframe"
#' )
#' }
#'
#' @export

# function body
bf_meta_random <- function(data,
                           d = prior("norm", c(mean = 0, sd = 0.3)),
                           tau = prior("invgamma", c(shape = 1, scale = 0.15)),
                           k = 2L,
                           output = "dataframe",
                           top.text = NULL,
                           ...) {

  # check the data contains needed column
  meta_data_check(data)

  #----------------------- meta-analysis -------------------------------

  # extracting results from random-effects meta-analysis
  bf_object <-
    metaBMA::meta_random(
      y = data$estimate,
      SE = data$std.error,
      d = d,
      tau = tau,
      ci = conf.level,
      ...
    )

  # return the text results or the dataframe with results
  switch(
    EXPR = output,
    "dataframe" = bf_extractor(bf_object),
    bf_expr(bf_object, k = k, top.text = top.text, ...)
  )
}

# aliases -----------------------------------------------------------------

#' @rdname bf_meta_random
#' @aliases bf_meta_random
#' @export

bf_meta <- bf_meta_random
