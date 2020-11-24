#' @title Bayes factor for random-effects meta-analysis
#' @name bf_meta_random
#'
#' @param data A dataframe. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used for `y`  and `SE` arguments in
#'   `metaBMA::meta_random`.
#' @param metaBMA.args A list of additional arguments to be passed to
#'   `metaBMA::meta_random`.
#' @inheritDotParams bf_extractor -bf.object -centrality -conf.method
#'
#' @importFrom metaBMA meta_random prior
#' @importFrom rlang exec !!!
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
#'   metaBMA.args = list(iter = 500, rscale_discrete = 0.880),
#'   output = "dataframe"
#' )
#' }
#'
#' @export

# function body
bf_meta_random <- function(data, metaBMA.args = list(), ...) {
  # extracting results from random-effects meta-analysis
  bf_object <-
    rlang::exec(
      .fn = metaBMA::meta_random,
      y = data$estimate,
      SE = data$std.error,
      !!!metaBMA.args
    )

  # final return
  bf_extractor(bf_object, centrality = "mean", conf.method = "hdi", ...)
}
