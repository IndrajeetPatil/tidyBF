#' @title Bayes factor for random-effects meta-analysis
#' @name bf_meta
#'
#' @importFrom metaBMA meta_random prior
#'
#' @param data A dataframe. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used for `y`  and `SE` arguments in
#'   `metaBMA::meta_random`.
#' @inheritParams bf_expr
#' @inheritParams bf_ttest
#' @inheritParams metaBMA::meta_random
#' @inheritDotParams metaBMA::meta_random -y -SE
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
#' bf_meta(
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
bf_meta <- function(data,
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
  mod <-
    metaBMA::meta_random(
      y = data$estimate,
      SE = data$std.error,
      d = d,
      tau = tau,
      ...
    )

  #----------------------- preparing top.text -------------------------------

  # creating a dataframe with posterior estimates
  df <-
    as_tibble(mod$estimates, rownames = "term") %>%
    dplyr::mutate(.data = ., bf10 = mod$BF["random_H1", "random_H0"])

  # prepare the Bayes factor message
  bf01_expr <-
    substitute(
      atop(displaystyle(top.text),
        expr = paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          bf,
          ", ",
          widehat(italic(delta))["mean"]^"posterior",
          " = ",
          d.pmean,
          ", CI"["95%"],
          " [",
          d.pmean.LB,
          ", ",
          d.pmean.UB,
          "]"
        )
      ),
      env = list(
        top.text = top.text,
        bf = specify_decimal_p(x = -log(df$bf10[[1]]), k = k),
        d.pmean = specify_decimal_p(x = df$mean[[1]], k = k),
        d.pmean.LB = specify_decimal_p(x = df$hpd95_lower[[1]], k = k),
        d.pmean.UB = specify_decimal_p(x = df$hpd95_upper[[1]], k = k)
      )
    )

  # return the final expression
  if (is.null(top.text)) {
    bf_message <- bf01_expr$expr
  } else {
    bf_message <- bf01_expr
  }

  # return the text results or the dataframe with results
  switch(
    EXPR = output,
    "dataframe" = df,
    bf_message
  )
}
