#' @title Bayes factor message for random-effects meta-analysis
#' @name bf_meta
#'
#' @importFrom metaBMA meta_random prior
#'
#' @inherit metaBMA::meta_random return Description
#'
#' @param data A dataframe. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used for `yi`  and `sei` arguments in `metafor::rma`
#'   (for parametric analysis) or `metaplus::metaplus` (for robust analysis).
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritParams bf_expr
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
#' # getting Bayes factor in favor of null hypothesis
#' bf_meta(
#'   data = df,
#'   k = 3,
#'   iter = 1500,
#'   messages = TRUE,
#'   # customizing analysis with additional arguments
#'   control = list(max_treedepth = 15)
#' )
#' }
#'
#' @export

# function body
bf_meta <- function(data,
                    d = prior("norm", c(mean = 0, sd = 0.3)),
                    tau = prior("invgamma", c(shape = 1, scale = 0.15)),
                    k = 2,
                    output = "results",
                    caption = NULL,
                    messages = TRUE,
                    ...) {

  # check the data contains needed column
  meta_data_check(data)

  #----------------------- meta-analysis -------------------------------

  # extracting results from random-effects meta-analysis
  meta_res <-
    metaBMA::meta_random(
      y = data$estimate,
      SE = data$std.error,
      d = d,
      tau = tau,
      ...
    )

  # print results from meta-analysis
  if (isTRUE(messages)) print(meta_res)

  #----------------------- preparing caption -------------------------------

  # creating a dataframe with posterior estimates
  df_estimates <-
    tibble::as_tibble(meta_res$estimates, rownames = "term") %>%
    dplyr::filter(.data = ., term == "d")

  # dataframe with bayes factors
  bf.df <-
    dplyr::tibble(bf10 = meta_res$BF["random_H1", "random_H0"]) %>%
    bf_formatter(.)

  # changing aspects of the caption based on what output is needed
  if (output %in% c("null", "caption", "H0", "h0")) {
    hypothesis.text <- "In favor of null: "
    bf.value <- bf.df$log_e_bf01[[1]]
    bf.subscript <- "01"
  } else {
    hypothesis.text <- "In favor of alternative: "
    bf.value <- bf.df$log_e_bf10[[1]]
    bf.subscript <- "10"
  }

  # prepare the Bayes factor message
  bf_message <-
    substitute(
      atop(displaystyle(top.text),
        expr = paste(
          hypothesis.text,
          "log"["e"],
          "(BF"[bf.subscript],
          ") = ",
          bf,
          ", ",
          italic("d")["mean"]^"posterior",
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
        hypothesis.text = hypothesis.text,
        top.text = caption,
        bf.subscript = bf.subscript,
        bf = specify_decimal_p(x = bf.value, k = k),
        d.pmean = specify_decimal_p(x = df_estimates$mean[[1]], k = k),
        d.pmean.LB = specify_decimal_p(x = df_estimates$hpd95_lower[[1]], k = k),
        d.pmean.UB = specify_decimal_p(x = df_estimates$hpd95_upper[[1]], k = k)
      )
    )

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf.df,
    bf_message
  ))
}


#' @noRd

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
