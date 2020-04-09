#' @title Extract Bayes Factors from `BayesFactor` model object.
#' @name bf_extractor
#'
#' @param bf.object An object from `BayesFactor` package.
#' @param ... Currently ignored.
#'
#' @importFrom BayesFactor extractBF
#' @importFrom dplyr rename select mutate
#'
#' @note *Important*: don't enter `1/bf_obj` to extract results for null
#'   hypothesis; # doing so will return wrong results.
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
  BayesFactor::extractBF(
    x = bf.object,
    logbf = FALSE,
    onlybf = FALSE
  ) %>%
    as_tibble(.) %>%
    dplyr::select(.data = ., -time, -code) %>%
    dplyr::rename(.data = ., bf10 = bf) %>%
    bf_formatter(.)
}

#' @title Prepare caption with expression for Bayes Factor results
#' @name bf_expr
#' @description Convenience function to create an expression with Bayes
#'   Factor results.
#'
#' @param bf.df A dataframe containing two columns `log_e_bf01` (for evidence in
#'   favor of null hypothesis) and `bf.prior`. If dataframe contains more than
#'   two rows, only the first row will be used.
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2`).
#' @param hypothesis.text Logical that decides whether the expression containing
#'   result should have text to describe the hypothesis test being described.
#'   For `output = "null"`, this is `"In favor of null: "`, otherwise
#'   `"In favor of alternative: "`.
#' @param caption Text to display as caption (will be displayed on top of the
#'   Bayes Factor caption/message).
#' @param output Can either be `"null"` (or `"caption"` or `"H0"` or `"h0"`),
#'   which will return expression with evidence in favor of the null hypothesis,
#'   or `"alternative"` (or `"title"` or `"H1"` or `"h1"`), which will return
#'   expression with evidence in favor of the alternative hypothesis, or
#'   `"results"`, which will return a dataframe with results all the details).
#' @param ... Additional arguments (ignored).
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(tidyBF)
#'
#' # dataframe containing results
#' bf.df <-
#'   bf_extractor(BayesFactor::correlationBF(
#'     x = iris$Sepal.Length,
#'     y = iris$Petal.Length
#'   )) %>%
#'   dplyr::mutate(.data = ., bf.prior = 0.707)
#'
#' # creating caption (for null)
#' bf_expr(
#'   bf.df,
#'   output = "null",
#'   k = 3,
#'   caption = "Note: Iris dataset"
#' )
#'
#' # creating caption (for alternative)
#' bf_expr(bf.df, output = "alternative")
#' }
#' @export

# function body
bf_expr <- function(bf.df,
                    k = 2L,
                    output = "null",
                    hypothesis.text = TRUE,
                    caption = NULL,
                    ...) {

  # changing aspects of the caption based on what output is needed
  if (output %in% c("null", "caption", "H0", "h0")) {
    # hypothesis text
    if (isTRUE(hypothesis.text)) hypothesis.text <- "In favor of null: "
    if (isFALSE(hypothesis.text)) hypothesis.text <- NULL

    # bf-related text
    bf.value <- bf.df$log_e_bf01[[1]]
    bf.subscript <- "01"
  } else {
    # hypothesis text
    if (isTRUE(hypothesis.text)) hypothesis.text <- "In favor of alternative: "
    if (isFALSE(hypothesis.text)) hypothesis.text <- NULL

    # bf-related text
    bf.value <- -bf.df$log_e_bf01[[1]]
    bf.subscript <- "10"
  }

  # prepare the Bayes Factor message
  substitute(
    atop(displaystyle(top.text),
      expr = paste(
        hypothesis.text,
        "log"["e"],
        "(BF"[bf.subscript],
        ") = ",
        bf,
        ", ",
        italic("r")["Cauchy"]^"JZS",
        " = ",
        bf_prior
      )
    ),
    env = list(
      hypothesis.text = hypothesis.text,
      top.text = caption,
      bf.subscript = bf.subscript,
      bf = specify_decimal_p(x = bf.value, k = k),
      bf_prior = specify_decimal_p(x = bf.df$bf.prior[[1]], k = k)
    )
  )
}


#' @noRd
#' @keywords internal

bf_formatter <- function(data) {
  dplyr::mutate(
    .data = data,
    bf01 = 1 / bf10,
    log_e_bf10 = log(bf10),
    log_e_bf01 = -1 * log_e_bf10,
    log_10_bf10 = log10(bf10),
    log_10_bf01 = -1 * log_10_bf10
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
