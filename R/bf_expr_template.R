#' @title Expression template for Bayes Factor results
#' @name bf_expr_template
#'
#' @param bf.value Numeric value corresponding to Bayes Factor.
#' @param prior.type A character that specifies the prior type.
#' @param estimate.type A character that specifies the relevant effect size.
#' @param estimate,estimate.LB,estimate.UB Values of posterior estimates and
#'   their credible intervals.
#' @param ... Currently ignored.
#' @inheritParams bf_expr
#' @inheritParams bf_ttest
#'
#' @export

bf_expr_template <- function(top.text,
                             bf.value,
                             bf.prior = 0.707,
                             prior.type = quote(italic("r")["Cauchy"]^"JZS"),
                             estimate.type = quote(delta),
                             estimate,
                             estimate.LB,
                             estimate.UB,
                             centrality = "median",
                             conf.level = 0.95,
                             conf.method = "HDI",
                             k = 2L,
                             ...) {
  # prepare the Bayes Factor message
  bf01_expr <-
    substitute(
      atop(
        displaystyle(top.text),
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
          prior.type,
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
        bf = specify_decimal_p(x = bf.value, k = k),
        estimate = specify_decimal_p(x = estimate, k = k),
        estimate.LB = specify_decimal_p(x = estimate.LB, k = k),
        estimate.UB = specify_decimal_p(x = estimate.UB, k = k),
        prior.type = prior.type,
        bf.prior = specify_decimal_p(x = bf.prior, k = k)
      )
    )

  # return the final expression
  if (is.null(top.text)) bf01_expr$expr else bf01_expr
}
