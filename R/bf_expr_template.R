#' @title Expression template for Bayes Factor results
#' @name bf_expr_template
#'
#' @param prior.type A character that specifies the prior type.
#' @param estimate.type A character that specifies the relevant effect size.
#' @param estimate.df Dataframe containing estimates and their credible
#'   intervals along with Bayes Factor value. The columns should be named as
#'   `estimate`, `estimate.LB`, `estimate.UB`, and `bf10`.
#' @param ... Currently ignored.
#' @inheritParams bf_expr
#' @inheritParams bf_ttest
#'
#' @export

bf_expr_template <- function(top.text,
                             bf.prior = 0.707,
                             prior.type = quote(italic("r")["Cauchy"]^"JZS"),
                             estimate.type = quote(delta),
                             estimate.df,
                             centrality = "median",
                             conf.level = 0.95,
                             conf.method = "HDI",
                             k = 2L,
                             ...) {
  # extracting estimate values
  if ("r2" %in% names(estimate.df)) {
    # for ANOVA designs
    c(estimate, estimate.LB, estimate.UB) %<-%
      c(estimate.df$r2[[1]], estimate.df$r2.conf.low[[1]], estimate.df$r2.conf.high[[1]])
  } else {
    # for non-ANOVA designs
    c(estimate, estimate.LB, estimate.UB) %<-%
      c(estimate.df$estimate[[1]], estimate.df$conf.low[[1]], estimate.df$conf.high[[1]])
  }

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
        bf = specify_decimal_p(x = -log(estimate.df$bf10[[1]]), k = k),
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
