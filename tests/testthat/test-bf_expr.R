
# bayes factor caption maker check --------------------------

testthat::test_that(
  desc = "bayes factor caption maker check",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # bayes factor results
    set.seed(123)
    bf_results <- tibble::tribble(
      ~log_e_bf01, ~bf.prior,
      1.1, 0.88
    )

    # expected
    using1 <- bf_expr(
      bf.df = bf_results,
      k = 3,
      caption = substitute(paste(italic("Note", ": made up data")))
    )
    using2 <- bf_expr(
      bf.df = bf_results,
      output = "H1",
      caption = substitute(paste(italic("Note", ": made up data")))
    )

    testthat::expect_identical(
      using1,
      ggplot2::expr(atop(
        displaystyle(paste(italic(
          "Note", ": made up data"
        ))),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "1.100",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.880"
        )
      ))
    )

    testthat::expect_identical(
      using2,
      ggplot2::expr(atop(
        displaystyle(paste(italic(
          "Note", ": made up data"
        ))),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-1.10",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.88"
        )
      ))
    )
  }
)
