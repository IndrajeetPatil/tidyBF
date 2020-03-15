# bayes factor (correlation test) --------------------------------------

testthat::test_that(
  desc = "bayes factor (correlation test)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      bf_contingency_tab(
        data = mtcars,
        x = wt,
        y = mpg,
        output = "results"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 14137.82, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 9.556609, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 4.150382, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    set.seed(123)
    subtitle1 <-
      bf_contingency_tab(
        data = mtcars,
        x = wt,
        y = mpg,
        output = "subtitle"
      )

    testthat::expect_identical(
      subtitle1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "9.56",
          ", sampling = ",
          "independent multinomial",
          ", ",
          italic("a"),
          " = ",
          "1.00"
        )
      ))
    )
  }
)
