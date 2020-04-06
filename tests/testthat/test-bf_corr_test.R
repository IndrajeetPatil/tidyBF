# bayes factor (correlation test) --------------------------------------

testthat::test_that(
  desc = "bayes factor (correlation test) - without NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      bf_corr_test(
        data = iris,
        y = Sepal.Length,
        x = "Sepal.Width",
        output = "results"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 0.3445379, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -1.065551, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.462763, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    set.seed(123)
    subtitle1 <-
      bf_corr_test(
        data = iris,
        y = Sepal.Length,
        x = Sepal.Width,
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
          "-1.07",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.71"
        )
      ))
    )
  }
)

testthat::test_that(
  desc = "bayes factor (correlation test) - with NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      bf_corr_test(
        data = ggplot2::msleep,
        y = names(ggplot2::msleep)[10],
        x = "sleep_rem",
        output = "results"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 0.6539296, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -0.4247555, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.184469, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    set.seed(123)
    subtitle1 <-
      bf_corr_test(
        data = ggplot2::msleep,
        y = brainwt,
        x = sleep_rem,
        output = "subtitle",
        bf.prior = 0.8
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
          "-0.49",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.80"
        )
      ))
    )
  }
)
