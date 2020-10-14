# bayes factor extractor works --------------------------

testthat::test_that(
  desc = "bayes factor (correlation)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(bf_extractor(
      BayesFactor::correlationBF(
        x = ggplot2::msleep$brainwt,
        y = ggplot2::msleep$sleep_total
      )
    ))

    # check bayes factor values
    testthat::expect_equal(df$bf10[[1]], 8.990505, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10[[1]], 2.196169, tolerance = 0.001)
  }
)
