# bayes factor (independent samples t-test) ----------------------

testthat::test_that(
  desc = "bayes factor (independent samples t-test)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # from Bayes Factor
    df <- suppressMessages(bf_extractor(
      BayesFactor::ttestBF(
        formula = len ~ supp,
        data = as.data.frame(ToothGrowth),
        rscale = 0.99,
        paired = FALSE
      )
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      bf_ttest(
        data = ToothGrowth,
        x = supp,
        y = "len",
        paired = FALSE,
        bf.prior = 0.99,
        output = "results"
      )

    # check bayes factor values
    testthat::expect_equal(df$log_e_bf10, -0.001119132, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.0004860328, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)
  }
)

# Bayes factor (paired t-test) ---------------------------------------------

testthat::test_that(
  desc = "bayes factor (paired t-test)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # data
    dat <- tidyr::spread(bugs_long, condition, desire) %>%
      dplyr::filter(.data = ., !is.na(HDLF), !is.na(HDHF))

    # BF output
    set.seed(123)
    df <- suppressMessages(bf_extractor(
      BayesFactor::ttestBF(
        x = dat$HDLF,
        y = dat$HDHF,
        rscale = 0.8,
        paired = TRUE
      )
    ))

    # creating a tidy dataframe
    dat_tidy <- dplyr::filter(bugs_long, condition %in% c("HDLF", "HDHF"))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      bf_ttest(
        data = dat_tidy,
        x = "condition",
        y = desire,
        paired = TRUE,
        bf.prior = 0.8,
        output = "results"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 40.36079, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 3.697859, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 1.60596, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)
  }
)

# bayes factor (one sample t-test) ----------------------

testthat::test_that(
  desc = "bayes factor (one sample t-test)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # creating a dataframe
    set.seed(123)
    df <-
      suppressMessages(bf_extractor(
        BayesFactor::ttestBF(
          x = iris$Petal.Length,
          mu = 5.5,
          rscale = 0.99
        )
      ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      bf_ttest(
        data = iris,
        x = Petal.Length,
        y = NULL,
        test.value = 5.5,
        bf.prior = 0.99,
        output = "results"
      )

    # check Bayes factor values
    testthat::expect_equal(df$bf10, 5.958171e+20, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 47.83647, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 20.77511, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # extracting subtitle
    set.seed(123)
    subtitle <-
      bf_ttest(
        data = iris,
        x = "Petal.Length",
        y = NULL,
        test.value = 5.5,
        bf.prior = 0.99,
        output = "subtitle"
      )

    testthat::expect_identical(
      subtitle,
      ggplot2::expr(
        atop(
          displaystyle(NULL),
          expr = paste(
            "In favor of alternative: ",
            "log"["e"],
            "(BF"["10"],
            ") = ",
            "47.84",
            ", ",
            italic("r")["Cauchy"]^"JZS",
            " = ",
            "0.99"
          )
        )
      )
    )
  }
)
