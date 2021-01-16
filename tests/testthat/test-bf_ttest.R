# bayes factor (independent samples t-test) ----------------------

test_that(
  desc = "bayes factor (independent samples t-test)",
  code = {
    skip_if(getRversion() < "3.6")
    skip_on_cran()

    # from Bayes Factor
    df <- suppressMessages(bf_extractor(
      BayesFactor::ttestBF(
        formula = len ~ supp,
        data = as.data.frame(ToothGrowth),
        rscale = 0.99,
        paired = FALSE
      )
    ))

    # check bayes factor values
    expect_type(df, "list")
    expect_identical(class(df), c("tbl_df", "tbl", "data.frame"))
    expect_equal(df$log_e_bf10[[1]], -0.001119132, tolerance = 0.001)
  }
)

# Bayes factor (paired t-test) ---------------------------------------------

test_that(
  desc = "bayes factor (paired t-test)",
  code = {
    skip_if(getRversion() < "3.6")
    skip_on_cran()

    # data
    dat <- tidyr::spread(bugs_long, condition, desire) %>%
      dplyr::filter(.data = ., !is.na(HDLF), !is.na(HDHF))

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
        bf.prior = 0.8
      )

    # check bayes factor values
    expect_equal(df_results$bf10[[1]], 40.36079, tolerance = 0.001)
    expect_equal(df_results$log_e_bf10[[1]], 3.697859, tolerance = 0.001)
  }
)

# bayes factor (one sample t-test) ----------------------

test_that(
  desc = "bayes factor (one sample t-test)",
  code = {
    skip_if(getRversion() < "3.6")
    skip_on_cran()

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      bf_ttest(
        data = iris,
        x = Petal.Length,
        y = NULL,
        test.value = 5.5,
        bf.prior = 0.99
      )

    # check Bayes factor values
    expect_equal(df_results$bf10[[1]], 5.958171e+20, tolerance = 0.001)
    expect_equal(df_results$log_e_bf10[[1]], 47.83647, tolerance = 0.001)

    # extracting subtitle (without NA)
    set.seed(123)
    subtitle <-
      bf_ttest(
        data = iris,
        x = "Petal.Length",
        y = NULL,
        test.value = 5.5,
        bf.prior = 0.99,
        output = "expression",
        centrality = "mean",
        conf.level = 0.90
      )

    expect_type(subtitle, "language")

    expect_identical(
      subtitle,
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "-47.84" * ", ",
          widehat(italic(delta))["mean"]^"posterior" * " = " * "1.75" * ", ",
          "CI"["90%"]^"HDI" * " [" * "1.52" * ", " * "1.99" * "], ",
          italic("r")["Cauchy"]^"JZS" * " = " * "0.99"
        )
      )
    )

    # extracting subtitle (with NA)
    set.seed(123)
    subtitle2 <-
      bf_ttest(
        data = ggplot2::msleep,
        x = brainwt,
        y = NULL,
        test.value = 0.25,
        bf.prior = 0.9,
        k = 3,
        output = "subtitle",
        conf.method = "eti"
      )

    expect_identical(
      subtitle2,
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "2.125" * ", ",
          widehat(italic(delta))["median"]^"posterior" * " = " * "-0.018" * ", ",
          "CI"["95%"]^"ETI" * " [" * "-0.274" * ", " * "0.234" * "], ",
          italic("r")["Cauchy"]^"JZS" * " = " * "0.900"
        )
      )
    )
  }
)
