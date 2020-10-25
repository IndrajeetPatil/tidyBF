# bayes factor (proportion test) --------------------------------------

testthat::test_that(
  desc = "bayes factor (proportion test)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      bf_contingency_tab(
        data = mtcars,
        x = am,
        output = "dataframe"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 0.2465787, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -1.400074, tolerance = 0.001)

    # expr
    set.seed(123)
    expr_text <-
      bf_contingency_tab(
        data = mtcars,
        x = "cyl",
        output = "expression",
        prior.concentration = 10,
        top.text = "duh"
      )

    testthat::expect_identical(
      expr_text,
      ggplot2::expr(
        atop(displaystyle("duh"),
          expr =
            paste(
              "log"["e"],
              "(BF"["01"],
              ") = ",
              "0.55",
              ", ",
              italic("a")["Gunel-Dickey"],
              " = ",
              "10.00"
            )
        )
      )
    )
  }
)

# bayes factor (contingency tab) --------------------------------------

testthat::test_that(
  desc = "bayes factor (contingency tab)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      suppressMessages(bf_extractor(
        BayesFactor::contingencyTableBF(
          x = table(mtcars$am, mtcars$cyl),
          sampleType = "jointMulti",
          fixedMargin = "rows"
        )
      ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      bf_contingency_tab(
        data = mtcars,
        x = am,
        y = cyl,
        sampling.plan = "jointMulti",
        fixed.margin = "rows",
        output = "dataframe"
      )

    # expr
    set.seed(123)
    expr_text <-
      bf_contingency_tab(
        data = mtcars,
        x = am,
        y = "cyl",
        sampling.plan = "jointMulti",
        fixed.margin = "rows",
        conf.level = 0.89,
        k = 3L,
        output = "expression"
      )

    # with counts
    set.seed(123)
    expr_text2 <-
      bf_contingency_tab(
        data = as.data.frame(Titanic),
        x = "Survived",
        y = Sex,
        counts = "Freq",
        sampling.plan = "jointMulti",
        fixed.margin = "rows",
        k = 3L,
        output = "expression",
        conf.level = 0.99,
        centrality = "mean"
      )

    # with counts
    set.seed(123)
    expr_text3 <-
      bf_contingency_tab(
        data = as.data.frame(Titanic),
        x = Survived,
        y = Sex,
        counts = "Freq",
        k = 3L,
        output = "expression",
        prior.concentration = 1.5
      )

    # check bayes factor values
    testthat::expect_is(df, "tbl_df")
    testthat::expect_equal(df$bf10, 28.07349, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 3.334826, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)


    # expr text
    testthat::expect_identical(
      expr_text,
      ggplot2::expr(
        paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-3.335",
          ", ",
          widehat(italic(widehat(italic("V"))["Cramer"]))["median"]^"posterior",
          " = ",
          "0.473",
          ", CI"["89%"]^"HDI",
          " [",
          "0.253",
          ", ",
          "0.668",
          "]",
          ", ",
          italic("a")["Gunel-Dickey"],
          " = ",
          "1.000"
        )
      )
    )

    testthat::expect_identical(
      expr_text2,
      ggplot2::expr(
        paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-214.255",
          ", ",
          widehat(italic(widehat(italic("V"))["Cramer"]))["mean"]^"posterior",
          " = ",
          "0.455",
          ", CI"["99%"]^"HDI",
          " [",
          "0.398",
          ", ",
          "0.505",
          "]",
          ", ",
          italic("a")["Gunel-Dickey"],
          " = ",
          "1.000"
        )
      )
    )

    testthat::expect_identical(
      expr_text3,
      ggplot2::expr(
        paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-213.873",
          ", ",
          widehat(italic(widehat(italic("V"))["Cramer"]))["median"]^"posterior",
          " = ",
          "0.455",
          ", CI"["95%"]^"HDI",
          " [",
          "0.415",
          ", ",
          "0.495",
          "]",
          ", ",
          italic("a")["Gunel-Dickey"],
          " = ",
          "1.500"
        )
      )
    )
  }
)

# check edge cases --------------------------------------------

testthat::test_that(
  desc = "check edge cases",
  code = {
    df <- data.frame(x = c("a"))

    testthat::expect_null(bf_contingency_tab(df, x))
  }
)
