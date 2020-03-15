# bayes factor (between-subjects - anova) ---------------------------------

testthat::test_that(
  desc = "bayes factor (between-subjects - anova)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # dataframe
    set.seed(123)
    dat <- dplyr::filter(ggplot2::msleep, !is.na(brainwt), !is.na(vore)) %>%
      dplyr::mutate(.data = ., vore = as.factor(vore))

    # creating a dataframe
    set.seed(123)
    df <-
      suppressMessages(bf_extractor(
        BayesFactor::anovaBF(
          formula = brainwt ~ vore,
          data = as.data.frame(dat),
          progress = FALSE,
          rscaleFixed = 0.99
        )
      ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      bf_oneway_anova(
        data = dat,
        x = vore,
        y = brainwt,
        bf.prior = 0.99,
        output = "results"
      )

    # extracting caption - null
    set.seed(123)
    results1 <-
      bf_oneway_anova(
        data = dat,
        x = vore,
        y = brainwt,
        bf.prior = 0.88,
        output = "null"
      )

    # extracting caption - alternative
    set.seed(123)
    results2 <-
      bf_oneway_anova(
        data = dat,
        x = vore,
        y = brainwt,
        bf.prior = 0.88,
        output = "alternative"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 0.1177186, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -2.139458, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.9291548, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "1.92",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.88"
        )
      ))
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-1.92",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.88"
        )
      ))
    )
  }
)

# bayes factor (within-subjects - anova) ---------------------------------

testthat::test_that(
  desc = "bayes factor (within-subjects - anova)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # dataframe
    dat <- WRS2::WineTasting

    # creating a dataframe
    set.seed(123)
    df <-
      suppressMessages(bf_extractor(
        BayesFactor::anovaBF(
          formula = Taste ~ Wine + Taster,
          data = as.data.frame(dat),
          progress = FALSE,
          whichRandom = "Taster",
          rscaleFixed = 0.99,
          rscaleRandom = 1
        )
      ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      bf_oneway_anova(
        data = dat,
        x = Wine,
        y = "Taste",
        paired = TRUE,
        bf.prior = 0.99,
        output = "results"
      )

    # extracting caption - null
    set.seed(123)
    results1 <-
      bf_oneway_anova(
        data = dat,
        x = "Wine",
        y = Taste,
        k = 4,
        paired = TRUE,
        bf.prior = 0.88,
        output = "null"
      )

    # extracting caption - alternative
    set.seed(123)
    results2 <-
      bf_oneway_anova(
        data = dat,
        x = Wine,
        y = Taste,
        k = 4,
        paired = TRUE,
        bf.prior = 0.88,
        output = "alternative"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 6.364917, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 1.850801, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 0.8037927, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-1.9580",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.8800"
        )
      ))
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "1.9580",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.8800"
        )
      ))
    )
  }
)
