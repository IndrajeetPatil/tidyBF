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
        output = "results"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 0.2465787, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -1.400074, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.6080444, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # caption
    set.seed(123)
    caption_text <-
      bf_contingency_tab(
        data = mtcars,
        x = "cyl",
        output = "alternative",
        prior.concentration = 10
      )

    testthat::expect_identical(
      caption_text,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-0.55",
          ", ",
          italic("a"),
          " = ",
          "10.00"
        )
      ))
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
        output = "results"
      )

    # caption
    caption_text <-
      bf_contingency_tab(
        data = mtcars,
        x = am,
        y = "cyl",
        sampling.plan = "jointMulti",
        fixed.margin = "rows",
        output = "alternative"
      )

    # with counts
    caption_text2 <-
      bf_contingency_tab(
        data = as.data.frame(Titanic),
        x = "Survived",
        y = Sex,
        counts = "Freq",
        sampling.plan = "jointMulti",
        fixed.margin = "rows",
        output = "alternative"
      )

    # with counts
    caption_text3 <-
      bf_contingency_tab(
        data = as.data.frame(Titanic),
        x = Survived,
        y = Sex,
        counts = "Freq",
        output = "H0"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 28.07349, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 3.334826, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 1.448296, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # caption text
    testthat::expect_identical(
      caption_text,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "3.33",
          ", ",
          italic("a"),
          " = ",
          "1.00"
        )
      ))
    )

    testthat::expect_identical(
      caption_text2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "214.25",
          ", ",
          italic("a"),
          " = ",
          "1.00"
        )
      ))
    )
    testthat::expect_identical(
      caption_text3,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-213.98",
          ", ",
          italic("a"),
          " = ",
          "1.00"
        )
      ))
    )
  }
)

# check edge cases --------------------------------------------

testthat::test_that(
  desc = "bayes factor caption maker check",
  code = {
    df <- data.frame(x = c("a"))

    testthat::expect_null(bf_onesample_proptest(df, x))
  }
)
