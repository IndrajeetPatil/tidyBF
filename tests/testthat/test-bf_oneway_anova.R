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
        data = ggplot2::msleep,
        x = "vore",
        y = brainwt,
        bf.prior = 0.99,
        output = "dataframe"
      )

    # extracting caption - null
    set.seed(123)
    results1 <-
      bf_oneway_anova(
        data = ggplot2::msleep,
        x = vore,
        y = "brainwt",
        bf.prior = 0.88,
        output = "null"
      )

    # extracting caption - alternative
    set.seed(123)
    results2 <-
      bf_oneway_anova(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        bf.prior = 0.88,
        output = "expression"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10[[1]], 0.1177186, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10[[1]], -2.139458, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10[[1]], df_results$bf10[[1]], tolerance = 0.001)

    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(
        paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "1.92"
        )
      )
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(
        paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "1.92"
        )
      )
    )
  }
)

# bayes factor (within-subjects - anova) ---------------------------------

testthat::test_that(
  desc = "bayes factor (within-subjects - anova)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # dataframe
    dat <- structure(list(Taste = c(
      5.4, 5.5, 5.55, 5.85, 5.7, 5.75, 5.2,
      5.6, 5.5, 5.55, 5.5, 5.4, 5.9, 5.85, 5.7, 5.45, 5.55, 5.6, 5.4,
      5.4, 5.35, 5.45, 5.5, 5.35, 5.25, 5.15, 5, 5.85, 5.8, 5.7, 5.25,
      5.2, 5.1, 5.65, 5.55, 5.45, 5.6, 5.35, 5.45, 5.05, 5, 4.95, 5.5,
      5.5, 5.4, 5.45, 5.55, 5.5, 5.55, 5.55, 5.35, 5.45, 5.5, 5.55,
      5.5, 5.45, 5.25, 5.65, 5.6, 5.4, 5.7, 5.65, 5.55, 6.3, 6.3, 6.25
    ), Wine = structure(c(
      1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L,
      2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L,
      3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L,
      1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L,
      2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L
    ), .Label = c(
      "Wine A", "Wine B",
      "Wine C"
    ), class = "factor"), Taster = structure(c(
      1L, 1L, 1L,
      2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 7L,
      7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L, 10L, 10L, 10L, 11L, 11L, 11L,
      12L, 12L, 12L, 13L, 13L, 13L, 14L, 14L, 14L, 15L, 15L, 15L, 16L,
      16L, 16L, 17L, 17L, 17L, 18L, 18L, 18L, 19L, 19L, 19L, 20L, 20L,
      20L, 21L, 21L, 21L, 22L, 22L, 22L
    ), .Label = c(
      "1", "2", "3",
      "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
      "16", "17", "18", "19", "20", "21", "22"
    ), class = "factor")), row.names = c(
      NA,
      -66L
    ), class = "data.frame")

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
        output = "dataframe"
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
        output = "expression"
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
        output = "expression"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10[[1]], 6.364917, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10[[1]], 1.850801, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10[[1]], df_results$bf10[[1]], tolerance = 0.001)


    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(
        paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-1.9580"
        )
      )
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(
        paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-1.9580"
        )
      )
    )

    # data with NA
    set.seed(123)
    df_results_na <-
      bf_oneway_anova(
        data = bugs_long,
        x = condition,
        y = "desire",
        paired = TRUE,
        output = "caption"
      )

    testthat::expect_identical(
      df_results_na,
      ggplot2::expr(paste(
        "log"["e"],
        "(BF"["01"],
        ") = ",
        "-21.04"
      ))
    )
  }
)

# with subject.id ---------------------------------

testthat::test_that(
  desc = "with subject.id",
  code = {
    testthat::skip_if(getRversion() < "3.6")
    # data
    df <-
      structure(list(
        score = c(
          70, 82.5, 97.5, 100, 52.5, 62.5,
          92.5, 70, 90, 92.5, 90, 75, 60, 90, 85, 67.5, 90, 72.5, 45, 60,
          72.5, 80, 100, 100, 97.5, 95, 65, 87.5, 90, 62.5, 100, 100, 97.5,
          100, 97.5, 95, 82.5, 82.5, 40, 92.5, 85, 72.5, 35, 27.5, 82.5
        ), condition = structure(c(
          5L, 1L, 2L, 3L, 4L, 4L, 5L, 1L,
          2L, 3L, 2L, 3L, 3L, 4L, 2L, 1L, 5L, 5L, 4L, 1L, 1L, 4L, 3L, 5L,
          2L, 5L, 1L, 2L, 3L, 4L, 4L, 5L, 1L, 2L, 3L, 2L, 3L, 4L, 1L, 5L,
          3L, 2L, 5L, 4L, 1L
        ), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
        id = structure(c(
          1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
          2L, 3L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 4L, 5L, 5L, 5L, 5L,
          5L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L,
          8L, 9L, 9L, 9L, 9L, 9L
        ), .Label = c(
          "1", "2", "3", "4", "5",
          "6", "7", "8", "9"
        ), class = "factor")
      ), row.names = c(
        NA,
        45L
      ), class = "data.frame")

    # incorrect
    set.seed(123)
    expr1 <-
      tidyBF::bf_oneway_anova(
        data = df,
        x = condition,
        y = score,
        subject.id = id,
        paired = TRUE,
        output = "expression"
      )

    # correct
    set.seed(123)
    expr2 <-
      tidyBF::bf_oneway_anova(
        data = dplyr::arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE,
        output = "expression"
      )

    testthat::expect_equal(expr2, expr1)
  }
)
