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
        output = "results"
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
        output = "alternative"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10[[1]], 0.1177186, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10[[1]], -2.139458, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10[[1]], -df$log_e_bf01[[1]], tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10[[1]], -0.9291548, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10[[1]], -df$log_10_bf01[[1]], tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10[[1]], df_results$bf10[[1]], tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01[[1]], df_results$log_e_bf01[[1]], tolerance = 0.001)

    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(atop(displaystyle(NULL),
        expr = paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "1.92"
        )
      ))
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-1.92"
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
    testthat::expect_equal(df$bf10[[1]], 6.364917, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10[[1]], 1.850801, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10[[1]], -df$log_e_bf01[[1]], tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10[[1]], 0.8037927, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10[[1]], -df$log_10_bf01[[1]], tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10[[1]], df_results$bf10[[1]], tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01[[1]], df_results$log_e_bf01[[1]], tolerance = 0.001)

    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-1.9580"
        )
      ))
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "1.9580"
        )
      ))
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
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-21.04"
        )
      ))
    )
  }
)
