# bf_meta_random works ----------------------------------------------------

testthat::test_that(
  desc = "bf_meta_random works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # setup
    set.seed(123)
    library(metaBMA)

    # creating a dataframe
    df1 <-
      structure(
        .Data = list(
          term = c("1", "2", "3", "4", "5"),
          estimate = c(
            0.382047603321706,
            0.780783111514665,
            0.425607573765058,
            0.558365541235078,
            0.956473848429961
          ),
          std.error = c(
            0.0465576338644502,
            0.0330218199731529,
            0.0362834986178494,
            0.0480571500648261,
            0.062215818388157
          )
        ),
        row.names = c(NA, -5L),
        class = c("tbl_df", "tbl", "data.frame")
      )

    # getting bayes factor in favor of null hypothesis
    set.seed(123)
    subtitle1 <-
      suppressWarnings(bf_meta_random(
        data = df1,
        k = 3,
        iter = 1000,
        summarize = "integrate",
        output = "expression"
      ))

    set.seed(123)
    subtitle2 <-
      suppressWarnings(bf_meta_random(
        data = df1,
        k = 3,
        iter = 1000,
        summarize = "integrate",
        output = "expression",
        top.text = "ayyo"
      ))


    set.seed(123)
    df <-
      suppressWarnings(bf_meta_random(
        data = df1,
        k = 3,
        iter = 1000,
        summarize = "integrate",
        output = "dataframe",
        top.text = "ayyo"
      ))

    testthat::expect_is(df, "tbl_df")

    testthat::expect_identical(
      subtitle1,
      ggplot2::expr(
        paste(
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-3.341",
          ", ",
          widehat(italic(delta))["mean"]^"posterior",
          " = ",
          "0.518",
          ", CI"["95%"]^"HDI",
          " [",
          "0.219",
          ", ",
          "0.766",
          "]",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.707"
        )
      )
    )

    testthat::expect_identical(
      subtitle2,
      ggplot2::expr(
        atop(
          displaystyle("ayyo"),
          expr = paste(
            "log"["e"],
            "(BF"["01"],
            ") = ",
            "-3.341",
            ", ",
            widehat(italic(delta))["mean"]^
              "posterior",
            " = ",
            "0.518",
            ", CI"["95%"]^"HDI",
            " [",
            "0.219",
            ", ",
            "0.766",
            "]",
            ", ",
            italic("r")["Cauchy"]^
              "JZS",
            " = ",
            "0.707"
          )
        )
      )
    )

    # checking message
    testthat::expect_error(
      bf_meta_random(
        data = dplyr::select(df1, -estimate),
        k = 3,
        iter = 1000,
        summarize = "integrate"
      )
    )
  }
)
