# bf_meta works ----------------------------------------------------

testthat::test_that(
  desc = "bf_meta works",
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
      suppressWarnings(bf_meta(
        data = df1,
        k = 3,
        messages = TRUE,
        iter = 1000,
        summarize = "integrate",
        output = "subtitle"
      ))

    set.seed(123)
    caption1 <-
      suppressWarnings(bf_meta(
        data = df1,
        k = 3,
        messages = FALSE,
        iter = 1000,
        summarize = "integrate",
        output = "caption"
      ))

    testthat::expect_identical(
      subtitle1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "3.341",
          ", ",
          italic("d")["mean"]^"posterior",
          " = ",
          "0.518",
          ", CI"["95%"],
          " [",
          "0.219",
          ", ",
          "0.766",
          "]"
        )
      ))
    )

    testthat::expect_identical(
      caption1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-3.341",
          ", ",
          italic("d")["mean"] ^ "posterior",
          " = ",
          "0.518",
          ", CI"["95%"],
          " [",
          "0.219",
          ", ",
          "0.766",
          "]"
        )
      ))
      )

    # checking message
    testthat::expect_error(
      bf_meta(
        data = dplyr::select(df1, -estimate),
        k = 3,
        messages = TRUE,
        iter = 1000,
        summarize = "integrate"
      )
    )
  }
)
