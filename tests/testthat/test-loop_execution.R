
# loops - correlation ---------------------------------------------------------

testthat::test_that(
  desc = "bf_corr_test works in loop",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    col.name <- colnames(mtcars)

    set.seed(123)
    ls <-
      capture.output(for (i in 4:5) {
        print(bf_corr_test(
          data = mtcars,
          x = disp,
          y = col.name[i]
        ))
      })

    testthat::expect_true(length(ls) >= 6L)
  }
)

# loops - contingency tabs ---------------------------------------------------

testthat::test_that(
  desc = "bf_contingency_tab works in loop",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    df <- dplyr::select(mtcars, am, cyl, vs)
    col.name <- colnames(df)

    set.seed(123)
    ls <-
      capture.output(for (i in 1:3) {
        print(bf_contingency_tab(
          data = mtcars,
          x = col.name[i]
        ))
      })

    testthat::expect_true(length(ls) >= 10L)
  }
)
