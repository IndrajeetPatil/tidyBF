
# loops - correlation ---------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works in loop",
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

    testthat::expect_true(length(ls) >= 1L)
  }
)
