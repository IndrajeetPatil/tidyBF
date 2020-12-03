
# loops - correlation ---------------------------------------------------------

testthat::test_that(
  desc = "bf_corr_test works in loop",
  code = {
    testthat::skip_on_cran()

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
    testthat::skip_on_cran()

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

# loops - bf_ttest ---------------------------------------------------

testthat::test_that(
  desc = "bf_ttest works in loop",
  code = {
    testthat::skip_on_cran()

    # working with loops
    df <- dplyr::select(mtcars, am, wt, mpg)
    col.name <- colnames(df)

    set.seed(123)
    ls1 <-
      capture.output(for (i in 2:length(col.name)) {
        print(bf_ttest(
          data = mtcars,
          x = am,
          y = !!col.name[i]
        ))
      })

    testthat::expect_true(length(ls1) >= 8L)

    set.seed(123)
    ls2 <-
      capture.output(for (i in 2:length(col.name)) {
        print(bf_ttest(
          data = mtcars,
          x = col.name[i],
          test.value = 3
        ))
      })

    testthat::expect_true(length(ls2) >= 8L)
  }
)

# loops - bf_anova ---------------------------------------------------

testthat::test_that(
  desc = "bf_anova works in loop",
  code = {
    testthat::skip_on_cran()

    # working with loops
    df <- dplyr::select(mtcars, cyl, wt, mpg)
    col.name <- colnames(df)

    set.seed(123)
    ls <-
      capture.output(for (i in 2:length(col.name)) {
        print(bf_oneway_anova(
          data = mtcars,
          x = cyl,
          y = !!col.name[i]
        ))
      })

    testthat::expect_true(length(ls) >= 22L)
  }
)
