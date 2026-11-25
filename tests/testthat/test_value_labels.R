testthat::test_that("value_labels works correctly", {
  iris_new <- labelled::set_value_labels(
    iris, Sepal.Length = c(mediumlength = 5.1, lowerlength = 4.6)
  )
  testthat::expect_no_error(corx(iris_new[, 1:4]))
})
