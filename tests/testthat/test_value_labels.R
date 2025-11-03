testthat::test_that("value_labels works correctly", {
  iris_new <- iris |>
    labelled::set_value_labels(
      Sepal.Length = c(mediumlength = 5.1, lowerlength = 4.6)
    )
  testthat::expect_no_error(
    iris_new[, 1:4] |>
      corx()
  )
})
