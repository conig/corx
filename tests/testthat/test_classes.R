# check classes

test_that("check multiple classes work", {

  dat = iris[,1:4]
  class(dat$Sepal.Length) <- c("labelled", "numeric")

  testthat::expect_null(check_classes(dat, c("numeric","integer"), "All classes must be numeric."))
})

testthat::test_that("check factors cause error", {

  testthat::expect_error(check_classes(iris, c("numeric","integer"), "All classes must be numeric."))

})
