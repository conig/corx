test_that("describe name vector works", {
  cx <- corx::corx(mtcars, describe = c(iqr, median))
  res <- all(c("iqr", "median") %in% colnames(cx$apa))
  testthat::expect_true(res)
})

test_that("describe name vector with rename works", {
  cx <- corx::corx(mtcars, describe = c("TEST" = iqr, "MEDIAN" = median))
  res <- all(
    c("TEST", "MEDIAN") %in% colnames(cx$apa)
  )
  testthat::expect_true(res)
})

test_that("describe bool works", {
  cx <- corx::corx(mtcars, describe = TRUE)
  res <- all(
    c("M", "SD") %in% colnames(cx$apa)
  )
  testthat::expect_true(res)
})

test_that("describe quote works", {
  cx <- corx::corx(mtcars, describe = c("TEST" = "iqr", "sd"))
  res <- all(
    c("TEST", "sd") %in% colnames(cx$apa)
  )
  testthat::expect_true(res)
})
