test_that("selectors see the columns; omitted axes still use their defaults", {
  d <- iris
  cols <- names(d)[1:4]
  expected <- cor(d[cols])
  expect_equal(corx(d, tidyselect::where(is.numeric))$r, expected)
  expect_equal(corx(as.matrix(d[cols]), tidyselect::all_of(cols))$r, expected)
  expect_error(corx(d, tidyselect::starts_with("absent")), "at least one")
  expect_error(corx(d, Sepal.Width, tidyselect::any_of("absent")), "at least one")
})

test_that("aliases label axes without changing correlations or the test family", {
  # Two labels for mpg and a wt label that collides with a source name.
  fit <- corx(mtcars, c(mpg = mpg, Miles = mpg, Weight = wt),
              c(mpg = wt, Mileage = mpg, drat), describe = TRUE, p_adjust = "holm")
  ref <- corx(mtcars, c(mpg, wt), c(wt, mpg, drat), p_adjust = "holm")
  for (part in c("r", "p", "n")) {
    expected <- ref[[part]][c(1, 1, 2), ]
    dimnames(expected) <- list(c("mpg", "Miles", "Weight"), c("mpg", "Mileage", "drat"))
    expect_equal(fit[[part]], expected)
  }
  expect_equal(fit$apa["Miles", "Mileage"], " - ")
  expect_equal(fit$apa["mpg", "mpg"], ref$apa["mpg", "wt"])
  expect_equal(as.numeric(fit$apa["Weight", "M"]), round(mean(mtcars$wt), 2))
})

test_that("control variables are excluded by source, not by label", {
  fit <- corx(mtcars, c(mpg, Control = wt, cyl), z = c(Weight = wt))
  expect_equal(fit$r, corx(mtcars, c("mpg", "cyl"), z = "wt")$r)
})
