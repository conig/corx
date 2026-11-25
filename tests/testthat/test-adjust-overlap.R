test_that("overlapping axes adjust each unordered non-self pair once", {
  # B and C have equal values but are distinct variables; A is on both axes.
  d <- data.frame(a = mtcars$mpg, b = mtcars$drat, c = mtcars$drat)
  raw <- corx(d, c(a, b), c(b, c, a))$p
  for (method in c("bonferroni", "holm", "BH")) {
    p <- p.adjust(c(raw["a", "b"], raw["a", "c"], raw["b", "c"]), method)
    expected <- raw
    expected["a", c("b", "c")] <- p[1:2]
    expected["b", c("a", "c")] <- p[c(1, 3)]
    expect_equal(corx(d, c(a, b), c(b, c, a), p_adjust = method)$p, expected)
  }
})

test_that("a single shared control-adjusted variable is not an extra BH test", {
  raw <- corx(mtcars, c(mpg, cyl), c(cyl, drat), z = hp)$p
  expected <- raw
  nonself <- row(raw) != 2L | col(raw) != 1L  # only cyl vs cyl is excluded
  expected[nonself] <- p.adjust(raw[nonself], "BH")
  expect_equal(corx(mtcars, c(mpg, cyl), c(cyl, drat), z = hp, p_adjust = "BH")$p,
               expected)
})
