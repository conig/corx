test_that("upper and lower triangles retain every distinct correlation", {
  d <- mtcars[1:3]
  full <- corx(d)
  for (side in c("upper", "lower")) {
    fit <- corx(d, triangle = side, describe = TRUE)
    pairs <- which(if (side == "upper") upper.tri(full$r) else lower.tri(full$r),
                   arr.ind = TRUE)
    for (k in seq_len(nrow(pairs))) {
      i <- pairs[k, 1]
      j <- pairs[k, 2]
      expect_equal(unname(fit$apa[i, as.character(j)]), unname(full$apa[i, j]))
    }
    tab <- to_table(fit, include_p = TRUE)
    expect_equal(nrow(tab), 2 * nrow(fit$r))
    expect_true(all(is.na(tab$M[seq(2, nrow(tab), 2)])))
    for (k in seq_len(nrow(pairs))) {
      i <- pairs[k, 1]
      j <- pairs[k, 2]
      expect_equal(tab[[as.character(j)]][2 * i],
                   round_p(full$p[i, j], leading.zero = FALSE, simplify = 1))
    }
  }
})

test_that("two-variable tables keep dimensions and use the evaluated method", {
  for (method in c("pearson", "spearman", "kendall")) {
    fit <- corx(mtcars[1:2], triangle = "lower", method = method)
    expect_equal(dim(fit$apa), c(2L, 1L))
    tab <- to_table(fit, include_p = TRUE)
    expect_equal(dim(tab), c(4L, 3L))
    expect_equal(tab[[2]][1], switch(method, pearson = "Pearson's r",
                                      spearman = "Spearman's rho", kendall = "Kendall's tau"))
    expect_equal(tab[[3]][4], "< .001")
  }
  expect_error(corx(mtcars, mpg, wt, triangle = "upper"))
})

test_that("one-column p-value output preserves missing cells and thresholds", {
  d <- data.frame(a = c(1:4, rep(NA, 4)), b = c(rep(NA, 4), 4:1), c = 1:8)
  fit <- corx(d, c(b, c), a)
  expect_identical(to_table(fit), fit$apa)
  tab <- to_table(fit, TRUE)
  expect_true(is.na(tab[[3]][2]))
  expect_equal(tab[[3]][4], "< .001")
})

test_that("the papaja method accepts the generic's named x argument", {
  skip_if_not_installed("papaja")
  fit <- corx(mtcars[1:3])
  expect_equal(papaja::apa_table(x = fit), papaja::apa_table(fit))
})

test_that("summary columns never overwrite correlations with the same label", {
  d <- mtcars[1:3]
  names(d)[1] <- "M"
  fit <- corx(d, describe = TRUE)
  expect_identical(fit$apa[, 1:3], corx(d)$apa)
  expect_equal(as.numeric(fit$apa[, 4]), unname(round(colMeans(d), 2)))
})

test_that("extraction, printing and plotting use the stored fit", {
  side <- "lower"
  fit <- corx(mtcars[1:3], triangle = side, caption = paste("Stored", "caption"))
  expect_equal(coef(fit), cor(mtcars[1:3]))
  expect_identical(as.matrix(as.data.frame(fit)), fit$apa)
  expect_identical(summary(fit), as.data.frame(fit))
  expect_output(print(fit), "Stored caption")
  expect_equal(plot(fit)$labels$title, fit$caption)
})

test_that("MDS rejects an asymmetric matrix even when axis labels match", {
  fit <- corx(mtcars, c(a = mpg, b = wt, c = drat), c(a = disp, b = gear, c = carb))
  expect_error(plot_mds(fit), "symmetr")
})

test_that("a programmatic NULL triangle uses the full table", {
  side <- NULL
  expect_identical(to_table(corx(mtcars[1:3], triangle = side), TRUE),
                   to_table(corx(mtcars[1:3]), TRUE))
})
