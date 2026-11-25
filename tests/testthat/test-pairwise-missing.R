test_that("disjoint groups leave only unobservable pairs missing (#13)", {
  # Maths and PE were measured in separate groups; bridge spans both.
  d <- data.frame(maths = c(1:4, rep(NA, 4)), pe = c(rep(NA, 4), 4:1),
                  bridge = c(3, 2, 4, 1, 5, 3, 1, 4))
  fit <- corx(d, p_adjust = "holm")
  expect_equal(fit$r, cor(d, use = "pairwise.complete.obs"))
  expect_equal(fit$n, crossprod(!is.na(d)))
  p <- vapply(d[1:2], function(v) cor.test(v, d$bridge)$p.value, numeric(1))
  expect_equal(fit$p[1:2, "bridge"], p.adjust(p, "holm"))
  expect_true(is.na(fit$p["maths", "pe"]))
})

test_that("small overlaps respect method and control-variable requirements", {
  d <- data.frame(x = 1:6, y = c(2, 1, rep(NA, 4)), z = c(NA, 1:5))
  for (method in c("pearson", "kendall", "spearman")) {
    fit <- corx(d, x, y, method = method)
    expect_equal(as.numeric(fit$r), -1)
    expected <- if (method != "kendall") NA_real_ else
      cor.test(d$x, d$y, method = method, exact = FALSE)$p.value
    expect_equal(as.numeric(fit$p), expected)
  }
  partial <- corx(d, x, y, z)
  expect_equal(as.numeric(partial$n), 1)
  expect_true(is.na(partial$r) && is.na(partial$p))
})

test_that("Pearson tails use the complete pairs for each comparison", {
  d <- data.frame(x = 1:100, strong = -(1:100) + sin(1:100),
                  weak = cos(1:100), perfect = 1:100)
  d$strong[1:20] <- NA
  fit <- corx(d, x, c(strong, weak, perfect))
  ref <- vapply(d[-1], function(y) cor.test(d$x, y)$p.value, numeric(1))
  # Compare on the log scale so tiny nonzero p-values cannot pass as zero.
  expect_equal(log(fit$p[1, ]), log(ref))
})
