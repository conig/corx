
corx_ob = corx(mtcars)
corx_ob.sp = suppressWarnings(corx(mtcars, method = "spearman"))
corx_ob.kn = suppressWarnings(corx(mtcars, method = "kendall"))


cor_ob = cor(mtcars)
cor_ob.sp = cor(mtcars, method = "spearman")
cor_ob.kn = cor(mtcars, method = "kendall")


test_that("R matrix identical (pearson)", {
  expect_equal(corx_ob$r, cor_ob)
})

test_that("R matrix identical (spearman)", {
  expect_equal(corx_ob.sp$r, cor_ob.sp)
})


test_that("R matrix identical (kendall)", {
  expect_equal(corx_ob.kn$r, cor_ob.kn)
})
