
corx_ob = corx(mtcars)
corx_ob.sp = corx(mtcars, method = "spearman")
corx_ob.kn = corx(mtcars, method = "kendall")

corx_mpg_cyl.p = corx_ob$p[1,2]



cor_ob = cor(mtcars)
cor_ob.sp = cor(mtcars, method = "spearman")
cor_ob.kn = cor(mtcars, method = "kendall")

cortest_mpg_cyl.p = cor.test(mtcars$mpg, mtcars$cyl)$p.value


test_that("R matrix identical (pearson)", {
  expect_equal(corx_ob$r, cor_ob)
})

test_that("R matrix identical (spearman)", {
  expect_equal(corx_ob.sp$r, cor_ob.sp)
})


test_that("R matrix identical (kendall)", {
  expect_equal(corx_ob.kn$r, cor_ob.kn)
})

test_that("p-values identical (pearson", {
  expect_equal(corx_mpg_cyl.p, cortest_mpg_cyl.p)
})

test_that("p-values identical (spearman", {
  corx_p = corx_ob.sp$p[1,2]
  obs_p = suppressWarnings(cor.test(mtcars$mpg, mtcars$cyl, method = "spearman")$p.value)
  expect_equal(corx_p, obs_p)
})

test_that("Weird names are working", {
  temp_dat = mtcars
  names(temp_dat)[1] = "mpee gee"
  temp_corx = corx(temp_dat)
  expect_equal(colnames(temp_corx$apa)[1], "mpee gee")
})

test_that("Are diagonals OK?", {
  temp_dat = as.character(corx(mtcars, mpg, cyl)$apa)
  expect_equal(temp_dat, "-.85*")
})

test_that("partial cor OK?", {

  ob = corx(iris[-5], partial = "Sepal.Width")
  r1 = ob$r[2,1]
  p1 = ob$p[2,1]

  pob = ppcor::pcor.test(iris$Sepal.Length, iris$Petal.Length, iris$Sepal.Width)
  r2  = pob$estimate
  p2 = pob$p.value

  expect_equal(r1, r2)
  expect_equal(p1,p2)
})

test_that("partial cor OK? (spearman)", {

  ob = corx(iris[-5], partial = "Sepal.Width", method = "spearman")
  r1 = ob$r[2,1]
  p1 = ob$p[2,1]

  pob = ppcor::pcor.test(iris$Sepal.Length, iris$Petal.Length, iris$Sepal.Width, method = "spearman")
  r2  = pob$estimate
  p2 = pob$p.value

  expect_equal(r1, r2)
  expect_equal(p1,p2)
})

test_that("partial cor OK? (kendall)", {

  ob = corx(mtcars, partial = "cyl", method = "kendall")
  r1 = ob$r[2,1]
  p1 = ob$p[2,1]

  pob = ppcor::pcor.test(mtcars$disp, mtcars$mpg, mtcars$cyl, method = "kendall")
  r2  = pob$estimate
  p2 = pob$p.value

  expect_equal(r1, r2)
  expect_equal(p1,p2)
})

test_that("plots ok", {

  x = plot(corx(mtcars), title = "", method = "circl")
  testthat::expect_equal(is.null(x), FALSE)
})

test_that("missing values ok", {
  dat = mtcars

  dat[1,3] = NA

  x = corx(dat)
  x2 = corx(dat, partial = "carb")

  testthat::expect_equal(x$n[1,3], 31)
  testthat::expect_equal(x$n[1,1], 32)
  testthat::expect_equal(x2$n[1,3], 31)
  testthat::expect_equal(x2$n[1,1], 32)

})


