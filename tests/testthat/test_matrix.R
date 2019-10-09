
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

  # What about asym?

  ob = corx(iris[-5], Sepal.Width, partial = "Petal.Width")
  pob = ppcor::pcor.test(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Width)

  expect_equal(ob$r[1,1], pob$estimate)
  expect_equal(ob$p[1,1], pob$p.value)

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


test_that("Assymetrical matricies are OK", {

  corx_1 = corx(mtcars, c(mpg, cyl), c(gear, disp, wt))
  corx_2 = corx(mtcars, c(mpg, cyl), c(gear, disp, wt), partial = "am")
  testthat::expect_equal(cor(mtcars$cyl, mtcars$disp), corx_1$r[2,2])
  testthat::expect_equal(rownames(corx_1$r), rownames(corx_2$r))
  testthat::expect_equal(colnames(corx_1$r), colnames(corx_2$r))

  cob_2 = ppcor::pcor.test(mtcars$disp, mtcars$cyl, mtcars$am)

  testthat::expect_equal(corx_2$r[2,2], cob_2$estimate)
  testthat::expect_equal(corx_2$p[2,2], cob_2$p.value)

})


test_that("Multiple partial variables", {

  corx_1 = corx(mtcars, c(mpg, cyl), c(gear, disp, wt), partial = c(am, drat))


  cob_2 = ppcor::pcor.test(mtcars$mpg, mtcars$disp, mtcars[,c("am","drat")])

  testthat::expect_equal(corx_1$r[1,2], cob_2$estimate)
  testthat::expect_equal(corx_1$p[1,2], cob_2$p.value)

})


test_that("Does describe work?", {

  cx = corx(mtcars, describe = T)

  cx_asem = corx(
    data = mtcars,
    x = c(mpg, cyl, disp),
    y = c(wt, drat, disp,
          qsec),
    partial = wt,
    stars = c(0.05),
    round = 2,
    describe = T
  )

  testthat::expect_equal(cx$apa[1,"M"], corx:::digits(mean(mtcars$mpg),2))
  testthat::expect_equal(cx_asem$apa[1,"M"], corx:::digits(mean(mtcars$mpg),2))

  testthat::expect_equal(cx$apa[1,"SD"], corx:::digits(sd(mtcars$mpg),2))
  testthat::expect_equal(cx_asem$apa[1,"SD"], corx:::digits(sd(mtcars$mpg),2))

})


