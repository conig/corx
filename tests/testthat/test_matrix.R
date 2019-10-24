
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

test_that("Rename working", {
  temp_dat = mtcars
  names(temp_dat)[1] = "mpee gee"
  temp_corx = corx(temp_dat, c(mpg = "mpee gee"))
  expect_equal(temp_corx$apa, corx(mtcars, mpg)$apa)
})

test_that("Are diagonals OK?", {
  temp_dat = as.character(corx(mtcars, mpg, cyl)$apa)
  expect_equal(temp_dat, "-.85*")
})

test_that("partial cor OK?", {

  ob = corx(iris[-5], z = "Sepal.Width")
  r1 = ob$r[2,1]
  p1 = ob$p[2,1]

  pob = ppcor::pcor.test(iris$Sepal.Length, iris$Petal.Length, iris$Sepal.Width)
  r2  = pob$estimate
  p2 = pob$p.value

  expect_equal(r1, r2)
  expect_equal(p1,p2)

  # What about asym?

  ob = corx(iris[-5], Sepal.Width, z = "Petal.Width")
  pob = ppcor::pcor.test(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Width)

  expect_equal(ob$r[1,1], pob$estimate)
  expect_equal(ob$p[1,1], pob$p.value)

})


test_that("partial cor OK? (spearman)", {

  ob = corx(iris[-5], z = "Sepal.Width", method = "spearman")
  r1 = ob$r[2,1]
  p1 = ob$p[2,1]

  pob = ppcor::pcor.test(iris$Sepal.Length, iris$Petal.Length, iris$Sepal.Width, method = "spearman")
  r2  = pob$estimate
  p2 = pob$p.value

  expect_equal(r1, r2)
  expect_equal(p1,p2)
})

test_that("partial cor OK? (kendall)", {

  ob = corx(mtcars, z = "cyl", method = "kendall")
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
  x2 = corx(dat, z = "carb")

  testthat::expect_equal(x$n[1,3], 31)
  testthat::expect_equal(x$n[1,1], 32)
  testthat::expect_equal(x2$n[1,3], 31)
  testthat::expect_equal(x2$n[1,1], 32)

})


test_that("Assymetrical matricies are OK", {

  corx_1 = corx(mtcars, c(mpg, cyl), c(gear, disp, wt))
  corx_2 = corx(mtcars, c(mpg, cyl), c(gear, disp, wt), z = "am")
  testthat::expect_equal(cor(mtcars$cyl, mtcars$disp), corx_1$r[2,2])
  testthat::expect_equal(rownames(corx_1$r), rownames(corx_2$r))
  testthat::expect_equal(colnames(corx_1$r), colnames(corx_2$r))

  cob_2 = ppcor::pcor.test(mtcars$disp, mtcars$cyl, mtcars$am)

  testthat::expect_equal(corx_2$r[2,2], cob_2$estimate)
  testthat::expect_equal(corx_2$p[2,2], cob_2$p.value)

})


test_that("Multiple partial variables", {

  corx_1 = corx(mtcars, c(mpg, cyl), c(gear, disp, wt), z = c(am, drat))


  cob_2 = ppcor::pcor.test(mtcars$mpg, mtcars$disp, mtcars[,c("am","drat")])
  cob_3 = ppcor::pcor.test(mtcars$cyl, mtcars$wt, mtcars[,c("am","drat")])

  testthat::expect_equal(corx_1$r[1,2], cob_2$estimate)
  testthat::expect_equal(corx_1$p[1,2], cob_2$p.value)
  testthat::expect_equal(corx_1$r[2,3], cob_3$estimate)
  testthat::expect_equal(corx_1$p[2,3], cob_3$p.value)

})

test_that("Does describe work?", {

  cx = corx(mtcars, describe = T)

  cx_asem = corx(
    data = mtcars,
    x = c(mpg, cyl, disp),
    y = c(wt, drat, disp,
          qsec),
    z = wt,
    stars = c(0.05),
    round = 2,
    describe = T
  )

  cx_asem2 = corx(
    data = mtcars,
    y = c(wt, drat, disp,
          qsec),
    z = wt,
    stars = c(0.05),
    round = 2,
    describe = T
  )

  all_means = cx_asem2$apa[,"M"]
  all_sds = cx_asem2$apa[,"SD"]
  lapply_means = unlist(lapply(colnames(mtcars), function(x) corx:::digits(mean(mtcars[,x]),2)))
  lapply_sds = unlist(lapply(colnames(mtcars), function(x) corx:::digits(sd(mtcars[,x]),2)))
  names(lapply_means) = names(mtcars)
  names(lapply_sds) = names(mtcars)

  testthat::expect_equal(cx$apa[1,"M"], corx:::digits(mean(mtcars$mpg),2))
  testthat::expect_equal(cx_asem$apa[1,"M"], corx:::digits(mean(mtcars$mpg),2))
  testthat::expect_equal(cx$apa[1,"SD"], corx:::digits(sd(mtcars$mpg),2))
  testthat::expect_equal(cx_asem$apa[1,"SD"], corx:::digits(sd(mtcars$mpg),2))
  testthat::expect_equal(cx_asem$apa[3,"SD"], corx:::digits(sd(mtcars$disp),2))
  testthat::expect_equal(all_means, lapply_means[names(lapply_means) != "wt"])
  testthat::expect_equal(all_sds, lapply_sds[names(lapply_sds) != "wt"])
})



test_that("quick kurtosis working", {
  cr = corx(mtcars, describe = kurtosis)

  x = unname(unlist(cr$apa[,"kurtosis"]))
  y = digits(unlist(lapply(names(mtcars), function(x) moments::kurtosis(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
})

test_that("quick mean working", {
  cr = corx(mtcars, describe = mean)

  x = unname(unlist(cr$apa[,"mean"]))
  y = digits(unlist(lapply(names(mtcars), function(x) mean(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
})

test_that("quick sd working", {
  cr = corx(mtcars, describe = sd)

  x = unname(unlist(cr$apa[,"sd"]))
  y = digits(unlist(lapply(names(mtcars), function(x) stats::sd(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
})

test_that("quick var working", {
  cr = corx(mtcars, describe = var)

  x = unname(unlist(cr$apa[,"var"]))
  y = digits(unlist(lapply(names(mtcars), function(x) stats::var(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
})

test_that("quick median working", {
  cr = corx(mtcars, describe = median)

  x = unname(unlist(cr$apa[,"median"]))
  y = digits(unlist(lapply(names(mtcars), function(x) stats::median(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
})

test_that("quick iqr working", {
  cr = corx(mtcars, describe = iqr)

  x = unname(unlist(cr$apa[,"iqr"]))
  y = digits(unlist(lapply(names(mtcars), function(x) stats::IQR(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
})

test_that("quick skew working", {
  cr = corx(mtcars, describe = skewness)

  x = unname(unlist(cr$apa[,"skewness"]))
  y = digits(unlist(lapply(names(mtcars), function(x) moments::skewness(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
})

test_that("quick kurtosis working", {
  cr = corx(mtcars, describe = kurtosis)

  x = unname(unlist(cr$apa[,"kurtosis"]))
  y = digits(unlist(lapply(names(mtcars), function(x) moments::kurtosis(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
})

test_that("quick kurtosis working", {
  cr = corx(mtcars, describe = kurtosis)

  x = unname(unlist(cr$apa[,"kurtosis"]))
  y = digits(unlist(lapply(names(mtcars), function(x) moments::kurtosis(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
})



test_that("multi-describe working", {
  cr = corx(mtcars, describe = c(kurtosis, mean))

  x = unname(unlist(cr$apa[,"kurtosis"]))
  x2 = unname(unlist(cr$apa[,"mean"]))
  y = digits(unlist(lapply(names(mtcars), function(x) moments::kurtosis(mtcars[,x], na.rm =T)),2))
  y2 = digits(unlist(lapply(names(mtcars), function(x) mean(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(x, y)
  testthat::expect_equal(x2, y2)
})

test_that("error when incorrect name supplied to describe", {
testthat::expect_error(corx(mtcars, describe = c(mean, sd, krt)))

})


test_that("suppling list of functions to describe still works", {

 cr =  corx(mtcars, describe = list(skew = function(x) moments::skewness(x, na.rm = T)))
 y = digits(unlist(lapply(names(mtcars), function(x) moments::skewness(mtcars[,x], na.rm =T)),2))
 testthat::expect_equal(unname(cr$apa[,"skew"]), y)

})


test_that("assymetical describe working OK", {
  cr = corx(mtcars, c(mpg,cyl) , c(drat, gear, am), describe = c(k = kurtosis))
  y = digits(unlist(lapply(c("mpg","cyl"), function(x) moments::kurtosis(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(unname(cr$apa[,"k"]), y)
})

test_that("assymetical describe inverted", {
  cr = corx(mtcars,c(drat, gear, am), c(mpg,cyl) , describe = c(k = kurtosis))
  y = digits(unlist(lapply(c("drat","gear", "am"), function(x) moments::kurtosis(mtcars[,x], na.rm =T)),2))

  testthat::expect_equal(unname(cr$apa[,"k"]), y)
})

test_that("describe working on iris",{

 cr = corx(iris[,-5], describe = c(test = median))
 y = digits(unlist(lapply(names(iris)[-5], function(x) stats::median(iris[,x], na.rm =T)),2))
 testthat::expect_equal(unname(cr$apa[,"test"]), y)
})


test_that("triangle lower works",{

  cr = corx(mtcars,  grey_nonsig = F, describe = c(test = median),
            triangle = "lower", round = 8, remove_lead = F, stars = 0,
            method = "kendall")$apa[10,6]

  y = suppressWarnings(unname(digits(cor.test(mtcars$gear, mtcars$wt, method = "kendall")$estimate, 8)))
  testthat::expect_equal(cr, y)
})

test_that("triangle upper works",{

  cr = corx(mtcars,  grey_nonsig = F, describe = c(test = median),
            triangle = "upper", round = 8, remove_lead = F, stars = 0,
            method = "spearman")$apa[3,7]

  y = suppressWarnings(unname(digits(cor.test(mtcars$disp, mtcars$qsec, method = "spearman")$estimate, 8)))
  testthat::expect_equal(cr, y)
})

# test_that("clipboard no error",{
#
#   cr = as.matrix(corx(mtcars)$apa)
#   diag(cr) = "-"
#   to_clipboard(cr, allow_non_interactive = TRUE)
#   from_clip = clipr::read_clip_tbl()
#
#   testthat::expect_equal(cr, as.matrix(from_clip))
# })
#
# test_that("clipboard matrix ok",{
#   cr = as.matrix(mtcars)
#   to_clipboard(cr, allow_non_interactive = TRUE)
#   from_clip = clipr::read_clip_tbl()
#   testthat::expect_equal(cr, as.matrix(from_clip))
# })




