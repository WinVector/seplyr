library('seplyr')

context("stringalg")

test_that("test_stringalg.R", {
  resCol1 <- "Sepal_Long"
  ratio <- 2
  compCol1 <- "Sepal.Width"
  target <- "'setosa'"
  pcol <- as.name("Petal.Width")

  # this one should stay "pcol"
  e1 <- expand_expr("pcol")
  testthat::expect_equal("pcol", e1)

  # this one should be "Petal.Width"
  e2 <- expand_expr(pcol)
  testthat::expect_equal("Petal.Width", e2)

  # this one should stay "compCol1"
  e3 <- expand_expr("compCol1")
  testthat::expect_equal("compCol1", e3)

  # this one should be "Sepal.Width"
  e4 <- expand_expr(compCol1)
  testthat::expect_equal("Sepal.Width", e4)

  # this one should stay xx
  e5 <- expand_expr(xx)
  testthat::expect_equal(e5, "xx")

  expr <- expand_expr("Sepal.Length" + pcol >= ( ratio * compCol1 +
                                                            ifelse(Species==target, 1, 0) ))
  # print(expr)
  got <- trimws(gsub("\\s+", " ", expr))

  testthat::expect_equal(got,
                         "Sepal.Length + Petal.Width >= (ratio * Sepal.Width + ifelse(Species == \"setosa\", 1, 0))")
  # should be valid mutate expr
  tmp <- datasets::iris %.>%
    mutate_se(., 'res' := expr) %.>%
    head(.)
})
