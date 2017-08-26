library('seplyr')

context("stringalg")

test_that("test_stringalg.R", {

  resCol1 <- "Sepal_Long"
  ratio <- 2
  compCol1 <- "Sepal.Width"
  target <- "'setosa'"
  pcol <- as.name("Petal.Width")
  expr <- expand_expr("Sepal.Length" + pcol + "pcol" >= ( ratio * compCol1 +
                                                            ifelse(Species==target, 1, 0) ))
  # print(expr)
  got <- trimws(gsub("\\s+", " ", expr))

  testthat::expect_equal(got,
                         "Sepal.Length + Petal.Width + Petal.Width >= (ratio * Sepal.Width + ifelse(Species == \"setosa\", 1, 0))")
  # should be valid mutate expr
  tmp <- datasets::iris %.>%
    mutate_se(., 'res' := expr) %.>%
    head(.)

})
