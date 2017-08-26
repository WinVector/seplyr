library('seplyr')

context("basictest")

test_that("test_basic.R", {
  suppressPackageStartupMessages(library("dplyr"))

  groupingCols <- c("cyl", "gear")
  r1 <- datasets::mtcars %.>%
    group_summarize(.,
                    groupingCols,
                    group_mean_mpg = mean(mpg),
                    group_mean_disp = mean(disp)) %.>%
    arrange_se(., groupingCols)

  r2 <- datasets::mtcars %>%
    group_by(cyl, gear) %>%
    summarize(group_mean_mpg = mean(mpg),
              group_mean_disp = mean(disp)) %>%
    ungroup() %>%
    arrange(cyl, gear)

  testthat::expect_equivalent(r1, r2)
})
