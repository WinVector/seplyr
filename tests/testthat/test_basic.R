library('seplyr')

context("basictest")

test_that("test_basic.R", {
  suppressPackageStartupMessages(library("dplyr"))
  if(!requireNamespace('datasets', quietly = TRUE)) {
    return(invisible(NULL))
  }

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


  r1 <- datasets::mtcars %.>%
    group_mutate(.,
                 c("cyl", "gear"),
                 rank = row_number(),
                 arrangeTerms = "-disp") %.>%
    arrange_se(.,
               wrapr::qc(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, rank))

  r2 <- datasets::mtcars %>%
    arrange(-disp) %>%
    group_by(cyl, gear) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    arrange(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, rank)

  testthat::expect_equivalent(r1, r2)

  invisible(NULL)
})
