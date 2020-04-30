library('seplyr')

context("gathertest")

test_that("test_gather.R", {
  d <- wrapr::build_frame(
    'id', 'measurement1', 'measurement2' |
      1   , 'a'           , 10             |
      2   , 'b'           , 20             )
  res <- gather_se(d,
            key = "value_came_from_column",
            value = "value_was",
            columns = c("measurement1", "measurement2"))
  expect <- wrapr::build_frame(
    "id"  , "value_came_from_column", "value_was" |
      1   , "measurement1"          , "a"         |
      2   , "measurement1"          , "b"         |
      1   , "measurement2"          , "10"        |
      2   , "measurement2"          , "20"        )

  testthat::expect_equal(res, expect, tolerance = 0.01)

})
