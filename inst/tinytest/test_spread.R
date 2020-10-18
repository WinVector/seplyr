

d <- wrapr::build_frame(
  'id', 'name_for_new_column' , 'value_to_take' |
    1   , 'col1'                , 'a'             |
    1   , 'col2'                , '10'            |
    2   , 'col1'                , 'b'             |
    2   , 'col2'                , '20'            )
res <- spread_se(d,
                 key = 'name_for_new_column',
                 value = 'value_to_take')
expect <- wrapr::build_frame(
  "id"  , "col1", "col2" |
    1   , "a"   , "10"   |
    2   , "b"   , "20"   )

expect_equal(res, expect, tolerance = 0.01)
