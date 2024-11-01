test_that("total_scores with .append and .drop works", {
  
  data_df <- tibble::tibble(
    x_1 = seq(10),
    x_2 = seq(10),
    x_3 = seq(10),
    y = seq(10)
  )
  
  data_df2 <- tibble::tibble(
    y = seq(10),
    x = seq(10)
  )
  
  # the mean of x_1:x_3 is identical to seq(10)
  # hence the result of the `total_score` should be `data_df2`
  expect_equal(total_scores(data_df, x = starts_with('x'), .append = T, .drop = T),
               data_df2)
  
  # this data frame has just one row
  data_df <- tibble::tibble(
    x_1 = 1,
    x_2 = 2,
    x_3 = 3,
    y = 4
  )
  
  # the sum of x_1:x_3 is 6 
  data_df2 <- tibble::tibble(
    y = 4,
    x = 6)
  
  expect_equal(total_scores(data_df, x = starts_with('x'), .method = 'sum', .append = T, .drop = T),
               data_df2)
  
  
})
