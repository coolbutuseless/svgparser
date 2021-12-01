

test_that("bezier3_to_df works", {

  df <- bezier3_to_df(
    x = c(0, 0.25, 0.75, 1),
    y = c(0,    1,    1, 0),
    N = 5
  )

  expect_equal(df$x, c(0, 0.2265625, 0.5, 0.7734375, 1))
  expect_equal(df$y, c(0, 0.5625, 0.75, 0.5625, 0))

})




test_that("bezier2_to_df works", {

  df <- bezier2_to_df(
    x = c(0, 0.25, 1),
    y = c(0,    1, 0),
    N = 5
  )

  expect_equal(df$x, c(0, 0.15625, 0.375, 0.65625, 1))
  expect_equal(df$y, c(0, 0.375, 0.5, 0.375, 0))

})
