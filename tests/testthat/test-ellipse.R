
test_that("ellipse_to_df works", {

  # A circle
  df <- ellipse_to_df(cx = 0, cy = 0, rx = 5, ry = 5, npoints = 2)
  expect_equal(df$x, c(5, 0, -5, 0))
  expect_equal(df$y, c(0, 5, 0, -5))


  df <- ellipse_to_df(cx = 4, cy = 1, rx = 5, ry = 5, npoints = 4)
  expect_equal(df$x, c(9, 4, -1, 4))
  expect_equal(df$y, c(1, 6, 1, -4))

})


