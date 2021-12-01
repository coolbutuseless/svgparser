

test_that("parse_points works", {
  points_string <- "0,100 50,25 50,75 100,0"

  df <- parse_points(points_string)

  expect_equal(df$x, c(0, 50, 50, 100))
  expect_equal(df$y, c(100, 25, 75, 0))

})





test_that("parse_points error traps", {
  points_string <- "0,100 50,25 50,75 100"

  expect_error(parse_points(points_string), "even number")
})
