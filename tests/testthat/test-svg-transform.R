test_that("svg-transform works", {

  # an empty string creates an identity matrix
  mat1 <- parse_transform_string_to_matrix("")
  expect_identical(mat1, diag(3))


  mat2 <- parse_transform_string_to_matrix("translate(10  10)")
  expect_identical(mat2, matrix(c(
    1, 0, 10,
    0, 1, 10,
    0, 0, 1
  ), 3, 3, byrow = TRUE))

  mat3 <- update_transform_with_string(mat2, "translate(-10 0)")
  expect_identical(mat3, matrix(c(
    1, 0, 0,
    0, 1, 10,
    0, 0, 1
  ), 3, 3, byrow = TRUE))


  mat4 <- parse_transform_string_to_matrix("skewY(45)")
  expect_equal(mat4, matrix(c(
    1, 0, 0,
    1, 1, 0,
    0, 0, 1
  ), 3, 3, byrow = TRUE))


})
