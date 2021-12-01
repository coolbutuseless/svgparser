test_that("debug/list/data.frame output works", {

  res <- load_supertinyicon('twitter', 'grob')
  expect_true(inherits(res, 'grob'))

  res <- load_supertinyicon('twitter', 'data.frame')
  expect_true(inherits(res, 'data.frame'))

  res <- load_supertinyicon('twitter', 'list')
  expect_true(inherits(res, 'list'))

  res <- load_supertinyicon('twitter', 'debug')
  expect_true(inherits(res, 'list'))

  res <- load_supertinyicon('twitter', 'svg')
  expect_true(inherits(res, 'character'))
})



test_that("correctly barfs when no <svg> tag in text", {

  bad_svg <- "hello"
  expect_error(
    read_svg(bad_svg),
    "does not exist"
  )


  bad_svg <- "<a>hello</a>"
  expect_error(
    read_svg(bad_svg),
    "No <svg> tag"
  )


})
