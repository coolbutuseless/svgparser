

test_that("supertinyincos work", {

  svg_obj <- load_supertinyicon('twitter', obj_type = 'svg')
  expect_true(is.character(svg_obj))

  grob <- load_supertinyicon('twitter')
  expect_true(inherits(grob, 'grob'))

  # nearest matching name is loaded if perfect match not found
  expect_message(
    grob <- load_supertinyicon('twotter'),
    "not found"
  )
  expect_true(inherits(grob, 'grob'))


})
