

normalize_colour <- function(x) {
  tolower(rgb(t(col2rgb(x)), maxColorValue = 255))
}

test_that("svg_colour_to_r_colour() parsing works", {

  state <- new.env()
  # state$defs[['url(#yes)']] <- 'green'

  expect_equal(
    svg_colour_to_r_colour('pink', state),
    "#FFC0CBFF"
  )

  expect_equal(
    svg_colour_to_r_colour('rgb(1, 2, 3)', state),
    '#010203FF'
  )

  expect_equal(
    svg_colour_to_r_colour('#102030', state),
    '#102030FF'
  )


  expect_equal(
    svg_colour_to_r_colour('#123', state),
    '#112233FF'
  )

  expect_equal(
    svg_colour_to_r_colour('none', state),
    '#00000000'
  )

  expect_message(
    expect_equal(
      svg_colour_to_r_colour('currentColor', state),
      "#FF69B4FF" #'hotpink'
    )
  )

  expect_equal(
    svg_colour_to_r_colour('hsl(0, 50%, 50%)', state),
    '#BF4040FF'
  )

  expect_message(
    expect_equal(
      svg_colour_to_r_colour('url(#ref)', state),
      "#FF69B4FF" # 'hotpink'
    ),
    "not found"
  )

  expect_message(
    expect_equal(
      svg_colour_to_r_colour('url(#yes)', state),
      "#FF69B4FF" # 'hotpink'
    ),
    "not found"
  )


})
