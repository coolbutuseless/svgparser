

test_that("parse_svg_linearGradient works", {

  elem_text <- '<linearGradient id="myGradient" gradientTransform="rotate(90)">
      <stop offset="20%" stop-color="gold" />
      <stop offset="90%" stop-color="red" />
    </linearGradient>'

  elem <- xml2::read_xml(elem_text)

  lg <- parse_svg_linearGradient(elem, state = list())

  # grid in R <4.1.0 doesn't support gradients
  skip_if(getRversion() < '4.1.0')

  expect_true(inherits(lg, "GridLinearGradient"))

  expect_equal(lg$x1, grid::unit(0, 'npc'))
  expect_equal(lg$x2, grid::unit(1, 'npc'))
  expect_equal(lg$y1, grid::unit(1, 'npc'))
  expect_equal(lg$y2, grid::unit(1, 'npc'))

  expect_equal(lg$stops, c(0.2, 0.9))
  expect_equal(lg$colours, toupper(c('#ffd700ff', '#ff0000ff')))
})




test_that("parse_svg_radialGradient works", {

  elem_text <- '<radialGradient id="myGradient">
      <stop offset="10%" stop-color="gold" />
      <stop offset="95%" stop-color="red" />
    </radialGradient>'

  elem <- xml2::read_xml(elem_text)

  rg <- parse_svg_radialGradient(elem, state = list())

  # grid in R <4.1.0 doesn't support gradients
  skip_if(getRversion() < '4.1.0')

  expect_true(inherits(rg, "GridRadialGradient"))

  expect_equal(rg$cx1, grid::unit(0.5, 'npc'))
  expect_equal(rg$cy1, grid::unit(0.5, 'npc'))
  expect_equal(rg$cx2, grid::unit(0.5, 'npc'))
  expect_equal(rg$cy2, grid::unit(0.5, 'npc'))

  expect_equal(rg$stops, c(0.1, 0.95))
  expect_equal(rg$colours, toupper(c('#ffd700ff', '#ff0000ff')))
})
