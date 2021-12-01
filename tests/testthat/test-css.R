

library(xml2)

style_xml <- xml2::read_xml('<style type="text/css"><![CDATA[
			circle {
				stroke: #909;
				stroke-width: 10;
				fill: #f6f;
			}
			rect {
			  fill: green;
			}
		]]></style>')


style_xml2 <- xml2::read_xml('<style><![CDATA[
			circle {
				stroke: #909;
				stroke-width: 10;
				fill: #f6f;
			}
			rect {
			  fill: green;
			}
		]]></style>')



test_that("style_to_gpar", {
  style <- list(
    stroke           = 'red',
    fill             = '#0000FFFF',
    `stroke-opacity` = 1,
    `fill-opacity`   = 0.5,
    `stroke-width`   = 1
  )

  state <- new.env()
  state$stroke_scale <- 2
  gp <- style_to_gpar(style, state)

  expect_equal(gp$fill, '#0000FF80')
  expect_equal(gp$col , '#FF0000FF')
  expect_equal(gp$lwd , 2)
})






