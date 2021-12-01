
library(grid)
library(vdiffr)


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





defs_xml <- xml2::read_xml('
<def>
<style type="text/css"><![CDATA[
			circle {
				stroke: #909;
				stroke-width: 10;
				fill: #f6f;
			}
			rect {
			  fill: green;
			}
		]]></style>

</def>
')



suppressMessages({
test_that("vdiffr test", {
  svg_file = 'svg/bezier2.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/defs-linear-gradient.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/defs-radial-gradient.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/ellipse.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/hacker-cat-svgrepo-com.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)

})


test_that("vdiffr test", {
  svg_file = 'svg/leaf.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/omega.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/pencils.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/polygon.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/polygon2.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/rect.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/Rlogo.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/style-inline.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/text.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/tiger.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


test_that("vdiffr test", {
  svg_file = 'svg/watermelon.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/test-transform-matrix.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/test-transform-rotate.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/test-transform-scale.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/test-transform-skewX.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/test-transform-translate.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/transform-heart.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/test-path-teeth.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/supertinyicons/adobe.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/supertinyicons/calendar.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file)
  vdiffr::expect_doppelganger(vdiff_name, g)
})

test_that("vdiffr test", {
  svg_file = 'svg/test-use-1.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file, scale = 0.5, stroke_scale = 10)
  vdiffr::expect_doppelganger(vdiff_name, g)
})


})








