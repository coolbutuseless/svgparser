
library(grid)
library(vdiffr)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example including rotaate text
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suppressMessages({
test_that("vdiffr test - text rotation", {
  svg_file = 'svg/chord-bostok-orig.svg'
  vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
  g <- read_svg(svg_file, user_css = ".ribbons {fill-opacity: 0.67 !important;} .group-tick line {stroke: #000 !important;}")
  vdiffr::expect_doppelganger(vdiff_name, g)
})

})





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rendering in 'mm'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suppressMessages({
  test_that("vdiffr test - text rotation", {
    svg_file = 'svg/chord-bostok-orig.svg'
    vdiff_name <- basename(tools::file_path_sans_ext(svg_file))
    vdiff_name <- paste0(vdiff_name, "-mm")
    g <- read_svg(
      svg_file,
      user_css = ".ribbons {fill-opacity: 0.67 !important;} .group-tick line {stroke: #000 !important;}",
      default.units = 'mm',
      scale = 0.25
    )
    vdiffr::expect_doppelganger(vdiff_name, g)
  })
})






