

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert Ellipse parameters into a data.frame of coords
#'
#' \code{cx, cy, rx, ry} are attribute parameters of the \code{<ellipse>} tag.
#' \code{npoints} is defined by the ser.
#'
#' @param cx,cy,rx,ry ellipse parameters
#' @param npoints number of points around ellipse
#'
#' @return data.frame of x,y coordinates around the ellipse
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ellipse_to_df <- function(cx, cy, rx, ry, npoints) {

  if (npoints < 4) {
    npoints <- 4
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaluate ellipse at npoints around [0,360]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  theta <- seq(0, 2 * pi, length.out = npoints + 1L)[seq_len(npoints)]

  data_frame(
    x = cx + rx * cos(theta),
    y = cy + ry * sin(theta)
  )
}
