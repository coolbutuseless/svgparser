


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a data.frame for a rectangle with rounded corners
#'
#' @param x,y,width,height,rx,ry rect parameters
#' @param npoints number of points along each of the rounded corners
#'
#' @return data.frame of (x, y) coordinates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_rect_df <- function(x, y, width, height, rx, ry, npoints) {

  if (npoints < 4) npoints <- 4

  theta <- seq(0, pi/2, length.out = npoints)[seq(npoints)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If rx==ry==0 then this is just a regular, sharp-cornered rectangle,
  # Otherwise it has rounded corners.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (rx == 0 && ry == 0) {
    points_df <- data_frame(
      x = c(x, x + width, x + width , x         ),
      y = c(y, y        , y + height, y + height)
    )
  } else {
    do.call(
      rbind,
      list(
        data_frame(
          x = c(x + rx, x + width - rx),
          y = c(y     , y)
        ),
        data_frame(
          x = (x + width - rx) + rx * cos(theta - pi/2),
          y = (y + ry)         + ry * sin(theta - pi/2)
        ),
        data_frame(
          x = c(x + width, x + width),
          y = c(y + ry   , y + height - ry)
        ),
        data_frame(
          x = (x + width  - rx) + rx * cos(theta),
          y = (y + height - ry) + ry * sin(theta)
        ),
        data_frame(
          x = c(x + width - rx, x + rx),
          y = c(y + height, y + height)
        ),
        data_frame(
          x = (x +          rx) + rx * cos(theta + pi/2),
          y = (y + height - ry) + ry * sin(theta + pi/2)
        ),
        data_frame(
          x = c(x, x),
          y = c(y + height - ry, y + ry)
        ),
        data_frame(
          x = (x + rx) + rx * cos(theta + pi),
          y = (y + ry) + ry * sin(theta + pi)
        )
      )
    )
  }
}
