

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert \code{<path>} arc to a data.frame of coordinates
#'
#' Reference \url{https://svgwg.org/svg2-draft/implnote.html#ArcConversionEndpointToCenter}
#'
#' ToDo: actual rotation of arc coords by 'phi'
#'
#' @param params named list of arc parameters from the path segment i.e. xo, y0,
#'        x, y, arcflag, sweepflag, rx, ry, rot (degrees)
#' @param npoints number of points along arc
#'
#' @return data.frame of coodinates of arc
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
arc_to_df <- function(params, npoints) {

  if (npoints < 4) {
    npoints <- 4
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract convert values from the path 'd' parameters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x1  <- params$x0
  y1  <- params$y0
  x2  <- params$x
  y2  <- params$y
  fa  <- params$arcflag
  fs  <- params$sweepflag
  rx  <- params$rx
  ry  <- params$ry
  phi <- params$rot * pi/180

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Section B.2.5 "Correction of out-of-range radii
  # https://svgwg.org/svg2-draft/implnote.html#ArcCorrectionOutOfRangeRadii
  #
  # Step 1: Ensure radii are non-zero
  #   - if either is zero, then assume a straight line between start/end
  # Step 2: Ensure radii are positive
  #   - just call abs()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (rx == 0 || ry == 0) {
    return(
      data_frame(
        x = c(x1, x2),
        y = c(y1, y2)
      )
    )
  }

  rx <- abs(rx)
  ry <- abs(ry)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Eqn 5.1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x1p <-  cos(phi) * ((x1 - x2)/2) + sin(phi) * ((y1 - y2)/2)
  y1p <- -sin(phi) * ((x1 - x2)/2) + cos(phi) * ((y1 - y2)/2)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Section B.2.5 "Correction of out-of-range radii
  # https://svgwg.org/svg2-draft/implnote.html#ArcCorrectionOutOfRangeRadii
  #
  # Step 3: Ensure radii are large enough
  #
  # It is possible (due to say, rounding), that the given rx and ry
  # can't actually span the gap between the start and the end of the arc.
  # In thise case, just tweak the radii so that they do actually
  # span the start/end gap
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gap <- x1p^2/rx^2 + y1p^2/ry^2
  if (gap > 1) {
    rx <- sqrt(gap) * rx
    ry <- sqrt(gap) * ry
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Eqn 5.2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  det_numer <- rx^2 * ry^2 - rx^2 * y1p^2 - ry^2 * x1p^2
  det_denom <- rx^2 * y1p^2 + ry^2 * x1p^2

  if (det_numer < 0) {
    if (abs(det_numer) < 1e-4) { # floating point precision. basically zero.
      det_numer <- 0
    } else {
      warning("det_number < 0.  replacing with 0. strangeness will ensue")
      det_number <- 0
    }
  }

  det <- sqrt(det_numer/det_denom)

  if (fa == fs) {
    det <- -det
  }

  cxp <- det *  rx * y1p / ry
  cyp <- det * -ry * x1p / rx

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Eqn 5.3
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cx <- cos(phi) * cxp - sin(phi) * cyp + (x1 + x2) / 2
  cy <- sin(phi) * cxp + cos(phi) * cyp + (y1 + y2) / 2


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Eqn 5.5, 5.6
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ref1 <- c(
    (x1p - cxp) / rx,
    (y1p - cyp) / ry
  )

  ref2 <- c(
    (-x1p - cxp) / rx,
    (-y1p - cyp) / ry
  )

  theta1 <- angle(c(1, 0), ref1)
  dtheta <- angle(ref1   , ref2)

  if (fs == 0 && dtheta > 0) {
    dtheta <- dtheta - 2 * pi
  } else if (fs == 1 && dtheta < 0) {
    dtheta <- dtheta + 2 * pi
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create some points along the arc
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  theta <- seq(theta1, theta1 + dtheta, length.out = npoints)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Points around the arc
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xoff <- rx * cos(theta)
  yoff <- ry * sin(theta)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rotate them around the centre of the arc
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xoff2 <- xoff * cos(phi) - yoff * sin(phi)
  yoff2 <- xoff * sin(phi) + yoff * cos(phi)

  res <- data_frame(
    x = cx + xoff2,
    y = cy + yoff2
  )

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Eqn 5.4 - angle between 2 vectors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
angle <- function(u, v){
  atan2(v[2], v[1]) - atan2(u[2], u[1])
}

