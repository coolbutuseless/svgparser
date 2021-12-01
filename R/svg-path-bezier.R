
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-calculate the basis functions for different N points:
# Cubic Beziers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bf3s <- lapply(1:100, function(N) {
  t  <- seq.int(0, 1, length.out = N)

  matrix(c(
    1 * t^0 * (1 - t)^3,
    3 * t^1 * (1 - t)^2,
    3 * t^2 * (1 - t)^1,
    1 * t^3 * (1 - t)^0
  ), ncol = 4
  )
})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-calculate the basis functions for different N points:
# Quadratic Beziers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bf2s <- lapply(1:100, function(N) {
  t  <- seq.int(0, 1, length.out = N)

  matrix(c(
    1 * t^0 * (1 - t)^2,
    2 * t^1 * (1 - t)^1,
    1 * t^2 * (1 - t)^0
  ), ncol = 3
  )
})



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert cubic and quadratic beziers to data.frame of coordinates along curve
#'
#' @param x,y coords of control points
#' @param N Number of output points
#'
#' @return data.frame of coords
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bezier3_to_df <- function(x, y, N = 5) {
  stopifnot(length(x)== 4, length(y) == 4)
  data_frame(
    x = rowSums(bf3s[[N]] * matrix(x, nrow = N, ncol = 4, byrow = TRUE)),
    y = rowSums(bf3s[[N]] * matrix(y, nrow = N, ncol = 4, byrow = TRUE))
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bezier3_to_df
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bezier2_to_df <- function(x, y, N = 5) {
  stopifnot(length(x)== 3, length(y) == 3)
  data_frame(
    x = rowSums(bf2s[[N]] * matrix(x, nrow = N, ncol = 3, byrow = TRUE)),
    y = rowSums(bf2s[[N]] * matrix(y, nrow = N, ncol = 3, byrow = TRUE))
  )
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert bezier control points to data.frame of coordinates along curve
#'
#' This is an old version - just keeping around for sanity checking.  For small
#' N this is the same speed as the current 'bezier_to_df', but for larger N
#' it is slower.
#'
#' @param x,y coords of control points
#' @param N Number of output points
#'
#' @return data.frame of coords along cubc bezier
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bezier3_to_df_loop <- function(x, y, N = 5) { #nocov start
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Allocate space for result
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xs <- numeric(N)
  ys <- numeric(N)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bezier param 't' at which to evaluate points
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ts <- seq.int(0, 1, length.out = N)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaluate bezier at N locations
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq.int(1, N, 1)) {
    t  <- ts[[i]]
    nt <- 1 - t
    bf <- c(
      1 * t^0 * nt^3,
      3 * t^1 * nt^2,
      3 * t^2 * nt^1,
      1 * t^3 * nt^0
    )
    xs[[i]] <- sum(bf * x)
    ys[[i]] <- sum(bf * y)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Faster data.frame creation than using 'data.frame(x=xs, y=ys)'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data_frame(x = xs, y = ys)
} #nocov end










