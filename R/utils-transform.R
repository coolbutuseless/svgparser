

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Decompose a 2D transformation matrix into translation, rotation
# and scaling components.
#
# Extracting Skew is currently not supported, and if there is
# skew as part of the transform, then the resulting scale and rotation
# extracted by this will be incorrect.
#
# Note: If angle != angle2 this is probably a good indicator of the
# presence of skew.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decompose_transform <- function(mat) {

  translation <- mat[1:2, 3]

  sx <- sqrt(sum(mat[,1] ^ 2))
  sy <- sqrt(sum(mat[,2] ^ 2))

  mat[,1] <- mat[,1] / sx
  mat[,2] <- mat[,2] / sy

  angle  <- atan2(mat[2, 1], mat[1, 1]) * -180/pi
  angle2 <- atan2(mat[1, 2], mat[2, 2]) *  180/pi

  scale <- c(sx, sy)

  list(
    angle       = angle,
    angle2      = angle2,
    translation = translation,
    scale       = scale
  )
}

# nocov start


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Most of the following transform/matrix code is currently unused
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a mtrix corresponding to 2d translation.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
translation_matrix <- function(x, y) {
  matrix(
    c(1, 0, x,
      0, 1, y,
      0, 0, 1),
    byrow = TRUE, ncol = 3
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a rotation matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rotation_matrix <- function(angle) {
  ct <- cos(angle * pi/180)
  st <- sin(angle * pi/180)
  matrix(
    c(ct ,-st, 0,
      st,  ct, 0,
      0 ,   0, 1),
    byrow = TRUE, ncol = 3
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a rotation matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rotation_around_point_matrix <- function(angle, x, y) {
  translation_matrix(x, y) %*%
    rotation_matrix(angle) %*%
    translation_matrix(-x, -y)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Scaling matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_matrix <- function(sx, sy = sx) {
  matrix(
    c(sx,  0, 0,
      0,  sy, 0,
      0,   0, 1),
    byrow = TRUE, ncol = 3
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Skew X
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
skewx_matrix <- function(angle) {
  matrix(
    c(1, tan(angle * pi/180), 0,
      0, 1, 0,
      0, 0, 1),
    byrow = TRUE, ncol = 3
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Skew Y
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
skewy_matrix <- function(angle) {
  matrix(
    c(1, 0, 0,
      tan(angle * pi/180), 1, 0,
      0, 0, 1),
    byrow = TRUE, ncol = 3
  )
}
# nocov end




if (FALSE) {
  rotation_matrix(30) %*%
  translation_matrix(2, 3) %*%
    translation_matrix(1, 2) %>%
    decompose_transform()
}





