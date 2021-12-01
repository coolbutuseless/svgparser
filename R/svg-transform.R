

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Apply the given transform to x,y coordds
#'
#' @param coords_df data.frame containing x,y coords
#' @param transform matrix transform to apply
#'
#' @return data.frame with updated coordinates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
apply_transform <- function(transform, coords_df) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract (x,y) coords as matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords_mat <- rbind(coords_df$x, coords_df$y, 1)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calcualte new values and update existing coords_df (so we
  # can keep all the other columns intact)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_coords_mat <- transform %*% coords_mat

  coords_df$x <- new_coords_mat[1L, ]
  coords_df$y <- new_coords_mat[2L, ]

  coords_df
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Update a transform matrix with an element
#'
#' @param transform matrix
#' @param elem svg element
#'
#' @return new transform matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_transform <- function(transform, elem) {
  update_transform_with_string(transform, xml2::xml_attr(elem, 'transform'))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Update a transform matrix with the new SVG transform string
#'
#' @param transform matrix
#' @param transform_string the transform string which is a presentation
#'        attribute on an element
#'
#' @return new transform matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_transform_with_string <- function(transform, transform_string) {

  if (is.null(transform_string) || length(transform_string) == 0 ||
      is.na(transform_string) || nchar(transform_string) == 0) {
    res <- transform
  } else {
    res <- transform %*% parse_transform_string_to_matrix(transform_string)
  }


  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Patterns for splitting a transform string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_patterns <- c(
  command    = "matrix|translate|scale|rotate|skewX|skewY",
  number     = number_pattern,
  comma      = ",",
  whitespace = "\\s+",
  open       = "\\(",
  close      = "\\)"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a mtrix corresponding to 2d translation.
#
# If y coords is not yet, then set it to 0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_translate_matrix <- function(params) {
  if (length(params) == 1) {
    x <- params
    y <- 0
  } else {
    x <- params[1]
    y <- params[2]
  }
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
create_rotate_matrix <- function(angle) {
  ct <- cos(angle * pi/180)
  st <- sin(angle * pi/180)
  matrix(
    c(ct ,-st, 0,
      st,  ct, 0,
      0 ,   0, 1),
    byrow = TRUE, ncol = 3
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse tokens representing a single transform instruction into a matrix
#'
#' @param tokens character vector of c(command, number, number, ...)
#'
#' @return matrix representing this transform
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_command_to_matrix <- function(tokens) {
  command <- tokens[[1]]
  numbers <- as.numeric(tokens[-1])

  if (command == 'matrix') {
    res <- matrix(numbers, ncol = 3)
    res <- rbind(res, c(0, 0, 1))
    res
  } else if (command == 'translate') {
    create_translate_matrix(numbers)

  } else if (command == 'scale') {
    if (length(numbers) == 1) {
      numbers <- c(numbers, numbers)
    }
    matrix(
      c(numbers[1], 0, 0,
        0, numbers[2], 0,
        0, 0,          1),
      byrow = TRUE, ncol = 3
    )
  } else if (command == 'rotate' && length(numbers) == 1) {
    create_rotate_matrix(numbers)
  } else if (command == 'rotate' && length(numbers) == 3) {
    create_translate_matrix(c(numbers[2], numbers[3])) %*%
      create_rotate_matrix(numbers[1]) %*%
      create_translate_matrix(c(-numbers[2], -numbers[3]))
  } else if (command == 'skewX') {
    matrix(
      c(1, tan(numbers * pi/180), 0,
        0, 1, 0,
        0, 0, 1),
      byrow = TRUE, ncol = 3
    )
  } else if (command == 'skewY') {
    matrix(
      c(1, 0, 0,
        tan(numbers * pi/180), 1, 0,
        0, 0, 1),
      byrow = TRUE, ncol = 3
    )
  } else {
    warning("Unknown transform: ", command)
    diag(3)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse a transform string to a list of matrices - 1 per command. Mainly for debugging
#'
#' Note there can be multiple transform instructions in a transform string
#'
#' @param transform_string e.g. "rotate(10) translate(3, 3)"
#'
#' @return list of matrices
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_transform_string_to_matrices <- function(transform_string) {

  if (trimws(transform_string) == "") {
    return(list(diag(3)))
  }

  tokens <- lex(transform_string, transform_patterns)
  tokens <- tokens[!names(tokens) %in% c('whitespace', 'comma', 'open', 'close')]
  tokens

  cidx <- c(
    which(names(tokens) == 'command'),
    length(tokens) + 1L
  )

  res <- vector('list', length(cidx) - 1L)
  for (i in seq(length(cidx) - 1)) {
    start <- cidx[i]
    end   <- cidx[i + 1L] - 1L
    res[[i]] <- parse_command_to_matrix(tokens[start:end])
  }
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse a transform string to asingle transform matrix
#'
#' Note there can be multiple transform instructions in a transform string
#'
#' @param transform_string e.g. "rotate(10) translate(3, 3)"
#'
#' @return combined transform marix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_transform_string_to_matrix <- function(transform_string) {

  matrices <- parse_transform_string_to_matrices(transform_string)

  Reduce(`%*%`, matrices)
}




