

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a string value from CSS into a numeric value
#'
#' @param x Character string of a CSS value e.g. "12", "12px", "3em", "47\%"
#' @param percentage_as_fraction Should percentages like "12%" be returned
#'        as a fraction (e.g. 0.12)?  Default: TRUE.   If FALSE then returns
#'        the percentage as a numeric value (e.g. 12)
#' @param ... other arguments passed to \code{css_length_as_pixels()}
#'
#' @return a numeric value as best we can with limited knowledge
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_value_as_numeric <- function(x, percentage_as_fraction = TRUE, ...) {

  # This function used to do a whole lot more, but now it has all been
  # collapsed into a call to {cssparser}.
  # Possibly remove this wrapper.  Mike 2021-11-25
  cssparser::css_string_as_pixels(x, percentage_as_fraction, ...)
}

