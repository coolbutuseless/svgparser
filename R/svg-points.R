

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The patters to tokenise a point string
# The number regex is a bit of work.
# in SVG paths, 2 numbers don't need a comma between them if the context
# indicates they're 2 numbers.
#  e.g. "1-2" is the number sequence c(1, -2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
points_patterns <- c(
  number              = "[+\\-]?(?:0|[\\.0-9]\\d*)(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?",
  comma               = ",",
  whitespace          = "\\s+"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse a 'points' string used in 'polyline' and 'polygon' tags
#'
#' @param points_string character string
#'
#' @return data.frame of x,y coords
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_points <- function(points_string) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the path into tokens and remove the junk tokens
  #  i.e. whitespace and commas
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- lex(points_string, points_patterns)
  tokens <- tokens[!names(tokens) %in% c('whitespace', 'comma')]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity Check
  #   - these should all be 'number' tokens
  #   - there should be an even number of numbers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!all(names(tokens) == 'number') || length(tokens) %% 2 != 0) {
    stop("parse_points(): Didn't get even number of numbers from: ", points_string)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split linear list into 2-column data.frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- as.numeric(tokens)

  data_frame(
    x = tokens[c(T, F)],
    y = tokens[c(F, T)]
  )
}
