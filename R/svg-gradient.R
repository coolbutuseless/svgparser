




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_radialGradient <- function(elem, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find information about the <stop>s in this gradient.
  # these are children of this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stop_info <- lapply(
    xml2::xml_children(elem),
    parse_svg_gradient_stop,
    state = state
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # list of stops and colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stops   <- vapply(stop_info, function(x) {x$stop}  , numeric(1))
  colours <- vapply(stop_info, function(x) {x$colour}, character(1))
  alphas  <- vapply(stop_info, function(x) {x$opacity}, numeric(1))

  final_colours <- character(length(colours))
  for (i in seq_along(stops)) {
    final_colours[i] <- r_colour_plus_alpha(colours[i], alphas[i])
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # https://developer.mozilla.org/en-US/docs/Web/SVG/Element/radialGradient
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cx <- xml2::xml_attr(elem, 'cx', default =  "50%" )
  cy <- xml2::xml_attr(elem, 'cy', default =  "50%" )
  r  <- xml2::xml_attr(elem, 'r' , default =  "50%" )
  fx <- xml2::xml_attr(elem, 'fx', default =  cx    )
  fy <- xml2::xml_attr(elem, 'fx', default =  cy    )
  fr <- xml2::xml_attr(elem, 'fr', default =  "0%"  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert values to numeric
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cx <- css_value_as_numeric(cx)
  cy <- css_value_as_numeric(cy)
  r  <- css_value_as_numeric(r )
  fx <- css_value_as_numeric(fx)
  fy <- css_value_as_numeric(fy)
  fr <- css_value_as_numeric(fr)

  # TODO: Actually need to convert SVG units into grid space
  if (any(c(cx, cy, fx, fy) > 1)) {
    cx <- 0.5
    cy <- 0.5
    fx <- 0.5
    fy <- 0.5
  }

  # flip coords between SVG and grid
  cy <- 1 - cy
  fy <- 1 - fy

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If R version is < 4.1.0 it does not support grid gradients, so
  # for now just return a single solid colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (getRversion() < '4.1.0') {
    final_colours[1]
  } else {
    grid::radialGradient(
      colours = final_colours,
      stops   = stops,
      cx1     = fx,
      cy1     = fy,
      r1      = fr,
      cx2     = cx,
      cy2     = cy,
      r2      = r,
      default.units = 'npc'
    )
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_linearGradient <- function(elem, state) {

  x1 <- css_value_as_numeric(xml2::xml_attr(elem, 'x1', default = 0))
  x2 <- css_value_as_numeric(xml2::xml_attr(elem, 'x2', default = 1))
  y1 <- css_value_as_numeric(xml2::xml_attr(elem, 'y1', default = 0))
  y2 <- css_value_as_numeric(xml2::xml_attr(elem, 'y2', default = 0))

  # TODO: Actually need to convert SVG units into grid space
  if (any(c(x1, x2, y1, y2) > 1)) {
    x1 <- 0
    x2 <- 1
    y1 <- 0
    y2 <- 0
  }

  y1 <- 1 - y1
  y2 <- 1 - y2


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find information about the <stop>s in this gradient.
  # these are children of this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stop_info <- lapply(
    xml2::xml_children(elem),
    parse_svg_gradient_stop,
    state = state
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # list of stops and colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stops   <- vapply(stop_info, function(x) {x$stop}   , numeric(1))
  colours <- vapply(stop_info, function(x) {x$colour} , character(1))
  alphas  <- vapply(stop_info, function(x) {x$opacity}, numeric(1))

  final_colours <- character(length(colours))
  for (i in seq_along(stops)) {
    final_colours[i] <- r_colour_plus_alpha(colours[i], alphas[i])
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If R version is < 4.1.0 it does not support grid gradients, so
  # for now just return a single solid colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (getRversion() < '4.1.0') {
    final_colours[1]
  } else {
    grid::linearGradient(
      colours = final_colours,
      stops   = stops,
      x1      = x1,
      x2      = x2,
      y1      = y1,
      y2      = y2,
      default.units = 'npc'
    )
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse a 'stop' tag inside a linear gradient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_gradient_stop <- function(elem, state) {

  xpstyle <- get_xpath_style(elem, state)

  offset <- xml2::xml_attr(elem, 'offset', default = 0)
  offset <- css_value_as_numeric(offset)

  stop_color <- xpstyle[['stop-color']] %||% xml2::xml_attr(elem, "stop-color", default = 'black')
  stop_color <- svg_colour_to_r_colour(stop_color, state)


  stop_opacity <- xpstyle[['stop-opacity']] %||% xml2::xml_attr(elem, "stop-opacity", default = 1)
  stop_opacity <- css_value_as_numeric(stop_opacity)


  list(
    stop    = offset,
    colour  = stop_color,
    opacity = stop_opacity
  )
}


