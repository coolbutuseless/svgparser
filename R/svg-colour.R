




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Augment colour with alpha channel.
#'
#' @param x colour
#' @param alpha alpha [0, 1]
#'
#' @importFrom  grDevices rgb col2rgb
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r_colour_plus_alpha <- function(x, alpha = 1) {

  # This ain't great, but it works "good enough" for now.  TODO: FIXME
  if (alpha > 1) {
    alpha <- alpha/100
  }
  if (alpha > 1) {
    alpha <- alpha / 2.55
  }

  # convert given colour to vector of values in range [0,1]
  v <- grDevices::col2rgb(x, alpha = TRUE)/255

  # Mutiple the alpha channel with more alpha
  v[4] <- v[4] * alpha

  # Recreate the colour
  grDevices::rgb(v[1], v[2], v[3], v[4])
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an SVG colour to an R colour
#'
#' Handles: False colouring, hex colours, oct colours, radial gradients.
#'
#' More complex CSS colour specifications like \code{rgb(10, 20, 30, 0.5)}
#' are handled by \code{cssparser::css_colour_to_hex()}
#'
#' @param col svg colour
#' @inheritParams parse_svg_group
#'
#' @importFrom grDevices rainbow hcl.colors heat.colors terrain.colors topo.colors cm.colors
#'
#' @return valid R colour
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_colour_to_r_colour <- function(col, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is gonzo hack that if 'debug' is set to a numeric value greater
  # than 1, then just replace any colour request with a random colour from
  # one of the `grDevices` palettes.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # nocov start    # don't worry about testing gonzo colour hacks!
  if (identical(state$false_colour, 'rainbow')) {
    return(sample(grDevices::rainbow(100), 1))
  } else if (identical(state$false_colour, 'hcl')) {
    return(sample(grDevices::hcl.colors(100), 1))
  } else if (identical(state$false_colour, 'heat')) {
    return(sample(grDevices::heat.colors(100), 1))
  } else if (identical(state$false_colour, 'terrain')) {
    return(sample(grDevices::terrain.colors(100), 1))
  } else if (identical(state$false_colour, 'topo')) {
    return(sample(grDevices::topo.colors(100), 1))
  } else if (identical(state$false_colour, 'cm')) {
    return(sample(grDevices::cm.colors(100), 1))
  }
  # nocov end

  if (is.null(col)) {
    # nocov start   This should never happen!  If it does it is definitely a weird error.
    warning("NULL colour. using hopink")
    "#FF69B4FF"   # 'hotpink'
    # nocov end
  } else if ((col == 'currentColor')) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if 'fill' or 'stroke' was set to 'currentColor', this is already
    # replaced when calculating final style of element.
    # However, there is the possibility that other colour values may
    # be set to currentColor, but I haven't seen it during testing.
    # i.e. the first colour-stop on a linear gradient could be 'currentColor'
    # If this warning ever triggers, it means I probably have to
    # rethink my approach for handling this special value
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!isTRUE(state$warned[['currentColor']])) {
      message(
        "Unexpected 'currentColor' error. ",
        "Please file an issue linking to this SVG file. ",
        "Color='hotpink' will be used instead"
      )
      state$warned[['currentColor']] <- TRUE
    }
    "#FF69B4FF"  # 'hotpink'
  } else if (startsWith(col, "url")) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # A colour which is a reference to a "url(#id)" is 100%(?) going to
    # be a reference to a linear or radial gradient.
    # Just pluck this particular element out of the SVG document (the
    # reference copy to this document is stored in `state$svg`)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xml <- get_element_by_id(state$svg, col)
    res <- "#FF69B4FF" # 'hotpink'
    if (!is.na(xml)) {
      tag <- xml2::xml_name(xml)
      if (tag == 'linearGradient') {
        # message("do lineargradient")
        res <- parse_svg_linearGradient(xml, state = state)
      } else if (tag == 'radialGradient') {
        # message("do radialgradient")
        res <- parse_svg_radialGradient(xml, state = state)
      } else {
        message("Colour '", col, "' of type <", tag, "> not currently handled")
      }
    } else {
      message("url() specified colour not found: ", col)
    }
    res
  } else {
    cssparser::css_colour_to_hex(col)
  }
}
