

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper functions
#  - Detect if something looks like a hex colour  i.e. '#RRGGBB
#  - Detect if something looks like an oct colour i.e. '#RRGGBBAA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_hex_colour <- function(x) { startsWith(x, '#') && nchar(x) == 7 }
is_oct_colour <- function(x) { startsWith(x, '#') && nchar(x) == 9 }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a list of style information for an element to a \code{gpar} object
#'
#' Convert a named list of CSS style information into a gpar object. E.g.
#' \code{list(stroke = 'black')} is converted to \code{gpar(col = 'black')}.
#'
#' This function handles stroke and fill colours (and separate alphas for each),
#' fonts, linear and radial gradients, line parameters (width, join and end
#' types etc).
#'
#' Not yet done:
#' \itemize{
#' \item{Conversion of line type information from CSS to gpar}
#' }
#'
#' @param style named list of style properties for an element. E.g.
#'        \code{list(stroke = 'black')}
#' @param state state information. See inline documentation for \code{read_svg} for
#'        more information.
#'
#' @return gpar object
#'
#' @importFrom grDevices col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
style_to_gpar <- function(style, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Outline colour with appropriate alpha
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  col   <- svg_colour_to_r_colour(style[['stroke']], state)
  if (is.character(col)) {
    alpha_frac <- css_value_as_numeric(style[['stroke-opacity']] %||% 1)
    alpha <- sprintf("%02x", as.integer(alpha_frac * 255))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Ensure that the specific 'stroke-opacity' is
    # applied to this colour only (and not the stroke as well)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is_hex_colour(col)) {
      # e.g. convert '#ff0000' to 'ff000012'
      col   <- paste0(col, alpha)
    } else if (!is_oct_colour(col) && col != 'transparent') {
      # e.g. convert 'red' to '#ff000012'
      col <- paste(c('#', sprintf("%02x", as.vector(col2rgb(col))), alpha), collapse = "")
    } else {
      col <- r_colour_plus_alpha(col, alpha_frac)
    }
  } else if (inherits(col, 'GridPattern')) {
    # SVG can have gradient applied to strokes, grid can't.
    message("stroke='url(...)' in SVG is not supported by grid. Setting: stroke='black'")
    col <- 'black'
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fill colour with appropriate alpha
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill  <- svg_colour_to_r_colour(style[['fill']], state)
  if (is.character(fill)) {
    alpha_frac <- css_value_as_numeric(style[['fill-opacity']] %||% 1)
    alpha <- sprintf("%02x", as.integer(alpha_frac * 255))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Ensure that the specific 'fill-opacity' is
    # applied to this colour only (and not the stroke as well)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is_hex_colour(fill)) {
      # e.g. convert '#ff0000' to 'ff000012'
      fill   <- paste0(fill, alpha)
    } else if (!is_oct_colour(fill) && fill != 'transparent') {
      # e.g. convert 'red' to '#ff000012'
      fill <- paste(c('#', sprintf("%02x", as.vector(col2rgb(fill))), alpha), collapse = "")
    } else {
      fill <- r_colour_plus_alpha(fill, alpha_frac)
    }
  } else {
    # this is a radialGradient or linearGradient of a pattern,
    # so mostly just pass it through as-is.
    # However, gradient-fill can further be affected by the
    # global 'fill-opacity', so adjust all colours in the
    # linearGradient or radialGradient by this alpha value.
    # Note: the 'linearGradient' and 'radialGradient' objects here
    # are just lists of values with the right class attached, so access
    # the colours on the gradient as "fill$colours"
    alpha <- css_value_as_numeric(style[['fill-opacity']] %||% 1)
    fill$colours <- vapply(
      fill$colours, r_colour_plus_alpha,
      character(1), alpha,
      USE.NAMES = FALSE
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Font stuff
  # 'font-family'   ??? serif | sans-serif | cursive | fantasy | monospace
  # 'font-size'     https://developer.mozilla.org/en-US/docs/Web/CSS/font-size
  # 'font-style'    'normal'  (italic, oblique)
  # 'font-weight'   'normal'  (bold, bolder, 'lighter')
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fontsize <- css_value_as_numeric(style[['font-size']] %||% 12)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Really naive font family mapping to R's sans, serif, mono
  # ToDo: This could be beefed up with {systemfonts} if desired
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  family <- style[['font-family']] %||% 'serif'
  family <- ifelse(grepl(family, "sans" ), "sans" , family)
  family <- ifelse(grepl(family, "serif"), "serif", family)
  family <- ifelse(grepl(family, "mono" ), "mono" , family)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Numeric R face: 1=plain, 2=bold 3=italic 4=bold italic
  # Char R faces: "plain", "bold", "italic", "oblique", "bold.italic"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  face <- style[['font-style']] %||% 'normal'
  face <- switch(
    family,
    normal  = 'plain',
    italic  = 'italic',
    oblique = 'italic',
    'plain'
  )

  weight <- style[['font-weight']] %||% 'normal'
  weight <- switch(
    weight,
    normal  = "normal",
    bold    = "bold",
    bolder  = "bold",
    lighter = "normal",
    "normal"
  )

  if (weight == 'bold' && face == 'plain') {
    face <- 'bold'
  } else if (weight == 'bold' && face == 'italic') {
    face <- 'bold.italic'
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # lwd
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lwd <- style[['stroke-width']] %||% 1
  lwd <- css_value_as_numeric(lwd)
  lwd <- lwd * state$stroke_scale

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Lineend
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lineend <- style[['stroke-linecap']] %||% 'butt'
  linejoin <- style[['stroke-linejoin']] %||% 'miter'
  linejoin <- switch(
    linejoin,
    miter = 'mitre',
    bevel = 'bevel',
    round = 'round',
    arcs  = 'round',
    `miter-clip` = 'mitre',
    'mitre'
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Mitre Limit. R default is 10
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  linemitre <- as.numeric(style[['stroke-miterlimit']] %||% 10)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Global setting of opacity for this grob.
  # Applies to both stroke and fill. Usuaully this is '1', and the
  # invidual 'stroke-opacity' and 'fill-opacity' are already
  # accounted for on the 'col' and 'fill' parameters for gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  alpha <- css_value_as_numeric(style[['opacity']] %||% 1)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gpar(
    col        = col,
    fill       = fill,
    alpha      = alpha,
    lwd        = lwd,
    lineend    = lineend,
    linejoin   = linejoin,
    linemitre  = linemitre,
    fontsize   = fontsize,
    cex        = state$font_scale %||% 1,
    fontface   = face,
    fontfamily = family
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a gpar object to a data.frame
#'
#' This converts a gpar object to a 1-row data.frame representation. This is
#' used when accumulating a data.frame representation to return to the user.
#'
#' Linear and Radial gradient objects are replaced with their first colour.
#' Although this could be something fancier e.g. list object, or some text
#' representation of the gradient parameters. Maybe just \code{deparse1()}?
#'
#' @param gp gpar object with a single specification. no vectors allowed
#'
#' @return data.frame with single row.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gpar_to_df <- function(gp) {
  ll <- unclass(gp)
  if (inherits(ll$fill, 'GridLinearGradient') || inherits(ll$fill, 'GridRadialGradient')) {
    ll$fill <- ll$fill$colours[[1]]
  }

  do.call(data_frame, ll)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a list of style information to a viewport object
#'
#' Currently this only supports setting the clipping path on the viewport.
#'
#' Masks would also be set here, but I don't have any good examples to
#' debug with, so leaving this for now.
#'
#' @param style named list of style properties
#' @param transform transform matrix of element being clipped
#' @param state state information
#'
#' @return viewport object
#'
#' @importFrom grDevices col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
style_to_viewport <- function(style, transform, state) {

  vp <- viewport()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Masks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mask_id <- style[['mask']] %||% 'none'
  if (mask_id != 'none') {
    message("Mask '", mask_id, "' requested, but masking not yet implemented")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clipping path
  #
  # TODO: clipPathUnits can be 'objectBoundingBox' or 'userSpaceOnUse'
  #       This implementation currently only handles the SVG default of
  #       'userSpaceOnUse'
  #       To support 'objectBoundingBox' would need to:
  #         - calculate a bounding box for indiivudal elements
  #         - calculate bbox for <g> tags
  #         - create a viewport in 'snpc' units matching the bbox
  #         - assign this viewport to the clippath grob
  #         - assign the clippath grob as the viewport of the element it
  #           is clipping
  #
  # TODO: How to handle 'clip-rule'?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  clip_id <- style[['clip-path']] %||% 'none'
  if (clip_id != 'none' && getRversion() >= '4.1.0') {

    clip_xml <- get_element_by_id(state$svg, clip_id)

    # message("Clipping requested to: ", clip_id)
    if (is.na(clip_id)) {
      message("Clipping requested to: '", clip_id, "' but ref not found in <svg> doc")
    } else {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Sanity check units
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      clipPathUnits <- style[['clipPathUnits']] %||% 'userSpaceOnUse'
      if (clipPathUnits != 'userSpaceOnUse') {
        message("Request clipping path units not handled: ", clipPathUnits)
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Assembler the clip grob
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      state$clip_style <- style
      state$clip_style[['clip-path']] <- NULL
      clip_grob <- parse_svg_group(elem = clip_xml, state = state)
      state$clip_style <- NULL

      vp <- viewport(clip = clip_grob)

    }
  }


  vp
}


