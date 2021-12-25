

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Final user adjustments
#   * flip y axis
#   * scale and offset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xpos <- function(x, state) { ((             x) * state$scale  + state$xoffset) * state$user_scale }
ypos <- function(y, state) { ((state$ymax - y) * state$scale  + state$yoffset) * state$user_scale }


user_adjust <- function(coords_df, state) {
  coords_df$x  = xpos(coords_df$x, state)
  coords_df$y  = ypos(coords_df$y, state)

  coords_df
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read an svg file (or text) into a \code{grid::grobTree} object or \code{data.frame}
#'
#' @param svg_file either a filename, or a single character string containing all the text of
#'        an SVG.  Filenames may either be \code{'.svg'} or \code{'.svgz'} (gzip
#'        compressed SVG)
#' @param xoffset,yoffset Extra offsets to element coordinates applied in the
#'        grob coordinate system (not the SVG coordinate system). Default: (0, 0)
#' @param npoints number of segmentation points per section of bezier, arc,
#'        circle or ellipse. Default 30
#'        Increase this number if the curves look too jaggy for your use case.
#' @param scale Scale factor to apply to all coordinates. Default: 1.
#' @param default.units the grid units to use throughout. The default (\code{'snpc'})
#'        is a pretty safe bet that will give you an auto-resizing vector object.
#'        Uou could also set it to an \emph{absolute}
#'        unit (like \code{'mm'}) and then play with the \code{scale} argument
#'        to get a fixed size grid object.
#' @param stroke_scale Default: 1.  Multiplication factor for width of strokes.
#'        The value to use here is heavily dependent upon what size output
#'        you are rendering to.
#' @param font_scale extra scaling applied to font parameters. Default: 1.
#'        The value to use here is heavily dependent upon the output size
#'        you are rendering to.
#' @param style_default a named list of CSS properties which should override
#'        the defaults.  default: \code{list()}.  E.g. set \code{style_default =
#'        list(fill = 'red')} to set the default fill style for all elements
#'        to 'red'. This style will still be overridden by inline styles, css styles,
#'        or presentation attributes. It is a useful way of setting the 'color'
#'        property which is often used in SVG icon sets (which make heavy use of
#'        the 'currentColor' property)
#' @param user_css single string containing CSS text e.g. "circle \{ fill: red !important; \}".
#'        Note: Normal cascading style rules apply i.e. more specific rules override
#'        those with lower specificity, and inline style specifications have the highest specificty.
#'        You may need to use \code{!important} to override styles consistently.
#' @param obj_type What kind of R object to return - choices
#'        \code{c('grob', 'data.frame', 'list', 'debug')}. Default: 'grob'.
#'
#'        The 'list' and 'data.frame' options are for advanced/debugging use,
#'        but some users may find them useful if trying to extract coordinate
#'        information etc.
#'
#'        The \code{data.frame} option could be used to recreate much of the SVG
#'        but it is missing key information such as clipping paths, and gradients
#'        (as these are pretty difficult to capture nicely in a data.frame).
#'
#'        The \code{debug} option returns all the possible information. Currently
#'        this returned object is undocumented. Use at your peril!
#'
#'        The \code{list} option returns a list containing the following elements
#'       for each parsed SVG element
#'        \itemize{
#'          \item{svg - the SVG string for this element}
#'          \item{tag - the SVG tag e.g. "path", "circle"}
#'          \item{svg_df - data.frame of coordinates in SVG coordinate system}
#'          \item{transform - the transform matrix for this element}
#'          \item{grid_df - the transformed coordinates in R/grid coordinate space}
#'          \item{style - caclculated style for this element. Named list of sttyle attributes}
#'          \item{gp - the \code{gpar()} equivalent of the style}
#'          \item{grob - the final generated grob for this element}
#'        }
#' @param false_colour Use false colouring on all elements, by selecting random
#'        colours from palettes in \code{grDevices}
#'        Default: NULL means to use actual colours.
#'        Possible values: 'rainbow', 'hcl', 'heat',
#'        'terrain', 'topo', 'cm'
#'
#' @return Return type determined by \code{obj_type} argument
#'
#' @importFrom utils modifyList
#' @import grid
#' @import xml2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_svg <- function(svg_file,
                     xoffset       = 0,
                     yoffset       = 0,
                     npoints       = 30,
                     scale         = 1,
                     default.units = 'snpc',
                     stroke_scale  = 1,
                     font_scale    = 1,
                     style_default = list(),
                     user_css      = NULL,
                     obj_type      = c('grob', 'data.frame', 'list', 'debug'),
                     false_colour  = NULL
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Strip the namespace from the XML and find the <svg> tag
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xml <- xml2::read_xml(svg_file)
  xml <- xml2::xml_ns_strip(xml)
  svg <- xml2::xml_find_first(xml, "//svg")

  obj_type <- match.arg(obj_type)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.na(svg)) {
    stop("No <svg> tag found")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set up the global state for this SVG file.
  #
  # This is created as an environment so that it can be passed into functions
  # and updated there and I never have to care about returning it as
  # environments act as if they're  passed by reference.
  #
  #  x,y  current rendering location
  #  xoffset,yoffset   set to non-zero if there are user-supplied offsets to
  #                    the coordinates during conversion.
  #  npoints           the granularity with which curves are turned into segments
  #                    bigger numbers give more segments give smoother curves
  # stroke_scale       user-supplied scaling factor for strokes.
  # font_scale         user-supplied scaling factor for fonts
  # default.units      default.units for the grob representation.
  # warned             keep track of which warnings have been given
  # obj_type           the type for the return object
  # obj_list          accumulation data structures for the returned object
  # style_of_use_container - when processing a <use> tag, the current style
  #                          is stored here and used when styling.  (rather than
  #                          using the style of the referenced element)
  # false_colour - false colour name (or NULL)
  # ymax - maximum 'y' coordinate.  This is used to invert the y-axis when
  #        translating coordinates from SVG space (with 0,0 at top-left) to
  #        gob space (with 0, 0 at bottom-left)
  # scale - internally defined scale factor. For 'npc' and 'snpc' units, this
  #         scale is set such that the SVG coorindates are all scaled into the
  #         range [0, 1].  For other units it is set to 1.0
  # user_scale - The user can then specify an extra scale factor to apply.  This
  #              is most useful when the units are *NOT* 'npc' or 'snpc'.
  # user_agent_css - the default style for all elements e.g. fill = 'black'
  # xpath_style - the final computed style for all elements as indexed by the
  #               xpath of the element. This is calculated by {cssparser}
  # svg - ready to use SVG (including any adjustmnets done by {cssparser})
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state               <- new.env()

  state$x             <- 0             # start point
  state$y             <- 0             # Start point
  state$xoffset       <- xoffset
  state$yoffset       <- yoffset
  state$npoints       <- npoints

  state$stroke_scale  <- stroke_scale
  state$font_scale    <- font_scale

  state$default.units <- default.units
  state$warned        <- list()        # Keep track of what warnings have been given

  # Accumulation of data for the returned object
  state$obj_type <- obj_type
  state$obj_list <- list()   # full list information

  state$style_of_use_container <- NULL
  state$false_colour  <- false_colour

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse out a viewbox for a width/height
  # According to spec:
  #   The value of the viewBox attribute is a list of four numbers:
  #   min-x, min-y, width and height.
  #   The numbers separated by whitespace and/or a comma
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  viewbox <- xml2::xml_attr(svg, "viewBox", default = NA)

  if (is.na(viewbox)) {
    viewbox <- c(
      0,
      0,
      css_value_as_numeric(xml2::xml_attr(svg, "width" , default = 400)),
      css_value_as_numeric(xml2::xml_attr(svg, "height", default = 400))
    )
  } else {
    viewbox <- as.numeric(strsplit(viewbox, "\\s+|,")[[1]])
  }

  state$ymax <- viewbox[[4]]

  state$width  <- viewbox[[3]]
  state$height <- viewbox[[4]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # With 'default.units' of 'snpc' or 'npc', just scale any coordinates by
  # the height in order to bring all coords into the range 0/1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (default.units %in% c('snpc', 'npc')) {
    maxdim      <- viewbox[4]
    state$scale <- 1 / maxdim
  } else {
    state$scale <- 1
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The user may further scale the output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$user_scale <- scale

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initialise the transform matrix to the identity
  # If the viewbox is offset at all, then set the default transform to
  # being the values into view
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  init_transform <- sprintf("translate(%f, %f)", -viewbox[1], -viewbox[2])
  state$user_agent_css <- list(
    `color`             = 'black',
    `clip-path`         = 'none',
    `clip-rule`         = 'nonzero',
    `clipPathUnits`     = 'userSpaceOnUse',
    `fill`              = 'black',
    `fill-opacity`      = 1,
    `fill-rule`         = 'nonzero',
    `font-family`       = 'serif',
    `font-size`         = 12,
    `font-style`        = 'normal',
    `font-weight`       = 'normal',
    `mask`              = 'none',
    `stroke`            = 'none',
    `stroke-dasharray`  = 'none',
    `stroke-dashoffset` = 0,
    `stroke-linecap`    = 'butt',
    `stroke-linejoin`   = 'miter',
    `stroke-miterlimit` = 4,
    `stroke-opacity`    = 1,
    `stroke-width`      = 1,
    `visibility`        = 'visible',
    `transform`         = init_transform
  )
  state$user_agent_css <- modify_list(state$user_agent_css, style_default)


  style_results <- css_calc_all_styles(svg, user_css)
  state$xpath_style <- style_results$xpath_style
  svg <- style_results$svg
  state$svg <- svg


  this_grob <- parse_svg_group(svg, state = state)

  if (obj_type == 'debug') {
    full_res <- state$obj_list
    for (i in seq_along(full_res)) {
      full_res[[i]]$svg_df$elem_idx  <- i
      full_res[[i]]$grid_df$elem_idx <- i
    }
    list(
      res = full_res,
      state = state
    )
  } else if (obj_type == 'list') {
    full_res <- state$obj_list
    for (i in seq_along(full_res)) {
      full_res[[i]]$svg_df$elem_idx <- i
      full_res[[i]]$grid_df$elem_idx <- i
    }
    full_res
  } else if (obj_type == 'data.frame') {
    df_list <- state$obj_list
    for (i in seq_along(df_list)) {
      df_list[[i]]$elem_idx <- i
    }
    do.call(rbind, df_list)
    # rbind_dfs(df_list)  # retired. All data.frames should have consistent columns now
  } else {
    # grob
    this_grob
  }

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse xml representing some SVG
#'
#' @param elem as SVG grouping element e.g. <svg> <g> <clipPath>
#' @param state environment containing state information for this SVG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_group <- function(elem, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a grob for each element in the SVG
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  elem_grobs <- lapply(
    xml2::xml_children(elem),
    parse_svg_elem,
    state = state
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine all grobs in a grobtree. Just filter out the NULL grobs before
  # proceeding
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  elem_grobs <- Filter(Negate(is.null), elem_grobs)

  do.call(grid::grobTree, elem_grobs)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse an individual element
#'
#' @param elem svg elem i.e. an xml2 document
#' @inheritParams parse_svg_group
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_elem <- function(elem, state) {
  tag <- xml2::xml_name(elem)
  xpstyle <- get_xpath_style(elem, state)

  elem_grob <- switch(
    tag,
    a        =,
    g        = parse_svg_group   (elem, state),
    path     = parse_svg_path    (elem, state),
    polyline = parse_svg_polyline(elem, state),
    polygon  = parse_svg_polygon (elem, state),
    line     = parse_svg_line    (elem, state),
    rect     = parse_svg_rect    (elem, state),
    circle   = parse_svg_circle  (elem, state),
    ellipse  = parse_svg_ellipse (elem, state),
    text     = parse_svg_text    (elem, state),
    use      = parse_svg_use     (elem, state),
    switch   = parse_svg_switch  (elem, state),
    image    = parse_svg_image   (elem, state),
    defs     = NULL,
    style    = NULL,
    metadata = NULL,
    clipPath = NULL,
    linearGradient = NULL,
    radialGradient = NULL, # pull these dynamically as needed. don't require any style cascade
    {
      if (!isTRUE(state$warned[[tag]])) {
        message("SVG contains a tag not currently handled: <", tag, ">")
        state$warned[[tag]] <- TRUE
      }

      if (state$obj_type == 'list') {
        this_list <- list(
          svg_df    = NULL,
          grid_df   = NULL,
          transform = transform,
          style     = xpstyle,
          gp        = NULL,
          grob      = nullGrob(),
          svg       = as.character(elem),
          tag       = xml2::xml_name(elem),
          xpath     = xml2::xml_path(elem)
        )
        state$obj_list <- c(state$obj_list, list(this_list))
      }


      NULL
    }
  )


  elem_grob
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Accumulate values for the different return types
#
# @params ... everything needed for data.frame and/or object return types
#
# @return NULL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
accumulate_return_obj <- function(
  state, elem, gp, points_df,
  coords_df, transform, xpstyle, this_grob
) {
  if (state$obj_type == 'data.frame') {
    this_obj       <- points_df
    style_df       <- gpar_to_df(gp)
    this_obj       <- cbind(this_obj, style_df)
    this_obj$xpath <- xml2::xml_path(elem)
    this_obj$tag   <- xml2::xml_name(elem)

    # If this is not a <path> then add in some default elements to the data.frame
    if (!'path' %in% names(this_obj)) {
      this_obj$name     <- this_obj$tag
      this_obj$path     <- ""
      this_obj$idx      <- 1L
      this_obj$path_idx <- 1L
      this_obj$inst_idx <- 1L
    }

  } else if (state$obj_type %in% c('list', 'debug')) {
    this_obj <- list(
      svg_df    = points_df,
      grid_df   = coords_df,
      transform = transform,
      style     = xpstyle,
      gp        = gp,
      grob      = this_grob,
      svg       = as.character(elem),
      clip      = state$in_clip,
      tag       = xml2::xml_name(elem),
      xpath     = xml2::xml_path(elem)
    )
  } else {
    return()
  }

  # For 'tiger.svg' there is only N=200 objects.
  # A quick experiemnt and benchmark didn't show it was
  # worth the expense of adding preallocation (+ bookkeeping +
  # growing the list). So just leaving as a simple
  # c() for now.  mikefc 2021-11-30
  state$obj_list <- c(state$obj_list, list(this_obj))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make a distinct, unlinked, independent copy of an xml node
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xml_duplicate <- function(x) {
  # xml2::read_xml(as.character(x))
  xml2::xml_unserialize(xml2::xml_serialize(x, NULL))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_switch <- function(elem, state) {

  if (!isTRUE(state$warned[['switch']])) {
    message("<switch> tag support is currently experimental. First non-NULL child element will be used.")
    state$warned[['switch']] <- TRUE
  }


  elem_grob <- NULL
  children  <- xml2::xml_children(elem)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Loop over all the child elements, and just pick the first one that is
  # not NULL or empty
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (elem in children) {
    elem_grob <- parse_svg_elem(elem, state = state)

    if (!is.null(elem_grob) && !inherits(elem_grob, 'null')) {
      break;
    }
  }

  elem_grob
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_image <- function(elem, state) {

  addr <- xml2::xml_attr(elem, "href") %||% xml2::xml_attr(elem, "xlink:href")


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find the size + location
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x_raw      <- xml2::xml_attr(elem, 'x'     , default = "0")
  y_raw      <- xml2::xml_attr(elem, 'y'     , default = "0")
  width_raw  <- xml2::xml_attr(elem, 'width' , default = "100") # SVG spec these *must* be present
  height_raw <- xml2::xml_attr(elem, 'height', default = "100") # SVG spec these *must* be present


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert to numeric values
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x      <- css_value_as_numeric(x_raw)
  y      <- css_value_as_numeric(y_raw)
  width  <- css_value_as_numeric(width_raw)
  height <- css_value_as_numeric(height_raw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert relative coords to absolute
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is_percentage(     x_raw)) x      <-      x * state$width
  if (is_percentage(     y_raw)) y      <-      y * state$height
  if (is_percentage( width_raw)) width  <-  width * state$width
  if (is_percentage(height_raw)) height <- height * state$height

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Scale for the device setting
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- xpos(x, state)
  y <- ypos(y, state)

  width  <- width  * state$scale * state$user_scale
  height <- height * state$scale * state$user_scale

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Move the coords of the image to it's centre (rather than corner)
  # This makes it easier to place tall/wide images
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- x + width/2
  y <- y - height/2


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fetch an image from a URL or data URI
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(addr)) {
    image <- NULL
  } else if (grepl("^data:.*;base64,", addr)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Decode the Base64 encoded image
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    base64_txt <- gsub("data:.*?;base64,", "", addr)

    if (requireNamespace('openssl', quietly = TRUE)) {
      obj <- openssl::base64_decode(base64_txt)
    } else {
      message("Install {openssl} for <image> support with base64 encoded images")
      image <- NULL
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # go via the file system to save the image and then load it back in
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpfile <- tempfile()
    writeBin(obj, tmpfile, size = raw())

    if (requireNamespace('magick', quietly = TRUE)) {
      image        <- magick::image_read(tmpfile)
      im_info      <- magick::image_info(image)
      image_aspect <- im_info$width / im_info$height
    } else {
      message("Install {magick} for <image> support")
      image <- NULL
    }

  } else {
    message("Loading image with {magick}: ", substr(addr, 1, 100))

    if (requireNamespace('magick', quietly = TRUE)) {
      image        <- magick::image_read(addr)
      im_info      <- magick::image_info(image)
      image_aspect <- im_info$width / im_info$height
    } else {
      message("Install {magick} for <image> support")
      image <- NULL
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Didn't get any image
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(image)) {
    return(NULL)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Aspect ratio fiddling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  display_aspect <- width/height

  if (display_aspect > image_aspect) {
    width <- width * image_aspect / display_aspect
  } else if (display_aspect < image_aspect) {
    height <- height * display_aspect / image_aspect
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create a rasterGrob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  elem_grob <- grid::rasterGrob(
    image,
    x             = x,
    y             = y,
    width         = width,
    height        = height,
    interpolate   = TRUE,
    default.units = state$default.units
  )



  elem_grob
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_use <- function(elem,  state) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # https://developer.mozilla.org/en-US/docs/Web/SVG/Element/use
  #
  # Most attributes on use do not override those already on the element
  # referenced by use.
  #
  # Only the  attributes x, y, width, height and href on the use element
  # will override those set on the referenced element.
  #
  # However, any other attributes not set on the referenced element will
  # be applied to the use element.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xpstyle <- get_xpath_style(elem, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check the <use> element actually references an ID
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ref <- xml2::xml_attr(elem, 'href') %||% xml2::xml_attr(elem, 'xlink:href') %||% NA
  if (is.na(ref)) {
    message("<use> href not set. skipping")
    print(as.character(elem))
    return()
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check the ID exists as a node in the document
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xml <- get_element_by_id(state$svg, ref)
  if (!is.na(xml)) {
    referenced_elem <- xml
  } else {
    message("<use> reference '", ref, "' not in <svg>. skipping")
    return()
  }

  deferred_tag  <- xml2::xml_name(referenced_elem)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Updating the attributes is tricky.
  # See: https://developer.mozilla.org/en-US/docs/Web/SVG/Element/use
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  override_attrs <- c('x', 'y', 'width', 'height', 'href')

  use_if_not_defined_atts <- setdiff(names(xml2::xml_attrs(elem)), c(override_attrs, 'id'))
  for (attr_name in use_if_not_defined_atts) {

    if (!xml2::xml_has_attr(referenced_elem, attr_name)) {
      value <- xml2::xml_attr(elem, attr_name)
      # message("setting ", attr_name, " = ", value)
      xml2::xml_attr(referenced_elem, attr_name) <- value
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Now Render this object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cached_style <- state$style_of_use_container
  state$style_of_use_container <- xpstyle
  this_grob <- parse_svg_elem(referenced_elem,  state)
  state$style_of_use_container <- cached_style


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Positino using viewports
  #  1. get the proposed shift from the attibutes of the <use> tag
  #  2. convert SVG coords to screen coords
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- as.numeric(xml2::xml_attr(elem, 'x', default = 0))
  y <- as.numeric(xml2::xml_attr(elem, 'y', default = 0))

  xscreen <- xpos(x, state)
  ypos2   <- function(y, state) { (-y * state$scale  + state$yoffset) * state$user_scale }
  yscreen <- ypos2(y, state)

  this_grob$vp <- viewport(x = xscreen, y = yscreen, just = c('left', 'bottom'), default.units = 'snpc')


  this_grob
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse an SVG tag of given type
#'
#' @param elem SVG element which is of known type
#' @inheritParams parse_svg_group
#'
#' @return grob
#' @importFrom stats aggregate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_path <- function(elem,  state) {

  xpstyle <- get_xpath_style(elem, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse the <path> 'd' attribute into a data.frame of x,y coords
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  path_d    <- xml2::xml_attr(elem, 'd')
  path_list <- parse_svg_path_d(path_d)
  points_df <- path_list_to_df(path_list, state = state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Even the root element could have a transform presentation attribute
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  transform_string <- paste(xpstyle$transform, collapse = " ")
  transform        <- parse_transform_string_to_matrix(transform_string)

  coords_df <- apply_transform(transform, points_df)
  coords_df <- user_adjust(coords_df, state)

  coords_df$id <- points_df$id

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare a gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp    <- style_to_gpar(xpstyle, state)
  vp    <- style_to_viewport(xpstyle, transform, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Is this a filled path?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  is_filled <- is.null(xpstyle$fill) || xpstyle$fill != 'none'

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If path is filled then draw the filled area without outline
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is_filled) {

    # Create 'gp' without an outline
    area_gp     <- gp
    area_gp$col <- 'transparent'

    # Determine fill rule
    if (identical(xpstyle$`fill-rule`, 'evenodd')) {
      rule <- 'evenodd'
    } else {
      rule <- 'winding'
    }

    # if any group has only a single point. remove it from the path
    counts <- aggregate(coords_df, list(coords_df$id), length)
    idx <- which(counts$x == 1)
    if (length(idx) > 0) {
      coords_df <- coords_df[!coords_df$id %in% idx, , drop = FALSE]
    }

    gfill <- grid::pathGrob(
      x    = coords_df$x,
      y    = coords_df$y,
      id   = coords_df$id,
      gp   = area_gp,
      vp   = vp,
      rule = rule,  # 'evenodd' or 'winding'
      default.units = state$default.units
    )
  } else {
    gfill <- grid::nullGrob()
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always draw outline.
  # Note: The polyline might not be closed, but if 'fill' is set, then the
  # version of the path that is closed would be filled.
  #
  # For closed path: polygonGrob
  # For open   path: polylineGrob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp_outline <- gp
  gp_outline$fill <- 'transparent'

  dfs <- split(coords_df, coords_df$idx)

  goutlines <- lapply(dfs, function(df) {

    closed <- df$name[length(df$name)] == 'close_path'

    if (closed) {
      goutline <- grid::polygonGrob(
        x  = coords_df$x,
        y  = coords_df$y,
        id = coords_df$id,
        gp = gp_outline,
        vp = vp,
        default.units = state$default.units
      )
    } else {
      goutline <- grid::polylineGrob(
        x  = coords_df$x,
        y  = coords_df$y,
        id = coords_df$id,
        gp = gp_outline,
        vp = vp,
        default.units = state$default.units
      )
    }
    goutline
  })

  this_grob <- do.call(grobTree, c(list(gfill), goutlines))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Accumulate return object data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  accumulate_return_obj(
    state, elem, gp, points_df,
    coords_df, transform, xpstyle, this_grob
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always return the created grob for this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_polyline <- function(elem,  state) {

  xpstyle <- get_xpath_style(elem, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse out the 'points' structure into a data.frame of coords
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  points_string <- xml_attr(elem, 'points')
  points_df <- parse_points(points_string)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate transformed coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  transform_string <- paste(xpstyle$transform, collapse = " ")
  transform        <- parse_transform_string_to_matrix(transform_string)

  coords_df <- apply_transform(transform, points_df)
  coords_df <- user_adjust(coords_df, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare a gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp    <- style_to_gpar(xpstyle, state)
  vp    <- style_to_viewport(xpstyle, transform, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # grob()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob <- grid::polylineGrob(
    x  = coords_df$x,
    y  = coords_df$y,
    gp = gp,
    vp = vp,
    default.units = state$default.units
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Accumulate return object data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  accumulate_return_obj(
    state, elem, gp, points_df,
    coords_df, transform, xpstyle, this_grob
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always return the created grob for this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_polygon <- function(elem,  state) {

  xpstyle <- get_xpath_style(elem, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse out the 'points' structure into a data.frame of coords
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  points_string <- xml_attr(elem, 'points')
  points_df <- parse_points(points_string)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate transformed coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  transform_string <- paste(xpstyle$transform, collapse = " ")
  transform        <- parse_transform_string_to_matrix(transform_string)

  coords_df <- apply_transform(transform, points_df)
  coords_df <- user_adjust(coords_df, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare a gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp    <- style_to_gpar(xpstyle, state)
  vp    <- style_to_viewport(xpstyle, transform, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # grob()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob <- grid::polygonGrob(
    x  = coords_df$x,
    y  = coords_df$y,
    gp = gp,
    vp = vp,
    default.units = state$default.units
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Accumulate return object data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  accumulate_return_obj(
    state, elem, gp, points_df,
    coords_df, transform, xpstyle, this_grob
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always return the created grob for this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_line <- function(elem,  state) {

  xpstyle <- get_xpath_style(elem, state)

  x1 <- css_value_as_numeric( xml2::xml_attr(elem, 'x1', default = 0) )
  y1 <- css_value_as_numeric( xml2::xml_attr(elem, 'y1', default = 0) )
  x2 <- css_value_as_numeric( xml2::xml_attr(elem, 'x2', default = 0) )
  y2 <- css_value_as_numeric( xml2::xml_attr(elem, 'y2', default = 0) )

  points_df <- data_frame(
    x = c(x1, x2),
    y = c(y1, y2)
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate transformed coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  transform_string <- paste(xpstyle$transform, collapse = " ")
  transform        <- parse_transform_string_to_matrix(transform_string)

  coords_df <- apply_transform(transform, points_df)
  coords_df <- user_adjust(coords_df, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare a gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp    <- style_to_gpar(xpstyle, state)
  vp    <- style_to_viewport(xpstyle, transform, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # grob()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob <- grid::linesGrob(
    x  = coords_df$x,
    y  = coords_df$y,
    gp = gp,
    vp = vp,
    default.units = state$default.units
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Accumulate return object data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  accumulate_return_obj(
    state, elem, gp, points_df,
    coords_df, transform, xpstyle, this_grob
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always return the created grob for this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob
}

is_percentage <- function(x) {
  endsWith(trimws(x), '%')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_rect <- function(elem,  state) {

  xpstyle <- get_xpath_style(elem, state)

  x      <- css_value_as_numeric(xpstyle[['x']]      %||% 0)
  y      <- css_value_as_numeric(xpstyle[['y']]      %||% 0)
  width  <- css_value_as_numeric(xpstyle[['width']]  %||% 0)
  height <- css_value_as_numeric(xpstyle[['height']] %||% 0)

  rx0  <- xpstyle[['rx']]
  ry0  <- xpstyle[['ry']] %||% rx0 %||% 0
  rx0  <- rx0 %||% ry0

  rx <- css_value_as_numeric(rx0 %||% 0)
  ry <- css_value_as_numeric(ry0 %||% 0)

  if (is_percentage(rx0)) { rx <- rx * width }
  if (is_percentage(ry0)) { ry <- ry * height }


  points_df <- create_rect_df(x, y, width, height, rx, ry, state$npoints)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate transformed coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  transform_string <- paste(xpstyle$transform, collapse = " ")
  transform        <- parse_transform_string_to_matrix(transform_string)

  coords_df <- apply_transform(transform, points_df)
  coords_df <- user_adjust(coords_df, state)



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare a gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp    <- style_to_gpar(xpstyle, state)
  vp    <- style_to_viewport(xpstyle, transform, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # grob()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob <- grid::polygonGrob(
    x      = coords_df$x,
    y      = coords_df$y,
    gp     = gp,
    vp     = vp,
    default.units = state$default.units
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Accumulate return object data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  accumulate_return_obj(
    state, elem, gp, points_df,
    coords_df, transform, xpstyle, this_grob
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always return the created grob for this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_circle <- function(elem,  state) {

  xpstyle <- get_xpath_style(elem, state)

  cx <- css_value_as_numeric(xpstyle[['cx']] %||% 0)
  cy <- css_value_as_numeric(xpstyle[['cy']] %||% 0)
  r  <- css_value_as_numeric(xpstyle[['r' ]] %||% 0)

  N <- (state$npoints %||% 8) * 4

  theta <- seq(0, 2*pi, length.out = N+1)[seq_len(N - 1)]
  points_df <- data_frame(
    x = cx + r * cos(theta),
    y = cy + r * sin(theta)
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate transformed coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  transform_string <- paste(xpstyle$transform, collapse = " ")
  transform        <- parse_transform_string_to_matrix(transform_string)

  coords_df <- apply_transform(transform, points_df)
  coords_df <- user_adjust(coords_df, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare a gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp    <- style_to_gpar(xpstyle, state)
  vp    <- style_to_viewport(xpstyle, transform, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # grob()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob <- grid::polygonGrob(
    x      = coords_df$x,
    y      = coords_df$y,
    gp     = gp,
    vp     = vp,
    default.units = state$default.units
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Accumulate return object data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  accumulate_return_obj(
    state, elem, gp, points_df,
    coords_df, transform, xpstyle, this_grob
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always return the created grob for this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_ellipse <- function(elem,  state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Params
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xpstyle <- get_xpath_style(elem, state)

  cx <- css_value_as_numeric(xpstyle[['cx']] %||% 0)
  cy <- css_value_as_numeric(xpstyle[['cy']] %||% 0)
  rx <- css_value_as_numeric(xpstyle[['rx']])
  ry <- css_value_as_numeric(xpstyle[['ry']])

  points_df <- ellipse_to_df(cx, cy, rx, ry, npoints = state$npoints * 2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate transformed coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  transform_string <- paste(xpstyle$transform, collapse = " ")
  transform        <- parse_transform_string_to_matrix(transform_string)

  coords_df <- apply_transform(transform, points_df)
  coords_df <- user_adjust(coords_df, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare a gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp    <- style_to_gpar(xpstyle, state)
  vp    <- style_to_viewport(xpstyle, transform, state)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # grob()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob <- grid::polygonGrob(
    x      = coords_df$x,
    y      = coords_df$y,
    gp     = gp,
    vp     = vp,
    default.units = state$default.units
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Accumulate return object data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  accumulate_return_obj(
    state, elem, gp, points_df,
    coords_df, transform, xpstyle, this_grob
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always return the created grob for this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname parse_svg_path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_text <- function(elem,  state) {

  xpstyle <- get_xpath_style(elem, state)

  # Maybe use gridtext::richtext_grob here?
  if (!isTRUE(state$warned[['text']])) {
    message("SVG includes text, but text support is very incomplete")
    state$warned[['text']] <- TRUE
  }

  x  <- css_value_as_numeric( xml2::xml_attr(elem,  'x', default = 0 ))
  y  <- css_value_as_numeric( xml2::xml_attr(elem,  'y', default = 0 ))
  dx <- css_value_as_numeric( xml2::xml_attr(elem, 'dx', default = 0 ))
  dy <- css_value_as_numeric( xml2::xml_attr(elem, 'dy', default = 0 ))

  label <- xml2::xml_text(elem)

  # cat("text:: ", x, y, label, "\n")

  points_df <- data_frame(
    x = x + dx,
    y = y + dy,
    label = label
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate transformed coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  transform_string <- paste(xpstyle$transform, collapse = " ")
  transform        <- parse_transform_string_to_matrix(transform_string)

  coords_df <- apply_transform(transform, points_df)
  coords_df <- user_adjust(coords_df, state)

  coords_df$label <- label

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare a gpar()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp    <- style_to_gpar(xpstyle, state)
  vp    <- style_to_viewport(xpstyle, transform, state)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # We'll render the text at (0, 0) within a viewport that's shifted to
  # the correct location.
  # By using a viewport to position the text, svgparser will be able to
  # support text rotation.
  # Steps:
  #   * create 'transform' matrix for this text
  #   * decompose the transform into rotation, translation and scale matrices
  #   * apply the scale matrix to the font size
  #   * apply translation and rotation to the viewport.
  #
  #  If there's 'skew' present in the transform for this element, then
  #  we're probably borked.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vp$justification <- c(0, 0) # bottom, left as the anchor point for positioning
  vp$valid.just    <- c(0, 0) # bottom, left as the anchor point for positioning
  vp$x             <- grid::unit(coords_df$x, state$default.units)
  vp$y             <- grid::unit(coords_df$y, state$default.units)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Basic rotation support
  #  - ref: https://math.stackexchange.com/questions/237369/given-this-transformation-matrix-how-do-i-decompose-it-into-translation-rotati
  #  - assumes that there is no scaling in x,y, and no skew
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dt       <- decompose_transform(transform)
  vp$angle <- dt$angle


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Short Explanation: SVG uses 'fill' property for text colour, but grid uses 'col'
  # Longer explanation:
  #   SVG fonts have both a stroke and fill property.  I'm not sure the best
  #   way to handle this when translating to R
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp$col <- gp$fill
  hjust <- xpstyle[['text-anchor']] %||% 'start'
  hjust <- c(start = 0, middle = 0.5, end = 1)[[hjust]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # grob()
  # <tspan> currently not done.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob <- grid::textGrob(
    label  = label,
    x      = 0, # was: coords_df$x,
    y      = 0, # was: coords_df$y,
    gp     = gp,
    vp     = vp,
    hjust  = hjust,
    vjust  = 0,
    default.units = state$default.units
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Accumulate return object data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  accumulate_return_obj(
    state, elem, gp, points_df,
    coords_df, transform, xpstyle, this_grob
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always return the created grob for this element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get the ultimate xpath style
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_xpath_style <- function(elem, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get the naively calculated style for the element in the xpath tree
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xpstyle <- state$xpath_style[[xml2::xml_path(elem)]]

  override_style<- list()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this node is being rendered as part of a "<use>" container,
  # need to override some of the attributes, but not all.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(state$style_of_use_container)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If these attributes exist on the <use> container, they override
    # anything on this current referenced element
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    override_attr_names <- intersect(
      names(state$style_of_use_container),
      c('x', 'y', 'width', 'height', 'href')
    )

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # All other attributes on the use container will not override anything.
    # But any attributes which don't already exist on the referenced element
    # will be copied across from the <use> element
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    soft_override_names <- setdiff(
      names(state$style_of_use_container),
      c(names(xpstyle), c('x', 'y', 'width', 'height', 'href'))
    )

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The full list of what we want to take from <use> and put on this element
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    override_attr_names <- c(
      override_attr_names,
      soft_override_names
    )

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Select out these particular attributes from the <use> container styles
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    override_style <- state$style_of_use_container[override_attr_names]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # clip style
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  clip_style <- state$clip_style

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Cascade these styles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xpstyle <- cssparser::css_merge(
    state$user_agent_css,
    clip_style,
    xpstyle,
    override_style
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Handle any 'currentcolor'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (identical(xpstyle[['fill']], 'currentColor')) {
    xpstyle[['fill']] <- xpstyle[['color']] %||% 'black'
  }

  if (identical(xpstyle[['stroke']], 'currentColor')) {
    xpstyle[['stroke']] <- xpstyle[['color']] %||% 'black'
  }


  xpstyle
}




if (FALSE) {

  read_svg("./working/style-inline.svg")


}


