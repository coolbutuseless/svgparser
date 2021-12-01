


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a named list to store computed styles indexed by the xpath of each element
#'
#' @param svg root node of svg
#' @param user_css user supplied CSS (as a single text string)
#'
#' @return list of style lookup by xpath
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_calc_all_styles <- function(svg, user_css = NULL) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find all the <style> blocks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  style_blocks <- xml2::xml_find_all(svg, "//style")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert all <style> blocks to rules
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  style_lists <- lapply(style_blocks, function(block) {
    cssparser::read_css(xml2::xml_text(block))
  })

  if (!is.null(user_css)) {
    if (is.character(user_css) && length(user_css) == 1) {
      user_css <- cssparser::read_css(user_css)
      style_lists <- c(style_lists, list(user_css))
    } else {
      stop("css_calc_all_styles(): 'user_css' should be single character string")
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collapse the rules into a single ruleset
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rules <- do.call(cssparser::css_merge, style_lists)
  if (is.null(rules)) {
    rules <-  list()
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Traverse the SVG and create
  #  - xpath_styles - lookup table of styles which is indexed by the xpath of the element
  #  - svg with styles applied inline.  This is needed for "LinearGradients"
  #    and many other elements where this parser might assume that the full
  #    style is available on the element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  list(
    xpath_style = cssparser::css_apply(svg, rules, svg = TRUE),
    svg         = cssparser::css_apply_inline(svg, rules)
  )
}


