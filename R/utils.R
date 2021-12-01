

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NULL or NA operator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"%||%" <- function(x, y) {
  if (is.null(x) || is.na(x)) {
    y
  } else {
    x
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Trim the end off a word.
#'
#' @param x string
#' @param len number of characters to trim
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
trim <- function(x, len) {
  substr(x, 1, max(nchar(x) - len, 1))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simpler version of 'modifyList()'.  Idea borrowed from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
modify_list <- function (base, new) {
  for (i in names(new)) base[[i]] <- new[[i]]
  base
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' data.frame creation
#'
#' 20x Faster than 'data.frame()', but with *ZERO* sanity checking and
#' no support for row.names
#'
#' @param ... named arguments
#'
#' @return data.frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_frame <- function(...) {
  ll <- list(...)
  structure(
    ll,
    class     = 'data.frame',
    row.names = .set_row_names(length(ll[[1]]))
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get an element from an XML document by ID. Return NA if ID does not exist.
#'
#' This function is called when a \code{<use>} tag references another element
#' or a a colour tag references a gradient element.
#'
#' @param svg xml document containing svg
#' @param id id, #id, url(#id), url('#id')
#'
#' @return SVG element with the given id, otherwise NA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_element_by_id <- function(svg, id) {

  # Sanity check that 'svg' and 'id' are valid
  if (is.null(svg) || is.null(id) || is.na(id)) {
    return(NA)
  }

  # Convert 'url(#id)' referees to just the 'id' part which is all
  # xml2 wants.
  id    <- gsub("url\\('#|url\\(#|'\\)|\\)|#", "", id)

  # Convert the id to an xpath
  xpath <- paste0("//*[@id='", id, "']")

  # featch the element in the svg
  xml   <- xml2::xml_find_first(svg, xpath)

  xml
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Internal function for perfroming do.call(rbind...)
#'
#' This function expands all data.frames to have the same columns, whereas
#' base R just complains a throws an error
#'
#' @param dfs list of data.frames
#'
#' @return single data.frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rbind_dfs <- function(dfs) {

  # Drop NULLs
  dfs <- Filter(Negate(is.null), dfs)

  # Compile full list of all column names in all data.frames.
  # TODO: Could do something tricky here to try and retain column ordering?
  all_names <- unique(unlist(lapply(dfs, names)))

  # Expand all data.frames to include all column names
  dfs <- lapply(dfs, function(df) {
    new_names <- setdiff(all_names, names(df))
    if (length(new_names)) {
      print(new_names)
    }
    for (name in new_names) {
      df[[name]] <- NA
    }
    df
  })

  # rbind should complain now as all data.frames are guaranteed to have the
  # same column names
  do.call(rbind, dfs)
}







