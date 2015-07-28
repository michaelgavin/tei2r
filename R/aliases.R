
#====================================================================
# This file contains aliases for the more complicated names of our
# other functions
#====================================================================
#'
#' This file contains aliases for the more complicated names of our
#' other functions.
#' 
#' @name aliases
NULL
#' @rdname getDocTexts
#' @export
texts = function(dl) {
  dt = getDocTexts(dl)
  return(dt)
}

#' @rdname getDocFrequencies
#' @export
frequencies = function(dt) {
  df = getDocFrequencies(dt)
  return(df)
}

#' @rdname buildConcordance
#' @export
concordance = function(dt, keyword, context) {
  dc = buildConcordance(dt, keyword, context)
  return(dc)
}

#' @rdname getDocAssociations
#' @export
associations = function(dc, df) {
  da = getDocAssociations(dc, df)
  return(da)
}

#' @rdname cleanup
#' @export
cleanup = function(filepath, removeCaps = TRUE, stopwords = dl@stopwords, removeStopwords = TRUE) {
  text = textCleanup(filepath, removeCaps = TRUE, stopwords = dl@stopwords, removeStopwords = TRUE)
  return(text)
}
