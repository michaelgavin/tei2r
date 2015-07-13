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
getTexts = function(dl) {
  dt = getDocTexts(dl)
  return(dt)
}

#' @rdname getDocFrequencies
getFrequencies = function(dt) {
  df = getDocFrequencies(dt)
  return(df)
}

#' @rdname getDocConcordance
getConcordance = function(dt, term, context) {
  dc = getDocConcordance(dt, term, context)
  return(dc)
}

#' @rdname getDocAssociations
getAssociations = function(dc, df) {
  da = getDocAssociations(dc, df)
  return(da)
}

#' @rdname cleanup
cleanup = function(filepath, removeCaps = TRUE, stopwords = dl@stopwords, removeStopwords = TRUE) {
  text = textCleanup(filepath, removeCaps = TRUE, stopwords = dl@stopwords, removeStopwords = TRUE)
  return(text)
}
