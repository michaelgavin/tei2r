<<<<<<< HEAD
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
=======
texts = function(dl) {
>>>>>>> 3a3e5442e76fc34b7250acc4c6d3654100e3b6a9
  dt = getDocTexts(dl)
  return(dt)
}

<<<<<<< HEAD
#' @rdname getDocFrequencies
getFrequencies = function(dt) {
=======
frequencies = function(dt) {
>>>>>>> 3a3e5442e76fc34b7250acc4c6d3654100e3b6a9
  df = getDocFrequencies(dt)
  return(df)
}

<<<<<<< HEAD
#' @rdname getDocConcordance
getConcordance = function(dt, term, context) {
=======
concordance = function(dt, term, context) {
>>>>>>> 3a3e5442e76fc34b7250acc4c6d3654100e3b6a9
  dc = getDocConcordance(dt, term, context)
  return(dc)
}

<<<<<<< HEAD
#' @rdname getDocAssociations
getAssociations = function(dc, df) {
=======
associations = function(dc, df) {
>>>>>>> 3a3e5442e76fc34b7250acc4c6d3654100e3b6a9
  da = getDocAssociations(dc, df)
  return(da)
}

#' @rdname cleanup
cleanup = function(filepath, removeCaps = TRUE, stopwords = dl@stopwords, removeStopwords = TRUE) {
  text = textCleanup(filepath, removeCaps = TRUE, stopwords = dl@stopwords, removeStopwords = TRUE)
  return(text)
}
