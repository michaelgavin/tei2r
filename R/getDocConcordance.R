#====================================================================
# Get document concordance for a term.  The first parameter is a 
# docTexts object that contains all of the texts that you want
# to find the concordance for term in. The second parameter is
# the term that you are interested in.  The third is the context
# that you want to use for the concordance, the value is the 
# number of words on each side of the term that you want to return.
#====================================================================
#' Get the concordance for \code{term} within \code{context} across
#' all texts in \code{dt}.
#' 
#' @param dt A \code{docTexts} object that contains your collection
#'        of texts.  These are the texts that will be used when 
#'        searching for \code{term}s concordance (the words that
#'        appear within a certain \code{context} of \code{term}.
#' @param term The term that you are interested in finding the
#'        concurring words for across the text.
#' @param context The number of words on each side of the 
#'        \code{term}.
#' @examples
#' dc = getDocConcordance(dt, "just", 5)
#'

getDocConcordance = function(dt, term, context) {
  dc = docConcordance()
  dc@directory   = dt@directory
  dc@indexFile   = dt@indexFile
  dc@term        = term
  dc@context     = context
  dc@concordance = list()
  for(i in 1:length(dt@text)) {
    text = dt@text[[i]]
    hits = which(text == term)
    concordance = list()
    #browser()
    #stringLen = context * 2 + 1
    if(term %in% text) {
      for(j in 1:length(hits)) {
        start = hits[j] - context
        end = hits[j] + context
        if(!is.na(start) && start < 1) {
          start = 1
        }
        concordance[[j]] = text[start:end]
      }
    } else {
      concordance = 'NA'
    }
    dc@concordance[[i]] = concordance
    #print(length(dc@concordance))
  }
  names(dc@concordance) = names(dt@text)
  return(dc)
}
