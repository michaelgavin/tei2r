#' Determine the associations (frequency of co-occurrance) for the
#' \code{keyword} that was chosen when making the \code{docConcordance}.
#' 
#' @param dc A \code{docConcordance} object
#' @param df A \code{docFrequencies} object
#' 
#' @section What it does:
#' Gets the associations across the collection for a given term.  This
#' function uses a \code{docConcordance} object to determine the frequency
#' with which words appear with the term you selected when calculating
#' the concordance.  It returns the raw frequencies for the term across
#' each document in the collection, and it also provides the total
#' across the collection as a whole.
getDocAssociations = function(dc, df) {
  da = docAssociations()
  da@directory    = df@directory
  da@indexFile    = df@indexFile
  da@keyword      = dc@keyword
  da@context      = dc@context
  da@associations = list()
  da@total        = array()
  tempText = c()
  allText = c()
  for(i in 1:length(dc@concordance)) {
    words = dc@concordance[[i]]
    allText = c(allText, words)
    tempText = rev(sort(table(unlist(words))))
    da@associations[[i]] = tempText
  }
  names(da@associations) = names(dc@concordance)
  da@total = rev(sort(table(unlist(allText))))
  return(da)
}
