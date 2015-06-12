#====================================================================
# Get the associations across the corpus for a given term.  This
# function uses a docConcordance object to determine the frequency
# with which words appear with the term you selected when calculating
# the concordance.  It returns both raw associations or frequencies
# and proportional associations that are a function of the word's
# cooccurrance with the main term divided by the percentage of the
# word's weight accross the document.
#====================================================================
#' Determine the associations (frequency of co-occurrance) for the
#' \code{term} that was chosen when making the \code{docConcordance}
#' in both raw count and proportion across the corpus.
#' 
#' @param dc
#' @param df
getDocAssociations = function(dc, df) {
  da = docAssociations()
  da@directory    = df@directory
  da@indexFile    = df@indexFile
  da@term         = dc@term
  da@context      = dc@context
  da@associations = list()
  da@proportions  = list()
  tempText = ""
  for(i in 1:length(dc@concordance)) {
    tempText = unlist(dc@concordance[[i]])
    da@associations[[i]] = rev(sort(table(tempText)))
  }
  names(da@associations) = names(dc@concordance)
  proportions = list()
  for(i in 1:length(da@associations)) {
    words = names(da@associations[[i]])
    #browser()
    wordProps = unlist(df@proportionalVocab[words])
    da@proportions[[i]] = (da@associations[[i]]/wordProps)
  }
  names(da@proportions) = names(da@associations)
  return(da)
}
