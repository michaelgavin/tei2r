#====================================================================
# 
#====================================================================
#' Build a concordance for a keyword
#' 
#' @param dt A \code{docTexts} object that contains your collection
#'        of texts.
#' @param keyword The keyword that you are studying.
#' @param context The number of words on each side of the 
#'        \code{keyword} to search for.
#'        
#' @section What it does:
#' Builds a concordance for a keyword, using keyword-in-context (KWIC) technique.
#' The first parameter is a \code{docTexts} object that contains all of your texts. 
#' The second parameter is the keyword you are interested studying. The third gives the
#' size of the context
#' that you want to use for the concordance: i.e., how many words on each side of 
#' your keyword that you want to look at. The context is sometimes referred to as the
#' 'search window'.
#' @return A \code{docConcordance} object: essentially a structured list that contains
#' every use of the keyword in the collection.
#' @examples
#' dc = buildConcordance(dt, "justice", 5)
#' @export
buildConcordance = function(dt, keyword, context) {
  dc = docConcordance()
  dc@directory   = dt@directory
  dc@indexFile   = dt@indexFile
  dc@keyword     = keyword
  dc@context     = context
  dc@concordance = list()
  for(i in 1:length(dt@text)) {
    text = dt@text[[i]]
    hits = which(text == keyword)
    concordance = list()
    #browser()
    #stringLen = context * 2 + 1
    if(keyword %in% text) {
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
  dc@frequencies = table(unlist(dc@concordance))
  return(dc)
}

# #' @rdname buildConcordance
# #' @export
# concordance = function(dt, keyword, context) {
#   dc = buildConcordance(dt, keyword, context)
#   return(dc)
# }
