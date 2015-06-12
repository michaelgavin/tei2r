#====================================================================
#
#
#
#
#====================================================================
#' 
#' Placeholder
#' 
#' @param dt
#' @param limit
#' 
#' @examples
#' placeholder
getDocFrequencies = function(dt, limit=0) {
  df = docFrequencies()
  df@directory         = dt@directory
  df@indexFile         = dt@indexFile
  df@raw               = list()
  df@proportional      = list()
  df@vocabulary        = array()
  df@proportionalVocab = list()
  print("Calculating raw frequencies for each text.  Accessible by df@raw")
  for (i in 1:length(dt@text)) {
    if (limit == 0) {
      df@raw[[i]] = rev(sort(table(dt@text[i])))
    } else {
      df@raw[[i]] = rev(sort(table(dt@text[i])))[1:limit]
    }
  }
  names(df@raw) = names(dt@text)
  print("Have calculated raw frequencies, moving on to proportional for each term in each text. Accessible by df@proportional.")
  for (i in 1:length(df@raw)) {
    total = sum(df@raw[[i]])
    df@proportional[[i]] = ((df@raw[[i]]) / total) #* 100
  }
  names(df@proportional) = names(dt@text)
  
  # Get the vocabulary
  print("Determining vocabulary for corpus.  Accessible through df@vocabulary.")
  freqs = rev(sort(table(unlist(dt@text))))
  #browser()
  df@vocabulary = freqs
  # Get proportional vocab
  print("Determining proportional frequency for each word in the vocabulary accross the whole corpus.  Accessible through df@proportionalVocab")
  total = sum(df@vocabulary)
  for(i in 1:nrow(df@vocabulary)){
    df@proportionalVocab[i] = ((df@vocabulary[i]) / total)
  }
  names(df@proportionalVocab) = names(df@vocabulary)
  return(df)
}
