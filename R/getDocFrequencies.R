#' Calculate word frequencies across a collection
#' 
#' Get the frequencies of all words in a collection.
#' 
#' @param dt      A \code{docTexts} object that contains the full
#'                text of each document in your corpus.
#' @param limit   A numeric value that limits the number of words
#'                to find the frequency of in each document in
#'                \code{dt}. Defaults to all terms.
#'                
#' @return df     a \code{docFrequencies} object with frequency
#'                data for the corpus.
#' 
#' @section What it does:
#' Get term frequency data for the terms in each document across the
#' collection, represented by a \code{docTexts} object.  Also 
#' determines the frequency and proportional 
#' frequency of all terms across the corpus. The first parameter is a
#' \code{docTexts} object containing the full cleaned text for each document
#' in your colleciton.  The second is a limit on the number of terms to
#' include from each document in the frequency lists, in case what you're
#' looking for are only the most frequent words.
#' 
#' @return Returns a docFrequencies object.
#' @examples
#' df = getDocFrequencies(dt)
#' df = getDocFrequencies(dt, 500)
#' df = getDocFrequencies(dt=dt, limit=1000)
getDocFrequencies = function(dt, limit=0) {
  df = docFrequencies()
  df@directory         = dt@directory
  df@indexFile         = dt@indexFile
  df@raw               = list()
  df@proportional      = list()
  df@vocabulary        = array()
  df@proportionalVocab = array()
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
  df@vocabulary = freqs

  # Get proportional vocab
  print("Determining proportional frequency for each word in the vocabulary accross the whole corpus.  Accessible through df@proportionalVocab")
  total = sum(df@vocabulary)
  df@proportionalVocab = df@vocabulary / total
  return(df)
}
