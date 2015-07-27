#' Calculate word frequencies across a collection
#' 
#' Get the frequency of all words in a collection.
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
#' collection, represented by a \code{docTexts} object.  Also (optionally) 
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
  vocabAnswer = readline("Would you like to determine the vocabulary for the corpus? [yes/no] > ")
  if(vocabAnswer == "yes" || vocabAnswer == "Yes") {
    print("Determining vocabulary for corpus.  Accessible through df@vocabulary.")
    freqs = rev(sort(table(unlist(dt@text))))
    #browser()
    df@vocabulary = freqs
  }
  # Get proportional vocab
  propsAnswer = readline("Would you like to calculate the proportional frequency for each term across the corpus? (This may take quite some time) [yes/no] > ")
  if(propsAnswer == "yes" && vocabAnswer == "yes") {
    print("Determining proportional frequency for each word in the vocabulary accross the whole corpus.  Accessible through df@proportionalVocab")
    total = sum(df@vocabulary)
     for(i in 1:nrow(df@vocabulary)){
       df@proportionalVocab[i] = ((df@vocabulary[i]) / total)
     }
    #df@proportionalVocab = (df@vocabulary/total)
    names(df@proportionalVocab) = names(df@vocabulary)
  } else if(propsAnswer == "yes" && vocabAnswer == "no") {
    print("We'll need to calculate the vocabulary for the corpus first.")
    ans = readline("Would you like to do that? [yes/no] > ")
    if (ans == "yes") {
      print("Determining vocabulary for corpus.  Accessible through df@vocabulary.")
      freqs = rev(sort(table(unlist(dt@text))))
      #browser()
      df@vocabulary = freqs
      print("Determining proportional frequency for each word in the vocabulary accross the whole corpus.  Accessible through df@proportionalVocab")
      total = sum(df@vocabulary)
      for(i in 1:nrow(df@vocabulary)){
        df@proportionalVocab[i] = ((df@vocabulary[i]) / total)
      }
      names(df@proportionalVocab) = names(df@vocabulary)
    } else {
      print("Skipping proportional frequencies.")
    }
  } else {
    print("skipping proportional frequencies.")
  }
  return(df)
}
