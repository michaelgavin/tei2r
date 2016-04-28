#====================================================================
#  This function collects, cleans up, and stores the text of the
#  corpus' documents in a single object in which the text is labeled
#  by the corresponding ID in the index file (usually the file name
#  sans extension).
#====================================================================
#' Import a collection of texts into R
#' 
#' Imports a collection of documents into R and performs basic text processing
#' 
#' @param dl The \code{docList} object that contains the \code{index} with
#'           the \code{paths} to the files for each text.
#'           
#' @param removeCaps Option to remove capitalization. Default is 'TRUE'.
#' 
#' @param removeStopwords Option to remove stopwords, stored in the corresponding \code{docList} object. Default is 'TRUE'.
#' 
#' @param normalizeLongS Option to normalize '∫' and 'ſ' to 's'. Default is 'TRUE'. 
#' 
#' @return dt The \code{docTexts} object that contains the texts of the corpus,
#'            the path to the \code{indexFile} and the original \code{directory}
#'            that the \code{docList} object was built from.
#' 
#' @section What it does:
#' This function collects, cleans up, and stores
#' the text of the collection's documents in a single object. The texts are held as
#' vectors in a single list, labeled by the corresponding id 
#' in the index file.  The id is usually the filename or the Text Creation Partnership number.
#' 
#' @seealso docTexts
#' 
#' @examples
#' dt = importTexts(dl)
#' @name importTexts
#' @export
importTexts <- function(dl, removeCaps = TRUE, removeStopwords = TRUE, normalizeLongS = TRUE) {
  dt = docTexts()
  indexFile = dl@index
  fileNames = dl@filenames
  dt@directory = dl@directory
  dt@indexFile = dl@indexFile
  prog = 1
  last = 0
  print("Compiling and cleaning up your texts.")
  for (i in 1:nrow(dl@index)) {
    #for (i in 1:15) {
    total = nrow(dl@index)
    percent = (i/total) * 100
    percents = c(10,20,30,40,50,60,70,80,90,100)
    if (round(percent) %in% percents && round(percent) != last) {
      print(paste(round(percent), "% complete!", " Document ", i, " of ", total, ".", sep=""))
      last = round(percent)
    }
    #print(paste("Importing Document: ", i, " of ", total, sep=""))
    dt@text[[i]] = cleanup(dl@paths[i],stopwords = dl@stopwords)
  }
  names(dt@text) = file_path_sans_ext(dl@filenames)
  return(dt)
}

