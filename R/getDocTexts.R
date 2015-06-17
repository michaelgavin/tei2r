#====================================================================
#  This function collects, cleans up, and stores the text of the
#  corpus' documents in a single object in which the text is labeled
#  by the corresponding ID in the index file (usually the file name
#  sans extension).
#====================================================================
#' 
#' This function collects, cleans up (removes stopwords), and stores
#' the text of the corpus' documents in a single object in which the
#' text is labeled by the corresponding \code{id} in the \code{index}
#' file.  This is usually the filename sans extension or the \code{tcp}
#' number.
#' 
#' @param dl The \code{docList} object that contains the \code{index} with
#'           the \code{paths} to the files that house the text.
#' 
#' @return dt The \code{docTexts} object that contains the texts of the corpus,
#'            the path to the \code{indexFile} and the original \code{directory}
#'            that the \code{docList} object was constructed with.  This information
#'            enables the user to keep track of which objects are associated with
#'            which corpus.
#' @examples
#' dt = getDocTexts(dl)
getDocTexts <- function(dl) {
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
    dt@text[[i]] = textCleanup(dl@paths[i],stopwords = dl@stopwords)
  }
  names(dt@text) = file_path_sans_ext(dl@filenames)
  return(dt)
}
