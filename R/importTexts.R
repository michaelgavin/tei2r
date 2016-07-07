#' Import a collection of texts into R
#' 
#' Imports a collection of documents into R and performs basic text processing
#' 
#' @param dl The \code{docList} object that contains the \code{index} with
#'           the \code{paths} to the files for each text.
#'           
#' @param normalize A logical condition. If "TRUE", text will be converterd to 
#'                  all lower case and stopwords will be removed. Also, all 
#'                  instances of the long-S will be converted to s, all 
#'                  numeric characters will be removed, vv will be converted to w, 
#'                  and 'd and 'ring will be converted to 'ed' and 'ering'
#'                  respectively, and all special characters will be removed.
#' 
#' @return dl The \code{docList} object that contains the texts of the corpus,
#'            the path to the \code{indexFile} and the original \code{directory}
#'            that the \code{docList} object was built from.
#' 
#' @section What it does:
#' This function collects, cleans up, and stores
#' the text of the collection's documents in a single object. Essentially, it
#' runs the \code{\link{cleanup}} function over a folder of documents. The texts are held as
#' vectors in a single list, labeled by the corresponding id 
#' in the index file.  The id is usually the filename or the Text Creation Partnership number.
#' 
#' @seealso cleanup
#' 
#' @examples
#' dt = importTexts(dl)
#' @name importTexts
#' @export
importTexts <- function(dl, normalize = TRUE) {
  indexFile = dl@index
  fileNames = dl@filenames
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
    dl@texts[[i]] = cleanup(dl@paths[i],stopwords = dl@stopwords, normalize = normalize)
  }
  names(dl@texts) = file_path_no_ext(dl@filenames)
  return(dl)
}

