#====================================================================
#
#
#
#
#====================================================================
#' 
#' Placeholder
#' 
#' @param dl
#' 
#' @examples
#' placeholder
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
