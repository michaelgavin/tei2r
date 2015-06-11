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
  for (i in 1:nrow(dl@index)) {
    #for (i in 1:15) {
    total = nrow(dl@index)
    percent = total/i
    percents = c(10,20,30,40,50,60,70,80,90,100)
    prog = 0
    if (round(percent) %in% percents) {
      if (prog != 0 && percents[prog] != percent) {
        print(paste(round(precent), "% complete!", sep=""))
        prog = prog + 1
      } else if(prog == 0) {
        print(paste(round(precent), "% complete!", sep=""))
        prog = prog + 1
      }
    }
    #print(paste("Importing Document: ", i, " of ", total, sep=""))
    dt@text[[i]] = textCleanup(dl@paths[i],stopwords = dl@stopwords)
  }
  names(dt@text) = file_path_sans_ext(dl@filenames)
  return(dt)
}