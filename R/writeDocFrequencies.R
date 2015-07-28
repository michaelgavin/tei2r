#====================================================================
# This funciton will write a docFrequencies object to a comma
# seperated values (csv) file.
#====================================================================
#' This function will write a \code{docFrequencies} object to a comma
#' seperated values \code{.csv} file.  The values are derived from a
#' \code{docFrequencies} object and use the \code{raw} slot.  Each
#' document will have a seperate set of columns that are the word
#' and frequency.  Each set of columns is labeled with the document's
#' ID that is derived from the index file.  The file is stored in the
#' directory that was provided when you built your \code{docList}.
#' 
#' @param df The \code{docFrequencies} object that holds your frequency values.
#' @param filename The name of the file you want the frequency tables to be saved to.
#'        \strong{requires} that the file eextension be a .csv file.  Stored in the \code{directory}
#'        provided in the \code{docList} object.
#' @param limit The number of words to include from each document.
#' @export
writeDocFrequencies = function(df, filename, limit=100) {
  #write.table(as.matrix(t(names(df@raw))), file = paste(df@directory, filename, sep="/"), append=F, 
  #                          col.names=F)
  fullD = data.frame()
  for(i in 1:length(df@raw)) {
    word = c()
    frequency = c()
    space = c()
    data = rev(sort(df@raw[[i]]))[1:limit]
    word = names(data)
    frequency = data
    space = rep("", limit)
    if(i == 1) {
      fullD = data.frame(word, frequency, space)
    } else {
      d = data.frame(word, frequency, space, stringsAsFactors=F)
      fullD = cbind(fullD, d)
    }
    
    #browser()
  }
  header = matrix("",1,length(df@raw)*3)
  for(i in 1:length(df@raw)) {
    position = ((i - 1) * 3) + 1
    header[1,position] = names(df@raw)[i]
  }
  #header = as.matrix(t(names))
  #browser()
  names(fullD) = rep(c("word", "frequency", ""), length(df@raw))
  write.table(header, file = paste(df@directory, filename, sep="/"), append=F, col.names=F, row.names=F, sep=",")
  write.table(fullD, file = paste(df@directory, filename, sep="/"), append=T, col.names=T, row.names=F, sep=",")
}
