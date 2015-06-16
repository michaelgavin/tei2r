#====================================================================
#
#
#
#
#
#====================================================================
#'
#'
#' @param directory       A string that is the path to the directory where the
#'                        files that make up your corpus are located.
#' @param stopwordsFile
#' @param indexFile
#' @param includeTexts
#' @param wizard
#' 
#' @examples
#' Placeholder
buildDocList = function(directory = "", stopwordsFile = "", indexFile ="", includeTexts = FALSE, wizard=FALSE) {
  dl = docList()
  if(wizard == FALSE) {
    dl@directory = directory
    if(indexFile == "") {
      print("It seems like we don't have an index file for you.  Building one now.")
      dl@indexFile = buildIndex(dl@directory)
      dl@index = read.csv(dl@indexFile,stringsAsFactors=FALSE)
      dl@filenames = dl@index$filenames
      dl@paths = dl@index$paths
    } else {
      dl@indexFile = indexFile
      #browser()
      dl@index = read.csv(dl@indexFile,stringsAsFactors=FALSE)
      dl@filenames = findFilenames(dl = dl, directory = directory)
      dl@paths = paste(dl@directory,"/",dl@filenames,sep="")
    }
    dl@stopwordsFile = stopwordsFile
    dl@stopwords = setStopwords(stopwordsFile)
  } else {
    dl = useWizard(dl)
  }
  #if (includeTexts == TRUE) {
  # for (i in 1:nrow(dl@index)) {
  #  if (length(grep(".txt",dl@paths[i])) == 1) {
  #   dl@text[[i]] = textCleanup(dl@paths[i],stopwords = dl@stopwords)
  #}
  #if (length(grep(".xml",dl@paths[i])) == 1) {
  #  dl@text[[i]] = teiTextCleanup(dl@paths[i],stopwords = dl@stopwords)
  #}
  #}
  #names(dl@text) = dl@filenames
  #}
  return(dl)
}
