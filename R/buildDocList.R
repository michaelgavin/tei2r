#====================================================================
# This function constructs a docList object.  The docList object is
# the foundation of the tei2r package and contains references to all
# of the basic information that is required to begin working with
# texts in R.  It holds corpus meta-data, the stopwords list, the
# corpus' home directory, filenames, and paths.
#====================================================================
#'
#' Create a structured list of your collection's metadata
#' 
#' @section Description:
#' 
#' The \code{docList} object is the foundation of the \code{tei2r} package and contains references
#' to all of the basic information that is required to begin working with
#' texts in R. The \code{buildDocList} function is designed to construct the \code{docList}
#' object with as much or as little information available.  You can begin this
#' process by simply including the parameter \code{wizard = T} which will walk
#' you through the process of building your intial \code{docList}.
#'
#' @param directory       A string that is the path to the directory where the
#'                        files that make up your corpus are located.
#' @param stopwordsFile   A string that is the path to the file that contains
#'                        the words that are to be removed from the text in the
#'                        \code{cleanup} function. If left blank, the default 
#'                        stopwords will be provided.
#' @param indexFile       A string that is the path to the index file for the
#'                        collection.  This function expects to find a .csv file
#'                        that contains the metadata for your collection, including
#'                        a column that points to the names of the files (with or
#'                        without the file extensions). If you do not have an index 
#'                        file, this parameter can be left blank and the collection will
#'                        take all files in the directory.
#' 
#'                        
#' @return dl             The completed \code{docList} object for use with the other
#'                        functions of the \code{tei2r} package.
#' 
#' @examples
#' dl = buildDocList(directory="~/path/to/your/collection/files")
#' dl = buildDocList(directory = "~/path/to/your/collection/files", stopwordsFile = "~/path/to/your/stopwords/file", indexFile = "~/path/to/your/index/File/")
#' @export
buildDocList = function(directory = "", stopwordsFile = "", indexFile ="") {
  dl = docList()
  if(directory == "") {
    stop("You must enter the correct path to your directory; that is, the
         folder that contains your document collection. Be sure to have
         the path correct.")
  } else {
    dl@directory = directory    
  }
  if(indexFile == "") {
    print("No index file provided. Using file names as identifiers.")
    dl@indexFile = ""
    dl@index = data.frame(dir(directory))
    dl@filenames = dir(directory)
    dl@paths = paste(directory,dl@filenames,sep="")
  } else {
    dl@indexFile = indexFile
    #browser()
    dl@index = read.csv(dl@indexFile,stringsAsFactors=FALSE)
    dl@filenames = findFilenames(dl = dl, directory = directory)
    dl@paths = paste(dl@directory,"/",dl@filenames,sep="")
  }
  dl@stopwordsFile = stopwordsFile
  if(stopwordsFile != "") {
    dl@stopwords = setStopwords(stopwordsFile)
  } else {
    data(stopwords)
    dl@stopwords = stopwords
  }
return(dl)
}
