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
#'                        \code{cleanup} function.
#' @param indexFile       A string that is the path to the index file for the
#'                        corpus.  This function expects to find a .csv file
#'                        that contains the metadata for your corpus.  At the
#'                        very least, it needs to have a column with \code{id}
#'                        numbers that correspond to filenames or vice versa.
#'                        If you do not have an index file, this parameter can
#'                        be left blank and the \code{buildIndex} function will
#'                        be called to construct one based on the files found in
#'                        \code{directory}.
#' @param wizard          A boolean value that determines whether the wizard function
#'                        will be used.  If this value is true, you will be prompted
#'                        for your corpus' directory, asked if you would like to build
#'                        an index, and if you have a stopwords file.  You may provide
#'                        the directory in addition to activating the wizard.
#'                        
#' @return dl             The completed \code{docList} object for use with the other
#'                        functions of the \code{tei2r} package.
#' 
#' @examples
#' dl = buildDocList(directory = "~/path/to/your/corpus/files", stopwordsFile = "~/path/to/your/stopwords/file", indexFile = "~/path/to/your/index/File/")
#' dl = buildDocList(wizard = T)
#' dl = buildDocList(directory="~/path/to/your/corpus/files", wizard=T)
#' dl = buildDocList(directory = "~/path/to/your/corpus/files", stopwordsFile = "~/path/to/your/stopwords/file")
#' @export
buildDocList = function(directory = "", stopwordsFile = "", indexFile ="", wizard=FALSE) {
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
    if(stopwordsFile != "") {
      dl@stopwords = setStopwords(stopwordsFile)
    }
  } else {
    dl = useWizard(dl, directory)
  }
  return(dl)
}
