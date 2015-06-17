#' An S4 class to represent a document collection.
#' 
#' \code{docList} returns a special \code{tei2r} object that contains
#' all of the necessary information to begin working with a corpus of
#' text in R.  
#' 
#' @slot directory      A string that gives the filepath to the main directory (folder)
#' @slot filenames      A vector containing all of the filenames for the documents in
#'                      the corpus.
#' @slot paths          A vector containing the full path to all files in the corpus.
#' @slot indexFile      A string that gives the filepath to the index file for the
#'                      corpus.  This file should house the meta-data for each file
#'                      in the corpus.
#' @slot index          A data frame that holds the meta-data for each document in
#'                      the corpus.  This data frame is created by reading the file
#'                      found at \code{indexFile.}
#' @slot stopwordsFile  A string that gives the filepath to the file that contains
#'                      a comma seperated list of words to be removed during text
#'                      cleanup.
#' @slot stopwords      A vector derived from the \code{stopwordsFile} that is passed
#'                      to the text cleanup functions in order for them to be removed
#'                      from the text.
#' 
#' @section What it does:
#' The \code{docList} is the foundaiton of the \code{tei2r}
#' package and, as such, should be the first object created when working
#' with the package.  The object is constructed by calling the
#' \code{buildDocList} function.  This function builds the object by
#' storing the path to the corpus' files (\code{directory}), the
#' file containing the corpus' meta-data (\code{indexFile}), and
#' the stopwords file (\code{stopwordsFile}).  From these pieces
#' of information, the function automatically determines the
#' \code{filenames} and \code{paths} for the corpus' files.
docList <- setClass("docList", 
                    slots = c(directory = "character",
                              filenames = "character",
                              paths = "character",
                              indexFile = "character",
                              index = "data.frame",
                              stopwordsFile = "character",
                              stopwords = "character"
                            ))


