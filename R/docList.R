#' An S4 class to represent a document collection
#' 
#' \code{docList} returns a special \code{tei2r} object called ...
#' 
#' @slot directory A string that gives the filepath to the main directory (folder)
#' @slot filenames A vector ...
#' @slot paths A vector...
#' @slot indexFile A string...
#' @slot index A data frame...
#' @slot stopwordsFile A string...
#' @slot stopwords A vector...
#' @slot text A list (EXCEPT WE'RE NOT USING 'TEXT' SLOT.)
#' 
#' @section What it does:
#' Here we can paste longer descriptions of what each function does.
docList <- setClass("docList", 
                    slots = c(directory = "character",
                              filenames = "character",
                              paths = "character",
                              indexFile = "character",
                              index = "data.frame",
                              stopwordsFile = "character",
                              stopwords = "character",
                              text = "list"))


