#' An S4 class that contains and manages text data
#' 
#' \code{docTexts} creates a special \code{tei2r} object that holds the
#' texts of a collection
#' 
#' @slot directory A string that gives the filepath to the main directory 
#' @slot indexFile A string that gives the filepath to the index file for the
#'                      collection.
#' @slot text A list of character vectors, each drawn from documents in the collection,
#'            and each placed in the order provided by the index.
#' 
#' @section What it does:
#' The \code{docTexts} object is one of the primary objects in \code{tei2r}. It
#' holds all of your text data in a structured list, controlled by an index.
#' 
#' @include docList.R
docTexts <- setClass("docTexts",
                     slots = c(directory = "character",
                               indexFile = "character",
                               text = "list"
                     ))

