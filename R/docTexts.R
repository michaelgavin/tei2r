#' An S4 class that contains and manages text data from document collections
#' 
#' \code{docTexts} creates a special \code{tei2r} object called
#' 
#' @slot directory A string ...
#' @slot indexFile A string [NOTE: THIS SHOULD BE CALLED indexFile.
#' @slot text A list ...
#' 
#' @section What it does:
#' Here we can paste longer descriptions of what each function does.
#' @include docList.R
docTexts <- setClass("docTexts",
                     slots = c(directory = "character",
                               indexFile = "character",
                               text = "list"
                     ))

