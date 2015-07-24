#' An S4 class that contains and manages word-frequency data
#'  
#' \code{docMatrix} creates a special \code{tei2r} object called
#' 
#' @slot directory A string ...
#' @slot indexFile A string [NOTE: THIS SHOULD BE CALLED indexFile.
#' @slot type A string ... [RENAME 'RAW']
#' @slot mat A matrix ...
#' 
#' @section What it does:
#' Here we can paste longer descriptions of what each function does.
#' @include docList.R docTexts.R
docMatrix <- setClass("docMatrix",
                           slots = c(directory        = "character",
                                     indexFile        = "character",
                                     type             = "character",
                                     mat              = "matrix"
                           ))
