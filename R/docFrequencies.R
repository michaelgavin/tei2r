#' An S4 class that contains and manages word-frequency data
#'  
#' \code{docFrequencies} creates a special \code{tei2r} object called
#' 
#' @slot directory A string ...
#' @slot indexFile A string [NOTE: THIS SHOULD BE CALLED indexFile.
#' @slot rawt A list ... [RENAME 'RAW']
#' @slot proportional A list ...
#' 
#' @section What it does:
#' Here we can paste longer descriptions of what each function does.
#' @include docList.R docTexts.R
docFrequencies <- setClass("docFrequencies",
                           slots = c(directory = "character",
                                     indexFile     = "character",
                                     proportional = "list",
                                     raw = "list"
                           ))

