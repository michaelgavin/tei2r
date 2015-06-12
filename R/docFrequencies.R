#' An S4 class that contains and manages word-frequency data
#'  
#' \code{docFrequencies} creates a special \code{tei2r} object called
#' 
#' @slot directory A string ...
#' @slot indexFile A string [NOTE: THIS SHOULD BE CALLED indexFile.
#' @slot raw A list ... [RENAME 'RAW']
#' @slot proportional A list ...
#' @slot vocabulary A list that holds the frequencies of each word across
#'                  the whole corpus.
#' @slot proportionalVocab A list that holds the proportional frequency of
#'                         of each word in the corpus. A funciton of the
#'                         word's frequency divided by the total number
#'                         of words in the corpus.
#' 
#' @section What it does:
#' Here we can paste longer descriptions of what each function does.
#' @include docList.R docTexts.R
docFrequencies <- setClass("docFrequencies",
                           slots = c(directory         = "character",
                                     indexFile         = "character",
                                     proportional      = "list",
                                     raw               = "list",
                                     vocabulary        = "array",
                                     proportionalVocab = "list"
                           ))

