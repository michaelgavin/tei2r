#' An S4 class that holds a concordance for one term across document collections
#' 
#' \code{docConcordance} creates a special \code{tei2r} object called
#' 
#' @slot directory A string ...
#' @slot indexFile A string [NOTE: THIS SHOULD BE CALLED indexFile.
#' @slot term A string ...
#' @slot context An integer [This probably doesn't matter, but it should
#'   be an integer rather than a numeric]
#' @slot concordance A list...
#' 
#' @section What it does:
#' Here we can paste longer descriptions of what each function does.
#' @include docList.R


docConcordance <- setClass("docConcordance",
                           slots = c(directory   = "character",
                                     indexFile   = "character",
                                     term        = "character",
                                     context     = "numeric",
                                     concordance = "list"
                           )
)

