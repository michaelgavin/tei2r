#' An S4 class that holds a concordance for one term
#' 
#' \code{docConcordance} creates a special \code{tei2r} object called a concordance
#' 
#' @slot directory A string that gives the filepath to the main directory
#' @slot indexFile A string providing the filepath to the index (usually a .csv file)
#' @slot keyword A string providing the keyword
#' @slot context An integer providing the context window, before and after the keyword
#' @slot concordance A list of "keyword-in-context" character vectors.
#' 
#' @section What it does:
#' The docConcordance object is a structured list that holds all uses of a keyword
#' found in a collection.
#' @include docList.R
docConcordance <- setClass("docConcordance",
                           slots = c(directory   = "character",
                                     indexFile   = "character",
                                     keyword     = "character",
                                     context     = "numeric",
                                     concordance = "list"
                           )
)

