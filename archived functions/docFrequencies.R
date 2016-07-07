# An S4 class that contains and manages word-frequency data
#  
# \code{docFrequencies} creates a special \code{tei2r} object that holds word-frequency data for each document.
# 
# @slot directory A string that gives the filepath to the main directory
# @slot indexFile A string providing the filepath to the index (usually a .csv file)
# @slot raw A list of named vectors, showing the raw frequency of each word in each document
# @slot proportional A list of vectors showing the proportional frequency of each word
# @slot vocabulary A vector that holds the frequencies of each word across
#                  the whole collection.
# @slot proportionalVocab A vector that holds the proportional frequency of
#                         of each word over the entire collection.
# 
# @section What it does:
# The \code{docFrequencies} object is a structured list that hold word frequencies
# for each document in the collection, as well as summary data about the collection
# as a whole.
# @include docList.R docTexts.R
# @export
docFrequencies <- setClass("docFrequencies",
                           slots = c(directory         = "character",
                                     indexFile         = "character",
                                     proportional      = "list",
                                     raw               = "list",
                                     vocabulary        = "array",
                                     proportionalVocab = "array"
                           ))

