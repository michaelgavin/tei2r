# An S4 class to represent a topic model.
# 
# \code{docModel} contains
# a structured list of information about a topic model.
# 
# @slot directory      A string that gives the filepath to the main directory.
# @slot index          A data frame that holds the meta-data for each document in
#                      the collection.
# @slot topics         A data frame that holds the most frequent words in each topic, for easy viewing.                     
# @slot frequencies    A matrix showing how frequent each topic is in each document.
# @slot weights        A list showing how heavily weighted each word is in each topic.
# @slot termMatrix     A complete matrix of every word in the collection, and its representation
#                      in each topic (rarely consulted directly).
# 
# 
# @section What it does:
# The \code{docModel} object provides an easy way of handling R objects
# creating while using the \code{mallet} package. It holds an index of your files along with
# all of the results from the model, which are formatted for easy viewing.
# 
# 
# @seealso buildModel
docModel <- setClass("docModel",
                            slots = c(directory    = "character",
                                      index        = "data.frame",
                                      topics       = "data.frame",
                                      frequencies  = "matrix",
                                      weights      = "list",
                                      termMatrix   = "matrix",
                                      malletObj    = "jobjRef"
                            )
                     )


