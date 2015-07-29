# An S4 class that holds associations across the document based
# on concordances.
# 
# \code{docAssociations} creates a \code{tei2r} object that
# 
# @slot directory      A string that retains the directory for the
#                      corpus that the \code{docAssociations} object
#                      is associated with. This is carried over from the 
#                      \code{docConcordance} object that is passed in to the 
#                      \code{getDocAssociations} function.
# @slot indexFile      A string that retains the path to the index file
#                      for the corpus that the \code{docAssociations}
#                      object is associated with.This is carried over from the 
#                      \code{docConcordance} object that is passed in to the 
#                      \code{getDocAssociations} function.
# @slot keyword        A string that retains the term that the
#                      \code{docAssociations} object provides associations
#                      for. This is carried over from the \code{docConcordance}
#                      object that is passed in to the \code{getDocAssociations}
#                      function.
# @slot context        A numeric that holds a reference to the context with which
#                      the concordance for \code{term} was created.This is carried 
#                      over from the \code{docConcordance} object that is passed in 
#                      to the \code{getDocAssociations} function.
# @slot associations   A list that contains a list of the frequencies of the words
#                      that are associated with \code{term} for each document.  The
#                      number of elements in the list will be equal to the number of 
#                      documents in the corpus.
# @slot proportions    A list that contains a list of the proportional frequency of the
#                      words associated with \code{term}.  This is derived by determining
#                      the ratio of the word's frequency of appearance with \code{term} 
#                      (co-occurrance) to its proportional frequency accross the whole
#                      document.  This enables an assesment of the importance of the 
#                      word's co-occurrance with \code{term}.  Exceedingly high values,
#                      for example, likely indicate that the word only appears with
#                      \code{term} in the document. The number of elements in the list 
#                      will be equal to the number of documents in the corpus.
# 
# @section What it does:
# Placeholder
# @include docList.R docConcordance.R docFrequencies.R
# @export
docAssociations <- setClass("docAssociations",
                            slots = c(directory    = "character",
                                      indexFile    = "character",
                                      keyword      = "character",
                                      context      = "numeric",
                                      associations = "list",
                                      total        = "array"
                            )
)
