#====================================================================
# This function calculates the Term Frequency Inverse Document
# Frequency in a variety of situations, including: a single term across
# all documents, multiple terms, all terms, all terms [with limit]. The
# first parameter is either a docFrequencies, docConcordance, or
# docAssociations object, the second is the term or terms, the third
# is a boolean for all terms, and the last is a numerical limit for
# the number of terms included in all.
#
# Returns a list of words and their tfidf value in each document.
#
#====================================================================
#'
#' This function calculates the Term Frequency Inverse Document
#' Frequency in a variety of situations, including: a single term
#' across all documents, multiple terms, all terms, all terms (with limit)
#' for \code{docFrequencies}, \code{docConcordance}, or \code{docAssociations}
#' objects.
#' 
#' @section What It Does:
#' 
#' The Term Frequency Inverse Document Frequency is determined
#' by multiplying the frequency of a term in a document by the
#' log of the total number of documents divided by the number
#' of documents in which the term appears:
#' 
#' f(term) * log(numDocuments/numDocumentsWithTerm)
#' 
#' @param df      A \code{tei2r} object of class \code{docFrequencies}, 
#'                \code{docConcordance}, or \code{docAssociations}. This
#'                object contains the terms to calculate the \code{tfidf}
#'                for.
#'               
#' @param term    The term or list of terms (\code{c("termOne", "termTwo")})
#'                to calculate \code{tfidf} for.
#'               
#' @param all     A boolean value that tells the function whether to calculate
#'                \code{tfidf} for all terms or not (limited to 1000 by default).
#'               
#' @param limit   A numerical value that tells the function how many terms to limit
#'                \code{all} to.  Defaults to 1000.
#'               
#' @return tf.idf A list of numerical values for each \code{term} that represents
#'                their \code{tfidf} value in each document.
#'                
#' @examples
#' tf = tfidf(df, term="just")
#' tf = tfidf(df, term=c('just', 'right'))
#' tf = tfidf(df, all=T)
#' tf = tfidf(df, all=T, limit=5000)
#' 
#' tf = tfidf(dc, all=T)
#' 
#' tf = tfidf(da, all=T)
#' @name tfidf
NULL
#' @rdname tfidf
tfidf = function(df, term = "", all=F, limit = 1000) {
  if (class(df) == 'docAssociations') {
    tf.idf = tfidfAssociation(df, term, all)
    return(tf.idf)
  } else if(class(df) == 'docConcordance') {
    tf.idf = tfidfConcordance(df, term, all)
    return(tf.idf)
  } else if(class(df) == 'docFrequencies') {
    if(!all && length(term) == 1) {
      numdocs = 0
      termFrequency = c()
      for(i in 1:length(df@raw)) {
        if(term %in% names(df@raw[[i]])) {
          numdocs = numdocs + 1
        }
        if(length(df@raw[[i]][which(names(df@raw[[i]]) == term)]) > 0){
          termFrequency[[i]] = df@raw[[i]][which(names(df@raw[[i]]) == term)]
        } else {
          termFrequency[[i]] = 0
        }
      }
      names(termFrequency) = names(df@raw)
      idf = log((length(df@raw))/numdocs)
      tf.idf = termFrequency * idf
    } else if(!all && length(term) > 1) {
      eachIdf = c()
      for(i in 1:length(term)) {
        eachIdf[[i]] = tfidf(df, term[i], all=F)
        names(eachIdf)[i] = term[i]
      }
      return(eachIdf)
    } else {
      eachIdf = c()
      for(i in 1:limit) {
        eachIdf[[i]] = tfidf(df, names(df@vocabulary[i]), all=F)
        names(eachIdf)[i] = names(df@vocabulary)[i]
      }
      names(eachIdf) = names(df@vocabulary)[1:limit]
      return(eachIdf)
    }
    return(tf.idf)
  } else {
    print("Please provide a compatible object: docFrequencies, docConcordance, or DocAssociations.")
  }
}

#' @rdname tfidf
tfidfAssociation = function(da, term = "", all=F){
  if(!all && length(term) == 1) {
    numdocs = 0
    termFrequency = c()
    for(i in 1:length(da@associations)) {
      if(term %in% names(da@associations[[i]])) {
        numdocs = numdocs + 1
      }
      if(length(da@associations[[i]][which(names(da@associations[[i]]) == term)]) > 0){
        termFrequency[[i]] = da@associations[[i]][which(names(da@associations[[i]]) == term)]
      } else {
        termFrequency[[i]] = 0
      }
    }
    names(termFrequency) = names(da@associations)
    idf = log((length(da@associations))/numdocs)
    tf.idf = termFrequency * idf
  } else if(!all && length(term) > 1) {
    eachIdf = c()
    for(i in 1:length(term)) {
      eachIdf[[i]] = tfidf(da, term[i], all=F)
      names(eachIdf)[i] = term[i]
    }
    return(eachIdf)
  } else {
    eachIdf = c()
    for(i in 1:length(da@associations)) {
#       for (j in 1:length(da@associations[[i]])) {
#         if(i == 1){
#           eachIdf[[i]] = tfidfAssociation(da, names(da@associations[i][j]), all=F)
#         } else {
#           eachIdf[[i]] = tfidfAssociation(da, names(da@associations[i][j]), all=F)
#           names(eachIdf)[i] = names(da@associations)[i]
#         }
#       }
     eachIdf[[i]] = tfidfAssociation(da, names(da@associations[[i]]), all=F)
     names(eachIdf)[i] = names(da@associations)[i]
    }
    names(eachIdf) = names(da@associations)
    return(eachIdf)
  }
  return(tf.idf)
}

#' @rdname tfidf
tfidfConcordance = function(dc, term, all){
  tempText = ""
  associations = list()
  for(i in 1:length(dc@concordance)) {
    tempText = unlist(dc@concordance[[i]])
    associations[[i]] = rev(sort(table(tempText)))
  }
  names(associations) = names(dc@concordance)
  if(!all && length(term) == 1) {
    numdocs = 0
    termFrequency = c()
    for(i in 1:length(associations)) {
      if(term %in% names(associations[[i]])) {
        numdocs = numdocs + 1
      }
      if(length(associations[[i]][which(names(associations[[i]]) == term)]) > 0){
        termFrequency[[i]] = associations[[i]][which(names(associations[[i]]) == term)]
      } else {
        termFrequency[[i]] = 0
      }
    }
    names(termFrequency) = names(associations)
    idf = log((length(associations))/numdocs)
    tf.idf = termFrequency * idf
  } else if(!all && length(term) > 1) {
    eachIdf = c()
    for(i in 1:length(term)) {
      eachIdf[[i]] = tfidf(da, term[i], all=F)
      names(eachIdf)[i] = term[i]
    }
    return(eachIdf)
  } else {
    eachIdf = c()
    for(i in 1:length(associations)) {
      eachIdf[[i]] = tfidfConcordance(dc, names(associations[[i]]), all=F)
      names(eachIdf)[i] = names(associations)[i]
    }
    names(eachIdf) = names(associations)
    return(eachIdf)
  }
  return(tf.idf)
}

