
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

