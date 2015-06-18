
tfidf = function(df, term = "", all=F, limit = 500) {
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
}

