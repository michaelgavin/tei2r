getTexts = function(dl) {
  dt = getDocTexts(dl)
  return(dt)
}

getFrequencies = function(dt) {
  df = getDocFrequencies(dt)
  return(df)
}

getConcordance = function(df, term, context) {
  dc = getDocConcordance(df, term, context)
  return(dc)
}

getAssociations = function(dc, df) {
  da = getDocConcordance(dc, df)
  return(da)
}
