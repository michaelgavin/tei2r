getTexts = function(dl) {
  dt = getDocTexts(dl)
  return(dt)
}

getFrequencies = function(dt) {
  df = getDocFrequencies(dt)
  return(df)
}

getConcordance = function(dt, term, context) {
  dc = getDocConcordance(dt, term, context)
  return(dc)
}

getAssociations = function(dc, df) {
  da = getDocAssociations(dc, df)
  return(da)
}
