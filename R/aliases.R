texts = function(dl) {
  dt = getDocTexts(dl)
  return(dt)
}

frequencies = function(dt) {
  df = getDocFrequencies(dt)
  return(df)
}

concordance = function(dt, term, context) {
  dc = getDocConcordance(dt, term, context)
  return(dc)
}

associations = function(dc, df) {
  da = getDocAssociations(dc, df)
  return(da)
}
